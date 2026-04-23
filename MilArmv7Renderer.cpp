/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Micron language project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include "MilArmv7Renderer.h"
#include <Micron/MicAtom.h>
#include <QtDebug>
#include <QFile>
using namespace Mil;
using namespace Vm;
using namespace Arm;

static const int Slot4 = 4;  // i4, u4, r4, p
static const int Slot8 = 8;  // i8, u8, r8, pp

// Convert Micron symbol names to ELF-compatible names: module!proc -> module$proc
static inline QByteArray elfSymName(const QByteArray& micronName)
{
    QByteArray result = micronName;
    result.replace('!', '$');
    return result;
}

static const int FP_TO_ARGS = 8;


Renderer::Renderer(AstModel* mdl)
    : d_mdl(mdl), d_code(mdl, 4, 4), d_hasHwDiv(true), d_emitDwarf(false), d_useAapcs(false),
      d_dwarf(0), d_localsSize(0), d_argsSize(0), d_returnSize(0), d_textSymIdx(0), d_dataSymIdx(0), d_rodataSymIdx(0), d_globalsSymIdx(0), d_elf(ElfWriter::ArchARM)
{
    d_code.setReverseArguments(true);
    memset(&d_sections, 0, sizeof(d_sections));

    registerExternals();
}

void Renderer::registerExternals()
{
    // Register all extern/foreign procedures from all modules so VmCode::compile()
    // can resolve cross-module calls. These become external symbol references in the
    // ELF object, resolved at link time.

    quint32 id = 0;
    foreach (Declaration* mod, d_mdl->getModules()) {
        const char* modName = mod->name.constData();
        for (Declaration* sub = mod->subs; sub; sub = sub->next) {
            if (sub->kind == Declaration::Procedure && (sub->extern_ || sub->foreign_)) {
                d_code.addExternal(modName, sub->name.constData(), id++);
            }
            // Also handle type-bound procedures in structs/objects
            if (sub->kind == Declaration::TypeDecl && sub->getType()) {
                Type* t = sub->getType();
                foreach (Declaration* msub, t->subs) {
                    if (msub->kind == Declaration::Procedure && (msub->extern_ || msub->foreign_)) {
                        d_code.addExternal(modName, msub->name.constData(), id++);
                    }
                }
            }
        }
    }
}

quint32 Renderer::getOrCreateExtSymbol(int procIdx)
{
    if (d_extProcSymbols.contains(procIdx))
        return d_extProcSymbols[procIdx];

    Vm::Procedure* proc = d_code.getProc(procIdx);
    Q_ASSERT(proc && proc->decl);

    QByteArray name;
    if (proc->decl->kind == Declaration::Module)
        name = elfSymName(proc->decl->name + QByteArray("$begin$"));
    else
        name = elfSymName(proc->decl->toPath());
    // Create an UNDEF GLOBAL symbol (section=0 means SHN_UNDEF)
    quint32 symIdx = d_elf.addSymbol(name, 0 /*SHN_UNDEF*/, 0, 0, STB_GLOBAL, STT_FUNC);
    d_extProcSymbols[procIdx] = symIdx;
    return symIdx;
}

quint32 Renderer::getOrEmitVtable(int vtableIdx)
{
    if (d_vtableOffsets.contains(vtableIdx))
        return d_vtableOffsets[vtableIdx];

    Vm::Vtable* vt = d_code.getVtable(vtableIdx);
    Q_ASSERT(vt);

    // Align to 4 bytes
    quint32 curSize = d_elf.sectionSize(d_sections.data);
    quint32 pad = (4 - (curSize & 3)) & 3;
    if (pad > 0) {
        char zeros[4] = {0};
        d_elf.appendToSection(d_sections.data, zeros, pad);
    }
    quint32 vtableOff = d_elf.sectionSize(d_sections.data);

    // Emit method pointers (zeros with relocations)
    for (size_t i = 0; i < vt->methods.size(); i++) {
        quint32 zero = 0;
        d_elf.appendToSection(d_sections.data, (const char*)&zero, 4);
        int procIdx = -1;
        for (int p = 0; p < d_code.procCount(); p++) {
            if (d_code.getProc(p) == vt->methods[i]) {
                procIdx = p;
                break;
            }
        }
        if (procIdx >= 0) {
            quint32 symIdx;
            if (d_procSymbols.contains(procIdx))
                symIdx = d_procSymbols[procIdx];
            else
                symIdx = getOrCreateExtSymbol(procIdx);
            d_fixups.recordDataRef(vtableOff + i * 4, symIdx);
        }
    }
    d_vtableOffsets[vtableIdx] = vtableOff;
    return vtableOff;
}

void Renderer::emitVtableFixupAt(Register destReg, quint32 offset, Type* t,
                                  const char* memData, quint32 memSize)
{
    using namespace Vm;
    while (t && t->kind == Type::NameRef)
        t = t->getType();
    if (!t)
        return;

    Emitter& em = d_emitter;
    FixupTracker& fix = d_fixups;

    if (t->kind == Type::Object && offset + 4 <= memSize) {
        // Extract vtable pointer from template mem at this offset.
        // NOTE: The VM's initMemory writes sizeof(Vtable*) bytes (8 on x86_64 host)
        // into template memory, but the template is sized for ARM's 4-byte pointers.
        // We must only read 4 bytes here (ARM pointer width) and compare against the
        // low 4 bytes of each host vtable pointer to avoid cross-element corruption.
        quint32 rawVtBits = 0;
        memcpy(&rawVtBits, memData + offset, 4);
        if (rawVtBits) {
            int vtIdx = -1;
            for (int i = 0; i < d_code.vtableCount(); i++) {
                quint32 vtLow;
                Vtable* vt = d_code.getVtable(i);
                memcpy(&vtLow, &vt, 4); // low 4 bytes of host pointer
                if (vtLow == rawVtBits) {
                    vtIdx = i;
                    break;
                }
            }
            if (vtIdx >= 0) {
                quint32 vtOff = getOrEmitVtable(vtIdx);
                // Load vtable address
                quint32 movwOff = em.currentPosition();
                em.movw_(AL, R2, vtOff & 0xFFFF);
                em.movt_(AL, R2, (vtOff >> 16) & 0xFFFF);
                fix.recordExternalAddr(movwOff, d_dataSymIdx);
                // Store vtable pointer
                if (offset == 0) {
                    em.str_(AL, R2, MemOp(destReg, 0));
                } else {
                    em.str_(AL, R2, MemOp(destReg, (qint32)offset));
                }
            }
        }
        // Recurse into fields
        foreach (Declaration* field, t->subs) {
            if (field->kind == Declaration::Field) {
                Type* ft = field->getType();
                while (ft && ft->kind == Type::NameRef)
                    ft = ft->getType();
                if (ft && ft->objectInit)
                    emitVtableFixupAt(destReg, offset + field->f.off, ft, memData, memSize);
            }
        }
    } else if (t->kind == Type::Struct) {
        foreach (Declaration* field, t->subs) {
            if (field->kind == Declaration::Field) {
                Type* ft = field->getType();
                while (ft && ft->kind == Type::NameRef)
                    ft = ft->getType();
                if (ft && ft->objectInit)
                    emitVtableFixupAt(destReg, offset + field->f.off, ft, memData, memSize);
            }
        }
    } else if (t->kind == Type::Array && t->len != 0) {
        Type* et = t->getType();
        while (et && et->kind == Type::NameRef)
            et = et->getType();
        if (et && et->objectInit) {
            quint32 elemSize = et->getByteSize(4); // 4 = ARM pointer width
            for (int i = 0; i < t->len; i++) {
                emitVtableFixupAt(destReg, offset + i * elemSize, et, memData, memSize);
            }
        }
    }
}

void Renderer::emitVtableFixups(Register destReg, const Vm::Template& tmpl)
{
    if (tmpl.type && tmpl.type->objectInit && tmpl.mem.size() >= 4) {
        emitVtableFixupAt(destReg, 0, tmpl.type, tmpl.mem.data(), tmpl.mem.size());
    }
}

void Renderer::initDataVtable(Type* t, quint32 baseOff)
{
    while (t && t->kind == Type::NameRef)
        t = t->getType();
    if (!t)
        return;

    if (t->kind == Type::Object) {
        int vtIdx = d_code.findVtableIdx(t);
        if (vtIdx >= 0) {
            quint32 vtOff = getOrEmitVtable(vtIdx);
            d_elf.addRelocation(d_sections.relData, baseOff, d_dataSymIdx, 2 /*R_ARM_ABS32*/);
            d_elf.patchWord(d_sections.data, baseOff, vtOff);
        }
        foreach (Declaration* field, t->subs) {
            if (field->kind == Declaration::Field) {
                Type* ft = field->getType();
                while (ft && ft->kind == Type::NameRef)
                    ft = ft->getType();
                if (ft && ft->objectInit)
                    initDataVtable(ft, baseOff + field->f.off);
            }
        }
    } else if (t->kind == Type::Struct) {
        foreach (Declaration* field, t->subs) {
            if (field->kind == Declaration::Field) {
                Type* ft = field->getType();
                while (ft && ft->kind == Type::NameRef)
                    ft = ft->getType();
                if (ft && ft->objectInit)
                    initDataVtable(ft, baseOff + field->f.off);
            }
        }
    } else if (t->kind == Type::Array && t->len != 0) {
        Type* et = t->getType();
        while (et && et->kind == Type::NameRef)
            et = et->getType();
        if (et && et->objectInit) {
            quint32 elemSize = et->getByteSize(4);
            for (int i = 0; i < t->len; i++) {
                initDataVtable(et, baseOff + i * elemSize);
            }
        }
    }
}

void Renderer::initGlobalVarVtables(Declaration* module)
{
    // Fix vtable pointers in global variables (.data section)
    QList<Declaration*> vars = module->getVars();
    for (int v = 0; v < vars.size(); v++) {
        Declaration* var = vars[v];
        Type* t = var->getType();
        while (t && t->kind == Type::NameRef)
            t = t->getType();
        if (t && t->objectInit)
            initDataVtable(t, var->off);
    }
}

// Emit runtime ARM code to store a vtable pointer for an object (or nested
// objects/arrays) at 'baseOff' within the MIC$GLOBALS COMMON area.
// Called during $begin$ rendering so that global object variables get their
// vtable pointers initialized at runtime (they live in BSS, not .data).
void Renderer::emitRuntimeVtableInit(Type* t, quint32 baseOff)
{
    using namespace Arm;
    while (t && t->kind == Type::NameRef)
        t = t->getType();
    if (!t)
        return;

    if (t->kind == Type::Object) {
        int vtIdx = d_code.findVtableIdx(t);
        if (vtIdx >= 0) {
            quint32 vtOff = getOrEmitVtable(vtIdx);
            // MOVW R0, #lo(baseOff); MOVT R0, #hi(baseOff)  -> addr in MIC$GLOBALS
            quint32 movwOff0 = d_emitter.currentPosition();
            d_emitter.movw_(AL, R0, baseOff & 0xFFFF);
            d_emitter.movt_(AL, R0, (baseOff >> 16) & 0xFFFF);
            d_fixups.recordExternalAddr(movwOff0, d_globalsSymIdx);

            // MOVW R1, #lo(vtOff); MOVT R1, #hi(vtOff)  -> addr of vtable in .data
            quint32 movwOff1 = d_emitter.currentPosition();
            d_emitter.movw_(AL, R1, vtOff & 0xFFFF);
            d_emitter.movt_(AL, R1, (vtOff >> 16) & 0xFFFF);
            d_fixups.recordExternalAddr(movwOff1, d_dataSymIdx);

            // STR R1, [R0]  -> store vtable pointer
            d_emitter.str_(AL, R1, MemOp(R0, 0));
        }
        foreach (Declaration* field, t->subs) {
            if (field->kind == Declaration::Field) {
                Type* ft = field->getType();
                while (ft && ft->kind == Type::NameRef)
                    ft = ft->getType();
                if (ft && ft->objectInit)
                    emitRuntimeVtableInit(ft, baseOff + field->f.off);
            }
        }
    } else if (t->kind == Type::Struct) {
        foreach (Declaration* field, t->subs) {
            if (field->kind == Declaration::Field) {
                Type* ft = field->getType();
                while (ft && ft->kind == Type::NameRef)
                    ft = ft->getType();
                if (ft && ft->objectInit)
                    emitRuntimeVtableInit(ft, baseOff + field->f.off);
            }
        }
    } else if (t->kind == Type::Array && t->len != 0) {
        Type* et = t->getType();
        while (et && et->kind == Type::NameRef)
            et = et->getType();
        if (et && et->objectInit) {
            quint32 elemSize = et->getByteSize(4);
            for (int i = 0; i < t->len; i++) {
                emitRuntimeVtableInit(et, baseOff + i * elemSize);
            }
        }
    }
}

// Emit runtime vtable initialization for all global object variables of a module.
// This replaces the static initGlobalVarVtables when variables live in COMMON/BSS.
void Renderer::emitGlobalVarVtableInits(Declaration* module)
{
    QList<Declaration*> vars = module->getVars();
    for (int v = 0; v < vars.size(); v++) {
        Declaration* var = vars[v];
        Type* t = var->getType();
        while (t && t->kind == Type::NameRef)
            t = t->getType();
        if (t && t->objectInit)
            emitRuntimeVtableInit(t, var->off);
    }
}

quint32 Renderer::getOrEmitTemplate(quint32 templateIdx)
{
    if (d_templateOffsets.contains(templateIdx))
        return d_templateOffsets[templateIdx];

    const Vm::Template& tmpl = d_code.getTemplate(templateIdx);
    quint32 tmplSize = tmpl.mem.size();

    quint32 curSize = d_elf.sectionSize(d_sections.rodata);
    quint32 pad = (4 - (curSize & 3)) & 3;
    if (pad > 0) {
        char zeros[4] = {0};
        d_elf.appendToSection(d_sections.rodata, zeros, pad);
    }
    quint32 tmplOff = d_elf.sectionSize(d_sections.rodata);
    d_elf.appendToSection(d_sections.rodata, tmpl.mem.data(), tmplSize);
    d_templateOffsets[templateIdx] = tmplOff;
    return tmplOff;
}

Renderer::~Renderer()
{
    clearLabels();
    delete d_dwarf;
}

bool Renderer::renderModule(Declaration* module)
{
    Q_ASSERT(module && module->kind == Declaration::Module);

    // Lower MIL AST to VmCode
    if (!d_code.compile(module))
        return setError(QString("VmCode compilation failed for module %1").arg(QString(module->name)));

    // Compile ALL procedures in this module (not just those reachable from init).
    for (Declaration* d = module->subs; d; d = d->next) {
        if (d->kind == Declaration::Procedure && !d->forward && !d->extern_ && !d->foreign_) {
            if (d_code.findProc(d) < 0) {
                if (!d_code.compileProc(d))
                    return setError(QString("VmCode compilation failed for procedure %1").arg(QString(d->toPath())));
            }
        }
        if (d->kind == Declaration::TypeDecl && d->getType()) {
            Type* t = d->getType();
            if (t->kind == Type::Object || t->kind == Type::Struct) {
                foreach (Declaration* sub, t->subs) {
                    if (sub->kind == Declaration::Procedure && !sub->forward && !sub->extern_ && !sub->foreign_) {
                        if (d_code.findProc(sub) < 0) {
                            if (!d_code.compileProc(sub))
                                return setError(QString("VmCode compilation failed for procedure %1").arg(QString(sub->toPath())));
                        }
                    }
                }
            }
        }
    }

    // Set up ELF sections
    d_sections = d_elf.createStandardSections();
    d_sections.micronMod = d_elf.addSection(QByteArray(".micron.mod"), SHT_PROGBITS, 0, 1);

    d_textSymIdx = d_elf.addSectionSymbol(d_sections.text);
    d_dataSymIdx = d_elf.addSectionSymbol(d_sections.data);
    d_rodataSymIdx = d_elf.addSectionSymbol(d_sections.rodata);

    // Create DWARF debug sections and their LOCAL section symbols BEFORE any
    // GLOBAL symbols are added.  ElfWriter::addSymbol inserts LOCAL symbols
    // before globals, which shifts global indices.  By creating debug section
    // symbols here, we ensure no already-emitted relocations are invalidated.
    if (d_emitDwarf) {
        delete d_dwarf;
        d_dwarf = new DwarfEmitter(d_elf);
        QByteArray srcFile = module->name + ".mic";
        QByteArray srcDir;
        if (module->md && !module->md->source.isEmpty())
            srcDir = module->md->source.toUtf8();
        d_dwarf->beginCompilationUnit(srcFile, srcDir);
    }

    // Create a COMMON symbol for the shared module variable memory.
    // Each module's .o defines the same COMMON symbol; the linker merges them
    // into a single BSS allocation so all modules reference the same addresses.
    quint32 varMemSize = d_mdl->getVarMemSize();
    if (varMemSize > 0) {
        static const quint16 SHN_COMMON = 0xFFF2;
        d_globalsSymIdx = d_elf.addSymbol("MIC$GLOBALS", SHN_COMMON,
                                           4 /*alignment*/, varMemSize,
                                           STB_GLOBAL, STT_OBJECT);
    }

    // Reserve 4 bytes in .data for module init guard (initialized to 0).
    quint32 initGuardOff = d_elf.sectionSize(d_sections.data);
    {
        quint32 zero = 0;
        d_elf.appendToSection(d_sections.data, (const char*)&zero, 4);
    }

    // Collect all procedure declarations
    struct ProcInfo {
        Declaration* decl;
        int codeIdx;
    };
    QList<ProcInfo> allProcs;

    {
        int idx = d_code.findProc(module);
        if (idx >= 0) {
            ProcInfo pi;
            pi.decl = module;
            pi.codeIdx = idx;
            allProcs.append(pi);
        }
    }

    for (Declaration* d = module->subs; d; d = d->next) {
        if (d->kind == Declaration::Procedure && !d->forward && !d->entryPoint) {
            int idx = d_code.findProc(d);
            if (idx >= 0) {
                ProcInfo pi;
                pi.decl = d;
                pi.codeIdx = idx;
                allProcs.append(pi);
            }
        }
        if (d->kind == Declaration::TypeDecl && d->getType()) {
            Type* t = d->getType();
            if (t->kind == Type::Object || t->kind == Type::Struct) {
                foreach (Declaration* sub, t->subs) {
                    if (sub->kind == Declaration::Procedure && !sub->forward) {
                        int idx = d_code.findProc(sub);
                        if (idx >= 0) {
                            ProcInfo pi;
                            pi.decl = sub;
                            pi.codeIdx = idx;
                            allProcs.append(pi);
                        }
                    }
                }
            }
        }
    }

    // Create ELF symbols for all procedures (offset=0, updated later)
    for (int i = 0; i < allProcs.size(); i++) {
        Declaration* d = allProcs[i].decl;
        QByteArray name;
        if (d->kind == Declaration::Module)
            name = elfSymName(d->name + QByteArray("$begin$"));
        else
            name = elfSymName(d->toPath());

        // Module init symbols must be GLOBAL (they're called by dependent modules).
        // Exported procedures (public_) are also GLOBAL. Everything else is LOCAL.
        // In AAPCS mode (C-compatible linking), all procedures must be GLOBAL
        // so the linker can resolve cross-module references (analogous to x86 cdeclReturns).
        bool isExported = (d->kind == Declaration::Module) || d->public_ || d_useAapcs;
        quint8 binding = isExported ? STB_GLOBAL : STB_LOCAL;

        quint32 symIdx = d_elf.addSymbol(name, d_sections.text, 0, 0, binding, STT_FUNC);
        d_procSymbols[allProcs[i].codeIdx] = symIdx;
    }

    // Pre-emit string constants to .rodata
    // We'll emit them lazily in emitOp when we first encounter ldstr/ldc_r4/ldc_r8

    // Render each procedure
    for (int i = 0; i < allProcs.size(); i++) {
        int procIdx = allProcs[i].codeIdx;
        Procedure* proc = d_code.getProc(procIdx);
        Q_ASSERT(proc);

        // Record the .text offset for this procedure
        d_procTextOffsets[procIdx] = d_emitter.currentPosition();

        // Update the ELF symbol with the correct offset
        d_elf.setSymbolValue(d_procSymbols[procIdx], d_emitter.currentPosition());

        quint32 procStartPC = d_emitter.currentPosition();

        Arm::Label initSkipLabel;
        bool isModuleInit = (allProcs[i].decl->kind == Declaration::Module);
        if (isModuleInit) {
            // Load guard address via MOVW/MOVT + relocation against .data
            quint32 movwOff = d_emitter.currentPosition();
            d_emitter.movw_(AL, R0, initGuardOff & 0xFFFF);
            d_emitter.movt_(AL, R0, (initGuardOff >> 16) & 0xFFFF);
            d_fixups.recordExternalAddr(movwOff, d_dataSymIdx);
            // Load guard value
            d_emitter.ldr_(AL, R1, MemOp(R0, 0));
            // Test: if already initialized, skip
            d_emitter.cmp(R1, Operand2((quint32)0));
            d_emitter.b_(NE, initSkipLabel);
            // Mark as initialized
            d_emitter.mov(R1, Operand2((quint32)1));
            d_emitter.str_(AL, R1, MemOp(R0, 0));

            // Emit runtime vtable initialization for global object variables.
            // Must happen before the module body runs so that virtual calls work.
            if (d_globalsSymIdx)
                emitGlobalVarVtableInits(allProcs[i].decl);

            // NOTE: Import $begin$ calls are NOT emitted here because ARM's BL
            // clobbers LR before the procedure prologue (emitted by renderProcedure)
            // has a chance to save it. The MIL bytecode body already contains
            // the import init calls, so they are emitted by renderProcedure instead.
            // (On x86, the wrapper calls are harmless since CALL pushes the return
            // address on the stack, but on ARM they cause an infinite loop.)
        }

        if (!renderProcedure(*proc, procIdx))
            return false;

        if (isModuleInit) {
            d_emitter.bind(initSkipLabel);
            // Emit a return so the skip path returns cleanly
            if (d_useAapcs) {
                d_emitter.bx_(AL, LR);
            } else {
                d_emitter.bx_(AL, LR);
            }
        }

        if (d_dwarf) {
            Declaration* decl = allProcs[i].decl;
            quint32 procEndPC = d_emitter.currentPosition();

            QByteArray procName;
            if (decl->kind == Declaration::Module)
                procName = decl->name + "$begin$";
            else
                procName = decl->toPath();

            quint32 declLine = decl->pos.line();
            d_dwarf->addSubprogram(procName, procStartPC, procEndPC, declLine);

            // Add line entry at procedure start
            if (declLine > 0)
                d_dwarf->addLineEntry(procStartPC, declLine);

            if (decl->kind == Declaration::Procedure) {
                QList<Declaration*> params = decl->getParams();
                for (int p = 0; p < params.size(); p++) {
                    Declaration* param = params[p];
                    quint32 typeRef = d_dwarf->getOrCreateType(param->getType());
                    // Params are at FP + 8 + param->off
                    qint32 fpOff = FP_TO_ARGS + param->off;
                    d_dwarf->addParameter(param->name, fpOff, typeRef);
                }

                QList<Declaration*> locals = decl->getLocals();
                for (int l = 0; l < locals.size(); l++) {
                    Declaration* loc = locals[l];
                    quint32 typeRef = d_dwarf->getOrCreateType(loc->getType());
                    // Locals are at FP - localsSize + loc->off
                    qint32 fpOff = -((qint32)d_localsSize) + loc->off;
                    d_dwarf->addLocalVariable(loc->name, fpOff, typeRef);
                }
            }

            if (decl->kind == Declaration::Procedure && !decl->forward) {
                ProcedureData* pd = decl->getPd();
                if (pd && pd->end.isValid())
                    d_dwarf->addLineEntry(procEndPC > 0 ? procEndPC - 4 : 0, pd->end.line());
            }

            d_dwarf->endSubprogram();
        }
    }

    // Static initGlobalVarVtables is no longer needed: global variables live in
    // COMMON/BSS (MIC$GLOBALS), so vtable pointers are initialized at runtime
    // via emitGlobalVarVtableInits() emitted in the $begin$ guard block above.

    d_elf.appendToSection(d_sections.text, d_emitter.machineCode());

    d_fixups.generateRelocations(d_elf, d_sections.relText, d_sections.relData);

    QByteArray modContent = "module " + module->name + "\n";
    d_elf.appendToSection(d_sections.micronMod, modContent);

    if (d_dwarf) {
        QList<Declaration*> vars = module->getVars();
        for (int v = 0; v < vars.size(); v++) {
            Declaration* var = vars[v];
            quint32 typeRef = d_dwarf->getOrCreateType(var->getType());
            // var->off is the offset in the .data/.bss section
            d_dwarf->addModuleVariable(var->name, var->off, typeRef, var->pos.line());
        }
    }

    if (d_dwarf) {
        d_dwarf->endCompilationUnit();
        d_dwarf->finalize(d_textSymIdx, d_dataSymIdx, d_globalsSymIdx);
    }

    return true;
}

bool Renderer::writeToFile(const QString& filename)
{
    return d_elf.write(filename);
}

QByteArray Renderer::toByteArray()
{
    return d_elf.toByteArray();
}

bool Renderer::renderProcedure(Procedure& proc, int procIdx)
{
    // Compute locals size and args size.
    // In AAPCS mode, align locals to 8 bytes for SP alignment.
    if (d_useAapcs)
        d_localsSize = (proc.localsSize + 7) & ~7;
    else
        d_localsSize = (proc.localsSize + 3) & ~3;
    d_argsSize = proc.argsSize;
    d_returnSize = proc.returnSize;

    scanBranchTargets(proc);

    emitPrologue(d_localsSize);

    int pc = 0;
    while (pc < (int)proc.ops.size()) {
        // Bind label if any branch targets this PC
        bindLabelIfNeeded(pc);

        int consumed = emitOp(proc, pc);
        if (consumed < 0)
            return false; // error already set
        pc += consumed;
    }

    bindLabelIfNeeded(pc);

    // Always emit an epilogue at end of procedure as safety net.
    // The VmCode may not always end with LL_ret/LL_ret_void (e.g., module init
    // procedures, or procedures where the last statement is not an explicit return).
    emitEpilogue();

    clearLabels();

    return true;
}

void Renderer::emitPrologue(quint32 localsSize)
{
    if (d_useAapcs) {
        // AAPCS mode: always dump R0-R3 to stack so args are at FP+8.
        // This ensures a uniform frame layout for all AAPCS functions.
        d_emitter.stmdb_(AL, SP, true, (1<<R0)|(1<<R1)|(1<<R2)|(1<<R3));
    }
    d_emitter.stmdb_(AL, SP, true, (1 << R11) | (1 << LR));
    d_emitter.mov(R11, Operand2(SP));
    if (localsSize > 0) {
        if (Operand2::isValidImmediate(localsSize)) {
            d_emitter.sub(SP, SP, Operand2(localsSize));
        } else {
            loadImm32(R12, localsSize);
            d_emitter.sub(SP, SP, Operand2(R12));
        }
    }
}

void Renderer::emitEpilogue()
{
    d_emitter.mov(SP, Operand2(R11));
    if (d_useAapcs) {
        // AAPCS mode: restore R11 and LR, return via BX LR.
        d_emitter.ldmia_(AL, SP, true, (1 << R11) | (1 << LR));
        if (d_returnSize <= 8) {
            // Small/void return: clean up the R0-R3 dump area.
            d_emitter.add(SP, SP, Operand2((quint32)16));
        }
        // For large returns (>8 bytes): do NOT clean dump area.
        // The return value was written to [FP+8..] by LL_ret and the
        // caller will read it from [SP] after we return.
        d_emitter.bx_(AL, LR);
    } else {
        // Old convention: POP {R11, PC} (returns by loading PC from saved LR)
        d_emitter.ldmia_(AL, SP, true, (1 << R11) | (1 << PC));
    }
}

void Renderer::loadImm32(Register rd, quint32 value)
{
    // Load a 32-bit immediate into a register (uses MOV/MVN for small, MOVW/MOVT for large).
    quint32 encoded;
    if (Operand2::isValidImmediate(value, &encoded)) {
        d_emitter.mov(rd, Operand2(value));
    } else if (Operand2::isValidImmediate(~value, &encoded)) {
        d_emitter.mvn_(AL, false, rd, Operand2(~value));
    } else {
        d_emitter.movw_(AL, rd, value & 0xFFFF);
        if (value > 0xFFFF)
            d_emitter.movt_(AL, rd, (value >> 16) & 0xFFFF);
    }
}

void Renderer::pushReg(Register r)
{
    d_emitter.str_(AL, r, MemOp(SP, -Slot4, MemOp::PreIndexed));
}

void Renderer::popReg(Register r)
{
    d_emitter.ldr_(AL, r, MemOp(SP, Slot4, MemOp::PostIndexed));
}

void Renderer::pushRegPair(Register rLo, Register rHi)
{
    // Push high word first (higher address), then low word
    d_emitter.sub(SP, SP, Operand2(Slot8));
    d_emitter.str_(AL, rLo, MemOp(SP, 0));
    d_emitter.str_(AL, rHi, MemOp(SP, 4));
}

void Renderer::popRegPair(Register rLo, Register rHi)
{
    d_emitter.ldr_(AL, rLo, MemOp(SP, 0));
    d_emitter.ldr_(AL, rHi, MemOp(SP, 4));
    d_emitter.add(SP, SP, Operand2(Slot8));
}

void Renderer::pushFloat(SRegister s)
{
    // Transfer S-reg to GPR, then push
    d_emitter.vmov_r_s(AL, R0, s);
    pushReg(R0);
}

void Renderer::popFloat(SRegister s)
{
    popReg(R0);
    d_emitter.vmov_s_r(AL, s, R0);
}

void Renderer::pushDouble(DRegister d)
{
    // Transfer D-reg to GPR pair, then push
    d_emitter.vmov_rr_d(AL, R0, R1, d);
    pushRegPair(R0, R1);
}

void Renderer::popDouble(DRegister d)
{
    popRegPair(R0, R1);
    d_emitter.vmov_d_rr(AL, d, R0, R1);
}


// Load address of a local variable into rd. milOffset is byte offset in locals area.
void Renderer::loadLocalAddr(Register rd, quint32 milOffset)
{
    // Local at FP - d_localsSize + milOffset
    qint32 fpOff = -((qint32)d_localsSize) + (qint32)milOffset;
    if (fpOff == 0) {
        d_emitter.mov(rd, Operand2(R11));
    } else if (fpOff > 0 && fpOff <= 255) {
        d_emitter.add(rd, R11, Operand2((quint32)fpOff));
    } else if (fpOff < 0 && (quint32)(-fpOff) <= 255) {
        d_emitter.sub(rd, R11, Operand2((quint32)(-fpOff)));
    } else if (fpOff > 0 && Operand2::isValidImmediate((quint32)fpOff)) {
        d_emitter.add(rd, R11, Operand2((quint32)fpOff));
    } else if (fpOff < 0 && Operand2::isValidImmediate((quint32)(-fpOff))) {
        d_emitter.sub(rd, R11, Operand2((quint32)(-fpOff)));
    } else {
        loadImm32(rd, (quint32)((fpOff < 0) ? -fpOff : fpOff));
        if (fpOff < 0)
            d_emitter.sub(rd, R11, Operand2(rd));
        else
            d_emitter.add(rd, R11, Operand2(rd));
    }
}

// Load address of an argument into rd. milOffset is byte offset in args area.
void Renderer::loadArgAddr(Register rd, quint32 milOffset)
{
    // Arg at FP + 8 + milOffset
    quint32 fpOff = FP_TO_ARGS + milOffset;
    if (Operand2::isValidImmediate(fpOff)) {
        d_emitter.add(rd, R11, Operand2(fpOff));
    } else {
        loadImm32(rd, fpOff);
        d_emitter.add(rd, R11, Operand2(rd));
    }
}

// Load address of a module variable into rd. milOffset is byte offset in module data.
void Renderer::loadVarAddr(Register rd, quint32 milOffset)
{
    quint32 movwOff = d_emitter.currentPosition();
    d_emitter.movw_(AL, rd, milOffset & 0xFFFF);
    quint32 movtOff = d_emitter.currentPosition();
    d_emitter.movt_(AL, rd, (milOffset >> 16) & 0xFFFF);

    // Record relocations against the shared MIC$GLOBALS COMMON symbol
    d_fixups.recordExternalAddr(movwOff, d_globalsSymIdx);
}


quint32 Renderer::emitArgAlignment(Declaration* decl)
{
    // The eval stack pushes arguments contiguously (each param's stack-aligned size,
    // no inter-param alignment gaps). But the callee reads args at aligned offsets
    // (computed by calcParamsLocalsLayout which inserts alignment padding between params).
    // This function expands the stack and moves args to their aligned positions.

    if (!decl)
        return 0;

    QList<Declaration*> params = decl->getParams(true);
    if (params.isEmpty())
        return 0;

    const quint8 ptrwidth = 4;
    const quint8 stackalig = 4;

    quint32 pushedSize = 0;
    for (int i = 0; i < params.size(); i++) {
        Type* t = params[i]->getType();
        int size = t->getByteSize(ptrwidth);
        pushedSize += qMax(size, (int)stackalig);
    }

    quint32 alignedSize = 0;
    {
        int off_p = 0;
        for (int i = 0; i < params.size(); i++) {
            Type* t = params[i]->getType();
            int size = t->getByteSize(ptrwidth);
            int alig = t->getAlignment(ptrwidth);
            off_p += AstModel::padding(off_p, alig);
            off_p += qMax(size, (int)stackalig);
        }
        alignedSize = d_code.stackAligned(off_p);
    }

    quint32 gap = alignedSize - pushedSize;
    if (gap == 0)
        return 0;

    Arm::Emitter& em = d_emitter;

    em.sub(SP, SP, Operand2(gap));

    struct ParamInfo {
        quint32 contigOff;  // offset from ESP (before expansion) in contiguous layout
        quint32 alignedOff; // offset from ESP (after expansion) in aligned layout
        quint32 size;       // stack-aligned size of this param
    };
    QVector<ParamInfo> pinfo(params.size());

    quint32 coff = 0;
    for (int i = 0; i < params.size(); i++) {
        Type* t = params[i]->getType();
        int size = t->getByteSize(ptrwidth);
        int ssize = qMax(size, (int)stackalig);
        pinfo[i].contigOff = coff;
        pinfo[i].size = ssize;
        coff += ssize;
    }

    {
        int off_p = 0;
        for (int i = 0; i < params.size(); i++) {
            Type* t = params[i]->getType();
            int size = t->getByteSize(ptrwidth);
            int alig = t->getAlignment(ptrwidth);
            off_p += AstModel::padding(off_p, alig);
            pinfo[i].alignedOff = off_p;
            off_p += qMax(size, (int)stackalig);
        }
    }

    for (int i = params.size() - 1; i >= 0; i--) {
        quint32 srcOff = gap + pinfo[i].contigOff;
        quint32 dstOff = pinfo[i].alignedOff;
        if (srcOff == dstOff)
            continue;
        // Move this param's words
        for (quint32 w = 0; w < pinfo[i].size; w += 4) {
            em.ldr_(AL, R0, MemOp(SP, srcOff + w));
            em.str_(AL, R0, MemOp(SP, dstOff + w));
        }
    }

    // Returns the total gap (argsSize - pushedSize) that was inserted.
    return gap;
}

// Pre-scan a procedure's ops to identify branch target PCs.
void Renderer::scanBranchTargets(Procedure& proc)
{
    for (int pc = 0; pc < (int)proc.ops.size(); pc++) {
        LL_op op = (LL_op)proc.ops[pc].op;
        if (op == LL_br || op == LL_brfalse_i4) {
            int target = pc + 1 + (proc.ops[pc].minus ? -1 : 1) * (int)proc.ops[pc].val;
            getLabel(target); // create if not exists
        }
    }
}

// Get or create a Label for a target MIL PC.
Label& Renderer::getLabel(int milPc)
{
    if (!d_branchLabels.contains(milPc)) {
        Label* lbl = new Label();
        d_branchLabels[milPc] = lbl;
        d_allLabels.append(lbl);
    }
    return *d_branchLabels[milPc];
}

// Bind a label at the current ARM code position if one exists for this MIL PC.
void Renderer::bindLabelIfNeeded(int milPc)
{
    if (d_branchLabels.contains(milPc)) {
        d_emitter.bind(*d_branchLabels[milPc]);
    }
}

// Clear per-procedure label state.
void Renderer::clearLabels()
{
    qDeleteAll(d_allLabels);
    d_allLabels.clear();
    d_branchLabels.clear();
}



void Renderer::emitCallStackAdj(quint32 argsSize, quint32 returnSize)
{
    // Emit stack adjustment after a call returns.
    // In the interpreter, call() pops argsSize and pushes returnSize.
    // In ARM, after callee returns, SP is where args were.
    // Return value is at [SP]. Adjust: SP += argsSize - returnSize.

    // After a call returns, SP points to the start of the arg area.
    // The return value (if any) is at [SP+0], stored by callee's LL_ret at [FP+8].
    // The interpreter does: pop(argsSize); push(returnValue, returnSize).
    // Net effect: SP += argsSize - returnSize.

    using namespace Arm;

    if (returnSize == 0) {
        // Void call: just pop all args
        if (argsSize > 0) {
            if (Operand2::isValidImmediate(argsSize))
                d_emitter.add(SP, SP, Operand2(argsSize));
            else {
                loadImm32(R12, argsSize);
                d_emitter.add(SP, SP, Operand2(R12));
            }
        }
    } else {
        // Value-returning call: return value is at [SP+0..SP+returnSize-1].
        qint32 adj = (qint32)argsSize - (qint32)returnSize;
        if (adj > 0) {
            // Move return value up by adj bytes (copy word by word, reverse order
            for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                d_emitter.ldr_(AL, R0, MemOp(SP, w));
                d_emitter.str_(AL, R0, MemOp(SP, w + adj));
            }
            if (Operand2::isValidImmediate(adj))
                d_emitter.add(SP, SP, Operand2(adj));
            else {
                loadImm32(R12, adj);
                d_emitter.add(SP, SP, Operand2(R12));
            }
        } else if (adj < 0) {
            // Return value is larger than args — need to grow stack
            quint32 neg = (quint32)(-adj);
            if (Operand2::isValidImmediate(neg))
                d_emitter.sub(SP, SP, Operand2(neg));
            else {
                loadImm32(R12, neg);
                d_emitter.sub(SP, SP, Operand2(R12));
            }
            // Copy return value down
            for (quint32 w = 0; w < returnSize; w += 4) {
                d_emitter.ldr_(AL, R0, MemOp(SP, (qint32)(w + neg)));
                d_emitter.str_(AL, R0, MemOp(SP, (qint32)w));
            }
        }
        // else adj == 0: return value is already in the right place
    }
}

bool Renderer::setError(const QString& msg)
{
    d_error = msg;
    qCritical() << "Armv7Renderer:" << msg;
    return false;
}

// Helper macros for common patterns
#define LOAD_LOCAL_I4(reg, off) \
    do { qint32 _fo = -((qint32)d_localsSize) + (qint32)(off); \
         d_emitter.ldr_(AL, reg, MemOp(R11, _fo)); } while(0)

#define STORE_LOCAL_I4(reg, off) \
    do { qint32 _fo = -((qint32)d_localsSize) + (qint32)(off); \
         d_emitter.str_(AL, reg, MemOp(R11, _fo)); } while(0)

#define LOAD_ARG_I4(reg, off) \
    d_emitter.ldr_(AL, reg, MemOp(R11, FP_TO_ARGS + (qint32)(off)))

#define STORE_ARG_I4(reg, off) \
    d_emitter.str_(AL, reg, MemOp(R11, FP_TO_ARGS + (qint32)(off)))

int Renderer::emitOp(Procedure& proc, int pc)
{
    // Emit a single LL_op. Returns the number of ops consumed (usually 1, 2 for OffSizeArgs).

    const Operation& op = proc.ops[pc];
    const LL_op opcode = (LL_op)op.op;
    const quint32 val = op.val;
    Arm::Emitter& em = d_emitter;
    Arm::FixupTracker& fix = d_fixups;

    switch (opcode) {

    // Integer Constants
    case LL_ldc_i4: {
        qint64 intVal = d_code.getInt(val);
        loadImm32(R0, (quint32)(qint32)intVal);
        pushReg(R0);
        return 1;
    }
    case LL_ldc_i8: {
        qint64 intVal = d_code.getInt(val);
        loadImm32(R0, (quint32)(intVal & 0xFFFFFFFF));
        loadImm32(R1, (quint32)((intVal >> 32) & 0xFFFFFFFF));
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_ldc_i4_m1:
        loadImm32(R0, 0xFFFFFFFF); pushReg(R0); return 1;
    case LL_ldc_i4_0:
        em.mov(R0, Operand2((quint32)0)); pushReg(R0); return 1;
    case LL_ldc_i4_1:
        em.mov(R0, Operand2((quint32)1)); pushReg(R0); return 1;
    case LL_ldc_i4_2:
        em.mov(R0, Operand2((quint32)2)); pushReg(R0); return 1;
    case LL_ldc_i4_3:
        em.mov(R0, Operand2((quint32)3)); pushReg(R0); return 1;
    case LL_ldc_i4_4:
        em.mov(R0, Operand2((quint32)4)); pushReg(R0); return 1;
    case LL_ldc_i4_5:
        em.mov(R0, Operand2((quint32)5)); pushReg(R0); return 1;
    case LL_ldc_i4_6:
        em.mov(R0, Operand2((quint32)6)); pushReg(R0); return 1;
    case LL_ldc_i4_7:
        em.mov(R0, Operand2((quint32)7)); pushReg(R0); return 1;
    case LL_ldc_i4_8:
        em.mov(R0, Operand2((quint32)8)); pushReg(R0); return 1;

    // Float/Double Constants: store in .rodata, load via address
    case LL_ldc_r4: {
        double dval = d_code.getDouble(val);
        float fval = (float)dval;
        quint32 rodataOff;
        if (d_floatOffsets.contains(val)) {
            rodataOff = d_floatOffsets[val];
        } else {
            // Align to 4 bytes for VLDR (requires word-aligned address)
            quint32 curSize = d_elf.sectionSize(d_sections.rodata);
            if (curSize & 3) {
                quint32 pad = 4 - (curSize & 3);
                char zeros[4] = {0};
                d_elf.appendToSection(d_sections.rodata, zeros, pad);
            }
            rodataOff = d_elf.sectionSize(d_sections.rodata);
            d_elf.appendToSection(d_sections.rodata, (const char*)&fval, sizeof(float));
            d_floatOffsets[val] = rodataOff;
        }
        // Load address of float constant via MOVW/MOVT + relocation
        quint32 movwPos = em.currentPosition();
        em.movw_(AL, R12, rodataOff & 0xFFFF);
        em.movt_(AL, R12, (rodataOff >> 16) & 0xFFFF);
        fix.recordExternalAddr(movwPos, d_rodataSymIdx);
        // Load float value
        em.vldrs(AL, S0, R12, 0);
        pushFloat(S0);
        return 1;
    }
    case LL_ldc_r8: {
        double dval = d_code.getDouble(val);
        quint32 rodataOff;
        if (d_doubleOffsets.contains(val)) {
            rodataOff = d_doubleOffsets[val];
        } else {
            // Align to 8 bytes for double
            quint32 curSize = d_elf.sectionSize(d_sections.rodata);
            if (curSize & 7) {
                quint32 pad = 8 - (curSize & 7);
                char zeros[8] = {0};
                d_elf.appendToSection(d_sections.rodata, zeros, pad);
            }
            rodataOff = d_elf.sectionSize(d_sections.rodata);
            d_elf.appendToSection(d_sections.rodata, (const char*)&dval, sizeof(double));
            d_doubleOffsets[val] = rodataOff;
        }

        quint32 movwPos = em.currentPosition();
        em.movw_(AL, R12, rodataOff & 0xFFFF);
        em.movt_(AL, R12, (rodataOff >> 16) & 0xFFFF);
        fix.recordExternalAddr(movwPos, d_rodataSymIdx);
        em.vldrd(AL, D0, R12, 0);
        pushDouble(D0);
        return 1;
    }

    case LL_ldnull:
        em.mov(R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;

    case LL_ldstr: {
        const char* str = d_code.getString(val);
        quint32 rodataOff;
        if (d_stringOffsets.contains(val)) {
            rodataOff = d_stringOffsets[val];
        } else {
            rodataOff = d_elf.sectionSize(d_sections.rodata);
            QByteArray sdata(str);
            sdata.append('\0');
            d_elf.appendToSection(d_sections.rodata, sdata);
            d_stringOffsets[val] = rodataOff;
        }
        // Load address of string via MOVW/MOVT + relocation
        quint32 movwPos = em.currentPosition();
        em.movw_(AL, R0, rodataOff & 0xFFFF);
        em.movt_(AL, R0, (rodataOff >> 16) & 0xFFFF);
        fix.recordExternalAddr(movwPos, d_rodataSymIdx);
        pushReg(R0);
        return 1;
    }

    case LL_ldobj: {
        const std::vector<char>& obj = d_code.getObject(val);
        // Place object data in .rodata (align to 4 bytes for word-access safety)
        quint32 curSize = d_elf.sectionSize(d_sections.rodata);
        if (curSize & 3) {
            quint32 pad = 4 - (curSize & 3);
            char zeros[4] = {0};
            d_elf.appendToSection(d_sections.rodata, zeros, pad);
        }
        quint32 rodataOff = d_elf.sectionSize(d_sections.rodata);
        d_elf.appendToSection(d_sections.rodata, obj.data(), obj.size());
        // Load address, then copy to stack
        quint32 movwPos = em.currentPosition();
        em.movw_(AL, R0, rodataOff & 0xFFFF);
        em.movt_(AL, R0, (rodataOff >> 16) & 0xFFFF);
        fix.recordExternalAddr(movwPos, d_rodataSymIdx);
        // For small objects, copy word by word; for larger, use a memcpy-like loop
        quint32 size = obj.size();
        quint32 aligned = (size + 3) & ~3;
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R1, MemOp(R0, i));
            em.str_(AL, R1, MemOp(SP, i));
        }
        return 1;
    }

    case LL_ldloc_i1: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        loadLocalAddr(R12, val);
        em.ldrsb_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldloc_i2: {
        loadLocalAddr(R12, val);
        em.ldrsh_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldloc_i4:
    case LL_ldloc_u4: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.ldr_(AL, R0, MemOp(R11, fo));
        pushReg(R0);
        return 1;
    }
    case LL_ldloc_i8:
    case LL_ldloc_u8: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.ldr_(AL, R0, MemOp(R11, fo));
        em.ldr_(AL, R1, MemOp(R11, fo + 4));
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_ldloc_u1: {
        loadLocalAddr(R12, val);
        em.ldrb_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldloc_u2: {
        loadLocalAddr(R12, val);
        em.ldrh_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldloc_r4: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.ldr_(AL, R0, MemOp(R11, fo));
        pushReg(R0);
        return 1;
    }
    case LL_ldloc_r8: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.ldr_(AL, R0, MemOp(R11, fo));
        em.ldr_(AL, R1, MemOp(R11, fo + 4));
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_ldloc_p: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.ldr_(AL, R0, MemOp(R11, fo));
        pushReg(R0);
        return 1;
    }
    case LL_ldloc_pp: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.ldr_(AL, R0, MemOp(R11, fo));
        em.ldr_(AL, R1, MemOp(R11, fo + 4));
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_ldloca:
        loadLocalAddr(R0, val);
        pushReg(R0);
        return 1;
    case LL_ldloc_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadLocalAddr(R12, val);
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(R12, i));
            em.str_(AL, R0, MemOp(SP, i));
        }
        return 2;
    }

    case LL_stloc_i1: {
        popReg(R0);
        loadLocalAddr(R12, val);
        em.strb_(AL, R0, MemOp(R12, 0));
        return 1;
    }
    case LL_stloc_i2: {
        popReg(R0);
        loadLocalAddr(R12, val);
        em.strh_(AL, R0, MemOp(R12, 0));
        return 1;
    }
    case LL_stloc_i4:
    case LL_stloc_r4:
    case LL_stloc_p: {
        popReg(R0);
        STORE_LOCAL_I4(R0, val);
        return 1;
    }
    case LL_stloc_i8:
    case LL_stloc_r8: {
        popRegPair(R0, R1);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.str_(AL, R0, MemOp(R11, fo));
        em.str_(AL, R1, MemOp(R11, fo + 4));
        return 1;
    }
    case LL_stloc_pp: {
        popRegPair(R0, R1);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.str_(AL, R0, MemOp(R11, fo));
        em.str_(AL, R1, MemOp(R11, fo + 4));
        return 1;
    }
    case LL_stloc_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadLocalAddr(R12, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(SP, i));
            em.str_(AL, R0, MemOp(R12, i));
        }
        em.add(SP, SP, Operand2(aligned));
        return 2;
    }

    case LL_ldarg_i1: {
        loadArgAddr(R12, val);
        em.ldrsb_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldarg_i2: {
        loadArgAddr(R12, val);
        em.ldrsh_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldarg_i4:
    case LL_ldarg_u4:
    case LL_ldarg_r4:
    case LL_ldarg_p: {
        LOAD_ARG_I4(R0, val);
        pushReg(R0);
        return 1;
    }
    case LL_ldarg_i8:
    case LL_ldarg_u8:
    case LL_ldarg_r8: {
        em.ldr_(AL, R0, MemOp(R11, FP_TO_ARGS + (qint32)val));
        em.ldr_(AL, R1, MemOp(R11, FP_TO_ARGS + (qint32)val + 4));
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_ldarg_u1: {
        loadArgAddr(R12, val);
        em.ldrb_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldarg_u2: {
        loadArgAddr(R12, val);
        em.ldrh_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldarg_pp: {
        em.ldr_(AL, R0, MemOp(R11, FP_TO_ARGS + (qint32)val));
        em.ldr_(AL, R1, MemOp(R11, FP_TO_ARGS + (qint32)val + 4));
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_ldarg_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadArgAddr(R12, val);
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(R12, i));
            em.str_(AL, R0, MemOp(SP, i));
        }
        return 2;
    }
    case LL_ldarga:
        loadArgAddr(R0, val);
        pushReg(R0);
        return 1;

    case LL_starg_i1: {
        popReg(R0);
        loadArgAddr(R12, val);
        em.strb_(AL, R0, MemOp(R12, 0));
        return 1;
    }
    case LL_starg_i2: {
        popReg(R0);
        loadArgAddr(R12, val);
        em.strh_(AL, R0, MemOp(R12, 0));
        return 1;
    }
    case LL_starg_i4:
    case LL_starg_r4:
    case LL_starg_p: {
        popReg(R0);
        STORE_ARG_I4(R0, val);
        return 1;
    }
    case LL_starg_i8:
    case LL_starg_r8: {
        popRegPair(R0, R1);
        em.str_(AL, R0, MemOp(R11, FP_TO_ARGS + (qint32)val));
        em.str_(AL, R1, MemOp(R11, FP_TO_ARGS + (qint32)val + 4));
        return 1;
    }
    case LL_starg_pp: {
        popRegPair(R0, R1);
        em.str_(AL, R0, MemOp(R11, FP_TO_ARGS + (qint32)val));
        em.str_(AL, R1, MemOp(R11, FP_TO_ARGS + (qint32)val + 4));
        return 1;
    }
    case LL_starg_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadArgAddr(R12, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(SP, i));
            em.str_(AL, R0, MemOp(R12, i));
        }
        em.add(SP, SP, Operand2(aligned));
        return 2;
    }

    case LL_ldvar_i1:
        loadVarAddr(R12, val);
        em.ldrsb_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    case LL_ldvar_i2:
        loadVarAddr(R12, val);
        em.ldrsh_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    case LL_ldvar_i4:
    case LL_ldvar_u4:
    case LL_ldvar_r4:
    case LL_ldvar_p:
        loadVarAddr(R12, val);
        em.ldr_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    case LL_ldvar_i8:
    case LL_ldvar_u8:
    case LL_ldvar_r8:
    case LL_ldvar_pp:
        loadVarAddr(R12, val);
        em.ldr_(AL, R0, MemOp(R12, 0));
        em.ldr_(AL, R1, MemOp(R12, 4));
        pushRegPair(R0, R1);
        return 1;
    case LL_ldvar_u1:
        loadVarAddr(R12, val);
        em.ldrb_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    case LL_ldvar_u2:
        loadVarAddr(R12, val);
        em.ldrh_(AL, R0, MemOp(R12, 0));
        pushReg(R0);
        return 1;
    case LL_ldvara:
        loadVarAddr(R0, val);
        pushReg(R0);
        return 1;
    case LL_ldvar_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadVarAddr(R12, val);
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(R12, i));
            em.str_(AL, R0, MemOp(SP, i));
        }
        return 2;
    }

    case LL_stvar_i1:
        popReg(R0);
        loadVarAddr(R12, val);
        em.strb_(AL, R0, MemOp(R12, 0));
        return 1;
    case LL_stvar_i2:
        popReg(R0);
        loadVarAddr(R12, val);
        em.strh_(AL, R0, MemOp(R12, 0));
        return 1;
    case LL_stvar_i4:
    case LL_stvar_r4:
    case LL_stvar_p:
        popReg(R0);
        loadVarAddr(R12, val);
        em.str_(AL, R0, MemOp(R12, 0));
        return 1;
    case LL_stvar_i8:
    case LL_stvar_r8:
    case LL_stvar_pp:
        popRegPair(R0, R1);
        loadVarAddr(R12, val);
        em.str_(AL, R0, MemOp(R12, 0));
        em.str_(AL, R1, MemOp(R12, 4));
        return 1;
    case LL_stvar_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadVarAddr(R12, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(SP, i));
            em.str_(AL, R0, MemOp(R12, i));
        }
        em.add(SP, SP, Operand2(aligned));
        return 2;
    }

    case LL_add_i4:
        popReg(R1); popReg(R0);
        em.add(R0, R0, Operand2(R1));
        pushReg(R0);
        return 1;
    case LL_sub_i4:
        popReg(R1); popReg(R0);
        em.sub(R0, R0, Operand2(R1));
        pushReg(R0);
        return 1;
    case LL_mul_i4:
        popReg(R1); popReg(R0);
        em.mul_(AL, false, R0, R0, R1);
        pushReg(R0);
        return 1;
    case LL_div_i4:
        popReg(R1); popReg(R0);
        if (d_hasHwDiv) {
            em.sdiv_(AL, R0, R0, R1);
        } else {
            // NOTE that this assumes C implemented intrinsics and doesn't consider d_useAapcs!
            // R0 = dividend, R1 = divisor; BL __mic$div_i4; result in R0
            quint32 sym = d_elf.addSymbol("__mic$div_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, sym);
        }
        pushReg(R0);
        return 1;
    case LL_div_un_i4:
        popReg(R1); popReg(R0);
        if (d_hasHwDiv) {
            em.udiv_(AL, R0, R0, R1);
        } else {
            // NOTE that this assumes C implemented intrinsics and doesn't consider d_useAapcs!
            // R0 = dividend, R1 = divisor; BL __mic$div_un_i4; result in R0
            quint32 sym = d_elf.addSymbol("__mic$div_un_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, sym);
        }
        pushReg(R0);
        return 1;
    case LL_rem_i4:
        popReg(R1); popReg(R0);
        if (d_hasHwDiv) {
            // R2 = R0 / R1; R0 = R0 - R2 * R1
            em.sdiv_(AL, R2, R0, R1);
            em.mul_(AL, false, R2, R2, R1);
            em.sub(R0, R0, Operand2(R2));
        } else {
            // NOTE that this assumes C implemented intrinsics and doesn't consider d_useAapcs!
            // R0 = dividend, R1 = divisor; BL __mic$rem_i4; result in R0
            quint32 sym = d_elf.addSymbol("__mic$rem_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, sym);
        }
        pushReg(R0);
        return 1;
    case LL_rem_un_i4:
        popReg(R1); popReg(R0);
        if (d_hasHwDiv) {
            em.udiv_(AL, R2, R0, R1);
            em.mul_(AL, false, R2, R2, R1);
            em.sub(R0, R0, Operand2(R2));
        } else {
            // NOTE that this assumes C implemented intrinsics and doesn't consider d_useAapcs!
            // R0 = dividend, R1 = divisor; BL __mic$rem_un_i4; result in R0
            quint32 sym = d_elf.addSymbol("__mic$rem_un_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, sym);
        }
        pushReg(R0);
        return 1;
    case LL_neg_i4:
        popReg(R0);
        em.rsb_(AL, false, R0, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_abs_i4:
        popReg(R0);
        em.cmp(R0, Operand2((quint32)0));
        em.rsb_(LT, false, R0, R0, Operand2((quint32)0)); // if R0 < 0, R0 = -R0
        pushReg(R0);
        return 1;

    case LL_add_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.add_(AL, true, R0, R0, Operand2(R2));
        em.adc_(AL, false, R1, R1, Operand2(R3));
        pushRegPair(R0, R1);
        return 1;
    case LL_sub_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.sub_(AL, true, R0, R0, Operand2(R2));
        em.sbc_(AL, false, R1, R1, Operand2(R3));
        pushRegPair(R0, R1);
        return 1;
    case LL_mul_i8:
        // 64x64->64 multiply: result_lo = lo(a_lo*b_lo), result_hi = hi(a_lo*b_lo) + a_lo*b_hi + a_hi*b_lo
        popRegPair(R2, R3); popRegPair(R0, R1);
        // R12 = R0, save a_lo before UMULL overwrites it
        em.mov(R12, Operand2(R0));
        em.umull_(AL, false, R0, LR, R12, R2);    // LR:R0 = a_lo * b_lo
        em.mla_(AL, false, LR, R12, R3, LR);      // LR += a_lo * b_hi
        em.mla_(AL, false, R1, R1, R2, LR);       // R1 = a_hi * b_lo + LR
        pushRegPair(R0, R1);
        return 1;
    case LL_div_i8:
    case LL_div_un_i8:
    case LL_rem_i8:
    case LL_rem_un_i8: {
        // NOTE that this assumes C implemented intrinsics and doesn't consider d_useAapcs!
        // 64-bit div/rem via __mic$ intrinsic call.
        // Stack has b (top) then a: pop both into R0:R1 (a) and R2:R3 (b).
        popRegPair(R2, R3); popRegPair(R0, R1);
        // AAPCS convention: R0:R1 = a (dividend), R2:R3 = b (divisor)
        const char* funcName;
        switch ((LL_op)op.op) {
        case LL_div_i8:    funcName = "__mic$div_i8"; break;
        case LL_div_un_i8: funcName = "__mic$div_un_i8"; break;
        case LL_rem_i8:    funcName = "__mic$rem_i8"; break;
        default:           funcName = "__mic$rem_un_i8"; break;
        }
        quint32 sym = d_elf.addSymbol(funcName, 0, 0, 0, STB_GLOBAL, STT_FUNC);
        quint32 blOffset = em.currentPosition();
        em.emitWord(0xEBFFFFFE);
        fix.recordExternalCall(blOffset, sym);
        // Result in R0:R1 (lo:hi)
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_neg_i8:
        popRegPair(R0, R1);
        // Negate 64-bit: RSBS R0, R0, #0; RSC R1, R1, #0
        em.rsb_(AL, true, R0, R0, Operand2((quint32)0));
        em.rsc_(AL, false, R1, R1, Operand2((quint32)0));
        pushRegPair(R0, R1);
        return 1;
    case LL_abs_i8:
        popRegPair(R0, R1);
        // If R1 (high word) < 0, negate
        em.cmp(R1, Operand2((quint32)0));
        em.rsb_(LT, true, R0, R0, Operand2((quint32)0));
        em.rsc_(LT, false, R1, R1, Operand2((quint32)0));
        pushRegPair(R0, R1);
        return 1;

    case LL_add_r4:
        popFloat(S1); popFloat(S0);
        em.vadds(AL, S0, S0, S1);
        pushFloat(S0);
        return 1;
    case LL_sub_r4:
        popFloat(S1); popFloat(S0);
        em.vsubs(AL, S0, S0, S1);
        pushFloat(S0);
        return 1;
    case LL_mul_r4:
        popFloat(S1); popFloat(S0);
        em.vmuls(AL, S0, S0, S1);
        pushFloat(S0);
        return 1;
    case LL_div_r4:
        popFloat(S1); popFloat(S0);
        em.vdivs(AL, S0, S0, S1);
        pushFloat(S0);
        return 1;
    case LL_neg_r4:
        popFloat(S0);
        em.vnegs(AL, S0, S0);
        pushFloat(S0);
        return 1;
    case LL_abs_r4:
        // VABS.F32 not in emitter; use: if neg, negate
        popFloat(S0);
        em.vcmps(AL, S0, S0); // compare with self to get sign
        // Workaround: VCMP S0, #0 not available; use neg+conditional
        // Actually simpler: load to GPR, clear sign bit
        em.vmov_r_s(AL, R0, S0);
        em.bic_(AL, false, R0, R0, Operand2(0x80000000u)); // clear sign bit
        em.vmov_s_r(AL, S0, R0);
        pushFloat(S0);
        return 1;

    case LL_add_r8:
        popDouble(D1); popDouble(D0);
        em.vaddd(AL, D0, D0, D1);
        pushDouble(D0);
        return 1;
    case LL_sub_r8:
        popDouble(D1); popDouble(D0);
        em.vsubd(AL, D0, D0, D1);
        pushDouble(D0);
        return 1;
    case LL_mul_r8:
        popDouble(D1); popDouble(D0);
        em.vmuld(AL, D0, D0, D1);
        pushDouble(D0);
        return 1;
    case LL_div_r8:
        popDouble(D1); popDouble(D0);
        em.vdivd(AL, D0, D0, D1);
        pushDouble(D0);
        return 1;
    case LL_neg_r8:
        popDouble(D0);
        em.vnegd(AL, D0, D0);
        pushDouble(D0);
        return 1;
    case LL_abs_r8:
        // Clear sign bit of double (bit 63)
        popRegPair(R0, R1);
        em.bic_(AL, false, R1, R1, Operand2(0x80000000u));
        pushRegPair(R0, R1);
        return 1;

    case LL_and_i4:
        popReg(R1); popReg(R0);
        em.and_(R0, R0, Operand2(R1));
        pushReg(R0);
        return 1;
    case LL_or_i4:
        popReg(R1); popReg(R0);
        em.orr_(AL, false, R0, R0, Operand2(R1));
        pushReg(R0);
        return 1;
    case LL_xor_i4:
        popReg(R1); popReg(R0);
        em.eor(R0, R0, Operand2(R1));
        pushReg(R0);
        return 1;
    case LL_not_i4:
        popReg(R0);
        em.mvn_(AL, false, R0, Operand2(R0));
        pushReg(R0);
        return 1;
    case LL_shl_i4:
        popReg(R1); popReg(R0);
        em.mov_(AL, false, R0, Operand2(R0, Shift(LSL, R1)));
        pushReg(R0);
        return 1;
    case LL_shr_i4:
        popReg(R1); popReg(R0);
        em.mov_(AL, false, R0, Operand2(R0, Shift(ASR, R1)));
        pushReg(R0);
        return 1;
    case LL_shr_un_i4:
        popReg(R1); popReg(R0);
        em.mov_(AL, false, R0, Operand2(R0, Shift(LSR, R1)));
        pushReg(R0);
        return 1;

    case LL_and_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.and_(R0, R0, Operand2(R2));
        em.and_(R1, R1, Operand2(R3));
        pushRegPair(R0, R1);
        return 1;
    case LL_or_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.orr_(AL, false, R0, R0, Operand2(R2));
        em.orr_(AL, false, R1, R1, Operand2(R3));
        pushRegPair(R0, R1);
        return 1;
    case LL_xor_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.eor(R0, R0, Operand2(R2));
        em.eor(R1, R1, Operand2(R3));
        pushRegPair(R0, R1);
        return 1;
    case LL_not_i8:
        popRegPair(R0, R1);
        em.mvn_(AL, false, R0, Operand2(R0));
        em.mvn_(AL, false, R1, Operand2(R1));
        pushRegPair(R0, R1);
        return 1;
    case LL_shl_i8:
        // 64-bit shift left by R2 (from stack as i4)
        popReg(R2); popRegPair(R0, R1);
        // R1 = (R1 << R2) | (R0 >> (32-R2)); R0 = R0 << R2
        em.rsb_(AL, false, R3, R2, Operand2((quint32)32));
        em.mov_(AL, false, R1, Operand2(R1, Shift(LSL, R2)));
        em.orr_(AL, false, R1, R1, Operand2(R0, Shift(LSR, R3)));
        em.mov_(AL, false, R0, Operand2(R0, Shift(LSL, R2)));
        pushRegPair(R0, R1);
        return 1;
    case LL_shr_i8:
        popReg(R2); popRegPair(R0, R1);
        em.rsb_(AL, false, R3, R2, Operand2((quint32)32));
        em.mov_(AL, false, R0, Operand2(R0, Shift(LSR, R2)));
        em.orr_(AL, false, R0, R0, Operand2(R1, Shift(LSL, R3)));
        em.mov_(AL, false, R1, Operand2(R1, Shift(ASR, R2)));
        pushRegPair(R0, R1);
        return 1;
    case LL_shr_un_i8:
        popReg(R2); popRegPair(R0, R1);
        em.rsb_(AL, false, R3, R2, Operand2((quint32)32));
        em.mov_(AL, false, R0, Operand2(R0, Shift(LSR, R2)));
        em.orr_(AL, false, R0, R0, Operand2(R1, Shift(LSL, R3)));
        em.mov_(AL, false, R1, Operand2(R1, Shift(LSR, R2)));
        pushRegPair(R0, R1);
        return 1;

    case LL_ceq_i4:
        popReg(R1); popReg(R0);
        em.cmp(R0, Operand2(R1));
        em.mov_(EQ, false, R0, Operand2((quint32)1));
        em.mov_(NE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_cgt_i4:
        popReg(R1); popReg(R0);
        em.cmp(R0, Operand2(R1));
        em.mov_(GT, false, R0, Operand2((quint32)1));
        em.mov_(LE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_clt_i4:
        popReg(R1); popReg(R0);
        em.cmp(R0, Operand2(R1));
        em.mov_(LT, false, R0, Operand2((quint32)1));
        em.mov_(GE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_cgt_u4:
        popReg(R1); popReg(R0);
        em.cmp(R0, Operand2(R1));
        em.mov_(HI, false, R0, Operand2((quint32)1));
        em.mov_(LS, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_clt_u4:
        popReg(R1); popReg(R0);
        em.cmp(R0, Operand2(R1));
        em.mov_(CC, false, R0, Operand2((quint32)1));
        em.mov_(CS, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_ceq_p:
    case LL_cgt_p:
    case LL_clt_p:
        // Pointer comparisons: same as unsigned 32-bit
        popReg(R1); popReg(R0);
        em.cmp(R0, Operand2(R1));
        if (opcode == LL_ceq_p) {
            em.mov_(EQ, false, R0, Operand2((quint32)1));
            em.mov_(NE, false, R0, Operand2((quint32)0));
        } else if (opcode == LL_cgt_p) {
            em.mov_(HI, false, R0, Operand2((quint32)1));
            em.mov_(LS, false, R0, Operand2((quint32)0));
        } else {
            em.mov_(CC, false, R0, Operand2((quint32)1));
            em.mov_(CS, false, R0, Operand2((quint32)0));
        }
        pushReg(R0);
        return 1;

    case LL_ceq_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        // Equal if both halves equal
        em.cmp(R0, Operand2(R2));
        em.cmp_(EQ, R1, Operand2(R3)); // only compare high if low equal
        em.mov_(EQ, false, R0, Operand2((quint32)1));
        em.mov_(NE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_cgt_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        // Signed > : compare high words; if equal, unsigned compare low words
        em.cmp(R1, Operand2(R3));
        em.cmp_(EQ, R0, Operand2(R2));
        em.mov_(GT, false, R0, Operand2((quint32)1)); // high > or (high == and low >)
        em.mov_(LE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_clt_i8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.cmp(R1, Operand2(R3));
        em.cmp_(EQ, R0, Operand2(R2));
        em.mov_(LT, false, R0, Operand2((quint32)1));
        em.mov_(GE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_cgt_u8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.cmp(R1, Operand2(R3));
        em.cmp_(EQ, R0, Operand2(R2));
        em.mov_(HI, false, R0, Operand2((quint32)1));
        em.mov_(LS, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_clt_u8:
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.cmp(R1, Operand2(R3));
        em.cmp_(EQ, R0, Operand2(R2));
        em.mov_(CC, false, R0, Operand2((quint32)1));
        em.mov_(CS, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;

    case LL_ceq_r4:
        popFloat(S1); popFloat(S0);
        em.vcmps(AL, S0, S1);
        em.vmrs_apsr(AL);
        em.mov_(EQ, false, R0, Operand2((quint32)1));
        em.mov_(NE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_cgt_r4:
        popFloat(S1); popFloat(S0);
        em.vcmps(AL, S0, S1);
        em.vmrs_apsr(AL);
        em.mov_(GT, false, R0, Operand2((quint32)1));
        em.mov_(LE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_clt_r4:
        popFloat(S1); popFloat(S0);
        em.vcmps(AL, S0, S1);
        em.vmrs_apsr(AL);
        em.mov_(MI, false, R0, Operand2((quint32)1));
        em.mov_(PL, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;

    case LL_ceq_r8:
        popDouble(D1); popDouble(D0);
        em.vcmpd(AL, D0, D1);
        em.vmrs_apsr(AL);
        em.mov_(EQ, false, R0, Operand2((quint32)1));
        em.mov_(NE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_cgt_r8:
        popDouble(D1); popDouble(D0);
        em.vcmpd(AL, D0, D1);
        em.vmrs_apsr(AL);
        em.mov_(GT, false, R0, Operand2((quint32)1));
        em.mov_(LE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;
    case LL_clt_r8:
        popDouble(D1); popDouble(D0);
        em.vcmpd(AL, D0, D1);
        em.vmrs_apsr(AL);
        em.mov_(MI, false, R0, Operand2((quint32)1));
        em.mov_(PL, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;

    case LL_ceq_pp:
        // Compare two method references (8 bytes each): both words must match
        popRegPair(R2, R3); popRegPair(R0, R1);
        em.cmp(R0, Operand2(R2));
        em.cmp_(EQ, R1, Operand2(R3));
        em.mov_(EQ, false, R0, Operand2((quint32)1));
        em.mov_(NE, false, R0, Operand2((quint32)0));
        pushReg(R0);
        return 1;

    // conv_TO_FROM: pop source, convert, push result
    // i1 (signed byte) from various types
    case LL_conv_i1_i4:
        popReg(R0);
        em.sxtb_(AL, R0, R0);
        pushReg(R0);
        return 1;
    case LL_conv_i1_i8:
        popRegPair(R0, R1);
        em.sxtb_(AL, R0, R0);
        pushReg(R0);
        return 1;
    case LL_conv_i1_r4:
        popFloat(S0);
        em.vcvts_i32(AL, S0, S0);
        em.vmov_r_s(AL, R0, S0);
        em.sxtb_(AL, R0, R0);
        pushReg(R0);
        return 1;
    case LL_conv_i1_r8:
        popDouble(D0);
        em.vcvtd_i32(AL, S0, D0);
        em.vmov_r_s(AL, R0, S0);
        em.sxtb_(AL, R0, R0);
        pushReg(R0);
        return 1;

    case LL_conv_i2_i4:
        popReg(R0);
        em.sxth_(AL, R0, R0);
        pushReg(R0);
        return 1;
    case LL_conv_i2_i8:
        popRegPair(R0, R1);
        em.sxth_(AL, R0, R0);
        pushReg(R0);
        return 1;
    case LL_conv_i2_r4:
        popFloat(S0); em.vcvts_i32(AL, S0, S0); em.vmov_r_s(AL, R0, S0);
        em.sxth_(AL, R0, R0); pushReg(R0);
        return 1;
    case LL_conv_i2_r8:
        popDouble(D0); em.vcvtd_i32(AL, S0, D0); em.vmov_r_s(AL, R0, S0);
        em.sxth_(AL, R0, R0); pushReg(R0);
        return 1;

    case LL_conv_i4_i8: popRegPair(R0, R1); pushReg(R0);
        return 1; // truncate to low word
    case LL_conv_i4_r4:
        popFloat(S0); em.vcvts_i32(AL, S0, S0);
        em.vmov_r_s(AL, R0, S0); pushReg(R0);
        return 1;
    case LL_conv_i4_r8:
        popDouble(D0); em.vcvtd_i32(AL, S0, D0);
        em.vmov_r_s(AL, R0, S0); pushReg(R0);
        return 1;

    case LL_conv_i8_i4:
        popReg(R0);
        em.mov_(AL, false, R1, Operand2(R0, Shift(ASR, 31))); // sign extend
        pushRegPair(R0, R1);
        return 1;
    case LL_conv_i8_r4:
        popFloat(S0); em.vcvts_i32(AL, S0, S0);
        em.vmov_r_s(AL, R0, S0);
        em.mov_(AL, false, R1, Operand2(R0, Shift(ASR, 31)));
        pushRegPair(R0, R1);
        return 1;
    case LL_conv_i8_r8:
        popDouble(D0); em.vcvtd_i32(AL, S0, D0);
        em.vmov_r_s(AL, R0, S0);
        em.mov_(AL, false, R1, Operand2(R0, Shift(ASR, 31)));
        pushRegPair(R0, R1);
        return 1;

    case LL_conv_u1_i4:
        popReg(R0); em.uxtb_(AL, R0, R0); pushReg(R0);
        return 1;
    case LL_conv_u1_i8:
        popRegPair(R0, R1); em.uxtb_(AL, R0, R0); pushReg(R0);
        return 1;
    case LL_conv_u1_r4:
        popFloat(S0); em.vcvts_i32(AL, S0, S0); em.vmov_r_s(AL, R0, S0);
        em.uxtb_(AL, R0, R0); pushReg(R0);
        return 1;
    case LL_conv_u1_r8:
        popDouble(D0); em.vcvtd_i32(AL, S0, D0); em.vmov_r_s(AL, R0, S0);
        em.uxtb_(AL, R0, R0); pushReg(R0);
        return 1;

    case LL_conv_u2_i4:
        popReg(R0); em.uxth_(AL, R0, R0); pushReg(R0);
        return 1;
    case LL_conv_u2_i8:
        popRegPair(R0, R1); em.uxth_(AL, R0, R0); pushReg(R0);
        return 1;
    case LL_conv_u2_r4:
        popFloat(S0); em.vcvts_i32(AL, S0, S0); em.vmov_r_s(AL, R0, S0);
        em.uxth_(AL, R0, R0); pushReg(R0);
        return 1;
    case LL_conv_u2_r8:
        popDouble(D0); em.vcvtd_i32(AL, S0, D0); em.vmov_r_s(AL, R0, S0);
        em.uxth_(AL, R0, R0); pushReg(R0);
        return 1;

    case LL_conv_u4_i8:
        popRegPair(R0, R1); pushReg(R0);
        return 1;
    case LL_conv_u4_r4:
        popFloat(S0); em.vcvts_i32(AL, S0, S0);
        em.vmov_r_s(AL, R0, S0); pushReg(R0); return 1;
    case LL_conv_u4_r8:
        popDouble(D0); em.vcvtd_i32(AL, S0, D0);
        em.vmov_r_s(AL, R0, S0); pushReg(R0); return 1;

    case LL_conv_u8_i4:
        popReg(R0);
        em.mov(R1, Operand2((quint32)0)); // zero-extend high word
        pushRegPair(R0, R1);
        return 1;
    case LL_conv_u8_r4:
        popFloat(S0); em.vcvts_i32(AL, S0, S0);
        em.vmov_r_s(AL, R0, S0);
        em.mov(R1, Operand2((quint32)0));
        pushRegPair(R0, R1);
        return 1;
    case LL_conv_u8_r8:
        popDouble(D0); em.vcvtd_i32(AL, S0, D0);
        em.vmov_r_s(AL, R0, S0);
        em.mov(R1, Operand2((quint32)0));
        pushRegPair(R0, R1);
        return 1;

    case LL_conv_r4_i4:
        popReg(R0); em.vmov_s_r(AL, S0, R0);
        em.vcvti32_s(AL, S0, S0);
        pushFloat(S0);
        return 1;
    case LL_conv_r4_i8:
        // Truncate to i4, then convert
        popRegPair(R0, R1); em.vmov_s_r(AL, S0, R0);
        em.vcvti32_s(AL, S0, S0);
        pushFloat(S0);
        return 1;
    case LL_conv_r4_r8:
        popDouble(D0);
        em.vcvtsd(AL, S0, D0);
        pushFloat(S0);
        return 1;

    case LL_conv_r8_i4:
        popReg(R0); em.vmov_s_r(AL, S0, R0);
        em.vcvti32_d(AL, D0, S0);
        pushDouble(D0);
        return 1;
    case LL_conv_r8_i8:
        popRegPair(R0, R1); em.vmov_s_r(AL, S0, R0);
        em.vcvti32_d(AL, D0, S0);
        pushDouble(D0);
        return 1;
    case LL_conv_r8_r4:
        popFloat(S0);
        em.vcvtds(AL, D0, S0);
        pushDouble(D0);
        return 1;

    case LL_br: {
        int target = pc + 1 + (op.minus ? -1 : 1) * (int)val;
        Label& lbl = getLabel(target);
        em.b_(AL, lbl);
        return 1;
    }
    case LL_brfalse_i4: {
        int target = pc + 1 + (op.minus ? -1 : 1) * (int)val;
        popReg(R0);
        em.cmp(R0, Operand2((quint32)0));
        Label& lbl = getLabel(target);
        em.b_(EQ, lbl);
        return 1;
    }

    case LL_call:
    case LL_callinst: {
        // val is the proc index in Vm::Code.
        // Args are already in correct order (Code2 pushes right-to-left).
        Procedure* callee = d_code.getProc(val);

        if (d_useAapcs) {
            // AAPCS mode: always route first 16 bytes of args to R0-R3,
            // leave overflow on stack. Return mechanism varies by size.

            // Fix argument alignment (insert inter-param alignment gaps)
            quint32 alignGap = emitArgAlignment(callee->decl);
            Q_UNUSED(alignGap);

            quint32 argsSize = callee->argsSize;
            quint32 returnSize = callee->returnSize;
            quint32 regBytes = argsSize < 16 ? argsSize : 16;
            quint32 overflowBytes = argsSize > 16 ? argsSize - 16 : 0;

            // Load register args from eval stack
            if (regBytes >= 4)  em.ldr_(AL, R0, MemOp(SP, 0));
            if (regBytes >= 8)  em.ldr_(AL, R1, MemOp(SP, 4));
            if (regBytes >= 12) em.ldr_(AL, R2, MemOp(SP, 8));
            if (regBytes >= 16) em.ldr_(AL, R3, MemOp(SP, 12));

            // Pop register args from eval stack
            if (regBytes > 0) {
                if (Operand2::isValidImmediate(regBytes))
                    em.add(SP, SP, Operand2(regBytes));
                else {
                    loadImm32(R12, regBytes);
                    em.add(SP, SP, Operand2(R12));
                }
            }

            // For large returns (>8 bytes): pre-allocate space so callee can
            // write return value to [FP+8..FP+8+returnSize-1].  The callee's
            // dump area provides 16 bytes; overflow args provide overflowBytes.
            // Any remaining space must be allocated by the caller as padding.
            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 16 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (Operand2::isValidImmediate(paddingNeeded))
                        em.sub(SP, SP, Operand2(paddingNeeded));
                    else {
                        loadImm32(R12, paddingNeeded);
                        em.sub(SP, SP, Operand2(R12));
                    }
                    // Move overflow args down by paddingNeeded bytes
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.ldr_(AL, R12, MemOp(SP, (qint32)(paddingNeeded + w)));
                        em.str_(AL, R12, MemOp(SP, (qint32)w));
                    }
                }
            }

            // BL (overflow args remain on stack)
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            if (d_procSymbols.contains(val))
                fix.recordLocalCall(blOffset, d_procSymbols[val]);
            else
                fix.recordExternalCall(blOffset, getOrCreateExtSymbol(val));

            if (returnSize <= 8) {
                // Small/void return: callee cleaned dump. Pop overflow.
                if (overflowBytes > 0) {
                    if (Operand2::isValidImmediate(overflowBytes))
                        em.add(SP, SP, Operand2(overflowBytes));
                    else {
                        loadImm32(R12, overflowBytes);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
                // Push return value from R0/R0-R1 onto eval stack
                if (returnSize >= 8)
                    pushRegPair(R0, R1);
                else if (returnSize >= 4)
                    pushReg(R0);
                // returnSize == 0: void
            } else {
                // Large return (>8 bytes): callee did NOT clean dump area.
                // Return value is at [SP..SP+returnSize-1].
                // Check for excess stack space above return value.
                quint32 totalOnStack = 16 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    // Slide return value up by excess (copy backward for safety)
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.ldr_(AL, R12, MemOp(SP, w));
                        em.str_(AL, R12, MemOp(SP, (qint32)((qint32)w + (qint32)excess)));
                    }
                    if (Operand2::isValidImmediate(excess))
                        em.add(SP, SP, Operand2(excess));
                    else {
                        loadImm32(R12, excess);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
                // Return value is now at [SP..SP+returnSize-1] on eval stack.
            }
        } else {
            // Old convention (unchanged)
            quint32 alignGap = emitArgAlignment(callee->decl);
            Q_UNUSED(alignGap);

            quint32 padding = 0;
            if (callee->returnSize > callee->argsSize)
                padding = callee->returnSize - callee->argsSize;
            if (padding > 0) {
                em.sub(SP, SP, Operand2(padding));
                for (quint32 w = 0; w < callee->argsSize; w += 4) {
                    em.ldr_(AL, R0, MemOp(SP, (qint32)(padding + w)));
                    em.str_(AL, R0, MemOp(SP, (qint32)w));
                }
            }

            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            if (d_procSymbols.contains(val))
                fix.recordLocalCall(blOffset, d_procSymbols[val]);
            else
                fix.recordExternalCall(blOffset, getOrCreateExtSymbol(val));

            emitCallStackAdj(callee->argsSize + padding, callee->returnSize);
        }
        return 1;
    }

    case LL_ret: {
        quint32 retSize = val; // val = return value size in bytes
        if (d_useAapcs) {
            if (retSize <= 8) {
                // AAPCS small return: pop return value into R0 (or R0-R1).
                if (retSize >= 8)
                    popRegPair(R0, R1);
                else if (retSize >= 4)
                    popReg(R0);
            } else {
                // AAPCS large return (>8 bytes): write return value to [FP+8]
                // (the R0-R3 dump area and any caller-allocated space beyond it).
                // The epilogue will NOT clean the dump area so the caller can
                // read the return value from [SP] after we return.
                for (quint32 w = 0; w < retSize; w += 4) {
                    em.ldr_(AL, R0, MemOp(SP, (qint32)w));
                    em.str_(AL, R0, MemOp(R11, (qint32)(8 + w)));
                }
            }
        } else {
            // Old convention: store return value at [FP+8].
            for (quint32 w = 0; w < retSize; w += 4) {
                em.ldr_(AL, R0, MemOp(SP, (qint32)w));
                em.str_(AL, R0, MemOp(R11, (qint32)(8 + w)));
            }
        }
        emitEpilogue();
        return 1;
    }
    case LL_ret_void:
        emitEpilogue();
        return 1;

    // Indirect Load/Store (pointer dereference)
    case LL_ldind_i1:
        popReg(R0);
        em.ldrsb_(AL, R0, MemOp(R0, 0));
        pushReg(R0);
        return 1;
    case LL_ldind_i2:
        popReg(R0); em.ldrsh_(AL, R0, MemOp(R0, 0)); pushReg(R0); return 1;
    case LL_ldind_i4:
        popReg(R0); em.ldr_(AL, R0, MemOp(R0, 0)); pushReg(R0); return 1;
    case LL_ldind_i8:
        popReg(R12);
        em.ldr_(AL, R0, MemOp(R12, 0));
        em.ldr_(AL, R1, MemOp(R12, 4));
        pushRegPair(R0, R1);
        return 1;
    case LL_ldind_u1:
        popReg(R0); em.ldrb_(AL, R0, MemOp(R0, 0)); pushReg(R0); return 1;
    case LL_ldind_u2:
        popReg(R0); em.ldrh_(AL, R0, MemOp(R0, 0)); pushReg(R0); return 1;
    case LL_ldind_u4:
        popReg(R0); em.ldr_(AL, R0, MemOp(R0, 0)); pushReg(R0); return 1;
    case LL_ldind_u8:
        popReg(R12);
        em.ldr_(AL, R0, MemOp(R12, 0));
        em.ldr_(AL, R1, MemOp(R12, 4));
        pushRegPair(R0, R1);
        return 1;
    case LL_ldind_r4:
        popReg(R0); em.ldr_(AL, R0, MemOp(R0, 0)); pushReg(R0); return 1;
    case LL_ldind_r8:
        popReg(R12);
        em.ldr_(AL, R0, MemOp(R12, 0));
        em.ldr_(AL, R1, MemOp(R12, 4));
        pushRegPair(R0, R1);
        return 1;
    case LL_ldind_p:
        popReg(R0); em.ldr_(AL, R0, MemOp(R0, 0)); pushReg(R0); return 1;
    case LL_ldind_vt: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        popReg(R12);
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(R12, i));
            em.str_(AL, R0, MemOp(SP, i));
        }
        return 1;
    }
    case LL_ldind_str: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        popReg(R12);
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(R12, i));
            em.str_(AL, R0, MemOp(SP, i));
        }
        return 1;
    }

    case LL_stind_i1:
        popReg(R0); popReg(R1); // R0=value, R1=address
        em.strb_(AL, R0, MemOp(R1, 0));
        return 1;
    case LL_stind_i2:
        popReg(R0); popReg(R1);
        em.strh_(AL, R0, MemOp(R1, 0));
        return 1;
    case LL_stind_i4:
    case LL_stind_r4:
    case LL_stind_p:
        popReg(R0); popReg(R1);
        em.str_(AL, R0, MemOp(R1, 0));
        return 1;
    case LL_stind_i8:
    case LL_stind_r8:
        popRegPair(R0, R1); // value (lo, hi)
        popReg(R2);         // address
        em.str_(AL, R0, MemOp(R2, 0));
        em.str_(AL, R1, MemOp(R2, 4));
        return 1;
    case LL_stind_vt: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        // Value is on top of stack (aligned bytes), address is below it
        // Load address from below the value
        em.ldr_(AL, R12, MemOp(SP, aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(SP, i));
            em.str_(AL, R0, MemOp(R12, i));
        }
        em.add(SP, SP, Operand2(aligned + Slot4)); // pop value + address
        return 1;
    }

    case LL_ldfld_i1:
        popReg(R0); em.ldrsb_(AL, R0, MemOp(R0, val)); pushReg(R0); return 1;
    case LL_ldfld_i2:
        popReg(R0); em.ldrsh_(AL, R0, MemOp(R0, val)); pushReg(R0); return 1;
    case LL_ldfld_i4:
    case LL_ldfld_u4:
    case LL_ldfld_r4:
    case LL_ldfld_p:
        popReg(R12);
        em.ldr_(AL, R0, MemOp(R12, val));
        pushReg(R0);
        return 1;
    case LL_ldfld_i8:
    case LL_ldfld_u8:
    case LL_ldfld_r8:
        popReg(R12);
        em.ldr_(AL, R0, MemOp(R12, val));
        em.ldr_(AL, R1, MemOp(R12, val + 4));
        pushRegPair(R0, R1);
        return 1;
    case LL_ldfld_u1:
        popReg(R0); em.ldrb_(AL, R0, MemOp(R0, val)); pushReg(R0); return 1;
    case LL_ldfld_u2:
        popReg(R0); em.ldrh_(AL, R0, MemOp(R0, val)); pushReg(R0); return 1;
    case LL_ldfld_pp:
        popReg(R12);
        em.ldr_(AL, R0, MemOp(R12, val));
        em.ldr_(AL, R1, MemOp(R12, val + 4));
        pushRegPair(R0, R1);
        return 1;
    case LL_ldflda:
        popReg(R0);
        if (val != 0)
            em.add(R0, R0, Operand2(val));
        pushReg(R0);
        return 1;
    case LL_ldfld_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        popReg(R12);
        if (val != 0)
            em.add(R12, R12, Operand2(val));
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(R12, i));
            em.str_(AL, R0, MemOp(SP, i));
        }
        return 2;
    }

    case LL_stfld_i1:
        popReg(R0); popReg(R1); // R0=value, R1=obj ptr
        em.strb_(AL, R0, MemOp(R1, val));
        return 1;
    case LL_stfld_i2:
        popReg(R0); popReg(R1);
        em.strh_(AL, R0, MemOp(R1, val));
        return 1;
    case LL_stfld_i4:
    case LL_stfld_r4:
    case LL_stfld_p:
        popReg(R0); popReg(R1);
        em.str_(AL, R0, MemOp(R1, val));
        return 1;
    case LL_stfld_i8:
    case LL_stfld_r8:
        popRegPair(R0, R1); popReg(R2); // R0:R1=value, R2=obj
        em.str_(AL, R0, MemOp(R2, val));
        em.str_(AL, R1, MemOp(R2, val + 4));
        return 1;
    case LL_stfld_pp:
        popRegPair(R0, R1); popReg(R2);
        em.str_(AL, R0, MemOp(R2, val));
        em.str_(AL, R1, MemOp(R2, val + 4));
        return 1;
    case LL_stfld_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        // Stack: [value (aligned bytes)] [obj ptr (4 bytes)]
        em.ldr_(AL, R12, MemOp(SP, aligned)); // load obj ptr from below value
        if (val != 0)
            em.add(R12, R12, Operand2(val));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(SP, i));
            em.str_(AL, R0, MemOp(R12, i));
        }
        em.add(SP, SP, Operand2(aligned + Slot4));
        return 2;
    }

    case LL_ldelem_i1: {
        popReg(R1); popReg(R0); // R1=index, R0=array ptr
        em.ldrsb_(AL, R0, MemOp(R0, R1));
        pushReg(R0);
        return 1;
    }
    case LL_ldelem_i2: {
        popReg(R1); popReg(R0);
        em.add(R0, R0, Operand2(R1, Shift(LSL, 1)));
        em.ldrsh_(AL, R0, MemOp(R0, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldelem_i4:
    case LL_ldelem_u4:
    case LL_ldelem_r4:
    case LL_ldelem_p: {
        popReg(R1); popReg(R0);
        em.ldr_(AL, R0, MemOp(R0, R1, Shift(LSL, 2)));
        pushReg(R0);
        return 1;
    }
    case LL_ldelem_i8:
    case LL_ldelem_u8:
    case LL_ldelem_r8: {
        popReg(R1); popReg(R12);
        em.add(R12, R12, Operand2(R1, Shift(LSL, 3)));
        em.ldr_(AL, R0, MemOp(R12, 0));
        em.ldr_(AL, R1, MemOp(R12, 4));
        pushRegPair(R0, R1);
        return 1;
    }
    case LL_ldelem_u1: {
        popReg(R1); popReg(R0);
        em.ldrb_(AL, R0, MemOp(R0, R1));
        pushReg(R0);
        return 1;
    }
    case LL_ldelem_u2: {
        popReg(R1); popReg(R0);
        em.add(R0, R0, Operand2(R1, Shift(LSL, 1)));
        em.ldrh_(AL, R0, MemOp(R0, 0));
        pushReg(R0);
        return 1;
    }
    case LL_ldelema: {
        // val = element size; stack: [index] [array ptr]
        popReg(R1); popReg(R0);
        // R0 = array + index * elem_size
        if (val == 1)
            em.add(R0, R0, Operand2(R1));
        else if (val == 2)
            em.add(R0, R0, Operand2(R1, Shift(LSL, 1)));
        else if (val == 4)
            em.add(R0, R0, Operand2(R1, Shift(LSL, 2)));
        else if (val == 8)
            em.add(R0, R0, Operand2(R1, Shift(LSL, 3)));
        else {
            loadImm32(R2, val);
            em.mul_(AL, false, R1, R1, R2);
            em.add(R0, R0, Operand2(R1));
        }
        pushReg(R0);
        return 1;
    }
    case LL_ldelem_vt: {
        quint32 elemSize = val;
        quint32 aligned = (elemSize + 3) & ~3;
        popReg(R1); popReg(R12); // index, array ptr
        // R12 = array + index * elemSize
        if (elemSize == 4) {
            em.add(R12, R12, Operand2(R1, Shift(LSL, 2)));
        } else {
            loadImm32(R2, elemSize);
            em.mul_(AL, false, R1, R1, R2);
            em.add(R12, R12, Operand2(R1));
        }
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(R12, i));
            em.str_(AL, R0, MemOp(SP, i));
        }
        return 1;
    }

    case LL_stelem_i1: {
        popReg(R0); popReg(R1); popReg(R2); // value, index, array
        em.strb_(AL, R0, MemOp(R2, R1));
        return 1;
    }
    case LL_stelem_i2: {
        popReg(R0); popReg(R1); popReg(R2);
        em.add(R2, R2, Operand2(R1, Shift(LSL, 1)));
        em.strh_(AL, R0, MemOp(R2, 0));
        return 1;
    }
    case LL_stelem_i4:
    case LL_stelem_r4:
    case LL_stelem_p: {
        popReg(R0); popReg(R1); popReg(R2);
        em.str_(AL, R0, MemOp(R2, R1, Shift(LSL, 2)));
        return 1;
    }
    case LL_stelem_i8:
    case LL_stelem_r8: {
        popRegPair(R0, R1); // value
        popReg(R2); // index
        popReg(R3); // array
        em.add(R3, R3, Operand2(R2, Shift(LSL, 3)));
        em.str_(AL, R0, MemOp(R3, 0));
        em.str_(AL, R1, MemOp(R3, 4));
        return 1;
    }
    case LL_stelem_vt: {
        quint32 elemSize = val;
        quint32 aligned = (elemSize + 3) & ~3;
        // Stack: [value (aligned)] [index (4)] [array ptr (4)]
        em.ldr_(AL, R1, MemOp(SP, aligned));       // index
        em.ldr_(AL, R2, MemOp(SP, aligned + Slot4)); // array ptr
        if (elemSize == 4) {
            em.add(R12, R2, Operand2(R1, Shift(LSL, 2)));
        } else {
            loadImm32(R3, elemSize);
            em.mul_(AL, false, R1, R1, R3);
            em.add(R12, R2, Operand2(R1));
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(SP, i));
            em.str_(AL, R0, MemOp(R12, i));
        }
        em.add(SP, SP, Operand2(aligned + Slot4 + Slot4));
        return 1;
    }

    case LL_pop: {
        quint32 aligned = (val + 3) & ~3;
        em.add(SP, SP, Operand2(aligned));
        return 1;
    }
    case LL_dup: {
        quint32 aligned = (val + 3) & ~3;
        // Copy top 'aligned' bytes
        for (int i = (int)aligned - 4; i >= 0; i -= 4) {
            em.ldr_(AL, R0, MemOp(SP, i));
            em.str_(AL, R0, MemOp(SP, -(int)(aligned - i), MemOp::PreIndexed));
        }
        // allocate space then copy
        em.sub(SP, SP, Operand2(aligned));
        for (quint32 i = 0; i < aligned; i += 4) {
            em.ldr_(AL, R0, MemOp(SP, aligned + i)); // source: old top
            em.str_(AL, R0, MemOp(SP, i));            // dest: new top
        }
        return 1;
    }

    case LL_alloc1: {
        // alloc1: allocate single object of 'val' bytes (or template size if minus)
        quint32 allocSize;
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            allocSize = tmpl.mem.size();
        } else {
            allocSize = val;
        }
        if (d_useAapcs) {
            // AAPCS mode: R0 = size, BL MIC$$alloc, result in R0
            loadImm32(R0, allocSize);
            quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, allocSym);
        } else {
            // Old convention: push size on stack, call MIC$$alloc
            em.sub(SP, SP, Operand2((quint32)4));
            loadImm32(R0, allocSize);
            em.str_(AL, R0, MemOp(SP, 0));
            quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, allocSym);
            em.ldr_(AL, R0, MemOp(SP, 0));
            em.add(SP, SP, Operand2((quint32)4));
        }
        // R0 = allocated pointer
        // If template, copy template data to allocated memory
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff = getOrEmitTemplate(val);
                // R0 = dest. Load template source address into R1
                quint32 movwOff = em.currentPosition();
                em.movw_(AL, R1, tmplOff & 0xFFFF);
                em.movt_(AL, R1, (tmplOff >> 16) & 0xFFFF);
                fix.recordExternalAddr(movwOff, d_rodataSymIdx);
                // Word-by-word copy
                quint32 aligned = (tmplSize + 3) & ~3;
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.ldr_(AL, R2, MemOp(R1, i));
                    em.str_(AL, R2, MemOp(R0, i));
                }
            }
            // Fix vtable pointer if this is an object type
            emitVtableFixups(R0, tmpl);
        }
        pushReg(R0); // push allocated pointer
        return 1;
    }
    case LL_allocN: {
        // allocN: stack has count (i4); val is element size (or template index if minus)
        quint32 elemSize;
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            elemSize = tmpl.mem.size();
        } else {
            elemSize = val;
        }
        popReg(R0); // R0 = count
        // total = count * elemSize
        loadImm32(R1, elemSize);
        em.mul_(AL, false, R0, R0, R1); // R0 = count * elemSize
        if (d_useAapcs) {
            // AAPCS mode: R0 = totalSize, BL MIC$$alloc.
            // Save totalSize for later template copy.
            em.str_(AL, R0, MemOp(SP, -4, MemOp::PreIndexed)); // push totalSize
            // MIC$$alloc(size=totalSize): R0=totalSize
            em.ldr_(AL, R0, MemOp(SP, 0)); // R0 = totalSize
            quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, allocSym);
            // R0 = allocated pointer
            // Restore totalSize
            em.ldr_(AL, R3, MemOp(SP, 0));
            em.add(SP, SP, Operand2((quint32)4)); // pop totalSize
        } else {
            // Old convention: push size on stack, call MIC$$alloc
            em.str_(AL, R0, MemOp(SP, -4, MemOp::PreIndexed)); // push totalSize
            em.str_(AL, R0, MemOp(SP, -4, MemOp::PreIndexed)); // push size arg
            quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, allocSym);
            em.ldr_(AL, R0, MemOp(SP, 0));
            em.add(SP, SP, Operand2((quint32)4)); // pop arg slot
            em.ldr_(AL, R3, MemOp(SP, 0));
            em.add(SP, SP, Operand2((quint32)4)); // pop totalSize
        }
        // R0 = allocated pointer, R3 = totalSize
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff = getOrEmitTemplate(val);
                // Save base pointer
                em.str_(AL, R0, MemOp(SP, -4, MemOp::PreIndexed)); // push base
                // R4 = current dest, R3 = end = base + totalSize
                // Use stack to preserve R0 (base) and loop vars
                // R12 = current dest, R3 = end
                em.mov(R12, Operand2(R0));    // R12 = current dest
                em.add(R3, R0, Operand2(R3)); // R3 = end = base + totalSize
                // Load template source address into R1
                quint32 movwOff = em.currentPosition();
                em.movw_(AL, R1, tmplOff & 0xFFFF);
                em.movt_(AL, R1, (tmplOff >> 16) & 0xFFFF);
                fix.recordExternalAddr(movwOff, d_rodataSymIdx);
                quint32 aligned = (tmplSize + 3) & ~3;
                // Loop: copy template to each element
                Arm::Label loopLbl, doneLbl;
                em.bind(loopLbl);
                em.cmp(R12, Operand2(R3));
                em.b_(CS, doneLbl); // if dest >= end, done (unsigned >=)
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.ldr_(AL, R2, MemOp(R1, i));
                    em.str_(AL, R2, MemOp(R12, i));
                }
                // Fix vtable pointer for this element
                // Save R3, R12, R1 since emitVtableFixups uses R2
                em.str_(AL, R3, MemOp(SP, -4, MemOp::PreIndexed));
                em.str_(AL, R12, MemOp(SP, -4, MemOp::PreIndexed));
                em.str_(AL, R1, MemOp(SP, -4, MemOp::PreIndexed));
                emitVtableFixups(R12, tmpl);
                em.ldr_(AL, R1, MemOp(SP, 4, MemOp::PostIndexed));
                em.ldr_(AL, R12, MemOp(SP, 4, MemOp::PostIndexed));
                em.ldr_(AL, R3, MemOp(SP, 4, MemOp::PostIndexed));
                loadImm32(R2, tmplSize);
                em.add(R12, R12, Operand2(R2)); // dest += tmplSize
                em.b_(AL, loopLbl);
                em.bind(doneLbl);
                // Restore base pointer
                em.ldr_(AL, R0, MemOp(SP, 4, MemOp::PostIndexed)); // pop base
            }
        }
        pushReg(R0); // push allocated pointer
        return 1;
    }
    case LL_free: {
        popReg(R0); // pointer to free
        if (d_useAapcs) {
            // AAPCS mode: R0 = ptr, BL MIC$$free
            quint32 freeSym = d_elf.addSymbol("MIC$$free", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, freeSym);
        } else {
            // Old convention: push arg on stack, call MIC$$free
            em.str_(AL, R0, MemOp(SP, -4, MemOp::PreIndexed));
            quint32 freeSym = d_elf.addSymbol("MIC$$free", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            fix.recordExternalCall(blOffset, freeSym);
            em.add(SP, SP, Operand2((quint32)4));
        }
        return 1;
    }
    case LL_initobj: {
        // Pop pointer, optionally copy template data into it.
        // If op.minus is set, val = template index; copy template memory.
        // Otherwise, val = byte size; zero-fill.
        popReg(R0); // R0 = destination pointer
        if (op.minus) {
            // Template initialization: copy preinitialized memory
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff = getOrEmitTemplate(val);
                // Load template source address
                quint32 movwOff = em.currentPosition();
                em.movw_(AL, R1, tmplOff & 0xFFFF);
                em.movt_(AL, R1, (tmplOff >> 16) & 0xFFFF);
                fix.recordExternalAddr(movwOff, d_rodataSymIdx);
                // Word-by-word copy: R0=dst, R1=src
                quint32 aligned = (tmplSize + 3) & ~3;
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.ldr_(AL, R2, MemOp(R1, i));
                    em.str_(AL, R2, MemOp(R0, i));
                }
            }
            // Fix vtable pointer if this is an object type
            emitVtableFixups(R0, tmpl);
        }
        // If !op.minus: no initialization needed (zero-fill would require size info)
        return 1;
    }

    case LL_strcpy: {
        // Stack: [rhs ptr] [lhs ptr]; val = max length (in bytes)
        popReg(R1); popReg(R0); // R1=src, R0=dst
        // Byte-copy loop: copies up to val bytes, stopping at null terminator.
        // R0=dst, R1=src, R2=counter (remaining), R3=temp byte
        loadImm32(R2, val);
        // loop:
        Arm::Label loopLabel, doneLabel;
        em.bind(loopLabel);
        em.cmp(R2, Operand2((quint32)0));
        em.b_(EQ, doneLabel);               // if counter==0, done
        em.ldrb_(AL, R3, MemOp(R1, 0));     // R3 = *src
        em.strb_(AL, R3, MemOp(R0, 0));     // *dst = R3
        em.cmp(R3, Operand2((quint32)0));
        em.b_(EQ, doneLabel);               // if byte==0, done
        em.add(R1, R1, Operand2((quint32)1)); // src++
        em.add(R0, R0, Operand2((quint32)1)); // dst++
        em.sub(R2, R2, Operand2((quint32)1)); // counter--
        em.b_(AL, loopLabel);                // loop
        em.bind(doneLabel);
        return 1;
    }

    // Indirect calls
    case LL_calli: {
        // Stack: [func_ptr] [arg1] ... [argN] (func_ptr on top, args in correct order)
        // val encodes argsSize (lower 11 bits) and returnSize (upper 11 bits).
        quint32 argsSize = val & 0x7FF;
        quint32 returnSize = (val >> 11) & 0x7FF;
        // TODO: Bit 0 of returnSize encodes fpReturn

        // Pop the function pointer
        popReg(R12);

        if (d_useAapcs) {
            // AAPCS mode: naive word routing (all args treated as 4-byte words)
            quint32 regBytes = argsSize < 16 ? argsSize : 16;
            quint32 overflowBytes = argsSize > 16 ? argsSize - 16 : 0;
            if (regBytes >= 4)  em.ldr_(AL, R0, MemOp(SP, 0));
            if (regBytes >= 8)  em.ldr_(AL, R1, MemOp(SP, 4));
            if (regBytes >= 12) em.ldr_(AL, R2, MemOp(SP, 8));
            if (regBytes >= 16) em.ldr_(AL, R3, MemOp(SP, 12));
            if (regBytes > 0) {
                if (Operand2::isValidImmediate(regBytes))
                    em.add(SP, SP, Operand2(regBytes));
                else {
                    loadImm32(R4, regBytes);  // R4 as temp (R12 holds func ptr)
                    em.add(SP, SP, Operand2(R4));
                }
            }

            // For large returns: pre-allocate space
            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 16 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (Operand2::isValidImmediate(paddingNeeded))
                        em.sub(SP, SP, Operand2(paddingNeeded));
                    else {
                        loadImm32(R4, paddingNeeded);
                        em.sub(SP, SP, Operand2(R4));
                    }
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.ldr_(AL, R4, MemOp(SP, (qint32)(paddingNeeded + w)));
                        em.str_(AL, R4, MemOp(SP, (qint32)w));
                    }
                }
            }

            em.blx_(AL, R12);

            if (returnSize <= 8) {
                // Small/void return: callee cleaned dump. Pop overflow.
                if (overflowBytes > 0) {
                    if (Operand2::isValidImmediate(overflowBytes))
                        em.add(SP, SP, Operand2(overflowBytes));
                    else {
                        loadImm32(R12, overflowBytes);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
                if (returnSize >= 8)
                    pushRegPair(R0, R1);
                else if (returnSize >= 4)
                    pushReg(R0);
            } else {
                // Large return: callee did NOT clean dump.
                quint32 totalOnStack = 16 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.ldr_(AL, R12, MemOp(SP, w));
                        em.str_(AL, R12, MemOp(SP, (qint32)((qint32)w + (qint32)excess)));
                    }
                    if (Operand2::isValidImmediate(excess))
                        em.add(SP, SP, Operand2(excess));
                    else {
                        loadImm32(R12, excess);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
            }
        } else {
            em.blx_(AL, R12);
            emitCallStackAdj(argsSize, returnSize);
        }
        return 1;
    }
    case LL_callmi: {
        // Stack: [method_ptr] [obj_ptr] [arg1] ... [argN]
        // MethRef is on top (8 bytes: method at SP, obj at SP+4).
        // Args underneath are already in correct order (Code2 pushes right-to-left).
        // val encodes argsSize (lower 11 bits) and returnSize (upper 11 bits).
        quint32 argsSize = val & 0x7FF;
        quint32 returnSize = (val >> 11) & 0x7FF;
        // TODO: Bit 0 of returnSize encodes fpReturn (same convention as LL_calli).

        // Pop method_ptr into R12
        popReg(R12);
        // Pop obj_ptr into R0
        popReg(R0);

        if (d_useAapcs) {
            // AAPCS mode: self (obj_ptr) in R0, remaining args from eval stack
            // R0 already holds self.
            // Load next register args from eval stack (remaining argsSize - 4 bytes)
            quint32 remainArgs = argsSize >= 4 ? argsSize - 4 : 0;
            quint32 regBytes = remainArgs < 12 ? remainArgs : 12;
            quint32 overflowBytes = remainArgs > 12 ? remainArgs - 12 : 0;
            if (regBytes >= 4)  em.ldr_(AL, R1, MemOp(SP, 0));
            if (regBytes >= 8)  em.ldr_(AL, R2, MemOp(SP, 4));
            if (regBytes >= 12) em.ldr_(AL, R3, MemOp(SP, 8));
            if (regBytes > 0) {
                if (Operand2::isValidImmediate(regBytes))
                    em.add(SP, SP, Operand2(regBytes));
                else {
                    loadImm32(R4, regBytes);
                    em.add(SP, SP, Operand2(R4));
                }
            }

            // For large returns: pre-allocate space
            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 16 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (Operand2::isValidImmediate(paddingNeeded))
                        em.sub(SP, SP, Operand2(paddingNeeded));
                    else {
                        loadImm32(R4, paddingNeeded);
                        em.sub(SP, SP, Operand2(R4));
                    }
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.ldr_(AL, R4, MemOp(SP, (qint32)(paddingNeeded + w)));
                        em.str_(AL, R4, MemOp(SP, (qint32)w));
                    }
                }
            }

            em.blx_(AL, R12);

            if (returnSize <= 8) {
                // Small/void return: callee cleaned dump. Pop overflow.
                if (overflowBytes > 0) {
                    if (Operand2::isValidImmediate(overflowBytes))
                        em.add(SP, SP, Operand2(overflowBytes));
                    else {
                        loadImm32(R12, overflowBytes);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
                if (returnSize >= 8)
                    pushRegPair(R0, R1);
                else if (returnSize >= 4)
                    pushReg(R0);
            } else {
                // Large return: callee did NOT clean dump.
                quint32 totalOnStack = 16 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.ldr_(AL, R12, MemOp(SP, w));
                        em.str_(AL, R12, MemOp(SP, (qint32)((qint32)w + (qint32)excess)));
                    }
                    if (Operand2::isValidImmediate(excess))
                        em.add(SP, SP, Operand2(excess));
                    else {
                        loadImm32(R12, excess);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
            }
        } else {
            // Old convention: push obj_ptr back as first arg, call, adjust stack
            pushReg(R0);
            em.blx_(AL, R12);
            emitCallStackAdj(argsSize, returnSize);
        }
        return 1;
    }

    case LL_callvirt: {
        // Virtual method dispatch via vtable.
        // val = proc index (used to get argsSize and slot).
        // Args (including self at offset 0) are already in correct order on the stack.
        // Self is at SP (first arg = lowest address, thanks to right-to-left push).
        Procedure* callee = d_code.getProc(val);

        // Fix argument alignment (same as LL_call)
        quint32 alignGap = emitArgAlignment(callee->decl);
        Q_UNUSED(alignGap);

        quint32 argsSize = callee->argsSize;
        quint32 returnSize = callee->returnSize;

        // Load self pointer from SP (first arg, already at lowest address)
        em.ldr_(AL, R0, MemOp(SP, 0));         // R0 = self (object pointer)
        em.ldr_(AL, R1, MemOp(R0, 0));         // R1 = vtable pointer (at object offset 0)

        // Get the method's slot from the procedure declaration
        quint32 slot = 0;
        if (callee->decl && callee->decl->getPd())
            slot = callee->decl->getPd()->slot;
        em.ldr_(AL, R12, MemOp(R1, (qint32)slot * 4)); // R12 = method addr from vtable[slot]

        if (d_useAapcs) {
            // AAPCS mode: route first 16 bytes of args to R0-R3
            quint32 regBytes = argsSize < 16 ? argsSize : 16;
            quint32 overflowBytes = argsSize > 16 ? argsSize - 16 : 0;
            // R0 already holds self from the load above, but we need to
            // reload it from stack since the vtable lookup clobbered R0/R1.
            if (regBytes >= 4)  em.ldr_(AL, R0, MemOp(SP, 0));
            if (regBytes >= 8)  em.ldr_(AL, R1, MemOp(SP, 4));
            if (regBytes >= 12) em.ldr_(AL, R2, MemOp(SP, 8));
            if (regBytes >= 16) em.ldr_(AL, R3, MemOp(SP, 12));
            if (regBytes > 0) {
                if (Operand2::isValidImmediate(regBytes))
                    em.add(SP, SP, Operand2(regBytes));
                else {
                    loadImm32(R4, regBytes);
                    em.add(SP, SP, Operand2(R4));
                }
            }

            // For large returns: pre-allocate space
            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 16 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (Operand2::isValidImmediate(paddingNeeded))
                        em.sub(SP, SP, Operand2(paddingNeeded));
                    else {
                        loadImm32(R4, paddingNeeded);
                        em.sub(SP, SP, Operand2(R4));
                    }
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.ldr_(AL, R4, MemOp(SP, (qint32)(paddingNeeded + w)));
                        em.str_(AL, R4, MemOp(SP, (qint32)w));
                    }
                }
            }

            em.blx_(AL, R12);

            if (returnSize <= 8) {
                // Small/void return: callee cleaned dump. Pop overflow.
                if (overflowBytes > 0) {
                    if (Operand2::isValidImmediate(overflowBytes))
                        em.add(SP, SP, Operand2(overflowBytes));
                    else {
                        loadImm32(R12, overflowBytes);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
                if (returnSize >= 8)
                    pushRegPair(R0, R1);
                else if (returnSize >= 4)
                    pushReg(R0);
            } else {
                // Large return: callee did NOT clean dump.
                quint32 totalOnStack = 16 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.ldr_(AL, R12, MemOp(SP, w));
                        em.str_(AL, R12, MemOp(SP, (qint32)((qint32)w + (qint32)excess)));
                    }
                    if (Operand2::isValidImmediate(excess))
                        em.add(SP, SP, Operand2(excess));
                    else {
                        loadImm32(R12, excess);
                        em.add(SP, SP, Operand2(R12));
                    }
                }
            }
        } else {
            // Old convention
            em.blx_(AL, R12);
            emitCallStackAdj(argsSize, returnSize);
        }
        return 1;
    }

    case LL_ldproc: {
        // Load the address of procedure 'val' onto the eval stack (4 bytes).
        quint32 movwOffset = em.currentPosition();
        em.movw_(AL, R0, 0);
        em.movt_(AL, R0, 0);
        if (d_procSymbols.contains(val)) {
            fix.recordExternalAddr(movwOffset, d_procSymbols[val]);
        } else {
            quint32 symIdx = getOrCreateExtSymbol(val);
            fix.recordExternalAddr(movwOffset, symIdx);
        }
        pushReg(R0);
        return 1;
    }
    case LL_ldmeth_struct: {
        // Pop object pointer, load procedure address, push 8-byte MethRef(obj, proc).
        // Interpreter: m.obj = popP(); m.proc = getProc(val); push(&m, 8);
        popReg(R1); // R1 = obj pointer

        // Load procedure address into R0
        quint32 movwOffset = em.currentPosition();
        em.movw_(AL, R0, 0);
        em.movt_(AL, R0, 0);
        if (d_procSymbols.contains(val)) {
            fix.recordExternalAddr(movwOffset, d_procSymbols[val]);
        } else {
            quint32 symIdx = getOrCreateExtSymbol(val);
            fix.recordExternalAddr(movwOffset, symIdx);
        }

        // Push MethRef: R0 = proc (low word), R1 = obj (high word)
        // In memory: [proc at SP] [obj at SP+4]
        pushRegPair(R0, R1);
        return 1;
    }

    case LL_ldmeth: {
        // Virtual method dispatch: object pointer is on eval stack,
        // val is the vtable slot index.
        // Load vtable pointer from object, then load method from slot.
        popReg(R0);                                    // R0 = object pointer
        em.ldr_(AL, R1, MemOp(R0, 0));         // R1 = vtable pointer (at offset 0)
        em.ldr_(AL, R0, MemOp(R1, (qint32)val * 4)); // R0 = method addr from vtable[slot]
        pushReg(R0);
        return 1;
    }

    case LL_ldmeth_iface: {
        // Pop IfaceRef(obj, vtable) from stack, load method from vtable[val].
        // Push MethRef(obj, method) as 8 bytes.
        // IfaceRef layout on stack: [vtable_addr at SP] [obj at SP+4]
        popRegPair(R2, R1); // R2 = vtable_addr, R1 = obj
        em.ldr_(AL, R0, MemOp(R2, (qint32)val * 4)); // R0 = method from vtable[slot]
        // Push MethRef: R0 = method (low), R1 = obj (high)
        pushRegPair(R0, R1);
        return 1;
    }

    case LL_ldiface: {
        // Pop object pointer, push IfaceRef(obj, vtable_addr) as 8 bytes.
        // val = vtable index in d_code.
        popReg(R1); // R1 = obj pointer

        // Get or emit vtable in .data
        quint32 vtableOff = getOrEmitVtable(val);

        // Load vtable address with relocation against .data
        quint32 movwOff = em.currentPosition();
        em.movw_(AL, R0, vtableOff & 0xFFFF);
        em.movt_(AL, R0, (vtableOff >> 16) & 0xFFFF);
        fix.recordExternalAddr(movwOff, d_dataSymIdx);

        // Push IfaceRef: R0 = vtable_addr (low), R1 = obj (high)
        pushRegPair(R0, R1);
        return 1;
    }

    case LL_isinst: {
        // Pop object pointer, check if obj's vtable type matches ref type.
        // For now: if obj != NULL, return 1 (assume type matches).
        // Full implementation would need Type::isA with runtime type info.
        qWarning() << "Armv7Renderer: isinst type check not fully implemented (always returns 1 if non-null)";
        popReg(R0);
        em.cmp(R0, Operand2((quint32)0));
        em.mov_(AL, false, R0, Operand2((quint32)0));  // R0 = 0 (default: false)
        em.mov_(NE, false, R0, Operand2((quint32)1));  // R0 = 1 if obj != NULL
        pushReg(R0);
        return 1;
    }

    case LL_vt_size:
        // This is a suffix consumed by the preceding _vt op; should not appear alone.
        Q_ASSERT(false);
        return 1;

    case LL_newvla:
        qWarning() << "Armv7Renderer: not yet implemented:" << Code::op_names[opcode];
        em.nop_();
        return 1;

    case LL_line:
        // Debug line marker; ignored in code generation (will be used for DWARF later)
        return 1;

    case LL_invalid:
        return 1;

    default:
        return setError(QString("Unknown LL_op: %1").arg(opcode)) ? -1 : -1;
    }

    return 1;
}

bool Renderer::generateMainObject(const QByteArrayList& moduleNames, const QString& outputPath, bool indirect)
{
    // Generate a standalone main.o with _start that calls each module's $begin$ in order,
    // then exits.
    //
    // When indirect is false (freestanding / semihosting):
    //   _start calls each begin$ directly, then exits via ARM semihosting (angel_SWI 0x18).
    //
    // When indirect is true (C library linking):
    //   _start extracts argc/argv from the kernel stack, calls __mic$init(argc, argv).
    //   __mic$main is a separate function that calls each begin$ and returns 0.
    //   __mic$init (provided externally in C) handles e.g. libc init and calls __mic$main.

    ElfWriter elf(ElfWriter::ArchARM);
    Arm::Emitter em;

    // Create standard sections
    ElfWriter::StandardSections sec = elf.createStandardSections();
    quint32 textSymIdx = elf.addSectionSymbol(sec.text);
    Q_UNUSED(textSymIdx);

    // Create external symbols for each module's init procedure
    QList<quint32> initSymbols;
    for (int i = 0; i < moduleNames.size(); i++) {
        QByteArray symName = moduleNames[i] + "$begin$";
        quint32 symIdx = elf.addSymbol(symName, 0, 0, 0, STB_GLOBAL, STT_FUNC);
        initSymbols.append(symIdx);
    }

    if (indirect) {
        // External symbol for __mic$init (provided by C support code)
        quint32 micInitSym = elf.addSymbol("__mic$init", 0, 0, 0, STB_GLOBAL, STT_FUNC);

        // Emit _start:
        em.mov(R11, Operand2((quint32)0));  // clear frame pointer
        em.ldr_(AL, R0, MemOp(SP, 4, MemOp::PostIndexed)); // R0 = argc (pop from kernel stack)
        em.mov(R1, Operand2(SP));           // R1 = argv (SP now points to argv[0])
        // BL __mic$init (call never returns)
        quint32 blOffset = em.currentPosition();
        em.emitWord(0xEBFFFFFE);
        elf.addRelocation(sec.relText, blOffset, micInitSym, 28 /*R_ARM_CALL*/);
        // BKPT (should never reach here)
        em.emitWord(0xE1200070); // bkpt #0

        // Record end of _start, start of __mic$main
        quint32 micMainOff = em.currentPosition();

        // Emit __mic$main:
        em.str_(AL, LR, MemOp(SP, -4, MemOp::PreIndexed)); // push LR
        em.str_(AL, R11, MemOp(SP, -4, MemOp::PreIndexed)); // push FP
        em.mov(R11, Operand2(SP));          // FP = SP

        for (int i = 0; i < initSymbols.size(); i++) {
            quint32 off = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            elf.addRelocation(sec.relText, off, initSymbols[i], 28 /*R_ARM_CALL*/);
        }

        em.mov(R0, Operand2((quint32)0));   // return 0
        em.mov(SP, Operand2(R11));          // SP = FP
        em.ldr_(AL, R11, MemOp(SP, 4, MemOp::PostIndexed)); // pop FP, SP += 4
        em.ldr_(AL, PC, MemOp(SP, 4, MemOp::PostIndexed));  // pop PC (= LR), SP += 4

        elf.appendToSection(sec.text, em.machineCode());

        // Add global symbols
        elf.addSymbol("_start", sec.text, 0, micMainOff, STB_GLOBAL, STT_FUNC);
        elf.addSymbol("__mic$main", sec.text, micMainOff, em.currentPosition() - micMainOff, STB_GLOBAL, STT_FUNC);
    } else {
        // Emit _start:
        // clear frame pointer, marks end of stack frames
        em.mov(R11, Operand2((quint32)0));

        // Call each module init in order
        for (int i = 0; i < initSymbols.size(); i++) {
            // BL with placeholder (addend = -8 for ARM pipeline: imm24 = 0xFFFFFE = -2)
            quint32 blOffset = em.currentPosition();
            em.emitWord(0xEBFFFFFE);
            elf.addRelocation(sec.relText, blOffset, initSymbols[i], 28 /*R_ARM_CALL*/);
        }

        // Exit via MIC$$exit(0):
        em.mov(R0, Operand2((quint32)0));   // exit code 0
        quint32 exitSym = elf.addSymbol("MIC$$exit", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        quint32 exitBlOff = em.currentPosition();
        em.emitWord(0xEBFFFFFE);
        elf.addRelocation(sec.relText, exitBlOff, exitSym, 28 /*R_ARM_CALL*/);

        elf.appendToSection(sec.text, em.machineCode());

        // Add _start as global symbol
        elf.addSymbol("_start", sec.text, 0, em.currentPosition(), STB_GLOBAL, STT_FUNC);
    }

    // Write the ELF object file
    return elf.write(outputPath);
}
