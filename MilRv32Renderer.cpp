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

// This is a pretty direct translation of MilArmv7Renderer to RV32 with options for the ESP32-P4, C3, C5 and C6

#include "MilRv32Renderer.h"
#include <Micron/MicAtom.h>
#include <QtDebug>
#include <QFile>
using namespace Mil;
using namespace Vm;
using namespace Rv32;

static const int Slot4 = 4;  // i4, u4, r4, p
static const int Slot8 = 8;  // i8, u8, r8, pp

// Convert Micron symbol names to ELF-compatible names: module!proc -> module$proc
static inline QByteArray elfSymName(const QByteArray& micronName)
{
    QByteArray result = micronName;
    result.replace('!', '$');
    return result;
}

// Frame layout:
//   [arguments]    FP + 8 + argOffset   (pushed by caller)
//   [saved RA]     FP + 4
//   [saved FP]     FP + 0               <- FP points here
//   [locals]       FP - localsSize ... FP - 4
//   [eval stack]   SP (grows downward)
static const int FP_TO_ARGS = 8;

Renderer::Renderer(AstModel* mdl)
    : d_mdl(mdl), d_code(mdl, 4, 4), d_hasFloat(true), d_emitDwarf(false), d_useRvAbi(false), d_hasHwDiv(true),
      d_dwarf(0), d_localsSize(0), d_argsSize(0), d_returnSize(0),
      d_textSymIdx(0), d_dataSymIdx(0), d_rodataSymIdx(0), d_globalsSymIdx(0),
      d_elf(ElfWriter::ArchRISCV)
{
    d_code.setReverseArguments(true);
    memset(&d_sections, 0, sizeof(d_sections));
    registerExternals();
}

void Renderer::registerExternals()
{
    quint32 id = 0;
    foreach (Declaration* mod, d_mdl->getModules()) {
        const char* modName = mod->name.constData();
        for (Declaration* sub = mod->subs; sub; sub = sub->next) {
            if (sub->kind == Declaration::Procedure && (sub->extern_ || sub->foreign_)) {
                d_code.addExternal(modName, sub->name.constData(), id++);
            }
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
    quint32 symIdx = d_elf.addSymbol(name, 0, 0, 0, STB_GLOBAL, STT_FUNC);
    d_extProcSymbols[procIdx] = symIdx;
    return symIdx;
}

quint32 Renderer::getOrEmitVtable(int vtableIdx)
{
    if (d_vtableOffsets.contains(vtableIdx))
        return d_vtableOffsets[vtableIdx];

    Vm::Vtable* vt = d_code.getVtable(vtableIdx);
    Q_ASSERT(vt);

    quint32 curSize = d_elf.sectionSize(d_sections.data);
    quint32 pad = (4 - (curSize & 3)) & 3;
    if (pad > 0) {
        char zeros[4] = {0};
        d_elf.appendToSection(d_sections.data, zeros, pad);
    }
    quint32 vtableOff = d_elf.sectionSize(d_sections.data);

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
    while (t && t->kind == Type::NameRef)
        t = t->getType();
    if (!t)
        return;

    Emitter& em = d_emitter;
    FixupTracker& fix = d_fixups;

    if (t->kind == Type::Object && offset + 4 <= memSize) {
        quint32 rawVtBits = 0;
        memcpy(&rawVtBits, memData + offset, 4);
        if (rawVtBits) {
            int vtIdx = -1;
            for (int i = 0; i < d_code.vtableCount(); i++) {
                quint32 vtLow;
                Vtable* vt = d_code.getVtable(i);
                memcpy(&vtLow, &vt, 4);
                if (vtLow == rawVtBits) {
                    vtIdx = i;
                    break;
                }
            }
            if (vtIdx >= 0) {
                quint32 vtOff = getOrEmitVtable(vtIdx);
                // Load vtable address into T4 via LUI+ADDI with relocation
                quint32 luiOff = em.currentPosition();
                em.lui(T4, 0);
                em.addi(T4, T4, 0);
                fix.recordAbsAddr(luiOff, d_dataSymIdx, vtOff);
                // Store vtable pointer at destReg + offset
                if (offset == 0) {
                    em.sw(T4, destReg, 0);
                } else if ((qint32)offset >= -2048 && (qint32)offset <= 2047) {
                    em.sw(T4, destReg, (qint32)offset);
                } else {
                    loadImm32(T5, offset);
                    em.add(T5, destReg, T5);
                    em.sw(T4, T5, 0);
                }
            }
        }
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
            quint32 elemSize = et->getByteSize(4);
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

    // RISC-V relocation type for absolute 32-bit
    static const quint8 R_RISCV_32 = 1;

    if (t->kind == Type::Object) {
        int vtIdx = d_code.findVtableIdx(t);
        if (vtIdx >= 0) {
            quint32 vtOff = getOrEmitVtable(vtIdx);
            d_elf.addRelocation(d_sections.relData, baseOff, d_dataSymIdx, R_RISCV_32);
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

void Renderer::emitRuntimeVtableInit(Type* t, quint32 baseOff)
{
    while (t && t->kind == Type::NameRef)
        t = t->getType();
    if (!t)
        return;

    Emitter& em = d_emitter;
    FixupTracker& fix = d_fixups;

    if (t->kind == Type::Object) {
        int vtIdx = d_code.findVtableIdx(t);
        if (vtIdx >= 0) {
            quint32 vtOff = getOrEmitVtable(vtIdx);
            // Load address of global var at baseOff in MIC$GLOBALS
            quint32 luiOff0 = em.currentPosition();
            em.lui(T0, 0);
            em.addi(T0, T0, 0);
            fix.recordAbsAddr(luiOff0, d_globalsSymIdx, baseOff);

            // Load address of vtable in .data
            quint32 luiOff1 = em.currentPosition();
            em.lui(T1, 0);
            em.addi(T1, T1, 0);
            fix.recordAbsAddr(luiOff1, d_dataSymIdx, vtOff);

            // Store vtable pointer
            em.sw(T1, T0, 0);
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

    if (!d_code.compile(module))
        return setError(QString("VmCode compilation failed for module %1").arg(QString(module->name)));

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

    // Set RISC-V ELF flags for float ABI compatibility.
    // Use double-float (0x4) when hardware float is enabled, soft-float (0x0) otherwise.
    // This must match the ABI of the C runtime / sysroot being linked against.
    if (d_hasFloat)
        d_elf.setElfFlags(0x4); // EF_RISCV_FLOAT_ABI_DOUBLE
    else
        d_elf.setElfFlags(0x0); // EF_RISCV_FLOAT_ABI_SOFT

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

    quint32 varMemSize = d_mdl->getVarMemSize();
    if (varMemSize > 0) {
        static const quint16 SHN_COMMON = 0xFFF2;
        d_globalsSymIdx = d_elf.addSymbol("MIC$GLOBALS", SHN_COMMON,
                                           4, varMemSize,
                                           STB_GLOBAL, STT_OBJECT);
    }

    // Reserve 4 bytes in .data for module init guard
    quint32 initGuardOff = d_elf.sectionSize(d_sections.data);
    {
        quint32 zero = 0;
        d_elf.appendToSection(d_sections.data, (const char*)&zero, 4);
    }

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

    for (int i = 0; i < allProcs.size(); i++) {
        Declaration* d = allProcs[i].decl;
        QByteArray name;
        if (d->kind == Declaration::Module)
            name = elfSymName(d->name + QByteArray("$begin$"));
        else
            name = elfSymName(d->toPath());

        // In RV-ABI mode, all procedures must be GLOBAL so the linker can resolve cross-module references.
        bool isExported = (d->kind == Declaration::Module) || d->public_ || d_useRvAbi;
        quint8 binding = isExported ? STB_GLOBAL : STB_LOCAL;

        quint32 symIdx = d_elf.addSymbol(name, d_sections.text, 0, 0, binding, STT_FUNC);
        d_procSymbols[allProcs[i].codeIdx] = symIdx;
    }

    for (int i = 0; i < allProcs.size(); i++) {
        int procIdx = allProcs[i].codeIdx;
        Procedure* proc = d_code.getProc(procIdx);
        Q_ASSERT(proc);

        d_procTextOffsets[procIdx] = d_emitter.currentPosition();
        d_elf.setSymbolValue(d_procSymbols[procIdx], d_emitter.currentPosition());

        quint32 procStartPC = d_emitter.currentPosition();

        Rv32::Label initSkipLabel;
        bool isModuleInit = (allProcs[i].decl->kind == Declaration::Module);
        if (isModuleInit) {
            // Load guard address via LUI+ADDI with relocation
            quint32 luiOff = d_emitter.currentPosition();
            d_emitter.lui(T0, 0);
            d_emitter.addi(T0, T0, 0);
            d_fixups.recordAbsAddr(luiOff, d_dataSymIdx, initGuardOff);
            // Load guard value
            d_emitter.lw(T1, T0, 0);
            // Test: if already initialized, skip
            d_emitter.bne(T1, Zero, initSkipLabel);
            // Mark as initialized
            d_emitter.addi(T1, Zero, 1);
            d_emitter.sw(T1, T0, 0);

            if (d_globalsSymIdx)
                emitGlobalVarVtableInits(allProcs[i].decl);

            // NOTE: Import $begin$ calls are NOT emitted here because RV32's JAL
            // clobbers RA before the procedure prologue (emitted by renderProcedure)
            // has a chance to save it. The MIL bytecode body already contains
            // the import init calls, so they are emitted by renderProcedure instead.
        }

        if (!renderProcedure(*proc, procIdx))
            return false;

        if (isModuleInit) {
            d_emitter.bind(initSkipLabel);
            d_emitter.ret();
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

            if (declLine > 0)
                d_dwarf->addLineEntry(procStartPC, declLine);

            if (decl->kind == Declaration::Procedure) {
                QList<Declaration*> params = decl->getParams();
                for (int p = 0; p < params.size(); p++) {
                    Declaration* param = params[p];
                    quint32 typeRef = d_dwarf->getOrCreateType(param->getType());
                    qint32 fpOff = FP_TO_ARGS + param->off;
                    d_dwarf->addParameter(param->name, fpOff, typeRef);
                }

                QList<Declaration*> locals = decl->getLocals();
                for (int l = 0; l < locals.size(); l++) {
                    Declaration* loc = locals[l];
                    quint32 typeRef = d_dwarf->getOrCreateType(loc->getType());
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

    d_elf.appendToSection(d_sections.text, d_emitter.machineCode());

    d_fixups.generateRelocations(d_elf, d_sections.relText, d_sections.relData);

    QByteArray modContent = "module " + module->name + "\n";
    d_elf.appendToSection(d_sections.micronMod, modContent);

    if (d_dwarf) {
        QList<Declaration*> vars = module->getVars();
        for (int v = 0; v < vars.size(); v++) {
            Declaration* var = vars[v];
            quint32 typeRef = d_dwarf->getOrCreateType(var->getType());
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

void Renderer::emitPrologue(quint32 localsSize)
{
    Emitter& em = d_emitter;

    if (d_useRvAbi) {
        // RISC-V ABI mode: dump a0-a7 (32 bytes) so args are at FP+8.
        em.addi(Sp, Sp, -32);
        em.sw(A0, Sp, 0);
        em.sw(A1, Sp, 4);
        em.sw(A2, Sp, 8);
        em.sw(A3, Sp, 12);
        em.sw(A4, Sp, 16);
        em.sw(A5, Sp, 20);
        em.sw(A6, Sp, 24);
        em.sw(A7, Sp, 28);
    }
    // Save RA and FP
    em.addi(Sp, Sp, -8);
    em.sw(Ra, Sp, 4);  // saved RA at FP+4
    em.sw(Fp, Sp, 0);  // saved FP at FP+0
    em.mv(Fp, Sp);  // FP = SP

    // Allocate locals
    if (localsSize > 0) {
        if (localsSize <= 2047) {
            em.addi(Sp, Sp, -(qint32)localsSize);
        } else {
            loadImm32(T3, localsSize);
            em.sub(Sp, Sp, T3);
        }
    }
}

void Renderer::emitEpilogue()
{
    Emitter& em = d_emitter;
    // SP = FP (discard locals and eval stack)
    em.mv(Sp, Fp);
    // Restore FP and RA
    em.lw(Fp, Sp, 0);
    em.lw(Ra, Sp, 4);
    em.addi(Sp, Sp, 8);

    if (d_useRvAbi) {
        // Pop the a0-a7 dump area
        em.addi(Sp, Sp, 32);
    }

    em.ret();
}

void Renderer::loadImm32(Register rd, quint32 value)
{
    d_emitter.li(rd, value);
}

void Renderer::loadAddr(Register rd, quint32 symbolIdx, qint32 addend)
{
    quint32 luiOff = d_emitter.currentPosition();
    d_emitter.lui(rd, 0);
    d_emitter.addi(rd, rd, 0);
    d_fixups.recordAbsAddr(luiOff, symbolIdx, addend);
}


void Renderer::pushReg(Register r)
{
    d_emitter.addi(Sp, Sp, -Slot4);
    d_emitter.sw(r, Sp, 0);
}

void Renderer::popReg(Register r)
{
    d_emitter.lw(r, Sp, 0);
    d_emitter.addi(Sp, Sp, Slot4);
}

void Renderer::pushRegPair(Register rLo, Register rHi)
{
    d_emitter.addi(Sp, Sp, -Slot8);
    d_emitter.sw(rLo, Sp, 0);
    d_emitter.sw(rHi, Sp, 4);
}

void Renderer::popRegPair(Register rLo, Register rHi)
{
    d_emitter.lw(rLo, Sp, 0);
    d_emitter.lw(rHi, Sp, 4);
    d_emitter.addi(Sp, Sp, Slot8);
}

void Renderer::pushFloat(FRegister f)
{
    d_emitter.addi(Sp, Sp, -Slot4);
    d_emitter.fsw(f, Sp, 0);
}

void Renderer::popFloat(FRegister f)
{
    d_emitter.flw(f, Sp, 0);
    d_emitter.addi(Sp, Sp, Slot4);
}

void Renderer::pushDouble_soft(Register rLo, Register rHi)
{
    pushRegPair(rLo, rHi);
}

void Renderer::popDouble_soft(Register rLo, Register rHi)
{
    popRegPair(rLo, rHi);
}


void Renderer::loadLocalAddr(Register rd, quint32 milOffset)
{
    // Local at FP - d_localsSize + milOffset
    qint32 fpOff = -((qint32)d_localsSize) + (qint32)milOffset;
    if (fpOff >= -2048 && fpOff <= 2047) {
        d_emitter.addi(rd, Fp, fpOff);
    } else {
        loadImm32(rd, (quint32)((fpOff < 0) ? -fpOff : fpOff));
        if (fpOff < 0)
            d_emitter.sub(rd, Fp, rd);
        else
            d_emitter.add(rd, Fp, rd);
    }
}

void Renderer::loadArgAddr(Register rd, quint32 milOffset)
{
    qint32 fpOff = FP_TO_ARGS + (qint32)milOffset;
    if (fpOff >= -2048 && fpOff <= 2047) {
        d_emitter.addi(rd, Fp, fpOff);
    } else {
        loadImm32(rd, (quint32)fpOff);
        d_emitter.add(rd, Fp, rd);
    }
}

void Renderer::loadVarAddr(Register rd, quint32 milOffset)
{
    // Load address of module variable via LUI+ADDI with relocation against MIC$GLOBALS
    quint32 luiOff = d_emitter.currentPosition();
    d_emitter.lui(rd, 0);
    d_emitter.addi(rd, rd, 0);
    d_fixups.recordAbsAddr(luiOff, d_globalsSymIdx, milOffset);
}


quint32 Renderer::emitArgAlignment(Declaration* decl)
{
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

    Emitter& em = d_emitter;

    if (gap <= 2047)
        em.addi(Sp, Sp, -(qint32)gap);
    else {
        loadImm32(T3, gap);
        em.sub(Sp, Sp, T3);
    }

    struct ParamInfo {
        quint32 contigOff;
        quint32 alignedOff;
        quint32 size;
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
        for (quint32 w = 0; w < pinfo[i].size; w += 4) {
            qint32 src = (qint32)(srcOff + w);
            qint32 dst = (qint32)(dstOff + w);
            if (src >= -2048 && src <= 2047 && dst >= -2048 && dst <= 2047) {
                em.lw(T0, Sp, src);
                em.sw(T0, Sp, dst);
            } else {
                loadImm32(T3, srcOff + w);
                em.add(T3, Sp, T3);
                em.lw(T0, T3, 0);
                loadImm32(T3, dstOff + w);
                em.add(T3, Sp, T3);
                em.sw(T0, T3, 0);
            }
        }
    }

    return gap;
}


void Renderer::scanBranchTargets(Procedure& proc)
{
    for (int pc = 0; pc < (int)proc.ops.size(); pc++) {
        LL_op op = (LL_op)proc.ops[pc].op;
        if (op == LL_br || op == LL_brfalse_i4) {
            int target = pc + 1 + (proc.ops[pc].minus ? -1 : 1) * (int)proc.ops[pc].val;
            getLabel(target);
        }
    }
}

Label& Renderer::getLabel(int milPc)
{
    if (!d_branchLabels.contains(milPc)) {
        Label* lbl = new Label();
        d_branchLabels[milPc] = lbl;
        d_allLabels.append(lbl);
    }
    return *d_branchLabels[milPc];
}

void Renderer::bindLabelIfNeeded(int milPc)
{
    if (d_branchLabels.contains(milPc)) {
        d_emitter.bind(*d_branchLabels[milPc]);
    }
}

void Renderer::clearLabels()
{
    qDeleteAll(d_allLabels);
    d_allLabels.clear();
    d_branchLabels.clear();
}


void Renderer::emitCallStackAdj(quint32 argsSize, quint32 returnSize)
{
    Emitter& em = d_emitter;

    if (returnSize == 0) {
        if (argsSize > 0) {
            if (argsSize <= 2047)
                em.addi(Sp, Sp, (qint32)argsSize);
            else {
                loadImm32(T3, argsSize);
                em.add(Sp, Sp, T3);
            }
        }
    } else {
        qint32 adj = (qint32)argsSize - (qint32)returnSize;
        if (adj > 0) {
            for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                qint32 srcOff = w;
                qint32 dstOff = w + adj;
                if (srcOff >= -2048 && srcOff <= 2047)
                    em.lw(T0, Sp, srcOff);
                else {
                    loadImm32(T3, (quint32)srcOff);
                    em.add(T3, Sp, T3);
                    em.lw(T0, T3, 0);
                }
                if (dstOff >= -2048 && dstOff <= 2047)
                    em.sw(T0, Sp, dstOff);
                else {
                    loadImm32(T3, (quint32)dstOff);
                    em.add(T3, Sp, T3);
                    em.sw(T0, T3, 0);
                }
            }
            if (adj <= 2047)
                em.addi(Sp, Sp, adj);
            else {
                loadImm32(T3, (quint32)adj);
                em.add(Sp, Sp, T3);
            }
        } else if (adj < 0) {
            quint32 neg = (quint32)(-adj);
            if (neg <= 2047)
                em.addi(Sp, Sp, -(qint32)neg);
            else {
                loadImm32(T3, neg);
                em.sub(Sp, Sp, T3);
            }
            for (quint32 w = 0; w < returnSize; w += 4) {
                qint32 srcOff = (qint32)(w + neg);
                qint32 dstOff = (qint32)w;
                if (srcOff >= -2048 && srcOff <= 2047)
                    em.lw(T0, Sp, srcOff);
                else {
                    loadImm32(T3, (quint32)srcOff);
                    em.add(T3, Sp, T3);
                    em.lw(T0, T3, 0);
                }
                if (dstOff >= -2048 && dstOff <= 2047)
                    em.sw(T0, Sp, dstOff);
                else {
                    loadImm32(T3, (quint32)dstOff);
                    em.add(T3, Sp, T3);
                    em.sw(T0, T3, 0);
                }
            }
        }
    }
}

bool Renderer::setError(const QString& msg)
{
    d_error = msg;
    qCritical() << "Rv32Renderer:" << msg;
    return false;
}


void Renderer::emitCall(quint32 symbolIdx)
{
    // Emit AUIPC+JALR pair (8 bytes) with R_RISCV_CALL relocation
    quint32 callOff = d_emitter.currentPosition();
    d_emitter.auipc(T3, 0); // T3 = PC + hi20
    d_emitter.jalr(Ra, T3, 0);  // call T3 + lo12
    d_fixups.recordCall(callOff, symbolIdx);
}

void Renderer::emitIndirectCall(Register target)
{
    d_emitter.jalr(Ra, target, 0);
}

// Call an external C function (e.g. __mic$ helpers, MIC$$alloc/free).
// In RV-ABI mode: loads args from the eval stack into a0-a7, calls,
// and pushes the return value (from a0/a0:a1) back onto the eval stack.
// In old convention mode: args are already on the stack; uses emitCallStackAdj.
void Renderer::emitExternCall(quint32 symbolIdx, quint32 argsSize, quint32 returnSize,
                              bool swapHalves)
{
    // swapHalves: for binary operations (e.g. add, sub, div, cmp) where the eval
    // stack has the RIGHT operand on top and the LEFT operand below, but the C
    // function expects the left operand first (in lower-numbered registers).
    // When true, loads the bottom half of the stack area into a0..aN first,
    // then the top half into aN+1..aM.

    using namespace Rv32;
    Emitter& em = d_emitter;

    if (d_useRvAbi) {
        quint32 words = argsSize / 4;
        Q_ASSERT(words <= 8);
        if (swapHalves && words >= 2) {
            // Binary operation: stack has [sp+0]=right_op, [sp+half]=left_op
            // C function expects a0..=left_op, then right_op
            quint32 half = words / 2;
            quint32 reg = 0;
            // Load left operand (from bottom half of stack area) into a0..
            for (quint32 i = 0; i < half; i++)
                em.lw((Register)(A0 + reg++), Sp, (qint32)((half + i) * 4));
            // Load right operand (from top half of stack area) into next regs
            for (quint32 i = 0; i < half; i++)
                em.lw((Register)(A0 + reg++), Sp, (qint32)(i * 4));
        } else {
            for (quint32 i = 0; i < words; i++)
                em.lw((Register)(A0 + i), Sp, (qint32)(i * 4));
        }
        if (argsSize > 0) {
            if ((qint32)argsSize <= 2047)
                em.addi(Sp, Sp, (qint32)argsSize);
            else {
                loadImm32(T3, argsSize);
                em.add(Sp, Sp, T3);
            }
        }
        emitCall(symbolIdx);
        if (returnSize >= 8)
            pushRegPair(A0, A1);
        else if (returnSize >= 4)
            pushReg(A0);
    } else {
        emitCall(symbolIdx);
        emitCallStackAdj(argsSize, returnSize);
    }
}


#define LOAD_LOCAL_I4(reg, off) \
    do { qint32 _fo = -((qint32)d_localsSize) + (qint32)(off); \
         if (_fo >= -2048 && _fo <= 2047) d_emitter.lw(reg, Fp, _fo); \
         else { loadLocalAddr(T6, off); d_emitter.lw(reg, T6, 0); } } while(0)

#define STORE_LOCAL_I4(reg, off) \
    do { qint32 _fo = -((qint32)d_localsSize) + (qint32)(off); \
         if (_fo >= -2048 && _fo <= 2047) d_emitter.sw(reg, Fp, _fo); \
         else { loadLocalAddr(T6, off); d_emitter.sw(reg, T6, 0); } } while(0)

#define LOAD_ARG_I4(reg, off) \
    do { qint32 _fo = FP_TO_ARGS + (qint32)(off); \
         if (_fo >= -2048 && _fo <= 2047) d_emitter.lw(reg, Fp, _fo); \
         else { loadArgAddr(T6, off); d_emitter.lw(reg, T6, 0); } } while(0)

#define STORE_ARG_I4(reg, off) \
    do { qint32 _fo = FP_TO_ARGS + (qint32)(off); \
         if (_fo >= -2048 && _fo <= 2047) d_emitter.sw(reg, Fp, _fo); \
         else { loadArgAddr(T6, off); d_emitter.sw(reg, T6, 0); } } while(0)

int Renderer::emitOp(Procedure& proc, int pc)
{
    const Operation& op = proc.ops[pc];
    const LL_op opcode = (LL_op)op.op;
    const quint32 val = op.val;
    Emitter& em = d_emitter;
    FixupTracker& fix = d_fixups;

    switch (opcode) {

    case LL_ldc_i4: {
        qint64 intVal = d_code.getInt(val);
        loadImm32(T0, (quint32)(qint32)intVal);
        pushReg(T0);
        return 1;
    }
    case LL_ldc_i8: {
        qint64 intVal = d_code.getInt(val);
        loadImm32(T0, (quint32)(intVal & 0xFFFFFFFF));
        loadImm32(T1, (quint32)((intVal >> 32) & 0xFFFFFFFF));
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldc_i4_m1:
        loadImm32(T0, 0xFFFFFFFF); pushReg(T0);
        return 1;
    case LL_ldc_i4_0:
        pushReg(Zero);
        return 1;
    case LL_ldc_i4_1:
        em.addi(T0, Zero, 1); pushReg(T0);
        return 1;
    case LL_ldc_i4_2:
        em.addi(T0, Zero, 2); pushReg(T0);
        return 1;
    case LL_ldc_i4_3:
        em.addi(T0, Zero, 3); pushReg(T0);
        return 1;
    case LL_ldc_i4_4:
        em.addi(T0, Zero, 4); pushReg(T0);
        return 1;
    case LL_ldc_i4_5:
        em.addi(T0, Zero, 5); pushReg(T0);
        return 1;
    case LL_ldc_i4_6:
        em.addi(T0, Zero, 6); pushReg(T0);
        return 1;
    case LL_ldc_i4_7:
        em.addi(T0, Zero, 7); pushReg(T0);
        return 1;
    case LL_ldc_i4_8:
        em.addi(T0, Zero, 8); pushReg(T0);
        return 1;

    case LL_ldc_r4: {
        double dval = d_code.getDouble(val);
        float fval = (float)dval;
        quint32 rodataOff;
        if (d_floatOffsets.contains(val)) {
            rodataOff = d_floatOffsets[val];
        } else {
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
        if (d_hasFloat) {
            // Load via LUI+FLW
            quint32 luiOff = em.currentPosition();
            em.lui(T3, 0);
            em.flw(Ft0, T3, 0);
            fix.recordAbsLoad(luiOff, d_rodataSymIdx, rodataOff);
            pushFloat(Ft0);
        } else {
            // No F extension: load as integer bits
            quint32 luiOff = em.currentPosition();
            em.lui(T3, 0);
            em.lw(T0, T3, 0);
            fix.recordAbsLoad(luiOff, d_rodataSymIdx, rodataOff);
            pushReg(T0);
        }
        return 1;
    }
    case LL_ldc_r8: {
        double dval = d_code.getDouble(val);
        quint32 rodataOff;
        if (d_doubleOffsets.contains(val)) {
            rodataOff = d_doubleOffsets[val];
        } else {
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
        // Doubles are always soft-float on RV32IMAF: load as two words
        quint32 luiOff = em.currentPosition();
        em.lui(T3, 0);
        em.addi(T3, T3, 0);
        fix.recordAbsAddr(luiOff, d_rodataSymIdx, rodataOff);
        em.lw(T0, T3, 0);
        em.lw(T1, T3, 4);
        pushRegPair(T0, T1);
        return 1;
    }

    case LL_ldnull:
        pushReg(Zero);
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
        loadAddr(T0, d_rodataSymIdx, rodataOff);
        pushReg(T0);
        return 1;
    }

    case LL_ldobj: {
        const std::vector<char>& obj = d_code.getObject(val);
        quint32 curSize = d_elf.sectionSize(d_sections.rodata);
        if (curSize & 3) {
            quint32 pad = 4 - (curSize & 3);
            char zeros[4] = {0};
            d_elf.appendToSection(d_sections.rodata, zeros, pad);
        }
        quint32 rodataOff = d_elf.sectionSize(d_sections.rodata);
        d_elf.appendToSection(d_sections.rodata, obj.data(), obj.size());
        loadAddr(T3, d_rodataSymIdx, rodataOff);
        quint32 size = obj.size();
        quint32 aligned = (size + 3) & ~3;
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 1;
    }

    case LL_ldloc_i1: {
        loadLocalAddr(T3, val);
        em.lb(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldloc_i2: {
        loadLocalAddr(T3, val);
        em.lh(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldloc_i4:
    case LL_ldloc_u4: {
        LOAD_LOCAL_I4(T0, val);
        pushReg(T0);
        return 1;
    }
    case LL_ldloc_i8:
    case LL_ldloc_u8: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.lw(T0, Fp, fo);
            em.lw(T1, Fp, fo + 4);
        } else {
            loadLocalAddr(T3, val);
            em.lw(T0, T3, 0);
            em.lw(T1, T3, 4);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldloc_u1: {
        loadLocalAddr(T3, val);
        em.lbu(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldloc_u2: {
        loadLocalAddr(T3, val);
        em.lhu(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldloc_r4: {
        LOAD_LOCAL_I4(T0, val);
        pushReg(T0);
        return 1;
    }
    case LL_ldloc_r8: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.lw(T0, Fp, fo);
            em.lw(T1, Fp, fo + 4);
        } else {
            loadLocalAddr(T3, val);
            em.lw(T0, T3, 0);
            em.lw(T1, T3, 4);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldloc_p: {
        LOAD_LOCAL_I4(T0, val);
        pushReg(T0);
        return 1;
    }
    case LL_ldloc_pp: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.lw(T0, Fp, fo);
            em.lw(T1, Fp, fo + 4);
        } else {
            loadLocalAddr(T3, val);
            em.lw(T0, T3, 0);
            em.lw(T1, T3, 4);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldloca:
        loadLocalAddr(T0, val);
        pushReg(T0);
        return 1;
    case LL_ldloc_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadLocalAddr(T3, val);
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 2;
    }

    case LL_stloc_i1: {
        popReg(T0);
        loadLocalAddr(T3, val);
        em.sb(T0, T3, 0);
        return 1;
    }
    case LL_stloc_i2: {
        popReg(T0);
        loadLocalAddr(T3, val);
        em.sh(T0, T3, 0);
        return 1;
    }
    case LL_stloc_i4:
    case LL_stloc_r4:
    case LL_stloc_p: {
        popReg(T0);
        STORE_LOCAL_I4(T0, val);
        return 1;
    }
    case LL_stloc_i8:
    case LL_stloc_r8: {
        popRegPair(T0, T1);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.sw(T0, Fp, fo);
            em.sw(T1, Fp, fo + 4);
        } else {
            loadLocalAddr(T3, val);
            em.sw(T0, T3, 0);
            em.sw(T1, T3, 4);
        }
        return 1;
    }
    case LL_stloc_pp: {
        popRegPair(T0, T1);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.sw(T0, Fp, fo);
            em.sw(T1, Fp, fo + 4);
        } else {
            loadLocalAddr(T3, val);
            em.sw(T0, T3, 0);
            em.sw(T1, T3, 4);
        }
        return 1;
    }
    case LL_stloc_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadLocalAddr(T3, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, Sp, (qint32)i);
            em.sw(T0, T3, (qint32)i);
        }
        if (aligned <= 2047)
            em.addi(Sp, Sp, (qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.add(Sp, Sp, T4);
        }
        return 2;
    }

    case LL_ldarg_i1: {
        loadArgAddr(T3, val);
        em.lb(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldarg_i2: {
        loadArgAddr(T3, val);
        em.lh(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldarg_i4:
    case LL_ldarg_u4:
    case LL_ldarg_r4:
    case LL_ldarg_p: {
        LOAD_ARG_I4(T0, val);
        pushReg(T0);
        return 1;
    }
    case LL_ldarg_i8:
    case LL_ldarg_u8:
    case LL_ldarg_r8: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.lw(T0, Fp, fo);
            em.lw(T1, Fp, fo + 4);
        } else {
            loadArgAddr(T3, val);
            em.lw(T0, T3, 0);
            em.lw(T1, T3, 4);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldarg_u1: {
        loadArgAddr(T3, val);
        em.lbu(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldarg_u2: {
        loadArgAddr(T3, val);
        em.lhu(T0, T3, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldarg_pp: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.lw(T0, Fp, fo);
            em.lw(T1, Fp, fo + 4);
        } else {
            loadArgAddr(T3, val);
            em.lw(T0, T3, 0);
            em.lw(T1, T3, 4);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldarg_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadArgAddr(T3, val);
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 2;
    }
    case LL_ldarga:
        loadArgAddr(T0, val);
        pushReg(T0);
        return 1;

    case LL_starg_i1: {
        popReg(T0);
        loadArgAddr(T3, val);
        em.sb(T0, T3, 0);
        return 1;
    }
    case LL_starg_i2: {
        popReg(T0);
        loadArgAddr(T3, val);
        em.sh(T0, T3, 0);
        return 1;
    }
    case LL_starg_i4:
    case LL_starg_r4:
    case LL_starg_p: {
        popReg(T0);
        STORE_ARG_I4(T0, val);
        return 1;
    }
    case LL_starg_i8:
    case LL_starg_r8: {
        popRegPair(T0, T1);
        qint32 fo = FP_TO_ARGS + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.sw(T0, Fp, fo);
            em.sw(T1, Fp, fo + 4);
        } else {
            loadArgAddr(T3, val);
            em.sw(T0, T3, 0);
            em.sw(T1, T3, 4);
        }
        return 1;
    }
    case LL_starg_pp: {
        popRegPair(T0, T1);
        qint32 fo = FP_TO_ARGS + (qint32)val;
        if (fo >= -2048 && fo + 4 <= 2047) {
            em.sw(T0, Fp, fo);
            em.sw(T1, Fp, fo + 4);
        } else {
            loadArgAddr(T3, val);
            em.sw(T0, T3, 0);
            em.sw(T1, T3, 4);
        }
        return 1;
    }
    case LL_starg_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadArgAddr(T3, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, Sp, (qint32)i);
            em.sw(T0, T3, (qint32)i);
        }
        if (aligned <= 2047)
            em.addi(Sp, Sp, (qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.add(Sp, Sp, T4);
        }
        return 2;
    }

    case LL_ldvar_i1:
        loadVarAddr(T3, val);
        em.lb(T0, T3, 0);
        pushReg(T0);
        return 1;
    case LL_ldvar_i2:
        loadVarAddr(T3, val);
        em.lh(T0, T3, 0);
        pushReg(T0);
        return 1;
    case LL_ldvar_i4:
    case LL_ldvar_u4:
    case LL_ldvar_r4:
    case LL_ldvar_p:
        loadVarAddr(T3, val);
        em.lw(T0, T3, 0);
        pushReg(T0);
        return 1;
    case LL_ldvar_i8:
    case LL_ldvar_u8:
    case LL_ldvar_r8:
    case LL_ldvar_pp:
        loadVarAddr(T3, val);
        em.lw(T0, T3, 0);
        em.lw(T1, T3, 4);
        pushRegPair(T0, T1);
        return 1;
    case LL_ldvar_u1:
        loadVarAddr(T3, val);
        em.lbu(T0, T3, 0);
        pushReg(T0);
        return 1;
    case LL_ldvar_u2:
        loadVarAddr(T3, val);
        em.lhu(T0, T3, 0);
        pushReg(T0);
        return 1;
    case LL_ldvara:
        loadVarAddr(T0, val);
        pushReg(T0);
        return 1;
    case LL_ldvar_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadVarAddr(T3, val);
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 2;
    }

    case LL_stvar_i1:
        popReg(T0);
        loadVarAddr(T3, val);
        em.sb(T0, T3, 0);
        return 1;
    case LL_stvar_i2:
        popReg(T0);
        loadVarAddr(T3, val);
        em.sh(T0, T3, 0);
        return 1;
    case LL_stvar_i4:
    case LL_stvar_r4:
    case LL_stvar_p:
        popReg(T0);
        loadVarAddr(T3, val);
        em.sw(T0, T3, 0);
        return 1;
    case LL_stvar_i8:
    case LL_stvar_r8:
    case LL_stvar_pp:
        popRegPair(T0, T1);
        loadVarAddr(T3, val);
        em.sw(T0, T3, 0);
        em.sw(T1, T3, 4);
        return 1;
    case LL_stvar_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadVarAddr(T3, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, Sp, (qint32)i);
            em.sw(T0, T3, (qint32)i);
        }
        if (aligned <= 2047)
            em.addi(Sp, Sp, (qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.add(Sp, Sp, T4);
        }
        return 2;
    }

    case LL_add_i4:
        popReg(T1); popReg(T0);
        em.add(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_sub_i4:
        popReg(T1); popReg(T0);
        em.sub(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_mul_i4:
        popReg(T1); popReg(T0);
        em.mul(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_div_i4:
        popReg(T1); popReg(T0);
        em.div(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_div_un_i4:
        popReg(T1); popReg(T0);
        em.divu(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_rem_i4:
        popReg(T1); popReg(T0);
        em.rem(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_rem_un_i4:
        popReg(T1); popReg(T0);
        em.remu(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_neg_i4:
        popReg(T0);
        em.sub(T0, Zero, T0);
        pushReg(T0);
        return 1;
    case LL_abs_i4: {
        popReg(T0);
        // if T0 < 0, negate: T1 = T0 >> 31 (all sign bits)
        em.srai(T1, T0, 31);
        em.xor_(T0, T0, T1); // flip bits if negative
        em.sub(T0, T0, T1); // add 1 if negative
        pushReg(T0);
        return 1;
    }

    case LL_add_i8:
        popRegPair(T4, T5); popRegPair(T0, T1);
        // 64-bit add: lo = T0+T4, carry = (lo < T0), hi = T1+T5+carry
        em.add(T2, T0, T4); // T2 = lo
        em.sltu(T3, T2, T0); // T3 = carry
        em.add(T1, T1, T5);
        em.add(T1, T1, T3); // hi += carry
        em.mv(T0, T2);
        pushRegPair(T0, T1);
        return 1;
    case LL_sub_i8:
        popRegPair(T4, T5); popRegPair(T0, T1);
        em.sltu(T3, T0, T4);  // borrow = (T0 < T4)
        em.sub(T0, T0, T4);
        em.sub(T1, T1, T5);
        em.sub(T1, T1, T3);  // hi -= borrow
        pushRegPair(T0, T1);
        return 1;
    case LL_mul_i8:
        // 64x64->64: result_lo = lo(a_lo*b_lo), result_hi = hi(a_lo*b_lo) + a_lo*b_hi + a_hi*b_lo
        popRegPair(T4, T5); popRegPair(T0, T1);
        // T2 = hi(T0 * T4), T3 = lo(T0 * T4)
        em.mulhu(T2, T0, T4);    // hi of unsigned a_lo * b_lo
        em.mul(T3, T0, T4); // lo = a_lo * b_lo
        em.mul(T6, T0, T5); // a_lo * b_hi
        em.add(T2, T2, T6);
        em.mul(T6, T1, T4);  // a_hi * b_lo
        em.add(T2, T2, T6);
        em.mv(T0, T3);
        em.mv(T1, T2);
        pushRegPair(T0, T1);
        return 1;
    case LL_div_i8:
    case LL_div_un_i8:
    case LL_rem_i8:
    case LL_rem_un_i8: {
        // 64-bit div/rem via __mic$ intrinsic call (C function).
        // Stack has b (top) then a.
        const char* funcName;
        switch ((LL_op)op.op) {
        case LL_div_i8:
            funcName = "__mic$div_i8"; break;
        case LL_div_un_i8:
            funcName = "__mic$div_un_i8"; break;
        case LL_rem_i8:
            funcName = "__mic$rem_i8"; break;
        default:
            funcName = "__mic$rem_un_i8"; break;
        }
        quint32 sym = d_elf.addSymbol(funcName, 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 8, true);
        return 1;
    }
    case LL_neg_i8:
        popRegPair(T0, T1);
        // negate 64-bit: ~T0:T1 + 1
        em.xori(T0, T0, -1);
        em.xori(T1, T1, -1);
        em.addi(T0, T0, 1);
        em.seqz(T2, T0); // carry if lo was 0 after +1 (wrapped)
        em.add(T1, T1, T2);
        pushRegPair(T0, T1);
        return 1;
    case LL_abs_i8: {
        popRegPair(T0, T1);
        // if hi < 0, negate
        Rv32::Label skipNeg;
        em.bge(T1, Zero, skipNeg);
        em.xori(T0, T0, -1);
        em.xori(T1, T1, -1);
        em.addi(T0, T0, 1);
        em.seqz(T2, T0);
        em.add(T1, T1, T2);
        em.bind(skipNeg);
        pushRegPair(T0, T1);
        return 1;
    }

    case LL_add_r4:
        if (d_hasFloat) {
            popFloat(Ft1); popFloat(Ft0);
            em.fadd_s(Ft0, Ft0, Ft1);
            pushFloat(Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$add_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 8, 4, true);
        }
        return 1;
    case LL_sub_r4:
        if (d_hasFloat) {
            popFloat(Ft1); popFloat(Ft0);
            em.fsub_s(Ft0, Ft0, Ft1);
            pushFloat(Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$sub_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 8, 4, true);
        }
        return 1;
    case LL_mul_r4:
        if (d_hasFloat) {
            popFloat(Ft1); popFloat(Ft0);
            em.fmul_s(Ft0, Ft0, Ft1);
            pushFloat(Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$mul_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 8, 4, true);
        }
        return 1;
    case LL_div_r4:
        if (d_hasFloat) {
            popFloat(Ft1); popFloat(Ft0);
            em.fdiv_s(Ft0, Ft0, Ft1);
            pushFloat(Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$div_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 8, 4, true);
        }
        return 1;
    case LL_neg_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fneg_s(Ft0, Ft0);
            pushFloat(Ft0);
        } else {
            // Flip sign bit in integer representation
            popReg(T0);
            em.lui(T1, 0x80000);  // 0x80000000
            em.xor_(T0, T0, T1);
            pushReg(T0);
        }
        return 1;
    case LL_abs_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fabs_s(Ft0, Ft0);
            pushFloat(Ft0);
        } else {
            // Clear sign bit
            popReg(T0);
            em.lui(T1, 0x80000);  // 0x80000000
            em.addi(T1, T1, -1);  // 0x7FFFFFFF
            em.and_(T0, T0, T1);
            pushReg(T0);
        }
        return 1;

    case LL_add_r8: {
        quint32 sym = d_elf.addSymbol("__mic$add_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 8, true);
        return 1;
    }
    case LL_sub_r8: {
        quint32 sym = d_elf.addSymbol("__mic$sub_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 8, true);
        return 1;
    }
    case LL_mul_r8: {
        quint32 sym = d_elf.addSymbol("__mic$mul_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 8, true);
        return 1;
    }
    case LL_div_r8: {
        quint32 sym = d_elf.addSymbol("__mic$div_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 8, true);
        return 1;
    }
    case LL_neg_r8:
        popRegPair(T0, T1);
        em.lui(T2, 0x80000);
        em.xor_(T1, T1, T2);  // flip sign bit in high word
        pushRegPair(T0, T1);
        return 1;
    case LL_abs_r8:
        popRegPair(T0, T1);
        em.lui(T2, 0x80000);
        em.addi(T2, T2, -1);  // 0x7FFFFFFF
        em.and_(T1, T1, T2);  // clear sign bit in high word
        pushRegPair(T0, T1);
        return 1;

    case LL_and_i4:
        popReg(T1); popReg(T0);
        em.and_(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_or_i4:
        popReg(T1); popReg(T0);
        em.or_(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_xor_i4:
        popReg(T1); popReg(T0);
        em.xor_(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_not_i4:
        popReg(T0);
        em.xori(T0, T0, -1);  // NOT = XOR with all 1s
        pushReg(T0);
        return 1;
    case LL_shl_i4:
        popReg(T1); popReg(T0);
        em.sll(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_shr_i4:
        popReg(T1); popReg(T0);
        em.sra(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_shr_un_i4:
        popReg(T1); popReg(T0);
        em.srl(T0, T0, T1);
        pushReg(T0);
        return 1;

    case LL_and_i8:
        popRegPair(T4, T5); popRegPair(T0, T1);
        em.and_(T0, T0, T4);
        em.and_(T1, T1, T5);
        pushRegPair(T0, T1);
        return 1;
    case LL_or_i8:
        popRegPair(T4, T5); popRegPair(T0, T1);
        em.or_(T0, T0, T4);
        em.or_(T1, T1, T5);
        pushRegPair(T0, T1);
        return 1;
    case LL_xor_i8:
        popRegPair(T4, T5); popRegPair(T0, T1);
        em.xor_(T0, T0, T4);
        em.xor_(T1, T1, T5);
        pushRegPair(T0, T1);
        return 1;
    case LL_not_i8:
        popRegPair(T0, T1);
        em.xori(T0, T0, -1);
        em.xori(T1, T1, -1);
        pushRegPair(T0, T1);
        return 1;
    case LL_shl_i8:
        // 64-bit shift left by T4 (shift amount from stack as i4)
        popReg(T4); popRegPair(T0, T1);
        // T1 = (T1 << T4) | (T0 >> (32-T4)); T0 = T0 << T4
        em.addi(T5, Zero, 32);
        em.sub(T5, T5, T4);  // T5 = 32 - shift
        em.srl(T6, T0, T5);   // T6 = T0 >> (32 - shift)
        em.sll(T1, T1, T4);  // T1 <<= shift
        em.or_(T1, T1, T6);
        em.sll(T0, T0, T4);
        pushRegPair(T0, T1);
        return 1;
    case LL_shr_i8:
        popReg(T4); popRegPair(T0, T1);
        em.addi(T5, Zero, 32);
        em.sub(T5, T5, T4);
        em.sll(T6, T1, T5);
        em.srl(T0, T0, T4);
        em.or_(T0, T0, T6);
        em.sra(T1, T1, T4);
        pushRegPair(T0, T1);
        return 1;
    case LL_shr_un_i8:
        popReg(T4); popRegPair(T0, T1);
        em.addi(T5, Zero, 32);
        em.sub(T5, T5, T4);
        em.sll(T6, T1, T5);
        em.srl(T0, T0, T4);
        em.or_(T0, T0, T6);
        em.srl(T1, T1, T4);
        pushRegPair(T0, T1);
        return 1;

    case LL_ceq_i4:
        popReg(T1); popReg(T0);
        em.xor_(T0, T0, T1);
        em.seqz(T0, T0);
        pushReg(T0);
        return 1;
    case LL_cgt_i4:
        popReg(T1); popReg(T0);
        em.slt(T0, T1, T0);  // T0 > T1 iff T1 < T0
        pushReg(T0);
        return 1;
    case LL_clt_i4:
        popReg(T1); popReg(T0);
        em.slt(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_cgt_u4:
        popReg(T1); popReg(T0);
        em.sltu(T0, T1, T0);
        pushReg(T0);
        return 1;
    case LL_clt_u4:
        popReg(T1); popReg(T0);
        em.sltu(T0, T0, T1);
        pushReg(T0);
        return 1;
    case LL_ceq_p:
    case LL_cgt_p:
    case LL_clt_p:
        popReg(T1); popReg(T0);
        if (opcode == LL_ceq_p) {
            em.xor_(T0, T0, T1);
            em.seqz(T0, T0);
        } else if (opcode == LL_cgt_p) {
            em.sltu(T0, T1, T0);
        } else {
            em.sltu(T0, T0, T1);
        }
        pushReg(T0);
        return 1;

    case LL_ceq_i8: {
        popRegPair(T4, T5); popRegPair(T0, T1);
        em.xor_(T0, T0, T4);
        em.xor_(T1, T1, T5);
        em.or_(T0, T0, T1);
        em.seqz(T0, T0);
        pushReg(T0);
        return 1;
    }
    case LL_cgt_i8: {
        // a > b (signed): hi(a) > hi(b) || (hi(a)==hi(b) && lo(a) > lo(b) unsigned)
        popRegPair(T4, T5); popRegPair(T0, T1);
        Rv32::Label hiGt, hiLt, done8;
        em.blt(T5, T1, hiGt);  // if b_hi < a_hi -> a > b
        em.blt(T1, T5, hiLt);   // if a_hi < b_hi -> a <= b
        // hi equal: unsigned compare lo
        em.sltu(T0, T4, T0);   // lo(a) > lo(b) unsigned
        em.j(done8);
        em.bind(hiGt);
        em.addi(T0, Zero, 1);
        em.j(done8);
        em.bind(hiLt);
        em.mv(T0, Zero);
        em.bind(done8);
        pushReg(T0);
        return 1;
    }
    case LL_clt_i8: {
        popRegPair(T4, T5); popRegPair(T0, T1);
        Rv32::Label hiLt2, hiGt2, done82;
        em.blt(T1, T5, hiLt2);
        em.blt(T5, T1, hiGt2);
        em.sltu(T0, T0, T4);
        em.j(done82);
        em.bind(hiLt2);
        em.addi(T0, Zero, 1);
        em.j(done82);
        em.bind(hiGt2);
        em.mv(T0, Zero);
        em.bind(done82);
        pushReg(T0);
        return 1;
    }
    case LL_cgt_u8: {
        popRegPair(T4, T5); popRegPair(T0, T1);
        Rv32::Label hiGtu, hiLtu, doneu;
        em.bltu(T5, T1, hiGtu);
        em.bltu(T1, T5, hiLtu);
        em.sltu(T0, T4, T0);
        em.j(doneu);
        em.bind(hiGtu);
        em.addi(T0, Zero, 1);
        em.j(doneu);
        em.bind(hiLtu);
        em.mv(T0, Zero);
        em.bind(doneu);
        pushReg(T0);
        return 1;
    }
    case LL_clt_u8: {
        popRegPair(T4, T5); popRegPair(T0, T1);
        Rv32::Label hiLtu2, hiGtu2, doneu2;
        em.bltu(T1, T5, hiLtu2);
        em.bltu(T5, T1, hiGtu2);
        em.sltu(T0, T0, T4);
        em.j(doneu2);
        em.bind(hiLtu2);
        em.addi(T0, Zero, 1);
        em.j(doneu2);
        em.bind(hiGtu2);
        em.mv(T0, Zero);
        em.bind(doneu2);
        pushReg(T0);
        return 1;
    }

    case LL_ceq_r4:
        if (d_hasFloat) {
            popFloat(Ft1); popFloat(Ft0);
            em.feq_s(T0, Ft0, Ft1);
            pushReg(T0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$ceq_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 8, 4, true);
        }
        return 1;
    case LL_cgt_r4:
        if (d_hasFloat) {
            popFloat(Ft1); popFloat(Ft0);
            em.flt_s(T0, Ft1, Ft0); // a > b iff b < a
            pushReg(T0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$cgt_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 8, 4, true);
        }
        return 1;
    case LL_clt_r4:
        if (d_hasFloat) {
            popFloat(Ft1); popFloat(Ft0);
            em.flt_s(T0, Ft0, Ft1);
            pushReg(T0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$clt_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 8, 4, true);
        }
        return 1;

    case LL_ceq_r8: {
        quint32 sym = d_elf.addSymbol("__mic$ceq_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 4, true);
        return 1;
    }
    case LL_cgt_r8: {
        quint32 sym = d_elf.addSymbol("__mic$cgt_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 4, true);
        return 1;
    }
    case LL_clt_r8: {
        quint32 sym = d_elf.addSymbol("__mic$clt_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 16, 4, true);
        return 1;
    }

    case LL_ceq_pp:
        popRegPair(T4, T5); popRegPair(T0, T1);
        em.xor_(T0, T0, T4);
        em.xor_(T1, T1, T5);
        em.or_(T0, T0, T1);
        em.seqz(T0, T0);
        pushReg(T0);
        return 1;

    case LL_conv_i1_i4:
        popReg(T0);
        em.slli(T0, T0, 24);
        em.srai(T0, T0, 24);
        pushReg(T0);
        return 1;
    case LL_conv_i1_i8:
        popRegPair(T0, T1);
        em.slli(T0, T0, 24);
        em.srai(T0, T0, 24);
        pushReg(T0);
        return 1;
    case LL_conv_i1_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtw_s(T0, Ft0);
            em.slli(T0, T0, 24);
            em.srai(T0, T0, 24);
            pushReg(T0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_i4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
            popReg(T0);
            em.slli(T0, T0, 24);
            em.srai(T0, T0, 24);
            pushReg(T0);
        }
        return 1;
    case LL_conv_i1_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_i4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        popReg(T0);
        em.slli(T0, T0, 24);
        em.srai(T0, T0, 24);
        pushReg(T0);
        return 1;
    }
    case LL_conv_i2_i4:
        popReg(T0);
        em.slli(T0, T0, 16);
        em.srai(T0, T0, 16);
        pushReg(T0);
        return 1;
    case LL_conv_i2_i8:
        popRegPair(T0, T1);
        em.slli(T0, T0, 16);
        em.srai(T0, T0, 16);
        pushReg(T0);
        return 1;
    case LL_conv_i2_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtw_s(T0, Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_i4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
            popReg(T0);
        }
        em.slli(T0, T0, 16);
        em.srai(T0, T0, 16);
        pushReg(T0);
        return 1;
    case LL_conv_i2_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_i4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        popReg(T0);
        em.slli(T0, T0, 16);
        em.srai(T0, T0, 16);
        pushReg(T0);
        return 1;
    }
    case LL_conv_i4_i8:
        popRegPair(T0, T1);
        pushReg(T0);
        return 1;
    case LL_conv_i4_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtw_s(T0, Ft0);
            pushReg(T0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_i4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
        }
        return 1;
    case LL_conv_i4_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_i4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        return 1;
    }
    case LL_conv_i8_i4:
        popReg(T0);
        em.srai(T1, T0, 31);
        pushRegPair(T0, T1);
        return 1;
    case LL_conv_i8_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtw_s(T0, Ft0);
            em.srai(T1, T0, 31);
            pushRegPair(T0, T1);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_i4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
            popReg(T0);
            em.srai(T1, T0, 31);
            pushRegPair(T0, T1);
        }
        return 1;
    case LL_conv_i8_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_i4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        popReg(T0);
        em.srai(T1, T0, 31);
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_conv_u1_i4:
        popReg(T0);
        em.andi(T0, T0, 0xFF);
        pushReg(T0);
        return 1;
    case LL_conv_u1_i8:
        popRegPair(T0, T1);
        em.andi(T0, T0, 0xFF);
        pushReg(T0);
        return 1;
    case LL_conv_u1_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtw_s(T0, Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_i4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
            popReg(T0);
        }
        em.andi(T0, T0, 0xFF);
        pushReg(T0);
        return 1;
    case LL_conv_u1_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_i4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        popReg(T0);
        em.andi(T0, T0, 0xFF);
        pushReg(T0);
        return 1;
    }
    case LL_conv_u2_i4:
        popReg(T0);
        em.slli(T0, T0, 16);
        em.srli(T0, T0, 16);
        pushReg(T0);
        return 1;
    case LL_conv_u2_i8:
        popRegPair(T0, T1);
        em.slli(T0, T0, 16);
        em.srli(T0, T0, 16);
        pushReg(T0);
        return 1;
    case LL_conv_u2_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtw_s(T0, Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_i4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
            popReg(T0);
        }
        em.slli(T0, T0, 16);
        em.srli(T0, T0, 16);
        pushReg(T0);
        return 1;
    case LL_conv_u2_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_i4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        popReg(T0);
        em.slli(T0, T0, 16);
        em.srli(T0, T0, 16);
        pushReg(T0);
        return 1;
    }
    case LL_conv_u4_i8:
        popRegPair(T0, T1);
        pushReg(T0);
        return 1;
    case LL_conv_u4_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtwu_s(T0, Ft0);
            pushReg(T0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_u4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
        }
        return 1;
    case LL_conv_u4_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_u4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        return 1;
    }
    case LL_conv_u8_i4:
        popReg(T0);
        em.mv(T1, Zero);
        pushRegPair(T0, T1);
        return 1;
    case LL_conv_u8_r4:
        if (d_hasFloat) {
            popFloat(Ft0);
            em.fcvtwu_s(T0, Ft0);
            em.mv(T1, Zero);
            pushRegPair(T0, T1);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_u4_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
            popReg(T0);
            em.mv(T1, Zero);
            pushRegPair(T0, T1);
        }
        return 1;
    case LL_conv_u8_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_u4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        popReg(T0);
        em.mv(T1, Zero);
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_conv_r4_i4:
        if (d_hasFloat) {
            popReg(T0);
            em.fmvw_x(Ft0, T0);
            em.fcvts_w(Ft0, T0);
            pushFloat(Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_r4_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            emitExternCall(sym, 4, 4);
        }
        return 1;
    case LL_conv_r4_i8:
        if (d_hasFloat) {
            popRegPair(T0, T1);
            em.fcvts_w(Ft0, T0);
            pushFloat(Ft0);
        } else {
            quint32 sym = d_elf.addSymbol("__mic$conv_r4_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
            popRegPair(T0, T1);
            pushReg(T0);
            emitExternCall(sym, 4, 4);
        }
        return 1;
    case LL_conv_r4_r8: {
        quint32 sym = d_elf.addSymbol("__mic$conv_r4_r8", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 8, 4);
        return 1;
    }
    case LL_conv_r8_i4: {
        quint32 sym = d_elf.addSymbol("__mic$conv_r8_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 4, 8);
        return 1;
    }
    case LL_conv_r8_i8: {
        popRegPair(T0, T1);
        pushReg(T0);
        quint32 sym = d_elf.addSymbol("__mic$conv_r8_i4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 4, 8);
        return 1;
    }
    case LL_conv_r8_r4: {
        quint32 sym = d_elf.addSymbol("__mic$conv_r8_r4", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        emitExternCall(sym, 4, 8);
        return 1;
    }

    case LL_br: {
        int target = pc + 1 + (op.minus ? -1 : 1) * (int)val;
        Label& lbl = getLabel(target);
        em.j(lbl);
        return 1;
    }
    case LL_brfalse_i4: {
        int target = pc + 1 + (op.minus ? -1 : 1) * (int)val;
        popReg(T0);
        Label& lbl = getLabel(target);
        em.beq(T0, Zero, lbl);
        return 1;
    }

    case LL_call:
    case LL_callinst: {
        Procedure* callee = d_code.getProc(val);

        if (d_useRvAbi) {
            quint32 alignGap = emitArgAlignment(callee->decl);
            Q_UNUSED(alignGap);

            quint32 argsSize = callee->argsSize;
            quint32 returnSize = callee->returnSize;
            quint32 regBytes = argsSize < 32 ? argsSize : 32;
            quint32 overflowBytes = argsSize > 32 ? argsSize - 32 : 0;

            if (regBytes >= 4)
                em.lw(A0, Sp, 0);
            if (regBytes >= 8)
                em.lw(A1, Sp, 4);
            if (regBytes >= 12)
                em.lw(A2, Sp, 8);
            if (regBytes >= 16)
                em.lw(A3, Sp, 12);
            if (regBytes >= 20)
                em.lw(A4, Sp, 16);
            if (regBytes >= 24)
                em.lw(A5, Sp, 20);
            if (regBytes >= 28)
                em.lw(A6, Sp, 24);
            if (regBytes >= 32)
                em.lw(A7, Sp, 28);

            if (regBytes > 0) {
                if (regBytes <= 2047)
                    em.addi(Sp, Sp, (qint32)regBytes);
                else {
                    loadImm32(T3, regBytes);
                    em.add(Sp, Sp, T3);
                }
            }

            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 32 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (paddingNeeded <= 2047)
                        em.addi(Sp, Sp, -(qint32)paddingNeeded);
                    else {
                        loadImm32(T3, paddingNeeded);
                        em.sub(Sp, Sp, T3);
                    }
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.lw(T3, Sp, (qint32)(paddingNeeded + w));
                        em.sw(T3, Sp, (qint32)w);
                    }
                }
            }

            if (d_procSymbols.contains(val))
                emitCall(d_procSymbols[val]);
            else
                emitCall(getOrCreateExtSymbol(val));

            if (returnSize <= 8) {
                if (overflowBytes > 0) {
                    if (overflowBytes <= 2047)
                        em.addi(Sp, Sp, (qint32)overflowBytes);
                    else {
                        loadImm32(T3, overflowBytes);
                        em.add(Sp, Sp, T3);
                    }
                }
                if (returnSize >= 8)
                    pushRegPair(A0, A1);
                else if (returnSize >= 4)
                    pushReg(A0);
            } else {
                quint32 totalOnStack = 32 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.lw(T3, Sp, w);
                        em.sw(T3, Sp, (qint32)((qint32)w + (qint32)excess));
                    }
                    if (excess <= 2047)
                        em.addi(Sp, Sp, (qint32)excess);
                    else {
                        loadImm32(T3, excess);
                        em.add(Sp, Sp, T3);
                    }
                }
            }
        } else {
            // Old convention
            quint32 alignGap = emitArgAlignment(callee->decl);
            Q_UNUSED(alignGap);

            quint32 padding = 0;
            if (callee->returnSize > callee->argsSize)
                padding = callee->returnSize - callee->argsSize;
            if (padding > 0) {
                if (padding <= 2047)
                    em.addi(Sp, Sp, -(qint32)padding);
                else {
                    loadImm32(T3, padding);
                    em.sub(Sp, Sp, T3);
                }
                for (quint32 w = 0; w < callee->argsSize; w += 4) {
                    em.lw(T0, Sp, (qint32)(padding + w));
                    em.sw(T0, Sp, (qint32)w);
                }
            }

            if (d_procSymbols.contains(val))
                emitCall(d_procSymbols[val]);
            else
                emitCall(getOrCreateExtSymbol(val));

            emitCallStackAdj(callee->argsSize + padding, callee->returnSize);
        }
        return 1;
    }

    case LL_ret: {
        quint32 retSize = val;
        if (d_useRvAbi) {
            if (retSize <= 8) {
                if (retSize >= 8)
                    popRegPair(A0, A1);
                else if (retSize >= 4)
                    popReg(A0);
            } else {
                for (quint32 w = 0; w < retSize; w += 4) {
                    em.lw(T0, Sp, (qint32)w);
                    qint32 dstOff = (qint32)(8 + w);
                    if (dstOff >= -2048 && dstOff <= 2047)
                        em.sw(T0, Fp, dstOff);
                    else {
                        loadImm32(T3, (quint32)dstOff);
                        em.add(T3, Fp, T3);
                        em.sw(T0, T3, 0);
                    }
                }
            }
        } else {
            for (quint32 w = 0; w < retSize; w += 4) {
                em.lw(T0, Sp, (qint32)w);
                qint32 dstOff = (qint32)(FP_TO_ARGS + w);
                if (dstOff >= -2048 && dstOff <= 2047)
                    em.sw(T0, Fp, dstOff);
                else {
                    loadImm32(T3, (quint32)dstOff);
                    em.add(T3, Fp, T3);
                    em.sw(T0, T3, 0);
                }
            }
        }
        emitEpilogue();
        return 1;
    }
    case LL_ret_void:
        emitEpilogue();
        return 1;

    case LL_ldind_i1:
        popReg(T0); em.lb(T0, T0, 0); pushReg(T0);
        return 1;
    case LL_ldind_i2:
        popReg(T0); em.lh(T0, T0, 0); pushReg(T0);
        return 1;
    case LL_ldind_i4:
    case LL_ldind_u4:
    case LL_ldind_r4:
    case LL_ldind_p:
        popReg(T0); em.lw(T0, T0, 0); pushReg(T0);
        return 1;
    case LL_ldind_i8:
    case LL_ldind_u8:
    case LL_ldind_r8:
        popReg(T3);
        em.lw(T0, T3, 0);
        em.lw(T1, T3, 4);
        pushRegPair(T0, T1);
        return 1;
    case LL_ldind_u1:
        popReg(T0); em.lbu(T0, T0, 0); pushReg(T0);
        return 1;
    case LL_ldind_u2:
        popReg(T0); em.lhu(T0, T0, 0); pushReg(T0);
        return 1;
    case LL_ldind_vt: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        popReg(T3);
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 1;
    }
    case LL_ldind_str: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        popReg(T3);
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 1;
    }

    case LL_stind_i1:
        popReg(T0); popReg(T1);
        em.sb(T0, T1, 0);
        return 1;
    case LL_stind_i2:
        popReg(T0); popReg(T1);
        em.sh(T0, T1, 0);
        return 1;
    case LL_stind_i4:
    case LL_stind_r4:
    case LL_stind_p:
        popReg(T0); popReg(T1);
        em.sw(T0, T1, 0);
        return 1;
    case LL_stind_i8:
    case LL_stind_r8:
        popRegPair(T0, T1);
        popReg(T4);
        em.sw(T0, T4, 0);
        em.sw(T1, T4, 4);
        return 1;
    case LL_stind_vt: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        em.lw(T3, Sp, (qint32)aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, Sp, (qint32)i);
            em.sw(T0, T3, (qint32)i);
        }
        quint32 total = aligned + Slot4;
        if (total <= 2047)
            em.addi(Sp, Sp, (qint32)total);
        else {
            loadImm32(T4, total);
            em.add(Sp, Sp, T4);
        }
        return 1;
    }

    case LL_ldfld_i1: {
        popReg(T0);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047) {
            em.lb(T0, T0, off);
        } else {
            loadImm32(T3, val);
            em.add(T0, T0, T3);
            em.lb(T0, T0, 0);
        }
        pushReg(T0);
        return 1;
    }
    case LL_ldfld_i2: {
        popReg(T0);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047) {
            em.lh(T0, T0, off);
        } else {
            loadImm32(T3, val);
            em.add(T0, T0, T3);
            em.lh(T0, T0, 0);
        }
        pushReg(T0);
        return 1;
    }
    case LL_ldfld_i4:
    case LL_ldfld_u4:
    case LL_ldfld_r4:
    case LL_ldfld_p: {
        popReg(T3);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047) {
            em.lw(T0, T3, off);
        } else {
            loadImm32(T4, val);
            em.add(T3, T3, T4);
            em.lw(T0, T3, 0);
        }
        pushReg(T0);
        return 1;
    }
    case LL_ldfld_i8:
    case LL_ldfld_u8:
    case LL_ldfld_r8: {
        popReg(T3);
        qint32 off = (qint32)val;
        if (off >= -2048 && off + 4 <= 2047) {
            em.lw(T0, T3, off);
            em.lw(T1, T3, off + 4);
        } else {
            loadImm32(T4, val);
            em.add(T3, T3, T4);
            em.lw(T0, T3, 0);
            em.lw(T1, T3, 4);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldfld_u1: {
        popReg(T0);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047) {
            em.lbu(T0, T0, off);
        } else {
            loadImm32(T3, val);
            em.add(T0, T0, T3);
            em.lbu(T0, T0, 0);
        }
        pushReg(T0);
        return 1;
    }
    case LL_ldfld_u2: {
        popReg(T0);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047) {
            em.lhu(T0, T0, off);
        } else {
            loadImm32(T3, val);
            em.add(T0, T0, T3);
            em.lhu(T0, T0, 0);
        }
        pushReg(T0);
        return 1;
    }
    case LL_ldfld_pp: {
        popReg(T3);
        qint32 off = (qint32)val;
        if (off >= -2048 && off + 4 <= 2047) {
            em.lw(T0, T3, off);
            em.lw(T1, T3, off + 4);
        } else {
            loadImm32(T4, val);
            em.add(T3, T3, T4);
            em.lw(T0, T3, 0);
            em.lw(T1, T3, 4);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldflda:
        popReg(T0);
        if (val != 0) {
            if ((qint32)val >= -2048 && (qint32)val <= 2047)
                em.addi(T0, T0, (qint32)val);
            else {
                loadImm32(T3, val);
                em.add(T0, T0, T3);
            }
        }
        pushReg(T0);
        return 1;
    case LL_ldfld_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        popReg(T3);
        if (val != 0) {
            if ((qint32)val >= -2048 && (qint32)val <= 2047)
                em.addi(T3, T3, (qint32)val);
            else {
                loadImm32(T4, val);
                em.add(T3, T3, T4);
            }
        }
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 2;
    }

    case LL_stfld_i1: {
        popReg(T0); popReg(T1);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047)
            em.sb(T0, T1, off);
        else {
            loadImm32(T3, val);
            em.add(T1, T1, T3);
            em.sb(T0, T1, 0);
        }
        return 1;
    }
    case LL_stfld_i2: {
        popReg(T0); popReg(T1);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047)
            em.sh(T0, T1, off);
        else {
            loadImm32(T3, val);
            em.add(T1, T1, T3);
            em.sh(T0, T1, 0);
        }
        return 1;
    }
    case LL_stfld_i4:
    case LL_stfld_r4:
    case LL_stfld_p: {
        popReg(T0); popReg(T1);
        qint32 off = (qint32)val;
        if (off >= -2048 && off <= 2047)
            em.sw(T0, T1, off);
        else {
            loadImm32(T3, val);
            em.add(T1, T1, T3);
            em.sw(T0, T1, 0);
        }
        return 1;
    }
    case LL_stfld_i8:
    case LL_stfld_r8: {
        popRegPair(T0, T1); popReg(T4);
        qint32 off = (qint32)val;
        if (off >= -2048 && off + 4 <= 2047) {
            em.sw(T0, T4, off);
            em.sw(T1, T4, off + 4);
        } else {
            loadImm32(T3, val);
            em.add(T4, T4, T3);
            em.sw(T0, T4, 0);
            em.sw(T1, T4, 4);
        }
        return 1;
    }
    case LL_stfld_pp: {
        popRegPair(T0, T1); popReg(T4);
        qint32 off = (qint32)val;
        if (off >= -2048 && off + 4 <= 2047) {
            em.sw(T0, T4, off);
            em.sw(T1, T4, off + 4);
        } else {
            loadImm32(T3, val);
            em.add(T4, T4, T3);
            em.sw(T0, T4, 0);
            em.sw(T1, T4, 4);
        }
        return 1;
    }
    case LL_stfld_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        em.lw(T3, Sp, (qint32)aligned);
        if (val != 0) {
            if ((qint32)val >= -2048 && (qint32)val <= 2047)
                em.addi(T3, T3, (qint32)val);
            else {
                loadImm32(T4, val);
                em.add(T3, T3, T4);
            }
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, Sp, (qint32)i);
            em.sw(T0, T3, (qint32)i);
        }
        quint32 total = aligned + Slot4;
        if (total <= 2047)
            em.addi(Sp, Sp, (qint32)total);
        else {
            loadImm32(T4, total);
            em.add(Sp, Sp, T4);
        }
        return 2;
    }

    case LL_ldelem_i1: {
        popReg(T1); popReg(T0);
        em.add(T0, T0, T1);
        em.lb(T0, T0, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldelem_i2: {
        popReg(T1); popReg(T0);
        em.slli(T1, T1, 1);
        em.add(T0, T0, T1);
        em.lh(T0, T0, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldelem_i4:
    case LL_ldelem_u4:
    case LL_ldelem_r4:
    case LL_ldelem_p: {
        popReg(T1); popReg(T0);
        em.slli(T1, T1, 2);
        em.add(T0, T0, T1);
        em.lw(T0, T0, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldelem_i8:
    case LL_ldelem_u8:
    case LL_ldelem_r8: {
        popReg(T1); popReg(T3);
        em.slli(T1, T1, 3);
        em.add(T3, T3, T1);
        em.lw(T0, T3, 0);
        em.lw(T1, T3, 4);
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldelem_u1: {
        popReg(T1); popReg(T0);
        em.add(T0, T0, T1);
        em.lbu(T0, T0, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldelem_u2: {
        popReg(T1); popReg(T0);
        em.slli(T1, T1, 1);
        em.add(T0, T0, T1);
        em.lhu(T0, T0, 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldelema: {
        popReg(T1); popReg(T0);
        if (val == 1)
            em.add(T0, T0, T1);
        else if (val == 2) {
            em.slli(T1, T1, 1);
            em.add(T0, T0, T1);
        } else if (val == 4) {
            em.slli(T1, T1, 2);
            em.add(T0, T0, T1);
        } else if (val == 8) {
            em.slli(T1, T1, 3);
            em.add(T0, T0, T1);
        } else {
            loadImm32(T4, val);
            em.mul(T1, T1, T4);
            em.add(T0, T0, T1);
        }
        pushReg(T0);
        return 1;
    }
    case LL_ldelem_vt: {
        quint32 elemSize = val;
        quint32 aligned = (elemSize + 3) & ~3;
        popReg(T1); popReg(T3);
        if (elemSize == 4) {
            em.slli(T1, T1, 2);
            em.add(T3, T3, T1);
        } else {
            loadImm32(T4, elemSize);
            em.mul(T1, T1, T4);
            em.add(T3, T3, T1);
        }
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T4, aligned);
            em.sub(Sp, Sp, T4);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, T3, (qint32)i);
            em.sw(T0, Sp, (qint32)i);
        }
        return 1;
    }

    case LL_stelem_i1: {
        popReg(T0); popReg(T1); popReg(T4);
        em.add(T4, T4, T1);
        em.sb(T0, T4, 0);
        return 1;
    }
    case LL_stelem_i2: {
        popReg(T0); popReg(T1); popReg(T4);
        em.slli(T1, T1, 1);
        em.add(T4, T4, T1);
        em.sh(T0, T4, 0);
        return 1;
    }
    case LL_stelem_i4:
    case LL_stelem_r4:
    case LL_stelem_p: {
        popReg(T0); popReg(T1); popReg(T4);
        em.slli(T1, T1, 2);
        em.add(T4, T4, T1);
        em.sw(T0, T4, 0);
        return 1;
    }
    case LL_stelem_i8:
    case LL_stelem_r8: {
        popRegPair(T0, T1);
        popReg(T4);
        popReg(T5);
        em.slli(T4, T4, 3);
        em.add(T5, T5, T4);
        em.sw(T0, T5, 0);
        em.sw(T1, T5, 4);
        return 1;
    }
    case LL_stelem_vt: {
        quint32 elemSize = val;
        quint32 aligned = (elemSize + 3) & ~3;
        em.lw(T1, Sp, (qint32)aligned);
        em.lw(T4, Sp, (qint32)(aligned + Slot4));
        if (elemSize == 4) {
            em.slli(T1, T1, 2);
            em.add(T3, T4, T1);
        } else {
            loadImm32(T5, elemSize);
            em.mul(T1, T1, T5);
            em.add(T3, T4, T1);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, Sp, (qint32)i);
            em.sw(T0, T3, (qint32)i);
        }
        quint32 total = aligned + Slot4 + Slot4;
        if (total <= 2047)
            em.addi(Sp, Sp, (qint32)total);
        else {
            loadImm32(T4, total);
            em.add(Sp, Sp, T4);
        }
        return 1;
    }

    case LL_pop: {
        quint32 aligned = (val + 3) & ~3;
        if (aligned <= 2047)
            em.addi(Sp, Sp, (qint32)aligned);
        else {
            loadImm32(T3, aligned);
            em.add(Sp, Sp, T3);
        }
        return 1;
    }
    case LL_dup: {
        quint32 aligned = (val + 3) & ~3;
        if (aligned <= 2047)
            em.addi(Sp, Sp, -(qint32)aligned);
        else {
            loadImm32(T3, aligned);
            em.sub(Sp, Sp, T3);
        }
        for (quint32 i = 0; i < aligned; i += 4) {
            em.lw(T0, Sp, (qint32)(aligned + i));
            em.sw(T0, Sp, (qint32)i);
        }
        return 1;
    }

    case LL_alloc1: {
        quint32 allocSize;
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            allocSize = tmpl.mem.size();
        } else {
            allocSize = val;
        }
        quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        if (d_useRvAbi) {
            // RV-ABI: arg in a0, return in a0
            loadImm32(A0, allocSize);
            emitCall(allocSym);
            em.mv(T0, A0); // T0 = allocated ptr
            pushReg(T0); // push result on eval stack
        } else {
            // Old convention: push size on stack, call MIC$$alloc
            loadImm32(T0, allocSize);
            pushReg(T0);
            emitCall(allocSym);
            emitCallStackAdj(4, 4); // noop: arg=4, ret=4
        }

        // T0 = allocated pointer (peek from stack top)
        em.lw(T0, Sp, 0);

        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff = getOrEmitTemplate(val);
                loadAddr(T1, d_rodataSymIdx, tmplOff);
                quint32 aligned = (tmplSize + 3) & ~3;
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.lw(T4, T1, (qint32)i);
                    em.sw(T4, T0, (qint32)i);
                }
            }
            emitVtableFixups(T0, tmpl);
        }
        return 1;
    }
    case LL_allocN: {
        quint32 elemSize;
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            elemSize = tmpl.mem.size();
        } else {
            elemSize = val;
        }
        popReg(T0); // T0 = count
        loadImm32(T1, elemSize);
        em.mul(T0, T0, T1); // T0 = totalSize

        quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        if (d_useRvAbi) {
            // RV-ABI: arg in a0, return in a0
            pushReg(T0);    // save totalSize on stack
            em.mv(A0, T0);   // a0 = totalSize (arg for MIC$$alloc)
            emitCall(allocSym);
            // a0 = allocated pointer
            em.mv(T0, A0);  // T0 = allocated ptr
            popReg(T5);  // T5 = totalSize (restore saved copy)
        } else {
            // Old convention: push size on stack, call MIC$$alloc
            pushReg(T0); // save totalSize
            pushReg(T0); // arg for alloc
            emitCall(allocSym);
            emitCallStackAdj(4, 4);
            // Stack: [ptr] [totalSize]
            em.lw(T0, Sp, 0);  // T0 = allocated ptr
            em.lw(T5, Sp, 4);  // T5 = totalSize
            em.addi(Sp, Sp, 4); // pop totalSize; stack: [ptr]
        }

        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff = getOrEmitTemplate(val);
                // Save base ptr
                pushReg(T0);
                // T3 = current dest, T5 = end
                em.mv(T3, T0);
                em.add(T5, T0, T5); // T5 = end
                loadAddr(T1, d_rodataSymIdx, tmplOff);
                quint32 aligned = (tmplSize + 3) & ~3;
                Rv32::Label loopLbl, doneLbl;
                em.bind(loopLbl);
                em.bgeu(T3, T5, doneLbl);
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.lw(T4, T1, (qint32)i);
                    em.sw(T4, T3, (qint32)i);
                }
                // Save regs before vtable fixup
                pushReg(T5); pushReg(T3); pushReg(T1);
                emitVtableFixups(T3, tmpl);
                popReg(T1); popReg(T3); popReg(T5);
                loadImm32(T4, tmplSize);
                em.add(T3, T3, T4);
                em.j(loopLbl);
                em.bind(doneLbl);
                popReg(T0); // restore base ptr
            }
        }
        pushReg(T0);
        return 1;
    }
    case LL_free: {
        quint32 freeSym = d_elf.addSymbol("MIC$$free", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        if (d_useRvAbi) {
            // RV-ABI: arg in a0
            popReg(A0);
            emitCall(freeSym);
        } else {
            // Old convention: ptr already on stack
            emitCall(freeSym);
            em.addi(Sp, Sp, 4); // pop arg
        }
        return 1;
    }
    case LL_initobj: {
        popReg(T0);
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff = getOrEmitTemplate(val);
                loadAddr(T1, d_rodataSymIdx, tmplOff);
                quint32 aligned = (tmplSize + 3) & ~3;
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.lw(T4, T1, (qint32)i);
                    em.sw(T4, T0, (qint32)i);
                }
            }
            emitVtableFixups(T0, tmpl);
        }
        return 1;
    }

    case LL_strcpy: {
        popReg(T1); popReg(T0); // T1=src, T0=dst
        loadImm32(T4, val); // T4 = max length
        Rv32::Label loopLabel, doneLabel;
        em.bind(loopLabel);
        em.beq(T4, Zero, doneLabel);
        em.lbu(T5, T1, 0);
        em.sb(T5, T0, 0);
        em.beq(T5, Zero, doneLabel);
        em.addi(T1, T1, 1);
        em.addi(T0, T0, 1);
        em.addi(T4, T4, -1);
        em.j(loopLabel);
        em.bind(doneLabel);
        return 1;
    }

    case LL_calli: {
        quint32 argsSize = val & 0x7FF;
        quint32 returnSize = (val >> 11) & 0x7FF;
        popReg(T3); // func ptr

        if (d_useRvAbi) {
            quint32 regBytes = argsSize < 32 ? argsSize : 32;
            quint32 overflowBytes = argsSize > 32 ? argsSize - 32 : 0;
            if (regBytes >= 4)  em.lw(A0, Sp, 0);
            if (regBytes >= 8)  em.lw(A1, Sp, 4);
            if (regBytes >= 12) em.lw(A2, Sp, 8);
            if (regBytes >= 16) em.lw(A3, Sp, 12);
            if (regBytes >= 20) em.lw(A4, Sp, 16);
            if (regBytes >= 24) em.lw(A5, Sp, 20);
            if (regBytes >= 28) em.lw(A6, Sp, 24);
            if (regBytes >= 32) em.lw(A7, Sp, 28);
            if (regBytes > 0) {
                if (regBytes <= 2047)
                    em.addi(Sp, Sp, (qint32)regBytes);
                else {
                    loadImm32(T4, regBytes);
                    em.add(Sp, Sp, T4);
                }
            }

            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 32 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (paddingNeeded <= 2047)
                        em.addi(Sp, Sp, -(qint32)paddingNeeded);
                    else {
                        loadImm32(T4, paddingNeeded);
                        em.sub(Sp, Sp, T4);
                    }
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.lw(T4, Sp, (qint32)(paddingNeeded + w));
                        em.sw(T4, Sp, (qint32)w);
                    }
                }
            }

            emitIndirectCall(T3);

            if (returnSize <= 8) {
                if (overflowBytes > 0) {
                    if (overflowBytes <= 2047)
                        em.addi(Sp, Sp, (qint32)overflowBytes);
                    else {
                        loadImm32(T3, overflowBytes);
                        em.add(Sp, Sp, T3);
                    }
                }
                if (returnSize >= 8)
                    pushRegPair(A0, A1);
                else if (returnSize >= 4)
                    pushReg(A0);
            } else {
                quint32 totalOnStack = 32 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.lw(T4, Sp, w);
                        em.sw(T4, Sp, (qint32)((qint32)w + (qint32)excess));
                    }
                    if (excess <= 2047)
                        em.addi(Sp, Sp, (qint32)excess);
                    else {
                        loadImm32(T4, excess);
                        em.add(Sp, Sp, T4);
                    }
                }
            }
        } else {
            emitIndirectCall(T3);
            emitCallStackAdj(argsSize, returnSize);
        }
        return 1;
    }
    case LL_callmi: {
        quint32 argsSize = val & 0x7FF;
        quint32 returnSize = (val >> 11) & 0x7FF;
        popReg(T3); // method_ptr
        popReg(T0); // obj_ptr

        if (d_useRvAbi) {
            em.mv(A0, T0); // self in A0
            quint32 remainArgs = argsSize >= 4 ? argsSize - 4 : 0;
            quint32 regBytes = remainArgs < 28 ? remainArgs : 28;
            quint32 overflowBytes = remainArgs > 28 ? remainArgs - 28 : 0;
            if (regBytes >= 4)
                em.lw(A1, Sp, 0);
            if (regBytes >= 8)
                em.lw(A2, Sp, 4);
            if (regBytes >= 12)
                em.lw(A3, Sp, 8);
            if (regBytes >= 16)
                em.lw(A4, Sp, 12);
            if (regBytes >= 20)
                em.lw(A5, Sp, 16);
            if (regBytes >= 24)
                em.lw(A6, Sp, 20);
            if (regBytes >= 28)
                em.lw(A7, Sp, 24);
            if (regBytes > 0) {
                if (regBytes <= 2047)
                    em.addi(Sp, Sp, (qint32)regBytes);
                else {
                    loadImm32(T4, regBytes);
                    em.add(Sp, Sp, T4);
                }
            }

            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 32 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (paddingNeeded <= 2047)
                        em.addi(Sp, Sp, -(qint32)paddingNeeded);
                    else {
                        loadImm32(T4, paddingNeeded);
                        em.sub(Sp, Sp, T4);
                    }
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.lw(T4, Sp, (qint32)(paddingNeeded + w));
                        em.sw(T4, Sp, (qint32)w);
                    }
                }
            }

            emitIndirectCall(T3);

            if (returnSize <= 8) {
                if (overflowBytes > 0) {
                    if (overflowBytes <= 2047)
                        em.addi(Sp, Sp, (qint32)overflowBytes);
                    else {
                        loadImm32(T4, overflowBytes);
                        em.add(Sp, Sp, T4);
                    }
                }
                if (returnSize >= 8)
                    pushRegPair(A0, A1);
                else if (returnSize >= 4)
                    pushReg(A0);
            } else {
                quint32 totalOnStack = 32 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.lw(T4, Sp, w);
                        em.sw(T4, Sp, (qint32)((qint32)w + (qint32)excess));
                    }
                    if (excess <= 2047)
                        em.addi(Sp, Sp, (qint32)excess);
                    else {
                        loadImm32(T4, excess);
                        em.add(Sp, Sp, T4);
                    }
                }
            }
        } else {
            pushReg(T0); // push obj_ptr back as first arg
            emitIndirectCall(T3);
            emitCallStackAdj(argsSize, returnSize);
        }
        return 1;
    }

    case LL_callvirt: {
        Procedure* callee = d_code.getProc(val);
        quint32 alignGap = emitArgAlignment(callee->decl);
        Q_UNUSED(alignGap);

        quint32 argsSize = callee->argsSize;
        quint32 returnSize = callee->returnSize;

        // Load self pointer from SP (first arg)
        em.lw(T0, Sp, 0); // T0 = self
        em.lw(T1, T0, 0); // T1 = vtable pointer

        quint32 slot = 0;
        if (callee->decl && callee->decl->getPd())
            slot = callee->decl->getPd()->slot;
        qint32 slotOff = (qint32)(slot * 4);
        if (slotOff >= -2048 && slotOff <= 2047)
            em.lw(T3, T1, slotOff);
        else {
            loadImm32(T4, slot * 4);
            em.add(T1, T1, T4);
            em.lw(T3, T1, 0);
        }

        if (d_useRvAbi) {
            quint32 regBytes = argsSize < 32 ? argsSize : 32;
            quint32 overflowBytes = argsSize > 32 ? argsSize - 32 : 0;
            if (regBytes >= 4)
                em.lw(A0, Sp, 0);
            if (regBytes >= 8)
                em.lw(A1, Sp, 4);
            if (regBytes >= 12)
                em.lw(A2, Sp, 8);
            if (regBytes >= 16)
                em.lw(A3, Sp, 12);
            if (regBytes >= 20)
                em.lw(A4, Sp, 16);
            if (regBytes >= 24)
                em.lw(A5, Sp, 20);
            if (regBytes >= 28)
                em.lw(A6, Sp, 24);
            if (regBytes >= 32)
                em.lw(A7, Sp, 28);
            if (regBytes > 0) {
                if (regBytes <= 2047)
                    em.addi(Sp, Sp, (qint32)regBytes);
                else {
                    loadImm32(T4, regBytes);
                    em.add(Sp, Sp, T4);
                }
            }

            quint32 paddingNeeded = 0;
            if (returnSize > 8) {
                qint32 extra = (qint32)returnSize - 32 - (qint32)overflowBytes;
                if (extra > 0) {
                    paddingNeeded = ((quint32)extra + 3) & ~3;
                    if (paddingNeeded <= 2047)
                        em.addi(Sp, Sp, -(qint32)paddingNeeded);
                    else {
                        loadImm32(T4, paddingNeeded);
                        em.sub(Sp, Sp, T4);
                    }
                    for (quint32 w = 0; w < overflowBytes; w += 4) {
                        em.lw(T4, Sp, (qint32)(paddingNeeded + w));
                        em.sw(T4, Sp, (qint32)w);
                    }
                }
            }

            emitIndirectCall(T3);

            if (returnSize <= 8) {
                if (overflowBytes > 0) {
                    if (overflowBytes <= 2047)
                        em.addi(Sp, Sp, (qint32)overflowBytes);
                    else {
                        loadImm32(T4, overflowBytes);
                        em.add(Sp, Sp, T4);
                    }
                }
                if (returnSize >= 8)
                    pushRegPair(A0, A1);
                else if (returnSize >= 4)
                    pushReg(A0);
            } else {
                quint32 totalOnStack = 32 + overflowBytes + paddingNeeded;
                if (totalOnStack > returnSize) {
                    quint32 excess = totalOnStack - returnSize;
                    for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                        em.lw(T4, Sp, w);
                        em.sw(T4, Sp, (qint32)((qint32)w + (qint32)excess));
                    }
                    if (excess <= 2047)
                        em.addi(Sp, Sp, (qint32)excess);
                    else {
                        loadImm32(T4, excess);
                        em.add(Sp, Sp, T4);
                    }
                }
            }
        } else {
            emitIndirectCall(T3);
            emitCallStackAdj(argsSize, returnSize);
        }
        return 1;
    }

    case LL_ldproc: {
        loadAddr(T0, d_procSymbols.contains(val) ? d_procSymbols[val] : getOrCreateExtSymbol(val), 0);
        pushReg(T0);
        return 1;
    }
    case LL_ldmeth_struct: {
        popReg(T1); // T1 = obj pointer
        loadAddr(T0, d_procSymbols.contains(val) ? d_procSymbols[val] : getOrCreateExtSymbol(val), 0);
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldmeth: {
        popReg(T0); // T0 = object pointer
        em.lw(T1, T0, 0); // T1 = vtable pointer
        qint32 slotOff = (qint32)(val * 4);
        if (slotOff >= -2048 && slotOff <= 2047)
            em.lw(T0, T1, slotOff);
        else {
            loadImm32(T3, val * 4);
            em.add(T1, T1, T3);
            em.lw(T0, T1, 0);
        }
        pushReg(T0);
        return 1;
    }
    case LL_ldmeth_iface: {
        popRegPair(T4, T1); // T4 = vtable_addr, T1 = obj
        qint32 slotOff = (qint32)(val * 4);
        if (slotOff >= -2048 && slotOff <= 2047)
            em.lw(T0, T4, slotOff);
        else {
            loadImm32(T3, val * 4);
            em.add(T4, T4, T3);
            em.lw(T0, T4, 0);
        }
        pushRegPair(T0, T1);
        return 1;
    }
    case LL_ldiface: {
        popReg(T1); // T1 = obj pointer
        quint32 vtableOff = getOrEmitVtable(val);
        loadAddr(T0, d_dataSymIdx, vtableOff);
        pushRegPair(T0, T1);
        return 1;
    }

    case LL_isinst: {
        qWarning() << "Rv32Renderer: isinst type check not fully implemented (always returns 1 if non-null)";
        popReg(T0);
        em.snez(T0, T0);
        pushReg(T0);
        return 1;
    }

    case LL_vt_size:
        Q_ASSERT(false);
        return 1;

    case LL_newvla:
        qWarning() << "Rv32Renderer: not yet implemented:" << Code::op_names[opcode];
        em.nop();
        return 1;

    case LL_line:
        return 1;

    case LL_invalid:
        return 1;

    default:
        return setError(QString("Unknown/unimplemented LL_op: %1").arg(opcode)) ? -1 : -1;
    }

    return 1;
}

bool Renderer::renderProcedure(Procedure& proc, int procIdx)
{
    if (d_useRvAbi)
        d_localsSize = (proc.localsSize + 7) & ~7;
    else
        d_localsSize = (proc.localsSize + 3) & ~3;
    d_argsSize = proc.argsSize;
    d_returnSize = proc.returnSize;

    scanBranchTargets(proc);

    emitPrologue(d_localsSize);

    int pc = 0;
    while (pc < (int)proc.ops.size()) {
        bindLabelIfNeeded(pc);

        int consumed = emitOp(proc, pc);
        if (consumed < 0)
            return false;
        pc += consumed;
    }

    bindLabelIfNeeded(pc);

    emitEpilogue();

    clearLabels();

    return true;
}

bool Renderer::generateMainObject(const QByteArrayList& moduleNames, const QString& outputPath, bool indirect)
{
    using namespace Rv32;

    ElfWriter elf(ElfWriter::ArchRISCV);
    // Default to double-float ABI for sysroot compatibility.
    // The entry point in main+.o contains no float instructions.
    elf.setElfFlags(0x4); // EF_RISCV_FLOAT_ABI_DOUBLE
    Rv32::Emitter em;

    ElfWriter::StandardSections sec = elf.createStandardSections();
    quint32 textSymIdx = elf.addSectionSymbol(sec.text);
    Q_UNUSED(textSymIdx);

    QList<quint32> initSymbols;
    for (int i = 0; i < moduleNames.size(); i++) {
        QByteArray symName = moduleNames[i] + "$begin$";
        quint32 symIdx = elf.addSymbol(symName, 0, 0, 0, STB_GLOBAL, STT_FUNC);
        initSymbols.append(symIdx);
    }

    Rv32::FixupTracker fix;

    if (indirect) {
        quint32 micInitSym = elf.addSymbol("__mic$init", 0, 0, 0, STB_GLOBAL, STT_FUNC);

        // _start:
        em.li(Fp, 0);  // clear frame pointer
        em.lw(A0, Sp, 0); // A0 = argc
        em.addi(Sp, Sp, 4); // pop argc
        em.mv(A1, Sp);  // A1 = argv

        // call __mic$init
        quint32 callOff = em.currentPosition();
        em.auipc(Ra, 0);
        em.jalr(Ra, Ra, 0);
        fix.recordCall(callOff, micInitSym);

        em.ebreak(); // should never return

        quint32 micMainOff = em.currentPosition();

        // __mic$main:
        em.addi(Sp, Sp, -8);
        em.sw(Ra, Sp, 4);
        em.sw(Fp, Sp, 0);
        em.mv(Fp, Sp);

        for (int i = 0; i < initSymbols.size(); i++) {
            quint32 off = em.currentPosition();
            em.auipc(Ra, 0);
            em.jalr(Ra, Ra, 0);
            fix.recordCall(off, initSymbols[i]);
        }

        em.li(A0, 0); // return 0
        em.mv(Sp, Fp);
        em.lw(Fp, Sp, 0);
        em.lw(Ra, Sp, 4);
        em.addi(Sp, Sp, 8);
        em.jalr(Zero, Ra, 0); // ret

        elf.appendToSection(sec.text, em.machineCode());
        fix.generateRelocations(elf, sec.relText, sec.relData);

        elf.addSymbol("_start", sec.text, 0, micMainOff, STB_GLOBAL, STT_FUNC);
        elf.addSymbol("__mic$main", sec.text, micMainOff,
                       em.currentPosition() - micMainOff, STB_GLOBAL, STT_FUNC);
    } else {
        // _start:
        em.li(Fp, 0);

        for (int i = 0; i < initSymbols.size(); i++) {
            quint32 off = em.currentPosition();
            em.auipc(Ra, 0);
            em.jalr(Ra, Ra, 0);
            fix.recordCall(off, initSymbols[i]);
        }

#if 0
        // exit makes no sense for the ESP32-P4 target
        em.li(A0, 0);
        quint32 exitSym = elf.addSymbol("MIC$$exit", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        quint32 exitOff = em.currentPosition();
        em.auipc(Ra, 0);
        em.jalr(Ra, Ra, 0);
        fix.recordCall(exitOff, exitSym);
#else
        // instead in case we really arrive here, just loop forever
        Rv32::Label haltLabel;
        em.bind(haltLabel);
        em.j(haltLabel);
        em.ebreak();  // should never come here
#endif

        elf.appendToSection(sec.text, em.machineCode());
        fix.generateRelocations(elf, sec.relText, sec.relData);

        elf.addSymbol("_start", sec.text, 0, em.currentPosition(), STB_GLOBAL, STT_FUNC);
    }

    return elf.write(outputPath);
}
