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

#include "MilX86Renderer.h"
#include <Micron/MicAtom.h>
#include <QtDebug>
#include <QFile>
#include <QFileInfo>
using namespace Mil;
using namespace Vm;
using namespace X86;

static const int Slot4 = 4;
static const int Slot8 = 8;

// Convert Micron symbol names to C compatible names
static inline QByteArray elfSymName(const QByteArray& micronName)
{
    QByteArray result = micronName;
    result.replace('!', '$');
    return result;
}

// EBP+0 = saved EBP, EBP+4 = return address, EBP+8 = first argument
static const int FP_TO_ARGS = 8;

// i386 relocation types
static const quint8 R_386_32   = 1;  // S + A (absolute 32-bit)
static const quint8 R_386_PC32 = 2;  // S + A - P (PC-relative 32-bit)

Renderer::Renderer(AstModel* mdl)
    : d_mdl(mdl), d_code(mdl, 4, 4), d_emitDwarf(false), d_cdeclReturns(false),
      d_dwarf(0), d_localsSize(0), d_argsSize(0),
      d_textSymIdx(0), d_dataSymIdx(0), d_rodataSymIdx(0),
      d_elf(ElfWriter::ArchX86)
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
            d_elf.addRelocation(d_sections.relData, vtableOff + i * 4, symIdx, R_386_32);
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

    if (t->kind == Type::Object && offset + 4 <= memSize) {
        // Extract vtable pointer from template mem at this offset.
        // NOTE: The VM's initMemory writes sizeof(Vtable*) bytes (8 on x86_64 host)
        // into template memory, but the template is sized for the target's 4-byte pointers.
        // We must only read 4 bytes here and compare against the low 4 bytes of each
        // host vtable pointer to avoid cross-element corruption in arrays.
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
                d_emitter.mov_ri(ECX, vtOff);
                quint32 immOff = d_emitter.currentPosition() - 4;
                d_elf.addRelocation(d_sections.relText, immOff, d_dataSymIdx, R_386_32);
                d_emitter.mov_mr(destReg, offset, ECX);
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

    if (t->kind == Type::Object) {
        int vtIdx = d_code.findVtableIdx(t);
        if (vtIdx >= 0) {
            quint32 vtOff = getOrEmitVtable(vtIdx);
            d_elf.addRelocation(d_sections.relData, baseOff, d_dataSymIdx, R_386_32);
            d_elf.patchWord(d_sections.data, baseOff, vtOff);
        }
        // Also recurse into fields (struct/object fields may contain nested objects)
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
            quint32 elemSize = et->getByteSize(4); // 4 = x86 pointer width
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

Renderer::~Renderer()
{
    clearLabels();
    delete d_dwarf;
}

void Renderer::setEmitDwarf(bool v)
{
    d_emitDwarf = v;
    d_code.setGenerateLines(d_emitDwarf);
}

bool Renderer::renderModule(Declaration* module)
{
    Q_ASSERT(module && module->kind == Declaration::Module);

    if (!d_code.compile(module))
        return setError(QString("VmCode compilation failed for module %1").arg(QString(module->name)));

    // Compile all procedures, not just the ones reachable from begin sections
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

    // Reserve space in .data for module global variables
    // Variables use offsets from .data base via loadVarAddr, so vtables
    // must be placed after this reserved space.
    quint32 varMemSize = d_mdl->getVarMemSize();
    if (varMemSize > 0) {
        QByteArray zeros(varMemSize, '\0');
        d_elf.appendToSection(d_sections.data, zeros.constData(), varMemSize);
    }

    // Reserve a 4-byte guard word in .data for the module init (once-only) check.
    // Initialized to 0; set to 1 on first call to $begin$ to prevent re-entry.
    quint32 initGuardOff = d_elf.sectionSize(d_sections.data);
    {
        quint32 zero = 0;
        d_elf.appendToSection(d_sections.data, (const char*)&zero, 4);
    }

    if (d_emitDwarf) {
        delete d_dwarf;
        d_dwarf = new DwarfEmitter(d_elf, ElfWriter::ArchX86);
        QByteArray srcFile;
        QByteArray srcDir;
        if (module->md && !module->md->source.isEmpty()) {
            QFileInfo info(module->md->source);
            srcDir = info.absolutePath().toUtf8();
            srcFile = info.fileName().toUtf8();
        }else {
            srcFile = module->name + ".mic";
            qWarning() << "X86::Renderer::renderModule: incomplete source file information for " << module->name;
        }
        d_dwarf->beginCompilationUnit(srcFile, srcDir);
    }

    // Collect all procedures
    struct ProcInfo {
        Declaration* decl;
        int codeIdx;
    };
    QList<ProcInfo> allProcs;
    QSet<int> seenCodeIdx;
    {
        int idx = d_code.findProc(module);
        if (idx >= 0) {
            ProcInfo pi;
            pi.decl = module;
            pi.codeIdx = idx;
            allProcs.append(pi);
            seenCodeIdx.insert(idx);
        }
    }

    for (Declaration* d = module->subs; d; d = d->next) {
        if (d->kind == Declaration::Procedure && !d->forward) {
            int idx = d_code.findProc(d);
            if (idx >= 0 && !seenCodeIdx.contains(idx)) {
                ProcInfo pi;
                pi.decl = d;
                pi.codeIdx = idx;
                allProcs.append(pi);
                seenCodeIdx.insert(idx);
            }
        }
        if (d->kind == Declaration::TypeDecl && d->getType()) {
            Type* t = d->getType();
            if (t->kind == Type::Object || t->kind == Type::Struct) {
                foreach (Declaration* sub, t->subs) {
                    if (sub->kind == Declaration::Procedure && !sub->forward) {
                        int idx = d_code.findProc(sub);
                        if (idx >= 0 && !seenCodeIdx.contains(idx)) {
                            ProcInfo pi;
                            pi.decl = sub;
                            pi.codeIdx = idx;
                            allProcs.append(pi);
                            seenCodeIdx.insert(idx);
                        }
                    }
                }
            }
        }
    }

    // Create ELF symbols
    for (int i = 0; i < allProcs.size(); i++) {
        Declaration* d = allProcs[i].decl;
        QByteArray name;
        if (d->kind == Declaration::Module)
            name = elfSymName(d->name + QByteArray("$begin$"));
        else
            name = elfSymName(d->toPath());

        bool isExported = (d->kind == Declaration::Module) || d->public_;
        quint8 binding = (isExported || d_cdeclReturns) ? STB_GLOBAL : STB_LOCAL;

        quint32 symIdx = d_elf.addSymbol(name, d_sections.text, 0, 0, binding, STT_FUNC);
        d_procSymbols[allProcs[i].codeIdx] = symIdx;
    }

    // Render each procedure
    for (int i = 0; i < allProcs.size(); i++) {
        int procIdx = allProcs[i].codeIdx;
        Procedure* proc = d_code.getProc(procIdx);
        Q_ASSERT(proc);

        d_procTextOffsets[procIdx] = d_emitter.currentPosition();
        d_elf.setSymbolValue(d_procSymbols[procIdx], d_emitter.currentPosition());

        quint32 procStartPC = d_emitter.currentPosition();

        // For the module init procedure ($begin$), emit a once-only guard:
        Label initSkipLabel;
        bool isModuleInit = (allProcs[i].decl->kind == Declaration::Module);
        if (isModuleInit) {
            //   mov ecx, [guard_addr]   ; load guard address (relocated)
            d_emitter.mov_ri(ECX, initGuardOff);
            quint32 immOff = d_emitter.currentPosition() - 4;
            d_elf.addRelocation(d_sections.relText, immOff, d_dataSymIdx, R_386_32);
            //   mov eax, [ecx]          ; load guard value
            d_emitter.mov_rm(EAX, ECX, 0);
            //   test eax, eax
            d_emitter.test_rr(EAX, EAX);
            //   jne skip                ; already initialized -> skip
            d_emitter.jcc(CC_NE, initSkipLabel);
            //   mov [ecx], 1            ; mark as initialized
            d_emitter.mov_mi(ECX, 0, 1);
        }

        // Record where the actual prologue (push ebp; mov ebp, esp) begins.
        // For begin$ functions this is after the guard check; for regular
        // procedures it equals procStartPC.
        quint32 prologuePC = d_emitter.currentPosition();

        if (!renderProcedure(*proc, procIdx))
            return false;

        if (isModuleInit) {
            d_emitter.bind(initSkipLabel);
            //   skip: ret
            d_emitter.ret();
        }

        // subprogram
        if (d_dwarf) {
            Declaration* decl = allProcs[i].decl;
            quint32 procEndPC = d_emitter.currentPosition();

            QByteArray procName;
            if (decl->kind == Declaration::Module)
                procName = decl->name + "$begin$";
            else
                procName = decl->toPath();

            quint32 declLine = decl->pos.line();
            d_dwarf->addSubprogram(procName, procStartPC, procEndPC, declLine, prologuePC);

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
                    d_dwarf->addLineEntry(procEndPC > 0 ? procEndPC - 1 : 0, pd->end.line());
            }

            d_dwarf->endSubprogram();
        }
    }

    // Copy machine code to .text
    d_elf.appendToSection(d_sections.text, d_emitter.code());

    // Initialize vtable pointers for global Object-type variables in .data
    initGlobalVarVtables(module);

    // Write .micron.mod
    QByteArray modContent = "module " + module->name + "\n";
    d_elf.appendToSection(d_sections.micronMod, modContent);

    // module variables
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
        d_dwarf->finalize(d_textSymIdx, d_dataSymIdx);
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

bool Renderer::generateMainObject(const QByteArrayList& moduleNames, const QString& outputPath, bool indirect)
{
    // moduleNames: list of module names whose <name>$begin$ will be called in order.
    // outputPath: path to write the generated main.o file.
    //
    // When indirect is false (freestanding):
    //   _start calls each begin$ directly, then exits via int $0x80.
    //
    // When indirect is true (C library linking):
    //   _start extracts argc/argv from the kernel stack, calls __mic$init(argc, argv).
    //   __mic$main is a separate function that calls each begin$ and returns 0.
    //   __mic$init (provided externally in C) handles e.g. libc init and calls __mic$main.

    ElfWriter elf(ElfWriter::ArchX86);
    X86::Emitter em;

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

        em.xor_rr(EBP, EBP);    // mark deepest stack frame
        em.pop_r(ESI);          // argc (kernel put it at top of stack)
        em.mov_rr(ECX, ESP);    // argv (now esp points to argv[0])
        em.and_ri(ESP, 0xFFFFFFF0); // 16-byte align stack (System V ABI)
        em.push_r(ECX);            // argv argument to __mic$init
        em.push_r(ESI);            // argc argument to __mic$init
        quint32 callOff = em.call_rel32(-4); // call never returns
        elf.addRelocation(sec.relText, callOff, micInitSym, 2 /*R_386_PC32*/);
        em.emitByte(0xF4);         // HLT (should never reach here)

        // Record end of _start, start of __mic$main
        quint32 micMainOff = em.currentPosition();

        // Emit __mic$main:
        em.push_r(EBP);
        em.mov_rr(EBP, ESP);

        for (int i = 0; i < initSymbols.size(); i++) {
            quint32 off = em.call_rel32(-4);
            elf.addRelocation(sec.relText, off, initSymbols[i], 2 /*R_386_PC32*/);
        }

        em.xor_rr(EAX, EAX);
        em.pop_r(EBP);
        em.ret();

        elf.appendToSection(sec.text, em.code());

        // Add global symbols
        elf.addSymbol("_start", sec.text, 0, micMainOff, STB_GLOBAL, STT_FUNC);
        elf.addSymbol("__mic$main", sec.text, micMainOff, em.currentPosition() - micMainOff, STB_GLOBAL, STT_FUNC);
    } else {
        // Emit _start
        em.xor_rr(EBP, EBP);

        // Call each module init in order
        for (int i = 0; i < initSymbols.size(); i++) {
            quint32 callOff = em.call_rel32(-4);
            elf.addRelocation(sec.relText, callOff, initSymbols[i], 2 /*R_386_PC32*/);
        }

        // Exit via Linux sys_exit syscall
        em.mov_rr(EBX, EAX);
        em.mov_ri(EAX, 1);
        em.int80();

        elf.appendToSection(sec.text, em.code());

        // Add _start as global symbol
        elf.addSymbol("_start", sec.text, 0, em.currentPosition(), STB_GLOBAL, STT_FUNC);
    }

    // Write the ELF object file
    return elf.write(outputPath);
}

bool Renderer::renderProcedure(Procedure& proc, int procIdx)
{
    d_localsSize = (proc.localsSize + 3) & ~3;
    d_argsSize = proc.argsSize;

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

void Renderer::emitPrologue(quint32 localsSize)
{
    d_emitter.push_r(EBP);
    d_emitter.mov_rr(EBP, ESP);
    if (localsSize > 0)
        d_emitter.sub_ri(ESP, localsSize);
}

void Renderer::emitEpilogue()
{
    d_emitter.mov_rr(ESP, EBP);
    d_emitter.pop_r(EBP);
    d_emitter.ret();
}

void Renderer::pushReg(Register r)
{
    d_emitter.push_r(r);
}

void Renderer::popReg(Register r)
{
    d_emitter.pop_r(r);
}

void Renderer::pushRegPair(Register rLo, Register rHi)
{
    // Push high word first so low word ends up at lower address
    d_emitter.push_r(rHi);
    d_emitter.push_r(rLo);
}

void Renderer::popRegPair(Register rLo, Register rHi)
{
    d_emitter.pop_r(rLo);
    d_emitter.pop_r(rHi);
}

void Renderer::loadLocalAddr(Register rd, quint32 milOffset)
{
    qint32 fpOff = -((qint32)d_localsSize) + (qint32)milOffset;
    d_emitter.lea(rd, EBP, fpOff);
}

void Renderer::loadArgAddr(Register rd, quint32 milOffset)
{
    qint32 fpOff = FP_TO_ARGS + (qint32)milOffset;
    d_emitter.lea(rd, EBP, fpOff);
}

void Renderer::loadVarAddr(Register rd, quint32 milOffset)
{
    // MOV rd, imm32 with R_386_32 relocation against .data section
    // The immediate is the offset within .data; linker adds section base.
    d_emitter.mov_ri(rd, milOffset);
    // The relocation patches the imm32 at (currentPosition - 4)
    quint32 immOffset = d_emitter.currentPosition() - 4;
    d_elf.addRelocation(d_sections.relText, immOffset, d_dataSymIdx, R_386_32);
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

void Renderer::emitCallStackAdj(quint32 argsSize, quint32 returnSize, bool fpReturn)
{
    if (d_cdeclReturns && returnSize > 0 && returnSize <= 8) {
        // cdecl mode: callee returned value in register(s).
        // Pop args, then push the result onto the eval stack.
        if (fpReturn) {
            // float/double returned in ST(0) per i386 cdecl ABI.
            // Make room on the eval stack and store from FPU.
            if (argsSize > returnSize) {
                d_emitter.add_ri(ESP, argsSize - returnSize);
            } else if (argsSize < returnSize) {
                d_emitter.sub_ri(ESP, returnSize - argsSize);
            }
            if (returnSize <= 4)
                d_emitter.fstp_s(ESP, 0);
            else
                d_emitter.fstp_d(ESP, 0);
        } else {
            // Integer/pointer returned in EAX (<=4) or EAX:EDX (<=8).
            if (argsSize > 0)
                d_emitter.add_ri(ESP, argsSize);
            if (returnSize <= 4)
                d_emitter.push_r(EAX);
            else
                pushRegPair(EAX, EDX);
        }
    } else if (returnSize == 0) {
        if (argsSize > 0)
            d_emitter.add_ri(ESP, argsSize);
    } else {
        // Stack-based return: value was written to [FP+8] area by callee.
        // Move return value and adjust stack.
        qint32 adj = (qint32)argsSize - (qint32)returnSize;
        if (adj > 0) {
            // Move return value up by adj bytes (reverse order to avoid overlap corruption)
            for (qint32 w = (qint32)returnSize - 4; w >= 0; w -= 4) {
                d_emitter.mov_rm(EAX, ESP, w);
                d_emitter.mov_mr(ESP, w + adj, EAX);
            }
            d_emitter.add_ri(ESP, adj);
        } else if (adj < 0) {
            quint32 neg = (quint32)(-adj);
            d_emitter.sub_ri(ESP, neg);
            for (quint32 w = 0; w < returnSize; w += 4) {
                d_emitter.mov_rm(EAX, ESP, (qint32)(w + neg));
                d_emitter.mov_mr(ESP, (qint32)w, EAX);
            }
        }
    }
}

quint32 Renderer::emitArgAlignment(Declaration* decl)
{
    // The eval stack pushes arguments contiguously (each param's stack-aligned size,
    // no inter-param alignment gaps). But the callee reads args at aligned offsets
    // (computed by calcParamsLocalsLayout which inserts alignment padding between params).
    // This function expands the stack and moves args to their aligned positions.
    //
    // Returns the total gap (argsSize - pushedSize) that was inserted.

    if (!decl)
        return 0;

    QList<Declaration*> params = decl->getParams(true);
    if (params.isEmpty())
        return 0;

    const quint8 pw = 4; // pointerWidth for x86-32
    const quint8 sa = 4; // stackAlignment

    // Compute pushed size: sum of each param's stack-aligned individual size (no gaps)
    quint32 pushedSize = 0;
    for (int i = 0; i < params.size(); i++) {
        Type* t = params[i]->getType();
        int size = t->getByteSize(pw);
        pushedSize += qMax(size, (int)sa);
    }

    // Compute aligned argsSize by replicating calcParamsLocalsLayout logic
    quint32 alignedSize = 0;
    {
        int off_p = 0;
        for (int i = 0; i < params.size(); i++) {
            Type* t = params[i]->getType();
            int size = t->getByteSize(pw);
            int alig = t->getAlignment(pw);
            off_p += AstModel::padding(off_p, alig);
            off_p += qMax(size, (int)sa);
        }
        alignedSize = d_code.stackAligned(off_p);
    }

    quint32 gap = alignedSize - pushedSize;
    if (gap == 0)
        return 0;

    X86::Emitter& em = d_emitter;

    // Expand the stack by the total gap
    em.sub_ri(ESP, gap);

    // Move each arg from its contiguous position to its aligned position.
    // Args are on stack in reverse push order (first param at lowest address).
    // We process from highest address to lowest to avoid overwriting.
    //
    // Build arrays of: contiguous offset, aligned offset, size for each param
    struct ParamInfo {
        quint32 contigOff;  // offset from ESP (before expansion) in contiguous layout
        quint32 alignedOff; // offset from ESP (after expansion) in aligned layout
        quint32 size;       // stack-aligned size of this param
    };
    QVector<ParamInfo> pinfo(params.size());

    // Contiguous offsets (from old ESP, i.e. from ESP+gap after expansion)
    quint32 coff = 0;
    for (int i = 0; i < params.size(); i++) {
        Type* t = params[i]->getType();
        int size = t->getByteSize(pw);
        int ssize = qMax(size, (int)sa);
        pinfo[i].contigOff = coff;
        pinfo[i].size = ssize;
        coff += ssize;
    }

    // Aligned offsets (from new ESP, matching callee's expected layout)
    {
        int off_p = 0;
        for (int i = 0; i < params.size(); i++) {
            Type* t = params[i]->getType();
            int size = t->getByteSize(pw);
            int alig = t->getAlignment(pw);
            off_p += AstModel::padding(off_p, alig);
            pinfo[i].alignedOff = off_p;
            off_p += qMax(size, (int)sa);
        }
    }

    // Move args from contiguous positions (at ESP+gap+contigOff) to aligned positions (at ESP+alignedOff).
    // Process from last param to first (highest to lowest address) to avoid overwrites.
    for (int i = params.size() - 1; i >= 0; i--) {
        quint32 srcOff = gap + pinfo[i].contigOff;
        quint32 dstOff = pinfo[i].alignedOff;
        if (srcOff == dstOff)
            continue;
        // Move this param's words
        for (quint32 w = 0; w < pinfo[i].size; w += 4) {
            em.mov_rm(EAX, ESP, (qint32)(srcOff + w));
            em.mov_mr(ESP, (qint32)(dstOff + w), EAX);
        }
    }

    return gap;
}

bool Renderer::setError(const QString& msg)
{
    d_error = msg;
    qCritical() << "X86Renderer:" << msg;
    return false;
}

int Renderer::emitOp(Procedure& proc, int pc)
{
    const Operation& op = proc.ops[pc];
    const LL_op opcode = (LL_op)op.op;
    const quint32 val = op.val;
    X86::Emitter& em = d_emitter;

    switch (opcode) {

    case LL_ldc_i4: {
        qint64 intVal = d_code.getInt(val);
        em.push_i((quint32)(qint32)intVal);
        return 1;
    }
    case LL_ldc_i8: {
        qint64 intVal = d_code.getInt(val);
        quint32 lo = (quint32)(intVal & 0xFFFFFFFF);
        quint32 hi = (quint32)((intVal >> 32) & 0xFFFFFFFF);
        em.push_i(hi);
        em.push_i(lo);
        return 1;
    }
    case LL_ldc_i4_m1: em.push_i(0xFFFFFFFF); return 1;
    case LL_ldc_i4_0:  em.push_i(0); return 1;
    case LL_ldc_i4_1:  em.push_i(1); return 1;
    case LL_ldc_i4_2:  em.push_i(2); return 1;
    case LL_ldc_i4_3:  em.push_i(3); return 1;
    case LL_ldc_i4_4:  em.push_i(4); return 1;
    case LL_ldc_i4_5:  em.push_i(5); return 1;
    case LL_ldc_i4_6:  em.push_i(6); return 1;
    case LL_ldc_i4_7:  em.push_i(7); return 1;
    case LL_ldc_i4_8:  em.push_i(8); return 1;

    // Float/Double Constants
    case LL_ldc_r4: {
        double dval = d_code.getDouble(val);
        float fval = (float)dval;
        quint32 rodataOff;
        if (d_floatOffsets.contains(val)) {
            rodataOff = d_floatOffsets[val];
        } else {
            // Align to 4-byte boundary
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
        // Load float from .rodata via absolute address (needs R_386_32 relocation)
        // MOV EAX, [rodataOff] with relocation
        // Actually: push the float value directly from .rodata address
        // Use: MOV EAX, imm32(rodata addr); PUSH [EAX]
        em.mov_ri(EAX, rodataOff);
        quint32 immOff = em.currentPosition() - 4;
        d_elf.addRelocation(d_sections.relText, immOff, d_rodataSymIdx, R_386_32);
        em.push_m(EAX, 0);
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

        em.mov_ri(EAX, rodataOff);
        quint32 immOff = em.currentPosition() - 4;
        d_elf.addRelocation(d_sections.relText, immOff, d_rodataSymIdx, R_386_32);
        // Push high word first, then low word
        em.push_m(EAX, 4);
        em.push_m(EAX, 0);
        return 1;
    }

    case LL_ldnull:
        em.push_i(0);
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
        // push address of string, relocated
        em.push_i(rodataOff);
        quint32 immOff = em.currentPosition() - 4;
        d_elf.addRelocation(d_sections.relText, immOff, d_rodataSymIdx, R_386_32);
        return 1;
    }

    case LL_ldobj: {
        const std::vector<char>& obj = d_code.getObject(val);
        quint32 rodataOff = d_elf.sectionSize(d_sections.rodata);
        d_elf.appendToSection(d_sections.rodata, obj.data(), obj.size());
        quint32 size = obj.size();
        quint32 aligned = (size + 3) & ~3;
        // Load source address
        em.mov_ri(EAX, rodataOff);
        quint32 immOff = em.currentPosition() - 4;
        d_elf.addRelocation(d_sections.relText, immOff, d_rodataSymIdx, R_386_32);
        // Allocate stack space and copy
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(ECX, EAX, i);
            em.mov_mr(ESP, i, ECX);
        }
        return 1;
    }

    case LL_ldloc_i1: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.movsx_rb(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldloc_i2: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.movsx_rw(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldloc_i4:
    case LL_ldloc_u4:
    case LL_ldloc_r4:
    case LL_ldloc_p: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.mov_rm(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldloc_i8:
    case LL_ldloc_u8:
    case LL_ldloc_r8:
    case LL_ldloc_pp: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.mov_rm(EDX, EBP, fo + 4);
        em.mov_rm(EAX, EBP, fo);
        pushRegPair(EAX, EDX);
        return 1;
    }
    case LL_ldloc_u1: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.movzx_rb(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldloc_u2: {
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.movzx_rw(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldloca:
        loadLocalAddr(EAX, val);
        pushReg(EAX);
        return 1;
    case LL_ldloc_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadLocalAddr(ECX, val);
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ECX, i);
            em.mov_mr(ESP, i, EAX);
        }
        return 2;
    }

    // Store locals
    case LL_stloc_i1: {
        popReg(EAX);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.mov_mr8(EBP, fo, EAX);
        return 1;
    }
    case LL_stloc_i2: {
        popReg(EAX);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.mov_mr16(EBP, fo, EAX);
        return 1;
    }
    case LL_stloc_i4:
    case LL_stloc_r4:
    case LL_stloc_p: {
        popReg(EAX);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.mov_mr(EBP, fo, EAX);
        return 1;
    }
    case LL_stloc_i8:
    case LL_stloc_r8:
    case LL_stloc_pp: {
        popRegPair(EAX, EDX);
        qint32 fo = -((qint32)d_localsSize) + (qint32)val;
        em.mov_mr(EBP, fo, EAX);
        em.mov_mr(EBP, fo + 4, EDX);
        return 1;
    }
    case LL_stloc_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadLocalAddr(ECX, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ESP, i);
            em.mov_mr(ECX, i, EAX);
        }
        em.add_ri(ESP, aligned);
        return 2;
    }

    case LL_ldarg_i1: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.movsx_rb(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldarg_i2: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.movsx_rw(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldarg_i4:
    case LL_ldarg_u4:
    case LL_ldarg_r4:
    case LL_ldarg_p: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.mov_rm(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldarg_i8:
    case LL_ldarg_u8:
    case LL_ldarg_r8:
    case LL_ldarg_pp: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.mov_rm(EDX, EBP, fo + 4);
        em.mov_rm(EAX, EBP, fo);
        pushRegPair(EAX, EDX);
        return 1;
    }
    case LL_ldarg_u1: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.movzx_rb(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldarg_u2: {
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.movzx_rw(EAX, EBP, fo);
        pushReg(EAX);
        return 1;
    }
    case LL_ldarg_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadArgAddr(ECX, val);
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ECX, i);
            em.mov_mr(ESP, i, EAX);
        }
        return 2;
    }
    case LL_ldarga:
        loadArgAddr(EAX, val);
        pushReg(EAX);
        return 1;

    // Store args
    case LL_starg_i1: {
        popReg(EAX);
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.mov_mr8(EBP, fo, EAX);
        return 1;
    }
    case LL_starg_i2: {
        popReg(EAX);
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.mov_mr16(EBP, fo, EAX);
        return 1;
    }
    case LL_starg_i4:
    case LL_starg_r4:
    case LL_starg_p: {
        popReg(EAX);
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.mov_mr(EBP, fo, EAX);
        return 1;
    }
    case LL_starg_i8:
    case LL_starg_r8:
    case LL_starg_pp: {
        popRegPair(EAX, EDX);
        qint32 fo = FP_TO_ARGS + (qint32)val;
        em.mov_mr(EBP, fo, EAX);
        em.mov_mr(EBP, fo + 4, EDX);
        return 1;
    }
    case LL_starg_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadArgAddr(ECX, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ESP, i);
            em.mov_mr(ECX, i, EAX);
        }
        em.add_ri(ESP, aligned);
        return 2;
    }

    case LL_ldvar_i1:
        loadVarAddr(ECX, val);
        em.movsx_rb(EAX, ECX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldvar_i2:
        loadVarAddr(ECX, val);
        em.movsx_rw(EAX, ECX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldvar_i4:
    case LL_ldvar_u4:
    case LL_ldvar_r4:
    case LL_ldvar_p:
        loadVarAddr(ECX, val);
        em.mov_rm(EAX, ECX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldvar_i8:
    case LL_ldvar_u8:
    case LL_ldvar_r8:
    case LL_ldvar_pp:
        loadVarAddr(ECX, val);
        em.mov_rm(EAX, ECX, 0);
        em.mov_rm(EDX, ECX, 4);
        pushRegPair(EAX, EDX);
        return 1;
    case LL_ldvar_u1:
        loadVarAddr(ECX, val);
        em.movzx_rb(EAX, ECX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldvar_u2:
        loadVarAddr(ECX, val);
        em.movzx_rw(EAX, ECX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldvara:
        loadVarAddr(EAX, val);
        pushReg(EAX);
        return 1;
    case LL_ldvar_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadVarAddr(ECX, val);
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ECX, i);
            em.mov_mr(ESP, i, EAX);
        }
        return 2;
    }

    // Store module variables
    case LL_stvar_i1:
        popReg(EAX);
        loadVarAddr(ECX, val);
        em.mov_mr8(ECX, 0, EAX);
        return 1;
    case LL_stvar_i2:
        popReg(EAX);
        loadVarAddr(ECX, val);
        em.mov_mr16(ECX, 0, EAX);
        return 1;
    case LL_stvar_i4:
    case LL_stvar_r4:
    case LL_stvar_p:
        popReg(EAX);
        loadVarAddr(ECX, val);
        em.mov_mr(ECX, 0, EAX);
        return 1;
    case LL_stvar_i8:
    case LL_stvar_r8:
    case LL_stvar_pp:
        popRegPair(EAX, EDX);
        loadVarAddr(ECX, val);
        em.mov_mr(ECX, 0, EAX);
        em.mov_mr(ECX, 4, EDX);
        return 1;
    case LL_stvar_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        loadVarAddr(ECX, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ESP, i);
            em.mov_mr(ECX, i, EAX);
        }
        em.add_ri(ESP, aligned);
        return 2;
    }

    case LL_add_i4:
        popReg(ECX); popReg(EAX);
        em.add_rr(EAX, ECX);
        pushReg(EAX);
        return 1;
    case LL_sub_i4:
        popReg(ECX); popReg(EAX);
        em.sub_rr(EAX, ECX);
        pushReg(EAX);
        return 1;
    case LL_mul_i4:
        popReg(ECX); popReg(EAX);
        em.imul_rr(EAX, ECX);
        pushReg(EAX);
        return 1;
    case LL_div_i4:
        popReg(ECX); popReg(EAX);
        em.cdq();           // sign-extend EAX into EDX:EAX
        em.idiv_r(ECX);     // EAX = quotient, EDX = remainder
        pushReg(EAX);
        return 1;
    case LL_div_un_i4:
        popReg(ECX); popReg(EAX);
        em.xor_rr(EDX, EDX); // zero EDX for unsigned divide
        em.div_r(ECX);
        pushReg(EAX);
        return 1;
    case LL_rem_i4:
        popReg(ECX); popReg(EAX);
        em.cdq();
        em.idiv_r(ECX);
        pushReg(EDX);        // remainder is in EDX
        return 1;
    case LL_rem_un_i4:
        popReg(ECX); popReg(EAX);
        em.xor_rr(EDX, EDX);
        em.div_r(ECX);
        pushReg(EDX);
        return 1;
    case LL_neg_i4:
        popReg(EAX);
        em.neg_r(EAX);
        pushReg(EAX);
        return 1;
    case LL_abs_i4: {
        popReg(EAX);
        em.mov_rr(ECX, EAX);
        em.sar_ri(ECX, 31);       // ECX = 0 if positive, -1 if negative
        em.xor_rr(EAX, ECX);      // flip bits if negative
        em.sub_rr(EAX, ECX);      // add 1 if was negative
        pushReg(EAX);
        return 1;
    }

    // Stack layout for binary ops: [a_lo][a_hi][b_lo][b_hi], b on top.
    // b_lo at ESP+0, b_hi at ESP+4, a_lo at ESP+8, a_hi at ESP+12.
    // We operate directly on stack memory using EAX/ECX/EDX as scratch.
    case LL_add_i8: {
        // Add b to a in place, then pop b
        em.mov_rm(EAX, ESP, 0);   // b_lo
        em.mov_rm(ECX, ESP, 8);   // a_lo
        em.add_rr(ECX, EAX);      // a_lo + b_lo, sets CF
        em.mov_mr(ESP, 8, ECX); // store result_lo
        em.mov_rm(EAX, ESP, 4);   // b_hi
        em.mov_rm(ECX, ESP, 12);  // a_hi
        em.adc_rr(ECX, EAX);      // a_hi + b_hi + CF
        em.mov_mr(ESP, 12, ECX);// store result_hi
        em.add_ri(ESP, 8);        // pop b (result now at top)
        return 1;
    }
    case LL_sub_i8: {
        em.mov_rm(EAX, ESP, 0);   // b_lo
        em.mov_rm(ECX, ESP, 8);   // a_lo
        em.sub_rr(ECX, EAX);      // a_lo - b_lo, sets CF
        em.mov_mr(ESP, 8, ECX);
        em.mov_rm(EAX, ESP, 4);   // b_hi
        em.mov_rm(ECX, ESP, 12);  // a_hi
        em.sbb_rr(ECX, EAX);      // a_hi - b_hi - CF
        em.mov_mr(ESP, 12, ECX);
        em.add_ri(ESP, 8);
        return 1;
    }
    case LL_mul_i8: {
        em.mov_rm(EAX, ESP, 8);   // a_lo
        em.mov_rm(ECX, ESP, 0);   // b_lo
        em.mul_r(ECX);             // EDX:EAX = a_lo * b_lo
        em.push_r(EAX);           // save result_lo
        em.push_r(EDX);           // save hi(a_lo * b_lo)
        // ESP shifted by 8 due to pushes: b_lo at ESP+8, b_hi at ESP+12, a_lo at ESP+16, a_hi at ESP+20
        em.mov_rm(EAX, ESP, 16);  // a_lo
        em.mov_rm(ECX, ESP, 12);  // b_hi
        em.imul_rr(EAX, ECX);     // a_lo * b_hi (low 32 bits)
        em.mov_rm(ECX, ESP, 20);  // a_hi
        em.mov_rm(EDX, ESP, 8);   // b_lo
        em.imul_rr(ECX, EDX);     // a_hi * b_lo
        em.add_rr(EAX, ECX);      // cross terms
        em.pop_r(EDX);            // hi(a_lo * b_lo)
        em.add_rr(EDX, EAX);      // result_hi = hi(a_lo*b_lo) + cross terms
        em.pop_r(EAX);            // result_lo
        em.add_ri(ESP, 8);        // pop b
        em.mov_mr(ESP, 0, EAX);   // store result_lo
        em.mov_mr(ESP, 4, EDX);   // store result_hi
        return 1;
    }
    case LL_div_i8:
    case LL_div_un_i8:
    case LL_rem_i8:
    case LL_rem_un_i8: {
        // NOTE that this assumes C implemented intrinsics and doesn't consider d_cdeclReturns!
        // 64-bit div/rem via __mic$ intrinsic call.
        // Stack: [b_lo ESP+0][b_hi ESP+4][a_lo ESP+8][a_hi ESP+12]
        // cdecl expects: [a_lo ESP+0][a_hi ESP+4][b_lo ESP+8][b_hi ESP+12]
        // Swap a and b on the stack:
        em.mov_rm(EAX, ESP, 0);   // b_lo
        em.mov_rm(ECX, ESP, 8);   // a_lo
        em.mov_mr(ESP, 0, ECX);   // a_lo -> ESP+0
        em.mov_mr(ESP, 8, EAX);   // b_lo -> ESP+8
        em.mov_rm(EAX, ESP, 4);   // b_hi
        em.mov_rm(ECX, ESP, 12);  // a_hi
        em.mov_mr(ESP, 4, ECX);   // a_hi -> ESP+4
        em.mov_mr(ESP, 12, EAX);  // b_hi -> ESP+12
        // Call the appropriate intrinsic
        const char* funcName;
        switch ((LL_op)op.op) {
        case LL_div_i8:    funcName = "__mic$div_i8"; break;
        case LL_div_un_i8: funcName = "__mic$div_un_i8"; break;
        case LL_rem_i8:    funcName = "__mic$rem_i8"; break;
        default:           funcName = "__mic$rem_un_i8"; break;
        }
        quint32 sym = d_elf.addSymbol(funcName, 0, 0, 0, STB_GLOBAL, STT_FUNC);
        quint32 callOff = em.call_rel32(-4);
        d_elf.addRelocation(d_sections.relText, callOff, sym, R_386_PC32);
        // cdecl return: 64-bit result in EDX:EAX (lo in EAX, hi in EDX)
        em.add_ri(ESP, 16);       // clean up all args
        pushRegPair(EAX, EDX);    // push result (lo, hi)
        return 1;
    }
    case LL_neg_i8: {
        // NEG lo sets CF if lo was nonzero; then SBB hi from 0
        em.mov_rm(EAX, ESP, 0);  // lo
        em.mov_rm(EDX, ESP, 4);  // hi
        em.neg_r(EAX);           // lo = -lo, sets CF if original lo != 0
        em.xor_rr(ECX, ECX);
        em.sbb_rr(ECX, EDX);     // ECX = 0 - hi - CF = -(hi + CF)
        em.mov_mr(ESP, 0, EAX);
        em.mov_mr(ESP, 4, ECX);
        return 1;
    }
    case LL_abs_i8: {
        em.mov_rm(EDX, ESP, 4);  // hi word
        em.test_rr(EDX, EDX);
        Label posLbl;
        em.jcc(CC_GE, posLbl);   // if hi >= 0, already positive
        // Negate
        em.mov_rm(EAX, ESP, 0);
        em.neg_r(EAX);
        em.xor_rr(ECX, ECX);
        em.sbb_rr(ECX, EDX);
        em.mov_mr(ESP, 0, EAX);
        em.mov_mr(ESP, 4, ECX);
        em.bind(posLbl);
        return 1;
    }

    case LL_add_r4:
        em.fld_s(ESP, 0);    // ST(0) = b
        em.fld_s(ESP, 4);    // ST(0) = a, ST(1) = b
        em.faddp();           // ST(0) = a + b
        em.add_ri(ESP, 4);   // pop one slot (keep one for result)
        em.fstp_s(ESP, 0);   // store result
        return 1;
    case LL_sub_r4:
        em.fld_s(ESP, 4);    // ST(0) = a
        em.fld_s(ESP, 0);    // ST(0) = b, ST(1) = a
        em.fxch();            // ST(0) = a, ST(1) = b
        em.fsubp();           // ST(0) = a - b
        em.add_ri(ESP, 4);
        em.fstp_s(ESP, 0);
        return 1;
    case LL_mul_r4:
        em.fld_s(ESP, 0);
        em.fld_s(ESP, 4);
        em.fmulp();
        em.add_ri(ESP, 4);
        em.fstp_s(ESP, 0);
        return 1;
    case LL_div_r4:
        em.fld_s(ESP, 4);    // a
        em.fld_s(ESP, 0);    // b
        em.fxch();            // ST(0)=a, ST(1)=b
        em.fdivp();           // a / b
        em.add_ri(ESP, 4);
        em.fstp_s(ESP, 0);
        return 1;
    case LL_neg_r4:
        em.fld_s(ESP, 0);
        em.fchs();
        em.fstp_s(ESP, 0);
        return 1;
    case LL_abs_r4:
        em.fld_s(ESP, 0);
        em.fabs_();
        em.fstp_s(ESP, 0);
        return 1;

    // Double arithmetic
    case LL_add_r8:
        em.fld_d(ESP, 0);    // b
        em.fld_d(ESP, 8);    // a
        em.faddp();
        em.add_ri(ESP, 8);
        em.fstp_d(ESP, 0);
        return 1;
    case LL_sub_r8:
        em.fld_d(ESP, 8);    // a
        em.fld_d(ESP, 0);    // b
        em.fxch();
        em.fsubp();
        em.add_ri(ESP, 8);
        em.fstp_d(ESP, 0);
        return 1;
    case LL_mul_r8:
        em.fld_d(ESP, 0);
        em.fld_d(ESP, 8);
        em.fmulp();
        em.add_ri(ESP, 8);
        em.fstp_d(ESP, 0);
        return 1;
    case LL_div_r8:
        em.fld_d(ESP, 8);
        em.fld_d(ESP, 0);
        em.fxch();
        em.fdivp();
        em.add_ri(ESP, 8);
        em.fstp_d(ESP, 0);
        return 1;
    case LL_neg_r8:
        em.fld_d(ESP, 0);
        em.fchs();
        em.fstp_d(ESP, 0);
        return 1;
    case LL_abs_r8:
        em.fld_d(ESP, 0);
        em.fabs_();
        em.fstp_d(ESP, 0);
        return 1;

    case LL_and_i4:
        popReg(ECX); popReg(EAX);
        em.and_rr(EAX, ECX);
        pushReg(EAX);
        return 1;
    case LL_or_i4:
        popReg(ECX); popReg(EAX);
        em.or_rr(EAX, ECX);
        pushReg(EAX);
        return 1;
    case LL_xor_i4:
        popReg(ECX); popReg(EAX);
        em.xor_rr(EAX, ECX);
        pushReg(EAX);
        return 1;
    case LL_not_i4:
        popReg(EAX);
        em.not_r(EAX);
        pushReg(EAX);
        return 1;
    case LL_shl_i4:
        popReg(ECX); popReg(EAX); // ECX = shift count
        em.shl_cl(EAX);
        pushReg(EAX);
        return 1;
    case LL_shr_i4:
        popReg(ECX); popReg(EAX);
        em.sar_cl(EAX);
        pushReg(EAX);
        return 1;
    case LL_shr_un_i4:
        popReg(ECX); popReg(EAX);
        em.shr_cl(EAX);
        pushReg(EAX);
        return 1;

    // Bitwise 64-bit
    case LL_and_i8: {
        em.mov_rm(EAX, ESP, 0);   // b_lo
        em.mov_rm(ECX, ESP, 8);   // a_lo
        em.and_rr(ECX, EAX);
        em.mov_mr(ESP, 8, ECX);   // result_lo
        em.mov_rm(EAX, ESP, 4);   // b_hi
        em.mov_rm(ECX, ESP, 12);  // a_hi
        em.and_rr(ECX, EAX);
        em.mov_mr(ESP, 12, ECX);  // result_hi
        em.add_ri(ESP, 8);        // pop b
        return 1;
    }
    case LL_or_i8:
        popRegPair(ECX, EDX);
        em.mov_rm(EAX, ESP, 0);
        em.or_rr(EAX, ECX);
        em.mov_mr(ESP, 0, EAX);
        em.mov_rm(EAX, ESP, 4);
        em.or_rr(EAX, EDX);
        em.mov_mr(ESP, 4, EAX);
        return 1;
    case LL_xor_i8:
        popRegPair(ECX, EDX);
        em.mov_rm(EAX, ESP, 0);
        em.xor_rr(EAX, ECX);
        em.mov_mr(ESP, 0, EAX);
        em.mov_rm(EAX, ESP, 4);
        em.xor_rr(EAX, EDX);
        em.mov_mr(ESP, 4, EAX);
        return 1;
    case LL_not_i8:
        em.mov_rm(EAX, ESP, 0);
        em.not_r(EAX);
        em.mov_mr(ESP, 0, EAX);
        em.mov_rm(EAX, ESP, 4);
        em.not_r(EAX);
        em.mov_mr(ESP, 4, EAX);
        return 1;
    case LL_shl_i8: {
        // Stack: [val_lo][val_hi][count], count on top (i4)
        popReg(ECX); // shift count
        // Stack: [val_lo][val_hi]
        em.mov_rm(EAX, ESP, 0);  // val_lo
        em.mov_rm(EDX, ESP, 4);  // val_hi
        em.shld_cl(EDX, EAX);    // shift high left, fill from low
        em.shl_cl(EAX);          // shift low left
        em.mov_mr(ESP, 0, EAX);
        em.mov_mr(ESP, 4, EDX);
        return 1;
    }
    case LL_shr_i8: {
        popReg(ECX);
        em.mov_rm(EAX, ESP, 0);
        em.mov_rm(EDX, ESP, 4);
        em.shrd_cl(EAX, EDX);
        em.sar_cl(EDX);
        em.mov_mr(ESP, 0, EAX);
        em.mov_mr(ESP, 4, EDX);
        return 1;
    }
    case LL_shr_un_i8: {
        popReg(ECX);
        em.mov_rm(EAX, ESP, 0);
        em.mov_rm(EDX, ESP, 4);
        em.shrd_cl(EAX, EDX);
        em.shr_cl(EDX);
        em.mov_mr(ESP, 0, EAX);
        em.mov_mr(ESP, 4, EDX);
        return 1;
    }

    case LL_ceq_i4:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_E, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_cgt_i4:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_G, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_clt_i4:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_L, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_cgt_u4:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_A, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_clt_u4:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_B, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;

    // Pointer comparisons (same as unsigned 32-bit)
    case LL_ceq_p:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_E, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_cgt_p:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_A, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_clt_p:
        popReg(ECX); popReg(EAX);
        em.cmp_rr(EAX, ECX);
        em.setcc(CC_B, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;

    // Comparisons (64-bit)
    case LL_ceq_i8: {
        popRegPair(ECX, EDX); // b
        em.mov_rm(EAX, ESP, 0); // a_lo
        em.cmp_rr(EAX, ECX);
        em.mov_rm(EAX, ESP, 4); // a_hi
        // Need both words equal
        Label neq, done;
        em.jcc(CC_NE, neq);
        em.cmp_rr(EAX, EDX);
        em.jcc(CC_NE, neq);
        em.add_ri(ESP, 8); // pop a
        em.push_i(1);
        em.jmp(done);
        em.bind(neq);
        em.add_ri(ESP, 8);
        em.push_i(0);
        em.bind(done);
        return 1;
    }
    case LL_cgt_i8:
    case LL_clt_i8:
    case LL_cgt_u8:
    case LL_clt_u8: {
        // Simplified: compare high words first, then low words if equal
        popRegPair(ECX, EDX); // b_lo, b_hi
        em.mov_rm(EAX, ESP, 4); // a_hi
        em.cmp_rr(EAX, EDX);
        Label highDiff, done2;
        em.jcc(CC_NE, highDiff);
        // High words equal, compare low words
        em.mov_rm(EAX, ESP, 0); // a_lo
        em.cmp_rr(EAX, ECX);
        em.bind(highDiff);
        em.add_ri(ESP, 8); // pop a
        Condition cc;
        if (opcode == LL_cgt_i8) cc = CC_G;
        else if (opcode == LL_clt_i8) cc = CC_L;
        else if (opcode == LL_cgt_u8) cc = CC_A;
        else cc = CC_B;
        em.setcc(cc, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    }

    // Float comparisons
    // NOTE: use lea instead of add to pop floats, because add clobbers EFLAGS set by fcomip
    case LL_ceq_r4:
        em.fld_s(ESP, 4);    // a
        em.fld_s(ESP, 0);    // b
        em.fcomip();          // compare ST(0) vs ST(1), set EFLAGS, pop
        em.fstp_st0();        // pop remaining
        em.lea(ESP, ESP, 8); // pop both floats (lea preserves EFLAGS)
        em.setcc(CC_E, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_cgt_r4:
        em.fld_s(ESP, 0);    // b
        em.fld_s(ESP, 4);    // a (now ST(0)=a, ST(1)=b)
        em.fcomip();          // compare a vs b
        em.fstp_st0();
        em.lea(ESP, ESP, 8);
        em.setcc(CC_A, EAX); // unsigned above for FPU comparison
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_clt_r4:
        em.fld_s(ESP, 0);    // b
        em.fld_s(ESP, 4);    // a
        em.fcomip();
        em.fstp_st0();
        em.lea(ESP, ESP, 8);
        em.setcc(CC_B, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;

    // Double comparisons
    // NOTE: use lea instead of add to pop doubles, because add clobbers EFLAGS set by fcomip
    case LL_ceq_r8:
        em.fld_d(ESP, 8);    // a
        em.fld_d(ESP, 0);    // b
        em.fcomip();
        em.fstp_st0();
        em.lea(ESP, ESP, 16); // lea preserves EFLAGS
        em.setcc(CC_E, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_cgt_r8:
        em.fld_d(ESP, 0);
        em.fld_d(ESP, 8);
        em.fcomip();
        em.fstp_st0();
        em.lea(ESP, ESP, 16);
        em.setcc(CC_A, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_clt_r8:
        em.fld_d(ESP, 0);
        em.fld_d(ESP, 8);
        em.fcomip();
        em.fstp_st0();
        em.lea(ESP, ESP, 16);
        em.setcc(CC_B, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;

    case LL_ceq_pp:
        // Compare two 8-byte method references
        popRegPair(ECX, EDX); // b
        em.mov_rm(EAX, ESP, 0);
        em.cmp_rr(EAX, ECX);
        {
            Label neq2, done3;
            em.jcc(CC_NE, neq2);
            em.mov_rm(EAX, ESP, 4);
            em.cmp_rr(EAX, EDX);
            em.jcc(CC_NE, neq2);
            em.add_ri(ESP, 8);
            em.push_i(1);
            em.jmp(done3);
            em.bind(neq2);
            em.add_ri(ESP, 8);
            em.push_i(0);
            em.bind(done3);
        }
        return 1;

    // i1 from various
    case LL_conv_i1_i4: popReg(EAX); em.movsx_rb_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_i1_i8: popRegPair(EAX, EDX); em.movsx_rb_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_i1_r4:
        em.fld_s(ESP, 0);
        em.fisttp_s(ESP, 0);
        popReg(EAX);
        em.movsx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    case LL_conv_i1_r8:
        em.fld_d(ESP, 0);
        em.add_ri(ESP, 4); // shrink from 8 to 4 bytes
        em.fisttp_s(ESP, 0);
        popReg(EAX);
        em.movsx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;

    // i2 conversions
    case LL_conv_i2_i4: popReg(EAX); em.movsx_rw_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_i2_i8: popRegPair(EAX, EDX); em.movsx_rw_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_i2_r4:
        em.fld_s(ESP, 0); em.fisttp_s(ESP, 0);
        popReg(EAX); em.movsx_rw_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_i2_r8:
        em.fld_d(ESP, 0); em.add_ri(ESP, 4); em.fisttp_s(ESP, 0);
        popReg(EAX); em.movsx_rw_reg(EAX, EAX); pushReg(EAX); return 1;

    // i4 conversions
    case LL_conv_i4_i8: popRegPair(EAX, EDX); pushReg(EAX); return 1;
    case LL_conv_i4_r4:
        em.fld_s(ESP, 0); em.fisttp_s(ESP, 0); return 1;
    case LL_conv_i4_r8:
        em.fld_d(ESP, 0); em.add_ri(ESP, 4); em.fisttp_s(ESP, 0); return 1;

    // i8 conversions (sign-extend i4 to i8)
    case LL_conv_i8_i4:
        popReg(EAX);
        em.cdq(); // sign-extend EAX into EDX
        pushRegPair(EAX, EDX);
        return 1;
    case LL_conv_i8_r4:
        em.fld_s(ESP, 0); em.fisttp_s(ESP, 0);
        popReg(EAX); em.cdq(); pushRegPair(EAX, EDX); return 1;
    case LL_conv_i8_r8:
        em.fld_d(ESP, 0); em.add_ri(ESP, 4); em.fisttp_s(ESP, 0);
        popReg(EAX); em.cdq(); pushRegPair(EAX, EDX); return 1;

    // u1 conversions
    case LL_conv_u1_i4: popReg(EAX); em.movzx_rb_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_u1_i8: popRegPair(EAX, EDX); em.movzx_rb_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_u1_r4:
        em.fld_s(ESP, 0); em.fisttp_s(ESP, 0);
        popReg(EAX); em.movzx_rb_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_u1_r8:
        em.fld_d(ESP, 0); em.add_ri(ESP, 4); em.fisttp_s(ESP, 0);
        popReg(EAX); em.movzx_rb_reg(EAX, EAX); pushReg(EAX); return 1;

    // u2 conversions
    case LL_conv_u2_i4: popReg(EAX); em.movzx_rw_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_u2_i8: popRegPair(EAX, EDX); em.movzx_rw_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_u2_r4:
        em.fld_s(ESP, 0); em.fisttp_s(ESP, 0);
        popReg(EAX); em.movzx_rw_reg(EAX, EAX); pushReg(EAX); return 1;
    case LL_conv_u2_r8:
        em.fld_d(ESP, 0); em.add_ri(ESP, 4); em.fisttp_s(ESP, 0);
        popReg(EAX); em.movzx_rw_reg(EAX, EAX); pushReg(EAX); return 1;

    // u4 conversions
    case LL_conv_u4_i8: popRegPair(EAX, EDX); pushReg(EAX); return 1;
    case LL_conv_u4_r4:
        em.fld_s(ESP, 0); em.fisttp_s(ESP, 0); return 1;
    case LL_conv_u4_r8:
        em.fld_d(ESP, 0); em.add_ri(ESP, 4); em.fisttp_s(ESP, 0); return 1;

    // u8 conversions (zero-extend i4 to u8)
    case LL_conv_u8_i4:
        popReg(EAX);
        em.xor_rr(EDX, EDX);
        pushRegPair(EAX, EDX);
        return 1;
    case LL_conv_u8_r4:
        em.fld_s(ESP, 0); em.fisttp_s(ESP, 0);
        popReg(EAX); em.xor_rr(EDX, EDX); pushRegPair(EAX, EDX); return 1;
    case LL_conv_u8_r8:
        em.fld_d(ESP, 0); em.add_ri(ESP, 4); em.fisttp_s(ESP, 0);
        popReg(EAX); em.xor_rr(EDX, EDX); pushRegPair(EAX, EDX); return 1;

    // r4 conversions (int -> float)
    case LL_conv_r4_i4:
        em.fild_s(ESP, 0);
        em.fstp_s(ESP, 0);
        return 1;
    case LL_conv_r4_i8:
        // Truncate to i4 first, then convert
        popRegPair(EAX, EDX);
        pushReg(EAX);
        em.fild_s(ESP, 0);
        em.fstp_s(ESP, 0);
        return 1;
    case LL_conv_r4_r8:
        em.fld_d(ESP, 0);
        em.add_ri(ESP, 4); // shrink 8 -> 4
        em.fstp_s(ESP, 0);
        return 1;

    // r8 conversions (int -> double)
    // NOTE: fstp QWORD writes 8 bytes. We must ensure 8 bytes are available
    // on the stack before the fstp. The pattern is: load into FPU, pop the
    // old value, push 8 bytes of space, then fstp.
    case LL_conv_r8_i4:
        em.fild_s(ESP, 0);   // load i4 from stack into FPU
        em.add_ri(ESP, 4);   // pop the i4 (4 bytes)
        em.sub_ri(ESP, 8);   // allocate 8 bytes for double
        em.fstp_d(ESP, 0);   // store double (8 bytes)
        return 1;
    case LL_conv_r8_i8:
        // Truncate to i4, then convert
        popRegPair(EAX, EDX);
        pushReg(EAX);
        em.fild_s(ESP, 0);   // load i4 into FPU
        em.add_ri(ESP, 4);   // pop the i4
        em.sub_ri(ESP, 8);   // allocate 8 bytes
        em.fstp_d(ESP, 0);   // store double
        return 1;
    case LL_conv_r8_r4:
        em.fld_s(ESP, 0);    // load float into FPU
        em.add_ri(ESP, 4);   // pop the float (4 bytes)
        em.sub_ri(ESP, 8);   // allocate 8 bytes for double
        em.fstp_d(ESP, 0);   // store double (8 bytes)
        return 1;

    case LL_br: {
        int target = pc + 1 + (op.minus ? -1 : 1) * (int)val;
        Label& lbl = getLabel(target);
        em.jmp(lbl);
        return 1;
    }
    case LL_brfalse_i4: {
        int target = pc + 1 + (op.minus ? -1 : 1) * (int)val;
        popReg(EAX);
        em.test_rr(EAX, EAX);
        Label& lbl = getLabel(target);
        em.jcc(CC_E, lbl);
        return 1;
    }

    case LL_call:
    case LL_callinst: {
        Procedure* callee = d_code.getProc(val);

        // Fix argument alignment: the eval stack pushes args contiguously,
        // but the callee expects them at aligned offsets (with inter-param gaps).
        // This inserts padding and rearranges args to match the callee's layout.
        quint32 alignGap = emitArgAlignment(callee->decl);

        // Pre-allocate padding when return value is written to [FP+8] and
        // returnSize > argsSize. In cdecl mode, returns <= 8 bytes go through
        // EAX/EAX:EDX so no padding is needed for those.
        quint32 padding = 0;
        bool stackReturn = !d_cdeclReturns || callee->returnSize > 8;
        if (stackReturn && callee->returnSize > callee->argsSize)
            padding = callee->returnSize - callee->argsSize;
        if (padding > 0) {
            // Grow the stack, then copy args down so they remain at the
            // lowest addresses (closest to [EBP+8] in the callee frame).
            // The padding (extra return space) sits above the args.
            em.sub_ri(ESP, padding);
            for (quint32 w = 0; w < callee->argsSize; w += 4) {
                em.mov_rm(EAX, ESP, (qint32)(padding + w));
                em.mov_mr(ESP, (qint32)w, EAX);
            }
        }

        // CALL rel32 with relocation
        // R_386_PC32: result = S + A - P, where P is address of the rel32 field.
        // CPU computes branch target as P + 4 + stored_value, so we need addend = -4
        // to compensate: S + (-4) - P = S - P - 4, which the CPU reads as (S - (P+4)).
        quint32 callOff = em.call_rel32(-4); // addend = -4 for R_386_PC32
        // callOff is the offset of the rel32 field
        if (d_procSymbols.contains(val)) {
            // Local call: R_386_PC32 relocation
            d_elf.addRelocation(d_sections.relText, callOff, d_procSymbols[val], R_386_PC32);
        } else {
            quint32 symIdx = getOrCreateExtSymbol(val);
            d_elf.addRelocation(d_sections.relText, callOff, symIdx, R_386_PC32);
        }

        const bool fpReturn = callee->decl && callee->decl->getType() && callee->decl->getType()->isFloat();
        emitCallStackAdj(callee->argsSize + padding, callee->returnSize, fpReturn);
        return 1;
    }

    case LL_ret: {
        quint32 retSize = val;
        if (d_cdeclReturns && retSize <= 4) {
            // Check if return type is float (retSize 4 = float)
            Type* retType = proc.decl->getType();
            if (retType && retType->isFloat()) {
                // cdecl: return float in ST(0) per i386 ABI
                em.fld_s(ESP, 0);
            } else {
                em.mov_rm(EAX, ESP, 0);
            }
        } else if (d_cdeclReturns && retSize <= 8) {
            // Check if return type is double (retSize 8 = double)
            Type* retType = proc.decl->getType();
            if (retType && retType->isFloat()) {
                // cdecl: return double in ST(0) per i386 ABI
                em.fld_d(ESP, 0);
            } else {
                em.mov_rm(EAX, ESP, 0);
                em.mov_rm(EDX, ESP, 4);
            }
        } else {
            for (quint32 w = 0; w < retSize; w += 4) {
                em.mov_rm(EAX, ESP, (qint32)w);
                em.mov_mr(EBP, (qint32)(8 + w), EAX);
            }
        }
        emitEpilogue();
        return 1;
    }
    case LL_ret_void:
        emitEpilogue();
        return 1;

    case LL_ldind_i1:
        popReg(EAX);
        em.movsx_rb(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldind_i2:
        popReg(EAX);
        em.movsx_rw(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldind_i4:
    case LL_ldind_u4:
    case LL_ldind_r4:
    case LL_ldind_p:
        popReg(EAX);
        em.mov_rm(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldind_i8:
    case LL_ldind_u8:
    case LL_ldind_r8:
        popReg(ECX);
        em.mov_rm(EAX, ECX, 0);
        em.mov_rm(EDX, ECX, 4);
        pushRegPair(EAX, EDX);
        return 1;
    case LL_ldind_u1:
        popReg(EAX);
        em.movzx_rb(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldind_u2:
        popReg(EAX);
        em.movzx_rw(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    case LL_ldind_vt:
    case LL_ldind_str: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        popReg(ECX);
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ECX, i);
            em.mov_mr(ESP, i, EAX);
        }
        return 1;
    }

    // Store indirect
    case LL_stind_i1:
        popReg(EAX); popReg(ECX); // value, addr
        em.mov_mr8(ECX, 0, EAX);
        return 1;
    case LL_stind_i2:
        popReg(EAX); popReg(ECX);
        em.mov_mr16(ECX, 0, EAX);
        return 1;
    case LL_stind_i4:
    case LL_stind_r4:
    case LL_stind_p:
        popReg(EAX); popReg(ECX);
        em.mov_mr(ECX, 0, EAX);
        return 1;
    case LL_stind_i8:
    case LL_stind_r8:
        popRegPair(EAX, EDX); popReg(ECX);
        em.mov_mr(ECX, 0, EAX);
        em.mov_mr(ECX, 4, EDX);
        return 1;
    case LL_stind_vt: {
        quint32 size = val;
        quint32 aligned = (size + 3) & ~3;
        // Stack: [value (aligned)] [addr (4)]
        em.mov_rm(ECX, ESP, aligned); // addr
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ESP, i);
            em.mov_mr(ECX, i, EAX);
        }
        em.add_ri(ESP, aligned + Slot4);
        return 1;
    }

    case LL_ldfld_i1:
        popReg(ECX);
        em.movsx_rb(EAX, ECX, val);
        pushReg(EAX);
        return 1;
    case LL_ldfld_i2:
        popReg(ECX);
        em.movsx_rw(EAX, ECX, val);
        pushReg(EAX);
        return 1;
    case LL_ldfld_i4:
    case LL_ldfld_u4:
    case LL_ldfld_r4:
    case LL_ldfld_p:
        popReg(ECX);
        em.mov_rm(EAX, ECX, val);
        pushReg(EAX);
        return 1;
    case LL_ldfld_i8:
    case LL_ldfld_u8:
    case LL_ldfld_r8:
    case LL_ldfld_pp:
        popReg(ECX);
        em.mov_rm(EAX, ECX, val);
        em.mov_rm(EDX, ECX, val + 4);
        pushRegPair(EAX, EDX);
        return 1;
    case LL_ldfld_u1:
        popReg(ECX);
        em.movzx_rb(EAX, ECX, val);
        pushReg(EAX);
        return 1;
    case LL_ldfld_u2:
        popReg(ECX);
        em.movzx_rw(EAX, ECX, val);
        pushReg(EAX);
        return 1;
    case LL_ldflda:
        popReg(EAX);
        if (val != 0)
            em.add_ri(EAX, val);
        pushReg(EAX);
        return 1;
    case LL_ldfld_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        popReg(ECX); // obj ptr
        if (val != 0)
            em.add_ri(ECX, val);
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ECX, i);
            em.mov_mr(ESP, i, EAX);
        }
        return 2;
    }

    // Store fields
    case LL_stfld_i1:
        popReg(EAX); popReg(ECX);
        em.mov_mr8(ECX, val, EAX);
        return 1;
    case LL_stfld_i2:
        popReg(EAX); popReg(ECX);
        em.mov_mr16(ECX, val, EAX);
        return 1;
    case LL_stfld_i4:
    case LL_stfld_r4:
    case LL_stfld_p:
        popReg(EAX); popReg(ECX);
        em.mov_mr(ECX, val, EAX);
        return 1;
    case LL_stfld_i8:
    case LL_stfld_r8:
    case LL_stfld_pp:
        popRegPair(EAX, EDX); popReg(ECX);
        em.mov_mr(ECX, val, EAX);
        em.mov_mr(ECX, val + 4, EDX);
        return 1;
    case LL_stfld_vt: {
        Q_ASSERT(pc + 1 < (int)proc.ops.size());
        quint32 size = proc.ops[pc + 1].val;
        quint32 aligned = (size + 3) & ~3;
        em.mov_rm(ECX, ESP, aligned); // obj ptr below value
        if (val != 0)
            em.add_ri(ECX, val);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ESP, i);
            em.mov_mr(ECX, i, EAX);
        }
        em.add_ri(ESP, aligned + Slot4);
        return 2;
    }

    case LL_ldelem_i1: {
        popReg(ECX); popReg(EAX); // index, array
        em.movsx_rb(EAX, EAX, 0); // need base+index addressing
        // x86 doesn't have base+index in movsx easily, use LEA
        em.add_rr(EAX, ECX);
        em.movsx_rb(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    }
    case LL_ldelem_i2: {
        popReg(ECX); popReg(EAX);
        em.shl_ri(ECX, 1);
        em.add_rr(EAX, ECX);
        em.movsx_rw(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    }
    case LL_ldelem_i4:
    case LL_ldelem_u4:
    case LL_ldelem_r4:
    case LL_ldelem_p: {
        popReg(ECX); popReg(EAX);
        em.shl_ri(ECX, 2);
        em.add_rr(EAX, ECX);
        em.mov_rm(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    }
    case LL_ldelem_i8:
    case LL_ldelem_u8:
    case LL_ldelem_r8: {
        popReg(ECX); popReg(EAX);
        em.shl_ri(ECX, 3);
        em.add_rr(ECX, EAX); // ECX = array + index*8
        em.mov_rm(EAX, ECX, 0);
        em.mov_rm(EDX, ECX, 4);
        pushRegPair(EAX, EDX);
        return 1;
    }
    case LL_ldelem_u1: {
        popReg(ECX); popReg(EAX);
        em.add_rr(EAX, ECX);
        em.movzx_rb(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    }
    case LL_ldelem_u2: {
        popReg(ECX); popReg(EAX);
        em.shl_ri(ECX, 1);
        em.add_rr(EAX, ECX);
        em.movzx_rw(EAX, EAX, 0);
        pushReg(EAX);
        return 1;
    }
    case LL_ldelema: {
        popReg(ECX); popReg(EAX); // index, array
        if (val == 1)
            em.add_rr(EAX, ECX);
        else if (val == 2) {
            em.shl_ri(ECX, 1);
            em.add_rr(EAX, ECX);
        } else if (val == 4) {
            em.shl_ri(ECX, 2);
            em.add_rr(EAX, ECX);
        } else if (val == 8) {
            em.shl_ri(ECX, 3);
            em.add_rr(EAX, ECX);
        } else {
            em.mov_ri(EDX, val);
            em.imul_rr(ECX, EDX);
            em.add_rr(EAX, ECX);
        }
        pushReg(EAX);
        return 1;
    }
    case LL_ldelem_vt: {
        quint32 elemSize = val;
        quint32 aligned = (elemSize + 3) & ~3;
        popReg(ECX); popReg(EAX); // index, array
        if (elemSize == 4) {
            em.shl_ri(ECX, 2);
        } else {
            em.mov_ri(EDX, elemSize);
            em.imul_rr(ECX, EDX);
        }
        em.add_rr(ECX, EAX); // ECX = base + index*size
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ECX, i);
            em.mov_mr(ESP, i, EAX);
        }
        return 1;
    }

    // Store elements
    case LL_stelem_i1: {
        popReg(EAX); popReg(ECX); popReg(EDX); // value, index, array
        em.add_rr(EDX, ECX);
        em.mov_mr8(EDX, 0, EAX);
        return 1;
    }
    case LL_stelem_i2: {
        popReg(EAX); popReg(ECX); popReg(EDX);
        em.shl_ri(ECX, 1);
        em.add_rr(EDX, ECX);
        em.mov_mr16(EDX, 0, EAX);
        return 1;
    }
    case LL_stelem_i4:
    case LL_stelem_r4:
    case LL_stelem_p: {
        popReg(EAX); popReg(ECX); popReg(EDX);
        em.shl_ri(ECX, 2);
        em.add_rr(EDX, ECX);
        em.mov_mr(EDX, 0, EAX);
        return 1;
    }
    case LL_stelem_i8:
    case LL_stelem_r8: {
        popRegPair(EAX, EDX); // value
        popReg(ECX);          // index
        // Need array ptr but we're out of regs. Use stack.
        em.push_r(EAX);      // save value_lo
        em.mov_rm(EAX, ESP, 4); // array ptr (was at ESP before push)
        em.shl_ri(ECX, 3);
        em.add_rr(EAX, ECX); // EAX = array + index*8
        em.pop_r(ECX);       // restore value_lo into ECX
        em.mov_mr(EAX, 0, ECX);
        em.mov_mr(EAX, 4, EDX);
        em.add_ri(ESP, 4);   // pop array ptr
        return 1;
    }
    case LL_stelem_vt: {
        quint32 elemSize = val;
        quint32 aligned = (elemSize + 3) & ~3;
        // Stack: [value (aligned)] [index (4)] [array (4)]
        em.mov_rm(ECX, ESP, aligned);       // index
        em.mov_rm(EDX, ESP, aligned + Slot4); // array
        if (elemSize == 4) {
            em.shl_ri(ECX, 2);
        } else {
            em.push_r(EAX);
            em.mov_ri(EAX, elemSize);
            em.imul_rr(ECX, EAX);
            em.pop_r(EAX);
        }
        em.add_rr(ECX, EDX); // ECX = dest addr
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ESP, i);
            em.mov_mr(ECX, i, EAX);
        }
        em.add_ri(ESP, aligned + Slot4 + Slot4);
        return 1;
    }

    case LL_pop: {
        quint32 aligned = (val + 3) & ~3;
        em.add_ri(ESP, aligned);
        return 1;
    }
    case LL_dup: {
        quint32 aligned = (val + 3) & ~3;
        em.sub_ri(ESP, aligned);
        for (quint32 i = 0; i < aligned; i += 4) {
            em.mov_rm(EAX, ESP, aligned + i);
            em.mov_mr(ESP, i, EAX);
        }
        return 1;
    }

    case LL_alloc1: {
        // alloc1: allocate single object of 'val' bytes (or template size if minus)
        // If op.minus, val is a template index; otherwise val is the byte size.
        quint32 allocSize;
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            allocSize = tmpl.mem.size();
        } else {
            allocSize = val;
        }
        em.push_i(allocSize);
        // CALL MIC$$alloc (external symbol)
        quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        quint32 callOff = em.call_rel32(-4);
        d_elf.addRelocation(d_sections.relText, callOff, allocSym, R_386_PC32);
        em.add_ri(ESP, 4); // clean up arg
        // EAX now has the pointer
        // If template, copy template data to allocated memory
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff;
                if (d_templateOffsets.contains(val)) {
                    tmplOff = d_templateOffsets[val];
                } else {
                    quint32 curSize = d_elf.sectionSize(d_sections.rodata);
                    quint32 pad = (4 - (curSize & 3)) & 3;
                    if (pad > 0) {
                        char zeros[4] = {0};
                        d_elf.appendToSection(d_sections.rodata, zeros, pad);
                    }
                    tmplOff = d_elf.sectionSize(d_sections.rodata);
                    d_elf.appendToSection(d_sections.rodata, tmpl.mem.data(), tmplSize);
                    d_templateOffsets[val] = tmplOff;
                }
                // ECX = template source address
                em.mov_ri(ECX, tmplOff);
                quint32 immOff = em.currentPosition() - 4;
                d_elf.addRelocation(d_sections.relText, immOff, d_rodataSymIdx, R_386_32);
                // Copy word by word (EAX = dest)
                quint32 aligned = (tmplSize + 3) & ~3;
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.mov_rm(EDX, ECX, i);
                    em.mov_mr(EAX, i, EDX);
                }
            }
            // Fix vtable pointer if this is an object type
            emitVtableFixups(EAX, tmpl);
        }
        pushReg(EAX); // push allocated pointer
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
        popReg(EAX); // count
        // total = count * elemSize
        em.mov_ri(ECX, elemSize);
        em.imul_rr(EAX, ECX);
        // Save total size for template init loop (push before call)
        em.mov_rr(ECX, EAX);
        em.push_r(ECX); // save totalSize on stack
        // Push size argument and call MIC$$alloc
        em.push_r(EAX);
        quint32 allocSym = d_elf.addSymbol("MIC$$alloc", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        quint32 callOff = em.call_rel32(-4);
        d_elf.addRelocation(d_sections.relText, callOff, allocSym, R_386_PC32);
        em.add_ri(ESP, 4); // clean up arg
        em.pop_r(ECX); // restore totalSize
        // EAX = allocated pointer, ECX = totalSize
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff;
                if (d_templateOffsets.contains(val)) {
                    tmplOff = d_templateOffsets[val];
                } else {
                    quint32 curSize = d_elf.sectionSize(d_sections.rodata);
                    quint32 pad = (4 - (curSize & 3)) & 3;
                    if (pad > 0) {
                        char zeros[4] = {0};
                        d_elf.appendToSection(d_sections.rodata, zeros, pad);
                    }
                    tmplOff = d_elf.sectionSize(d_sections.rodata);
                    d_elf.appendToSection(d_sections.rodata, tmpl.mem.data(), tmplSize);
                    d_templateOffsets[val] = tmplOff;
                }
                // Template init loop: copy template to each element
                // EAX = base pointer, save it
                em.push_r(EAX); // save base
                // EDI = current dest, ECX = end = base + totalSize
                em.mov_rr(EDI, EAX);
                em.add_rr(ECX, EAX); // ECX = end = base + totalSize
                // ESI = template address
                em.mov_ri(ESI, tmplOff);
                quint32 immOff = em.currentPosition() - 4;
                d_elf.addRelocation(d_sections.relText, immOff, d_rodataSymIdx, R_386_32);
                quint32 aligned = (tmplSize + 3) & ~3;
                Label loopLbl, doneLbl;
                em.bind(loopLbl);
                em.cmp_rr(EDI, ECX);
                em.jcc(CC_AE, doneLbl);
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.mov_rm(EDX, ESI, i);
                    em.mov_mr(EDI, i, EDX);
                }
                // Fix vtable pointer for this element
                // Save ECX (loop end pointer) since emitVtableFixups clobbers it
                em.push_r(ECX);
                emitVtableFixups(EDI, tmpl);
                em.pop_r(ECX);
                em.add_ri(EDI, tmplSize);
                em.jmp(loopLbl);
                em.bind(doneLbl);
                em.pop_r(EAX); // restore base pointer
            }
        }
        pushReg(EAX); // push allocated pointer
        return 1;
    }
    case LL_free: {
        popReg(EAX); // pointer to free
        em.push_r(EAX);
        quint32 freeSym = d_elf.addSymbol("MIC$$free", 0, 0, 0, STB_GLOBAL, STT_FUNC);
        quint32 callOff = em.call_rel32(-4);
        d_elf.addRelocation(d_sections.relText, callOff, freeSym, R_386_PC32);
        em.add_ri(ESP, 4); // clean up arg
        return 1;
    }
    case LL_initobj: {
        popReg(EAX); // destination pointer
        if (op.minus) {
            const Template& tmpl = d_code.getTemplate(val);
            quint32 tmplSize = tmpl.mem.size();
            if (tmplSize > 0) {
                quint32 tmplOff;
                if (d_templateOffsets.contains(val)) {
                    tmplOff = d_templateOffsets[val];
                } else {
                    quint32 curSize = d_elf.sectionSize(d_sections.rodata);
                    quint32 pad = (4 - (curSize & 3)) & 3;
                    if (pad > 0) {
                        char zeros[4] = {0};
                        d_elf.appendToSection(d_sections.rodata, zeros, pad);
                    }
                    tmplOff = d_elf.sectionSize(d_sections.rodata);
                    d_elf.appendToSection(d_sections.rodata, tmpl.mem.data(), tmplSize);
                    d_templateOffsets[val] = tmplOff;
                }
                // Load template source address
                em.mov_ri(ECX, tmplOff);
                quint32 immOff = em.currentPosition() - 4;
                d_elf.addRelocation(d_sections.relText, immOff, d_rodataSymIdx, R_386_32);
                // Copy word by word
                quint32 aligned = (tmplSize + 3) & ~3;
                for (quint32 i = 0; i < aligned; i += 4) {
                    em.mov_rm(EDX, ECX, i);
                    em.mov_mr(EAX, i, EDX);
                }
            }
            // Fix vtable pointer if this is an object type
            emitVtableFixups(EAX, tmpl);
        }
        return 1;
    }

    case LL_strcpy: {
        popReg(ECX); popReg(EDX); // src, dst
        em.mov_ri(EAX, val);      // max length
        // Byte-copy loop
        Label loopLbl, doneLbl;
        em.bind(loopLbl);
        em.test_rr(EAX, EAX);
        em.jcc(CC_E, doneLbl);
        // Load byte from src
        em.movzx_rb(EBX, ECX, 0);  // use EBX as temp (callee-saved, but we're in dumb mode)
        em.mov_mr8(EDX, 0, EBX);
        em.test_rr(EBX, EBX);
        em.jcc(CC_E, doneLbl);
        em.add_ri(ECX, 1);
        em.add_ri(EDX, 1);
        em.sub_ri(EAX, 1);
        em.jmp(loopLbl);
        em.bind(doneLbl);
        return 1;
    }

    case LL_calli: {
        quint32 rawReturnSize = (val >> 11) & 0x7FF;
        bool fpReturn = rawReturnSize & 1;
        quint32 argsSize = val & 0x7FF;
        quint32 returnSize = rawReturnSize & ~1u;
        popReg(EAX); // function pointer
        em.call_r(EAX);
        emitCallStackAdj(argsSize, returnSize, fpReturn);
        return 1;
    }
    case LL_callmi: {
        quint32 rawReturnSize = (val >> 11) & 0x7FF;
        bool fpReturn = rawReturnSize & 1;
        quint32 argsSize = val & 0x7FF;
        quint32 returnSize = rawReturnSize & ~1u;
        popReg(EAX); // method_ptr
        popReg(ECX); // obj_ptr
        pushReg(ECX); // push obj back as self (first arg)
        em.call_r(EAX);
        emitCallStackAdj(argsSize, returnSize, fpReturn);
        return 1;
    }

    case LL_callvirt: {
        Procedure* callee = d_code.getProc(val);

        // Fix argument alignment (same as LL_call)
        quint32 alignGap = emitArgAlignment(callee->decl);

        quint32 argsSize = callee->argsSize;
        quint32 padding = 0;
        bool stackReturn = !d_cdeclReturns || callee->returnSize > 8;
        if (stackReturn && callee->returnSize > argsSize)
            padding = callee->returnSize - argsSize;
        if (padding > 0) {
            em.sub_ri(ESP, padding);
            for (quint32 w = 0; w < argsSize; w += 4) {
                em.mov_rm(EAX, ESP, (qint32)(padding + w));
                em.mov_mr(ESP, (qint32)w, EAX);
            }
        }
        // Load self from stack (first arg at ESP+0 after alignment)
        em.mov_rm(EAX, ESP, 0);  // self
        em.mov_rm(ECX, EAX, 0);       // vtable ptr
        quint32 slot = 0;
        if (callee->decl && callee->decl->getPd())
            slot = callee->decl->getPd()->slot;
        em.mov_rm(EAX, ECX, (qint32)slot * 4); // method addr
        em.call_r(EAX);
        const bool fpReturn = callee->decl && callee->decl->getType() && callee->decl->getType()->isFloat();
        emitCallStackAdj(argsSize + padding, callee->returnSize, fpReturn);
        return 1;
    }

    case LL_ldproc: {
        // Push address of procedure (needs R_386_32 relocation)
        em.push_i(0); // placeholder
        quint32 immOff = em.currentPosition() - 4;
        if (d_procSymbols.contains(val)) {
            d_elf.addRelocation(d_sections.relText, immOff, d_procSymbols[val], R_386_32);
        } else {
            quint32 symIdx = getOrCreateExtSymbol(val);
            d_elf.addRelocation(d_sections.relText, immOff, symIdx, R_386_32);
        }
        return 1;
    }
    case LL_ldmeth_struct: {
        popReg(ECX); // obj pointer
        em.push_r(ECX); // push obj (will be high word)
        em.push_i(0);   // push proc addr placeholder (low word)
        quint32 immOff = em.currentPosition() - 4;
        if (d_procSymbols.contains(val)) {
            d_elf.addRelocation(d_sections.relText, immOff, d_procSymbols[val], R_386_32);
        } else {
            quint32 symIdx = getOrCreateExtSymbol(val);
            d_elf.addRelocation(d_sections.relText, immOff, symIdx, R_386_32);
        }
        return 1;
    }

    case LL_ldmeth: {
        popReg(EAX);                          // object pointer
        em.mov_rm(ECX, EAX, 0);              // vtable pointer
        em.mov_rm(ECX, ECX, (qint32)val * 4); // method addr
        // Push MethRef: [proc_addr at SP] [obj at SP+4]
        pushRegPair(ECX, EAX);
        return 1;
    }

    case LL_ldmeth_iface: {
        popRegPair(ECX, EDX); // ECX=vtable_addr, EDX=obj
        em.mov_rm(EAX, ECX, (qint32)val * 4); // method from vtable[slot]
        pushRegPair(EAX, EDX); // MethRef: method(lo), obj(hi)
        return 1;
    }

    case LL_ldiface: {
        popReg(ECX); // obj pointer
        quint32 vtableOff = getOrEmitVtable(val);
        // Load vtable address
        em.mov_ri(EAX, vtableOff);
        quint32 immOff = em.currentPosition() - 4;
        d_elf.addRelocation(d_sections.relText, immOff, d_dataSymIdx, R_386_32);
        // Push IfaceRef: [vtable_addr(lo)] [obj(hi)]
        pushRegPair(EAX, ECX);
        return 1;
    }

    case LL_isinst: {
        qWarning() << "X86Renderer: isinst type check not fully implemented";
        popReg(EAX);
        em.test_rr(EAX, EAX);
        em.setcc(CC_NE, EAX);
        em.movzx_rb_reg(EAX, EAX);
        pushReg(EAX);
        return 1;
    }

    case LL_vt_size:
        Q_ASSERT(false);
        return 1;

    case LL_sizeof:
    case LL_ptroff:
    case LL_newvla:
        qWarning() << "X86Renderer: not yet implemented:" << Code::op_names[opcode];
        em.nop();
        return 1;

    case LL_line:
        if (d_dwarf) {
            quint32 line = val;
            if (line > 0)
                d_dwarf->addLineEntry(em.currentPosition(), line);
        }
        return 1;

    case LL_invalid:
        return 1;

    default:
        return setError(QString("Unknown LL_op: %1").arg(opcode)) ? -1 : -1;
    }

    return 1;
}
