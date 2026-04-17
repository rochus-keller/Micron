#ifndef MILARMV7RENDERER_H
#define MILARMV7RENDERER_H

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

#include <Micron/MilVmCode2.h>
#include <Micron/MilArmv7Emitter.h>
#include <Micron/MilArmFixup.h>
#include <Micron/MilElfWriter.h>
#include <Micron/MilDwarfEmitter.h>
#include <QMap>

namespace Mil
{
namespace Arm
{
    // Direct stack-based translation.
    // Every MIL stack operation maps to ARM push/pop without register allocation.
    // All values flow through the ARM hardware stack (SP-relative).
    //
    // Stack frame layout (OP2-compatible):
    //   High addresses
    //     [arguments]        FP + 8 + argOffset   (pushed by caller)
    //     [saved LR]         FP + 4
    //     [saved FP]         FP + 0
    //     [locals]           FP - localsSize ... FP - 4
    //     [eval stack]       SP (grows downward)
    //   Low addresses
    //
    // Register usage:
    //   R0-R3:  scratch (operands, results, AAPCS caller-saved)
    //   R4-R10: callee-saved (unused in dumb translator)
    //   R11:    frame pointer (FP)
    //   R12:    scratch (IP, inter-procedure call scratch)
    //   R13:    stack pointer (SP)
    //   R14:    link register (LR)
    //   S0-S1:  VFP float scratch
    //   D0-D1:  VFP double scratch

    // NOTE: this renderer is not yet suited for Micron level 0!

    class Renderer
    {
    public:
        Renderer(AstModel* mdl);
        ~Renderer();

        void setHardwareDivide(bool v) { d_hasHwDiv = v; }
        bool hardwareDivide() const { return d_hasHwDiv; }
        void setEmitDwarf(bool v) { d_emitDwarf = v; }
        bool emitDwarf() const { return d_emitDwarf; }
        void setUseAapcs(bool v) { d_useAapcs = v; }
        bool useAapcs() const { return d_useAapcs; }

        // Render a complete module to an ELF relocatable object.
        // TODO assure TestSymChars
        bool renderModule(Declaration* module);

        // Generate a standalone main.o that calls each module's $begin$ in order,
        // then exits. Replaces the startup assembler.
        static bool generateMainObject(const QByteArrayList& moduleNames, const QString& outputPath, bool indirect = false);

        bool writeToFile(const QString& filename);

        QByteArray toByteArray();

        QString errorMessage() const { return d_error; }

        const Arm::Emitter& emitter() const { return d_emitter; }
        const ElfWriter& elfWriter() const { return d_elf; }

    private:
        bool renderProcedure(Vm::Procedure& proc, int procIdx);

        void emitPrologue(quint32 localsSize);

        void emitEpilogue();

        int emitOp(Vm::Procedure& proc, int pc);

        void loadImm32(Arm::Register rd, quint32 value);

        void pushReg(Arm::Register r);
        void popReg(Arm::Register r);

        void pushRegPair(Arm::Register rLo, Arm::Register rHi);
        void popRegPair(Arm::Register rLo, Arm::Register rHi);

        void pushFloat(Arm::SRegister s);
        void popFloat(Arm::SRegister s);

        void pushDouble(Arm::DRegister d);
        void popDouble(Arm::DRegister d);


        void loadLocalAddr(Arm::Register rd, quint32 milOffset);

        void loadArgAddr(Arm::Register rd, quint32 milOffset);

        void loadVarAddr(Arm::Register rd, quint32 milOffset);

        void scanBranchTargets(Vm::Procedure& proc);

        Arm::Label& getLabel(int milPc);

        void bindLabelIfNeeded(int milPc);

        void clearLabels();

        void emitCallStackAdj(quint32 argsSize, quint32 returnSize);

        bool setError(const QString& msg);

        void registerExternals();

        AstModel* d_mdl;
        Vm::Code2 d_code; // VmCode2 lowering (reversed arg order for ARM)
        Arm::Emitter d_emitter;
        Arm::FixupTracker d_fixups;
        ElfWriter d_elf;

        bool d_hasHwDiv;  // true for Cortex-A7+ (SDIV/UDIV available)
        bool d_emitDwarf; // true to generate DWARF debug sections
        bool d_useAapcs;  // true to generate AAPCS32-compatible calling convention

        DwarfEmitter* d_dwarf;

        quint32 d_localsSize; // Current proc's locals size (4-byte aligned)
        quint32 d_argsSize; // Current proc's args size (for inverted arg offsets)
        quint32 d_returnSize; // Current procs's return size (for AAPCS large-return fallback)

        QString d_error;
        ElfWriter::StandardSections d_sections;
        quint32 d_textSymIdx;
        quint32 d_dataSymIdx;
        quint32 d_rodataSymIdx;

        QMap<int,quint32> d_procTextOffsets; // proc index -> byte offset in .text
        QMap<int,quint32> d_procSymbols; // proc index -> ELF symbol index

        QMap<int,quint32> d_stringOffsets; // string index -> .rodata offset
        QMap<int,quint32> d_floatOffsets;  // float (r4) index -> .rodata offset
        QMap<int,quint32> d_doubleOffsets; // double (r8) index -> .rodata offset

        QMap<int,quint32> d_extProcSymbols; // proc index -> ELF symbol index

        QMap<int,quint32> d_vtableOffsets; //  vtable index -> .rodata byte offset

        QMap<int,quint32> d_templateOffsets; // template index -> .rodata byte offset

        quint32 getOrCreateExtSymbol(int procIdx);

        quint32 emitArgAlignment(Declaration* decl);

        quint32 getOrEmitVtable(int vtableIdx);
        void emitVtableFixupAt(Arm::Register destReg, quint32 offset, Type* t, const char* memData, quint32 memSize);
        void emitVtableFixups(Arm::Register destReg, const Vm::Template& tmpl);
        void initDataVtable(Type* t, quint32 baseOff);
        void initGlobalVarVtables(Declaration* module);

        quint32 getOrEmitTemplate(quint32 templateIdx);

        QMap<int,Arm::Label*> d_branchLabels; // target MIL PC -> Label
        QList<Arm::Label*> d_allLabels;  // for cleanup
    };
} // Arm
} // Mil

#endif // MILARMV7RENDERER_H
