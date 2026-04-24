#ifndef MILRV32RENDERER_H
#define MILRV32RENDERER_H

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
#include <Micron/MilRv32Emitter.h>
#include <Micron/MilRv32Fixup.h>
#include <Micron/MilElfWriter.h>
#include <Micron/MilDwarfEmitter.h>
#include <QMap>

namespace Mil
{
namespace Rv32
{
    // Direct stack-based translation of MIL to RISC-V 32-bit machine code.
    // Every MIL stack operation maps to RV32 push/pop without register allocation.
    // All values flow through the hardware stack (SP-relative).
    //
    // Target ISA: RV32IMAF (ESP32-P4 compatible)
    //   - 32-bit instructions only (no C/Zc compressed extensions)
    //   - Hardware integer multiply/divide (M extension)
    //   - Single-precision hardware FPU (F extension, optional via setHasFloat)
    //   - Double-precision via soft-float (__mic$ intrinsic calls)
    //
    // Stack frame layout (analogous to ARMv7 old convention):
    //   High addresses
    //     [arguments]        FP + 8 + argOffset   (pushed by caller)
    //     [saved RA]         FP + 4
    //     [saved FP]         FP + 0
    //     [locals]           FP - localsSize ... FP - 4
    //     [eval stack]       SP (grows downward)
    //   Low addresses
    //
    // Register usage:
    //   t0-t2 (x5-x7):     scratch (operands, results)
    //   t3-t6 (x28-x31):   scratch (address materialization, etc.)
    //   s0/fp (x8):         frame pointer (FP)
    //   s1 (x9):            reserved scratch for renderer
    //   s2-s11 (x18-x27):   callee-saved (unused in dumb translator)
    //   a0-a7 (x10-x17):    argument/return registers (RISC-V calling convention)
    //   x0 (zero):          hardwired zero
    //   x1 (ra):            return address
    //   x2 (sp):            stack pointer
    //   x3 (gp):            global pointer (reserved)
    //   x4 (tp):            thread pointer (reserved)
    //   ft0-ft1 (f0-f1):    FP scratch registers
    //   fa0-fa7 (f10-f17):  FP argument/return (RISC-V F calling convention)

    class Renderer
    {
    public:
        Renderer(AstModel* mdl);
        ~Renderer();

        void setHasFloat(bool v) { d_hasFloat = v; }
        bool hasFloat() const { return d_hasFloat; }
        void setEmitDwarf(bool v) { d_emitDwarf = v; }
        bool emitDwarf() const { return d_emitDwarf; }
        void setUseRvAbi(bool v) { d_useRvAbi = v; }
        bool useRvAbi() const { return d_useRvAbi; }
        void setHardwareDivide(bool v) { d_hasHwDiv = v; }
        bool hardwareDivide() const { return d_hasHwDiv; }

        // Render a complete module to an ELF relocatable object.
        bool renderModule(Declaration* module);

        // Generate a standalone main.o that calls each module's $begin$ in order,
        // then exits.
        static bool generateMainObject(const QByteArrayList& moduleNames, const QString& outputPath, bool indirect = false);

        bool writeToFile(const QString& filename);

        QByteArray toByteArray();

        QString errorMessage() const { return d_error; }

        const Rv32::Emitter& emitter() const { return d_emitter; }
        const ElfWriter& elfWriter() const { return d_elf; }

    private:
        bool renderProcedure(Vm::Procedure& proc, int procIdx);

        void emitPrologue(quint32 localsSize);

        void emitEpilogue();

        int emitOp(Vm::Procedure& proc, int pc);

        // Load a 32-bit immediate into register rd using LUI+ADDI
        void loadImm32(Rv32::Register rd, quint32 value);

        // Load an absolute address into rd with relocations against symbolIdx
        void loadAddr(Rv32::Register rd, quint32 symbolIdx, qint32 addend = 0);

        void pushReg(Rv32::Register r);
        void popReg(Rv32::Register r);

        void pushRegPair(Rv32::Register rLo, Rv32::Register rHi);
        void popRegPair(Rv32::Register rLo, Rv32::Register rHi);

        void pushFloat(Rv32::FRegister f);
        void popFloat(Rv32::FRegister f);

        // Doubles are 8 bytes, stored as two 32-bit words on the integer stack
        void pushDouble_soft(Rv32::Register rLo, Rv32::Register rHi);
        void popDouble_soft(Rv32::Register rLo, Rv32::Register rHi);

        void loadLocalAddr(Rv32::Register rd, quint32 milOffset);

        void loadArgAddr(Rv32::Register rd, quint32 milOffset);

        void loadVarAddr(Rv32::Register rd, quint32 milOffset);

        void scanBranchTargets(Vm::Procedure& proc);

        Rv32::Label& getLabel(int milPc);

        void bindLabelIfNeeded(int milPc);

        void clearLabels();

        void emitCallStackAdj(quint32 argsSize, quint32 returnSize);

        bool setError(const QString& msg);

        void registerExternals();

        // Emit an AUIPC+JALR call pair with relocation to symbolIdx
        void emitCall(quint32 symbolIdx);

        // Call an external C function with proper calling convention handling.
        // In RV-ABI mode: loads args from stack into a0-a7, calls, pushes return value.
        // swapHalves=true for binary ops where stack has right operand on top.
        void emitExternCall(quint32 symbolIdx, quint32 argsSize, quint32 returnSize,
                            bool swapHalves = false);

        // Emit an indirect call via register
        void emitIndirectCall(Rv32::Register target);

        AstModel* d_mdl;
        Vm::Code2 d_code;
        Rv32::Emitter d_emitter;
        Rv32::FixupTracker d_fixups;
        ElfWriter d_elf;

        bool d_hasFloat;  // true for F extension (ESP32-P4); false = all float via soft-float
        bool d_emitDwarf; // true to generate DWARF debug sections
        bool d_useRvAbi;  // true for RISC-V standard ABI (a0-a7 for args), false for old stack-only
        bool d_hasHwDiv;  // true for M extension (hardware div/rem); false = external helpers

        DwarfEmitter* d_dwarf;

        quint32 d_localsSize;
        quint32 d_argsSize;
        quint32 d_returnSize;

        QString d_error;
        ElfWriter::StandardSections d_sections;
        quint32 d_textSymIdx;
        quint32 d_dataSymIdx;
        quint32 d_rodataSymIdx;
        quint32 d_globalsSymIdx;

        QMap<int,quint32> d_procTextOffsets;
        QMap<int,quint32> d_procSymbols;

        QMap<int,quint32> d_stringOffsets;
        QMap<int,quint32> d_floatOffsets;
        QMap<int,quint32> d_doubleOffsets;

        QMap<int,quint32> d_extProcSymbols;

        QMap<int,quint32> d_vtableOffsets;

        QMap<int,quint32> d_templateOffsets;

        quint32 getOrCreateExtSymbol(int procIdx);

        quint32 emitArgAlignment(Declaration* decl);

        quint32 getOrEmitVtable(int vtableIdx);
        void emitVtableFixupAt(Rv32::Register destReg, quint32 offset, Type* t, const char* memData, quint32 memSize);
        void emitVtableFixups(Rv32::Register destReg, const Vm::Template& tmpl);
        void initDataVtable(Type* t, quint32 baseOff);
        void initGlobalVarVtables(Declaration* module);
        void emitRuntimeVtableInit(Type* t, quint32 baseOff);
        void emitGlobalVarVtableInits(Declaration* module);

        quint32 getOrEmitTemplate(quint32 templateIdx);

        QMap<int,Rv32::Label*> d_branchLabels;
        QList<Rv32::Label*> d_allLabels;
    };
} // Rv32
} // Mil

#endif // MILRV32RENDERER_H
