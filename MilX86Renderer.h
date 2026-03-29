#ifndef MILX86RENDERER_H
#define MILX86RENDERER_H

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
#include <Micron/MilX86Emitter.h>
#include <Micron/MilElfWriter.h>
#include <QMap>

namespace Mil
{
namespace X86
{
    // Direct stack-based translation of MIL VmCode to x86-32 (i386).
    // Every MIL stack operation maps to x86 push/pop without register allocation.
    // All values flow through the hardware stack (ESP-relative).
    //
    // Stack frame layout (cdecl):
    //   High addresses
    //     [arguments]        EBP + 8 + argOffset   (pushed by caller)
    //     [saved EIP]        EBP + 4               (return address, pushed by CALL)
    //     [saved EBP]        EBP + 0
    //     [locals]           EBP - localsSize ... EBP - 4
    //     [eval stack]       ESP (grows downward)
    //   Low addresses
    //
    // Register usage:
    //   EAX, ECX, EDX: scratch (operands, results, caller-saved)
    //   EBX, ESI, EDI: callee-saved (unused in dumb translator)
    //   EBP: frame pointer
    //   ESP: stack pointer
    class Renderer
    {
    public:
        Renderer(AstModel* mdl);
        ~Renderer();

        // Configuration
        void setCdeclReturns(bool v) { d_cdeclReturns = v; }

        // Returns via register for values <= 8 bytes
        bool cdeclReturns() const { return d_cdeclReturns; }

        // Render a complete module to an ELF relocatable object.
        bool renderModule(Declaration* module);

        // Write the generated ELF to a file.
        bool writeToFile(const QString& filename);

        // Write the generated ELF to a byte array.
        QByteArray toByteArray();

        // Generate a standalone object file that calls each module's init procedure
        // in the given order, then exits.
        static bool generateMainObject(const QByteArrayList &moduleNames, const QString& outputPath, bool indirect = false);

        QString errorMessage() const { return d_error; }

        const X86::Emitter& emitter() const { return d_emitter; }
        const ElfWriter& elfWriter() const { return d_elf; }

    private:
        bool renderProcedure(Vm::Procedure& proc, int procIdx);
        void emitPrologue(quint32 localsSize);
        void emitEpilogue();
        int emitOp(Vm::Procedure& proc, int pc);

        // 32-bit value to/from eval stack
        void pushReg(X86::Register r);
        void popReg(X86::Register r);

        // 64-bit value (low word first on stack, i.e. [lo at SP, hi at SP+4])
        void pushRegPair(X86::Register rLo, X86::Register rHi);
        void popRegPair(X86::Register rLo, X86::Register rHi);

        void loadLocalAddr(X86::Register rd, quint32 milOffset);

        void loadArgAddr(X86::Register rd, quint32 milOffset);

        void loadVarAddr(X86::Register rd, quint32 milOffset);

        void scanBranchTargets(Vm::Procedure& proc);
        X86::Label& getLabel(int milPc);
        void bindLabelIfNeeded(int milPc);
        void clearLabels();

        // Stack adjustment after call, copy return value, adjust SP
        void emitCallStackAdj(quint32 argsSize, quint32 returnSize);

        // Compute contiguous "pushed size" and rearrange stack to match aligned layout
        quint32 emitArgAlignment(Declaration* decl);

        bool setError(const QString& msg);
        void registerExternals();
        quint32 getOrCreateExtSymbol(int procIdx);
        quint32 getOrEmitVtable(int vtableIdx);
        void emitVtableFixupAt(X86::Register destReg, quint32 offset, Type* t,
                               const char* memData, quint32 memSize);
        void emitVtableFixups(X86::Register destReg, const Vm::Template& tmpl);
        void initDataVtable(Type* t, quint32 baseOff);
        void initGlobalVarVtables(Declaration* module);

        AstModel* d_mdl;
        Vm::Code2 d_code;
        X86::Emitter d_emitter;
        ElfWriter d_elf;

        bool d_cdeclReturns;

        quint32 d_localsSize;
        quint32 d_argsSize;

        QString d_error;
        ElfWriter::StandardSections d_sections;
        quint32 d_textSymIdx;
        quint32 d_dataSymIdx;
        quint32 d_rodataSymIdx;

        QMap<int,quint32> d_procTextOffsets;
        QMap<int,quint32> d_procSymbols;
        QMap<int,quint32> d_stringOffsets;
        QMap<int,quint32> d_floatOffsets;
        QMap<int,quint32> d_doubleOffsets;
        QMap<int,quint32> d_extProcSymbols;
        QMap<int,quint32> d_vtableOffsets;
        QMap<int,quint32> d_templateOffsets;
        QMap<Type*,int> d_typeToVtableIdx;

        QMap<int,X86::Label*> d_branchLabels;
        QList<X86::Label*> d_allLabels;
    };
} // X86
} // Mil

#endif // MILX86RENDERER_H
