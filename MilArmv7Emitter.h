#ifndef ARMV7ASSEMBLER_H
#define ARMV7ASSEMBLER_H

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


#include <QByteArray>
#include <QVector>
#include <QtGlobal>

namespace Mil
{
namespace Arm
{
    enum Opcode {
        OP_AND = 0x0, // Logical AND
        OP_EOR = 0x1, // Logical Exclusive OR
        OP_SUB = 0x2, // Subtract
        OP_RSB = 0x3, // Reverse Subtract
        OP_ADD = 0x4,
        OP_ADC = 0x5, // Add with Carry
        OP_SBC = 0x6, // Subtract with Carry
        OP_RSC = 0x7, // Reverse Subtract with Carry
        OP_TST = 0x8, // Test
        OP_TEQ = 0x9, // Test Equivalence
        OP_CMP = 0xA, // Compare
        OP_CMN = 0xB, // Compare Negated
        OP_ORR = 0xC, // Logical OR
        OP_MOV = 0xD,
        OP_BIC = 0xE, // Bit Clear
        OP_MVN = 0xF  // Move Not
    };

    enum Condition {
        EQ = 0x0, // Equal
        NE = 0x1, // Not equal
        CS = 0x2, // Carry set, Unsigned higher or same
        CC = 0x3, // Carry clear, Unsigned lower
        MI = 0x4, // Minus, Negative
        PL = 0x5, // Plus, Positive or zero
        VS = 0x6, // Overflow
        VC = 0x7, // No overflow
        HI = 0x8, // Unsigned higher
        LS = 0x9, // Unsigned lower or same
        GE = 0xA, // Signed greater than or equal
        LT = 0xB, // Signed less than
        GT = 0xC, // Signed greater than
        LE = 0xD, // Signed less than or equal
        AL = 0xE, // Always (unconditional)
        NV = 0xF  // Unpredictable
    };

    enum Register {
        R0 = 0, R1 = 1, R2 = 2, R3 = 3, R4 = 4, R5 = 5, R6 = 6, R7 = 7,
        R8 = 8, R9 = 9, R10 = 10, R11 = 11, R12 = 12,
        SP = 13, LR = 14, PC = 15, InvalidReg = -1
    };

    enum SRegister { S0=0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13,
                     S14, S15, S16, S17, S18, S19, S20, S21, S22, S23, S24, S25, S26, S27, S28,
                     S29, S30, S31=31, InvalidSReg = -1 };

    enum DRegister { D0=0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13,
                     D14, D15=15, InvalidDReg = -1 };

    enum ShiftType { LSL = 0, LSR = 1, ASR = 2, ROR = 3 };

    enum StatusRegister { CPSR = 0, SPSR = 1 };
    enum Coprocessor { p10=10, p11=11, p14=14, p15=15 };
    
    enum InterruptMask { 
        FIQ_Bit = 1, 
        IRQ_Bit = 2,
        AsyncAbort_Bit = 4 
    };

    enum BarrierOption {
        SY = 15, // Full system
        ST = 14, // Store only
        ISH = 11 // Inner shareable
    };

    struct Shift {
        ShiftType type;
        quint8 amount; // 0-31
        Register rs;   // If shift amount is in a register
        bool isRegShift;

        explicit Shift(ShiftType t, quint8 amt) : type(t), amount(amt), rs(InvalidReg), isRegShift(false) {}
        explicit Shift(ShiftType t, Register r) : type(t), amount(0), rs(r), isRegShift(true) {}
    };

    class Operand2 {
    public:
        enum Type { Immediate, Reg, ShiftedReg };

        explicit Operand2(quint32 imm);
        explicit Operand2(Register rm);
        Operand2(Register rm, const Shift& shift);

        Type type() const { return d_type; }
        quint32 immediate() const { return d_imm; }
        Register rm() const { return d_rm; }
        const Shift& shift() const { return d_shift; }

        static bool isValidImmediate(quint32 val, quint32* encodedOut = 0);

    private:
        Type d_type;
        quint32 d_imm;
        Register d_rm;
        Shift d_shift;
    };

    class MemOp {
    public:
        enum AddrMode { Offset, PreIndexed, PostIndexed };

        explicit MemOp(Register rn, qint32 offset = 0, AddrMode mode = Offset);
        MemOp(Register rn, Register rm, AddrMode mode = Offset);
        MemOp(Register rn, Register rm, const Shift& shift, AddrMode mode = Offset);

        Register rn() const { return d_rn; }
        Register rm() const { return d_rm; }
        qint32 offset() const { return d_offset; }
        bool isRegOffset() const { return d_isRegOffset; }
        AddrMode mode() const { return d_mode; }
        const Shift& shift() const { return d_shift; }

    private:
        Register d_rn; Register d_rm;
        qint32 d_offset; bool d_isRegOffset;
        AddrMode d_mode; Shift d_shift;
    };

    class Label {
    public:
        Label() : d_bound(false), d_position(-1) {}
        bool isBound() const { return d_bound; }
        int position() const { return d_position; }
        void bindTo(int pos) { d_bound = true; d_position = pos; }
        void addUnresolvedUse(int codeOffset) { d_unresolvedUses.append(codeOffset); }
        const QVector<int>& unresolvedUses() const { return d_unresolvedUses; }
    private:
        bool d_bound; int d_position; QVector<int> d_unresolvedUses;
    };

    class Emitter {
    public:
        Emitter();
        virtual ~Emitter() {}

        const QByteArray& machineCode() const { return d_buffer; }
        int currentPosition() const { return d_buffer.size(); }
        void bind(Label& label);

        void patchWord(int offset, quint32 word);

        void emitWord(quint32 word) { emit32(word); }

        void and_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void eor_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void sub_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void rsb_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void add_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void adc_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void sbc_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void rsc_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void tst_(Condition cond, Register rn, const Operand2& op2);
        void teq_(Condition cond, Register rn, const Operand2& op2);
        void cmp_(Condition cond, Register rn, const Operand2& op2);
        void cmn_(Condition cond, Register rn, const Operand2& op2);
        void orr_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void mov_(Condition cond, bool s, Register rd, const Operand2& op2);
        void bic_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2);
        void mvn_(Condition cond, bool s, Register rd, const Operand2& op2);

        void mul_(Condition cond, bool s, Register rd, Register rm, Register rs);
        void mla_(Condition cond, bool s, Register rd, Register rm, Register rs, Register rn);
        void umull_(Condition cond, bool s, Register rdLo, Register rdHi, Register rm, Register rs);
        void smull_(Condition cond, bool s, Register rdLo, Register rdHi, Register rm, Register rs);
        void sdiv_(Condition cond, Register rd, Register rn, Register rm);
        void udiv_(Condition cond, Register rd, Register rn, Register rm);

        void b_(Condition cond, Label& label);
        void bl_(Condition cond, Label& label);
        void bx_(Condition cond, Register rm);
        void blx_(Condition cond, Register rm);

        void ldr_(Condition cond, Register rd, const MemOp& op);
        void str_(Condition cond, Register rd, const MemOp& op);
        void ldrb_(Condition cond, Register rd, const MemOp& op);
        void strb_(Condition cond, Register rd, const MemOp& op);
        
        void ldrh_(Condition cond, Register rd, const MemOp& op);
        void strh_(Condition cond, Register rd, const MemOp& op);
        void ldrsb_(Condition cond, Register rd, const MemOp& op); // i1 sign extension
        void ldrsh_(Condition cond, Register rd, const MemOp& op); // i2 sign extension
        void ldrd_(Condition cond, Register rdLo, Register rdHi, const MemOp& op); // i8/u8
        void strd_(Condition cond, Register rdLo, Register rdHi, const MemOp& op); // i8/u8

        void ldmia_(Condition cond, Register rn, bool writeback, quint16 regList);
        void stmdb_(Condition cond, Register rn, bool writeback, quint16 regList);

        void movw_(Condition cond, Register rd, quint16 imm16); // Move wide (low 16 bits)
        void movt_(Condition cond, Register rd, quint16 imm16); // Move top (high 16 bits)

        void sxtb_(Condition cond, Register rd, Register rm); // Sign-extend byte
        void sxth_(Condition cond, Register rd, Register rm); // Sign-extend halfword
        void uxtb_(Condition cond, Register rd, Register rm); // Zero-extend byte
        void uxth_(Condition cond, Register rd, Register rm); // Zero-extend halfword

        // VFPv3
        void vadds(Condition cond, SRegister sd, SRegister sn, SRegister sm);
        void vaddd(Condition cond, DRegister dd, DRegister dn, DRegister dm);
        void vsubs(Condition cond, SRegister sd, SRegister sn, SRegister sm);
        void vsubd(Condition cond, DRegister dd, DRegister dn, DRegister dm);
        void vmuls(Condition cond, SRegister sd, SRegister sn, SRegister sm);
        void vmuld(Condition cond, DRegister dd, DRegister dn, DRegister dm);
        void vdivs(Condition cond, SRegister sd, SRegister sn, SRegister sm);
        void vdivd(Condition cond, DRegister dd, DRegister dn, DRegister dm);
        void vnegs(Condition cond, SRegister sd, SRegister sm);              // Negate f32
        void vnegd(Condition cond, DRegister dd, DRegister dm);              // Negate f64
        void vcvtds(Condition cond, DRegister dd, SRegister sm);             // f32 -> f64
        void vcvtsd(Condition cond, SRegister sd, DRegister dm);             // f64 -> f32
        void vcvts_i32(Condition cond, SRegister sd, SRegister sm);          // f32 -> int32 (truncate)
        void vcvti32_s(Condition cond, SRegister sd, SRegister sm);          // int32 -> f32
        void vcvtd_i32(Condition cond, SRegister sd, DRegister dm);          // f64 -> int32 (truncate)
        void vcvti32_d(Condition cond, DRegister dd, SRegister sm);          // int32 -> f64
        void vcmps(Condition cond, SRegister sd, SRegister sm);              // Compare f32
        void vcmpd(Condition cond, DRegister dd, DRegister dm);              // Compare f64
        void vmrs_apsr(Condition cond);                                      // Move FPU flags to CPU flags
        void vldrs(Condition cond, SRegister sd, Register rn, qint32 offset); // Load f32
        void vstrs(Condition cond, SRegister sd, Register rn, qint32 offset); // Store f32
        void vldrd(Condition cond, DRegister dd, Register rn, qint32 offset); // Load f64
        void vstrd(Condition cond, DRegister dd, Register rn, qint32 offset); // Store f64
        void vmov_s_r(Condition cond, SRegister sn, Register rt);            // GPR -> S-reg
        void vmov_r_s(Condition cond, Register rt, SRegister sn);            // S-reg -> GPR
        void vmov_d_rr(Condition cond, DRegister dm, Register rtLo, Register rtHi); // 2xGPR -> D-reg
        void vmov_rr_d(Condition cond, Register rtLo, Register rtHi, DRegister dm); // D-reg -> 2xGPR

        void nop_();
        void svc_(Condition cond, quint32 imm24); // Supervisor Call (Syscall)
        
        void cpsie_(quint8 flags); // Enable Interrupts
        void cpsid_(quint8 flags); // Disable Interrupts
        
        void mrs_(Condition cond, Register rd, StatusRegister sr);
        void msr_(Condition cond, StatusRegister sr, const Operand2& op2);
        
        void mcr_(Condition cond, Coprocessor coproc, quint8 opc1, Register rt, quint8 crn, quint8 crm, quint8 opc2);
        void mrc_(Condition cond, Coprocessor coproc, quint8 opc1, Register rt, quint8 crn, quint8 crm, quint8 opc2);
        
        void dmb_(BarrierOption option);
        void dsb_(BarrierOption option);
        void isb_(BarrierOption option);

        void ldrex_(Condition cond, Register rt, Register rn);
        void strex_(Condition cond, Register rd, Register rt, Register rn);
        void clrex_(); // Unconditional (encoding uses 0xF prefix)

        ///////////////////////////
        // Sugar:

        inline void add(Register rd, Register rn, const Operand2& op2) { add_(AL, false, rd, rn, op2); }
        inline void adds(Register rd, Register rn, const Operand2& op2) { add_(AL, true, rd, rn, op2); }
        inline void sub(Register rd, Register rn, const Operand2& op2) { sub_(AL, false, rd, rn, op2); }
        inline void mov(Register rd, const Operand2& op2) { mov_(AL, false, rd, op2); }
        inline void cmp(Register rn, const Operand2& op2) { cmp_(AL, rn, op2); }
        inline void and_(Register rd, Register rn, const Operand2& op2) { Emitter::and_(AL, false, rd, rn, op2); }
        inline void eor(Register rd, Register rn, const Operand2& op2) { eor_(AL, false, rd, rn, op2); }
        inline void orr(Register rd, Register rn, const Operand2& op2) { orr_(AL, false, rd, rn, op2); }

        inline void movw(Register rd, quint16 imm16) { movw_(AL, rd, imm16); }
        inline void movt(Register rd, quint16 imm16) { movt_(AL, rd, imm16); }
        void movImm32(Register rd, quint32 imm32); // MOVW+MOVT pair

        inline void sxtb(Register rd, Register rm) { sxtb_(AL, rd, rm); }
        inline void sxth(Register rd, Register rm) { sxth_(AL, rd, rm); }
        inline void uxtb(Register rd, Register rm) { uxtb_(AL, rd, rm); }
        inline void uxth(Register rd, Register rm) { uxth_(AL, rd, rm); }

        inline void lsl(Register rd, Register rm, quint8 amt) { mov_(AL, false, rd, Operand2(rm, Shift(LSL, amt))); }
        inline void lsr(Register rd, Register rm, quint8 amt) { mov_(AL, false, rd, Operand2(rm, Shift(LSR, amt))); }
        inline void asr(Register rd, Register rm, quint8 amt) { mov_(AL, false, rd, Operand2(rm, Shift(ASR, amt))); }

        inline void ldr(Register rd, const MemOp& op) { ldr_(AL, rd, op); }       // 32-bit u/i
        inline void ldrh(Register rd, const MemOp& op) { ldrh_(AL, rd, op); }     // 16-bit u
        inline void ldrsh(Register rd, const MemOp& op) { ldrsh_(AL, rd, op); }   // 16-bit i (sign extend)
        inline void ldrb(Register rd, const MemOp& op) { ldrb_(AL, rd, op); }     // 8-bit u
        inline void ldrsb(Register rd, const MemOp& op) { ldrsb_(AL, rd, op); }   // 8-bit i (sign extend)
        inline void str(Register rd, const MemOp& op) { str_(AL, rd, op); }
        inline void strh(Register rd, const MemOp& op) { strh_(AL, rd, op); }
        inline void strb(Register rd, const MemOp& op) { strb_(AL, rd, op); }

        inline void push(quint16 regList) { stmdb_(AL, SP, true, regList); }
        inline void pop(quint16 regList)  { ldmia_(AL, SP, true, regList); }

        inline void nop() { nop_(); }
        inline void cli() { cpsid_(IRQ_Bit | FIQ_Bit); } // Disable both IRQ and FIQ
        inline void sti() { cpsie_(IRQ_Bit | FIQ_Bit); } // Enable both

        inline void getreg_cpsr(Register rd) { mrs_(AL, rd, CPSR); }
        inline void putreg_cpsr(Register rm) { msr_(AL, CPSR, Operand2(rm)); }

        inline void dmb() { dmb_(SY); }
        inline void dsb() { dsb_(SY); }
        inline void isb() { isb_(SY); }

    protected:
        void emit32(quint32 inst);
        void patchBranch(int offset, int targetPosition);

        void emitDataProcessing(Condition cond, int op, bool s, Register rn, Register rd, const Operand2& op2);
        void emitLoadStore(Condition cond, bool load, bool byte, Register rd, const MemOp& op);
        void emitExtraLoadStore(Condition cond, int op1, int op2, Register rd, const MemOp& op);
        void emitBranch(Condition cond, bool link, Label& label);
        void emitVfpTransfer(Condition cond, quint32 opcode, quint32 regBits);


    private:
        QByteArray d_buffer;
    };

}
}

#endif // ARMV7ASSEMBLER_H
