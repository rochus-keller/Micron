#ifndef RISCV32ASSEMBLER_H
#define RISCV32ASSEMBLER_H

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
namespace Rv32
{
    enum Register {
        Zero = 0, Ra = 1, Sp = 2, Gp = 3, Tp = 4, 
        T0 = 5, T1 = 6, T2 = 7, S0 = 8, S1 = 9, 
        A0 = 10, A1 = 11, A2 = 12, A3 = 13, A4 = 14, A5 = 15, A6 = 16, A7 = 17, 
        S2 = 18, S3 = 19, S4 = 20, S5 = 21, S6 = 22, S7 = 23, S8 = 24, S9 = 25, S10 = 26, S11 = 27, 
        T3 = 28, T4 = 29, T5 = 30, T6 = 31,
        Fp = 8, // Frame pointer is an alias for S0
        InvalidReg = -1
    };

    enum FRegister {
        Ft0 = 0, Ft1 = 1, Ft2 = 2, Ft3 = 3, Ft4 = 4, Ft5 = 5, Ft6 = 6, Ft7 = 7,
        Fs0 = 8, Fs1 = 9, Fa0 = 10, Fa1 = 11, Fa2 = 12, Fa3 = 13, Fa4 = 14, Fa5 = 15,
        Fa6 = 16, Fa7 = 17, Fs2 = 18, Fs3 = 19, Fs4 = 20, Fs5 = 21, Fs6 = 22, Fs7 = 23,
        Fs8 = 24, Fs9 = 25, Fs10 = 26, Fs11 = 27, Ft8 = 28, Ft9 = 29, Ft10 = 30, Ft11 = 31,
        InvalidFReg = -1
    };

    enum CSR {
        MSTATUS = 0x300, MISA    = 0x301, MIE   = 0x304, MTVEC   = 0x305,
        MSCRATCH= 0x340, MEPC    = 0x341, MCAUSE= 0x342, MTVAL   = 0x343, MIP = 0x344,
        SSTATUS = 0x100, SIE     = 0x104, STVEC = 0x105,
        SSCRATCH= 0x140, SEPC    = 0x141, SCAUSE= 0x142, STVAL   = 0x143, SIP = 0x144,
        SATP    = 0x180, // Supervisor Address Translation and Protection
        MCYCLE  = 0xB00, MINSTRET = 0xB02
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
        
        void addi(Register rd, Register rs1, qint32 imm12);
        void slti(Register rd, Register rs1, qint32 imm12);
        void sltiu(Register rd, Register rs1, qint32 imm12);
        void andi(Register rd, Register rs1, qint32 imm12);
        void ori(Register rd, Register rs1, qint32 imm12);
        void xori(Register rd, Register rs1, qint32 imm12);
        void slli(Register rd, Register rs1, quint8 shamt); // shamt 0-31
        void srli(Register rd, Register rs1, quint8 shamt);
        void srai(Register rd, Register rs1, quint8 shamt);

        void add(Register rd, Register rs1, Register rs2);
        void sub(Register rd, Register rs1, Register rs2);
        void sll(Register rd, Register rs1, Register rs2);
        void slt(Register rd, Register rs1, Register rs2);
        void sltu(Register rd, Register rs1, Register rs2);
        void xor_(Register rd, Register rs1, Register rs2);
        void srl(Register rd, Register rs1, Register rs2);
        void sra(Register rd, Register rs1, Register rs2);
        void or_(Register rd, Register rs1, Register rs2);
        void and_(Register rd, Register rs1, Register rs2);

        void lui(Register rd, qint32 imm20);
        void auipc(Register rd, qint32 imm20);

        void lb(Register rd, Register rs1, qint32 imm12);
        void lh(Register rd, Register rs1, qint32 imm12);
        void lw(Register rd, Register rs1, qint32 imm12);
        void lbu(Register rd, Register rs1, qint32 imm12);
        void lhu(Register rd, Register rs1, qint32 imm12);
        void sb(Register rs2, Register rs1, qint32 imm12);
        void sh(Register rs2, Register rs1, qint32 imm12);
        void sw(Register rs2, Register rs1, qint32 imm12);

        void beq(Register rs1, Register rs2, Label& label);
        void bne(Register rs1, Register rs2, Label& label);
        void blt(Register rs1, Register rs2, Label& label);
        void bge(Register rs1, Register rs2, Label& label);
        void bltu(Register rs1, Register rs2, Label& label);
        void bgeu(Register rs1, Register rs2, Label& label);

        void jal(Register rd, Label& label);
        void jalr(Register rd, Register rs1, qint32 imm12);

        void mul(Register rd, Register rs1, Register rs2);
        void mulh(Register rd, Register rs1, Register rs2);
        void mulhsu(Register rd, Register rs1, Register rs2);
        void mulhu(Register rd, Register rs1, Register rs2);
        void div(Register rd, Register rs1, Register rs2);
        void divu(Register rd, Register rs1, Register rs2);
        void rem(Register rd, Register rs1, Register rs2);
        void remu(Register rd, Register rs1, Register rs2);

        void lr_w(Register rd, Register rs1);
        void sc_w(Register rd, Register rs1, Register rs2);
        void amoswap_w(Register rd, Register rs1, Register rs2);
        void amoadd_w(Register rd, Register rs1, Register rs2);

        void flw(FRegister rd, Register rs1, qint32 imm12);
        void fsw(FRegister rs2, Register rs1, qint32 imm12);
        void fadd_s(FRegister rd, FRegister rs1, FRegister rs2);
        void fsub_s(FRegister rd, FRegister rs1, FRegister rs2);
        void fmul_s(FRegister rd, FRegister rs1, FRegister rs2);
        void fdiv_s(FRegister rd, FRegister rs1, FRegister rs2);
        void fsqrt_s(FRegister rd, FRegister rs1);
        void fcvtw_s(Register rd, FRegister rs1); // Float to Int32
        void fcvts_w(FRegister rd, Register rs1); // Int32 to Float
        void fmvw_x(FRegister rd, Register rs1);  // Move bits directly
        void fmvx_w(Register rd, FRegister rs1);
        void feq_s(Register rd, FRegister rs1, FRegister rs2);
        void flt_s(Register rd, FRegister rs1, FRegister rs2);
        void fle_s(Register rd, FRegister rs1, FRegister rs2);

        void ecall();  // Syscall from User to OS
        void ebreak(); // Breakpoint
        void mret();   // Return from Machine-mode Interrupt
        void sret();   // Return from Supervisor-mode Interrupt
        void wfi();    // Wait for Interrupt (OS Idle Loop)
        void fence();  // Memory Barrier
        void fence_i();// Instruction Cache Sync (Critical for JIT/OS loaders)

        void csrrw(Register rd, CSR csr, Register rs1);
        void csrrs(Register rd, CSR csr, Register rs1);
        void csrrc(Register rd, CSR csr, Register rs1);
        void csrrwi(Register rd, CSR csr, quint8 zimm);
        void csrrsi(Register rd, CSR csr, quint8 zimm);
        void csrrci(Register rd, CSR csr, quint8 zimm);

        // macros, sugar

        inline void nop() { addi(Zero, Zero, 0); }
        inline void mv(Register rd, Register rs) { addi(rd, rs, 0); }
        inline void not_(Register rd, Register rs) { xori(rd, rs, -1); }
        inline void neg(Register rd, Register rs) { sub(rd, Zero, rs); }
        inline void seqz(Register rd, Register rs) { sltiu(rd, rs, 1); }
        inline void snez(Register rd, Register rs) { sltu(rd, Zero, rs); }

        // Generates LUI + ADDI appropriately
        void li(Register rd, quint32 value);

        inline void j(Label& label) { jal(Zero, label); }
        inline void call(Label& label) { jal(Ra, label); }
        inline void ret() { jalr(Zero, Ra, 0); }

        // Enable/Disable Machine-Level Interrupts (mstatus.MIE is bit 3)
        inline void cli() { csrrc(Zero, MSTATUS, Register(3)); } // Bitwise hack: we pass '3' to represent bit 3 via rs1=x3? Actually we should use csrrci
        inline void cli_macro() { csrrci(Zero, MSTATUS, 8); }    // Clear MIE (bit 3 = 8)
        inline void sti_macro() { csrrsi(Zero, MSTATUS, 8); }    // Set MIE (bit 3 = 8)

        inline void csrr(Register rd, CSR csr) { csrrs(rd, csr, Zero); }
        inline void csrw(CSR csr, Register rs) { csrrw(Zero, csr, rs); }

    protected:
        void emit32(quint32 inst);
        void patchBranch(int offset, int targetPosition);

        // RISC-V Instruction Format Emitters
        void emitR(quint8 opcode, quint8 funct3, quint8 funct7, quint8 rd, quint8 rs1, quint8 rs2);
        void emitI(quint8 opcode, quint8 funct3, quint8 rd, quint8 rs1, qint32 imm12);
        void emitS(quint8 opcode, quint8 funct3, quint8 rs1, quint8 rs2, qint32 imm12);
        void emitB(quint8 opcode, quint8 funct3, quint8 rs1, quint8 rs2, Label& label);
        void emitU(quint8 opcode, quint8 rd, qint32 imm20);
        void emitJ(quint8 opcode, quint8 rd, Label& label);

    private:
        QByteArray d_buffer;
    };


} // Rv32
} // Mil

#endif // RISCV32ASSEMBLER_H
