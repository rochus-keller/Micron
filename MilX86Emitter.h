#ifndef MILX86EMITTER_H
#define MILX86EMITTER_H

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
#include <QList>
#include <QtGlobal>

namespace Mil
{
namespace X86
{
    // x86-32 GPR encoding (matches ModR/M register field)
    enum Register {
        EAX = 0, ECX = 1, EDX = 2, EBX = 3,
        ESP = 4, EBP = 5, ESI = 6, EDI = 7
    };

    // Condition codes for Jcc/SETcc (lower nibble of opcode)
    enum Condition {
        CC_O  = 0x0, CC_NO = 0x1,
        CC_B  = 0x2, CC_AE = 0x3, // unsigned below / above-or-equal
        CC_E  = 0x4, CC_NE = 0x5,
        CC_BE = 0x6, CC_A  = 0x7, // unsigned below-or-equal / above
        CC_S  = 0x8, CC_NS = 0x9,
        CC_P  = 0xA, CC_NP = 0xB,
        CC_L  = 0xC, CC_GE = 0xD, // signed less / greater-or-equal
        CC_LE = 0xE, CC_G  = 0xF  // signed less-or-equal / greater
    };

    // Forward branch label for intra-procedure jumps
    struct Label {
        int patchOffset; // offset within code buffer of the rel32 to patch (-1 if unbound)
        int boundOffset;  // offset in code where label is bound (-1 if not yet bound)
        QList<int> pendingPatches; // list of offsets needing patching when bound
        Label() : patchOffset(-1), boundOffset(-1) {}
    };

    // Low-level x86-32 instruction encoder.
    // Emits variable-length instructions into a byte buffer.
    // No register allocation, just encodes what the renderer asks for.
    class Emitter
    {
    public:
        Emitter();

        const QByteArray& code() const { return d_code; }
        quint32 currentPosition() const { return d_code.size(); }

        void reset();

        // Emit raw bytes
        void emitByte(quint8 b);
        void emitLE32(quint32 v);
        void emitLE16(quint16 v);

        // Patch a 32-bit value at a given offset
        void patchLE32(int offset, quint32 v);

        void mov_rr(Register dst, Register src);
        // MOV reg, imm32
        void mov_ri(Register dst, quint32 imm);
        // MOV reg, [base + disp32]
        void mov_rm(Register dst, Register base, qint32 disp);
        // MOV [base + disp32], reg
        void mov_mr(Register base, qint32 disp, Register src);
        // MOV [base + disp32], imm32
        void mov_mi(Register base, qint32 disp, quint32 imm);

        // MOVSX reg, byte [base + disp]
        void movsx_rb(Register dst, Register base, qint32 disp);
        // MOVSX reg, word [base + disp]
        void movsx_rw(Register dst, Register base, qint32 disp);
        // MOVZX reg, byte [base + disp]
        void movzx_rb(Register dst, Register base, qint32 disp);
        // MOVZX reg, word [base + disp]
        void movzx_rw(Register dst, Register base, qint32 disp);

        void movsx_rb_reg(Register dst, Register src);
        void movsx_rw_reg(Register dst, Register src);
        void movzx_rb_reg(Register dst, Register src);
        void movzx_rw_reg(Register dst, Register src);

        // MOV byte [base + disp], reg
        void mov_mr8(Register base, qint32 disp, Register src);
        // MOV word [base + disp], reg
        void mov_mr16(Register base, qint32 disp, Register src);

        // LEA reg, [base + disp32]
        void lea(Register dst, Register base, qint32 disp);

        void push_r(Register r);
        // PUSH imm32
        void push_i(quint32 imm);
        // PUSH [base + disp32]
        void push_m(Register base, qint32 disp);
        void pop_r(Register r);

        void xchg(Register a, Register b);

        // CDQ (sign-extend EAX into EDX:EAX)
        void cdq();

        void add_rr(Register dst, Register src);
        // ADD reg, imm32
        void add_ri(Register dst, qint32 imm);
        // ADD [base+disp], imm32
        void add_mi(Register base, qint32 disp, qint32 imm);

        void sub_rr(Register dst, Register src);
        // SUB reg, imm32
        void sub_ri(Register dst, qint32 imm);

        void adc_rr(Register dst, Register src);
        void sbb_rr(Register dst, Register src);

        void imul_rr(Register dst, Register src);
        void mul_r(Register src);
        // IMUL reg (signed multiply EDX:EAX = EAX * reg)
        void imul_r(Register src);

        // IDIV reg (signed divide EDX:EAX by reg; quotient=EAX, remainder=EDX)
        void idiv_r(Register src);
        void div_r(Register src);

        void neg_r(Register r);
        void not_r(Register r);

        void and_rr(Register dst, Register src);
        // AND reg, imm32
        void and_ri(Register dst, quint32 imm);
        void or_rr(Register dst, Register src);
        void xor_rr(Register dst, Register src);

        // SHL reg, CL
        void shl_cl(Register r);
        // SHR reg, CL
        void shr_cl(Register r);
        // SAR reg, CL
        void sar_cl(Register r);
        // SHL reg, imm8
        void shl_ri(Register r, quint8 count);
        // SHR reg, imm8
        void shr_ri(Register r, quint8 count);
        // SAR reg, imm8
        void sar_ri(Register r, quint8 count);

        // SHLD reg, reg, CL
        void shld_cl(Register dst, Register src);
        // SHRD reg, reg, CL
        void shrd_cl(Register dst, Register src);


        void cmp_rr(Register a, Register b);
        void cmp_ri(Register r, qint32 imm);
        void test_rr(Register a, Register b);

        void setcc(Condition cc, Register r);


        void jmp_rel32(qint32 offset);
        void jcc_rel32(Condition cc, qint32 offset);

        void jmp(Label& lbl);
        void jcc(Condition cc, Label& lbl);

        // Bind a label at current position
        void bind(Label& lbl);

        // CALL rel32 (near call, relative)
        quint32 call_rel32(qint32 offset);

        void call_r(Register r);

        void ret();

        void nop();

        void int80();

        // FLD dword [base + disp]
        void fld_s(Register base, qint32 disp);
        // FLD qword [base + disp]
        void fld_d(Register base, qint32 disp);
        // FSTP dword [base + disp]
        void fstp_s(Register base, qint32 disp);
        // FSTP qword [base + disp]
        void fstp_d(Register base, qint32 disp);
        // FILD dword [base + disp]
        void fild_s(Register base, qint32 disp);
        // FISTP dword [base + disp]
        void fistp_s(Register base, qint32 disp);

        void faddp();
        void fsubp();
        void fmulp();
        void fdivp();
        void fchs();
        void fabs_();
        void fcomip();
        void fstp_st0();

        void fxch();

        void fld_st0();

    private:
        void emitModRM_rr(quint8 reg, quint8 rm);
        void emitModRM_m(quint8 reg, Register base, qint32 disp);

        QByteArray d_code;
    };

} // X86
} // Mil

#endif // MILX86EMITTER_H
