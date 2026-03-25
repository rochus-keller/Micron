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

#include "MilX86Emitter.h"

using namespace Mil;
using namespace Mil::X86;

Emitter::Emitter()
{
}

void Emitter::reset()
{
    d_code.clear();
}

void Emitter::emitByte(quint8 b)
{
    d_code.append(char(b));
}

void Emitter::emitLE32(quint32 v)
{
    char buf[4];
    buf[0] = char(v & 0xFF);
    buf[1] = char((v >> 8) & 0xFF);
    buf[2] = char((v >> 16) & 0xFF);
    buf[3] = char((v >> 24) & 0xFF);
    d_code.append(buf, 4);
}

void Emitter::emitLE16(quint16 v)
{
    char buf[2];
    buf[0] = char(v & 0xFF);
    buf[1] = char((v >> 8) & 0xFF);
    d_code.append(buf, 2);
}

void Emitter::patchLE32(int offset, quint32 v)
{
    Q_ASSERT(offset >= 0 && offset + 4 <= d_code.size());
    char* p = d_code.data() + offset;
    p[0] = char(v & 0xFF);
    p[1] = char((v >> 8) & 0xFF);
    p[2] = char((v >> 16) & 0xFF);
    p[3] = char((v >> 24) & 0xFF);
}

void Emitter::emitModRM_rr(quint8 reg, quint8 rm)
{
    emitByte(0xC0 | ((reg & 7) << 3) | (rm & 7));
}

void Emitter::emitModRM_m(quint8 reg, Register base, qint32 disp)
{
    quint8 regField = (reg & 7) << 3;

    if (base == ESP) {
        // ESP as base requires SIB byte
        if (disp == 0) {
            emitByte(0x04 | regField); // mod=00, r/m=100 (SIB follows)
            emitByte(0x24);            // SIB: scale=0, index=ESP(none), base=ESP
        } else if (disp >= -128 && disp <= 127) {
            emitByte(0x44 | regField); // mod=01, r/m=100
            emitByte(0x24);            // SIB
            emitByte(quint8(disp));
        } else {
            emitByte(0x84 | regField); // mod=10, r/m=100
            emitByte(0x24);            // SIB
            emitLE32(quint32(disp));
        }
    } else if (base == EBP) {
        // EBP with mod=00 means [disp32], so always use mod=01 or mod=10
        if (disp >= -128 && disp <= 127) {
            emitByte(0x45 | regField); // mod=01, r/m=101 (EBP)
            emitByte(quint8(disp));
        } else {
            emitByte(0x85 | regField); // mod=10, r/m=101
            emitLE32(quint32(disp));
        }
    } else {
        if (disp == 0) {
            emitByte(0x00 | regField | (base & 7)); // mod=00
        } else if (disp >= -128 && disp <= 127) {
            emitByte(0x40 | regField | (base & 7)); // mod=01
            emitByte(quint8(disp));
        } else {
            emitByte(0x80 | regField | (base & 7)); // mod=10
            emitLE32(quint32(disp));
        }
    }
}

void Emitter::mov_rr(Register dst, Register src)
{
    emitByte(0x89);
    emitModRM_rr(src, dst);
}

void Emitter::mov_ri(Register dst, quint32 imm)
{
    emitByte(0xB8 + (dst & 7));
    emitLE32(imm);
}

void Emitter::mov_rm(Register dst, Register base, qint32 disp)
{
    emitByte(0x8B);
    emitModRM_m(dst, base, disp);
}

void Emitter::mov_mr(Register base, qint32 disp, Register src)
{
    emitByte(0x89);
    emitModRM_m(src, base, disp);
}

void Emitter::mov_mi(Register base, qint32 disp, quint32 imm)
{
    emitByte(0xC7);
    emitModRM_m(0, base, disp);
    emitLE32(imm);
}

void Emitter::movsx_rb(Register dst, Register base, qint32 disp)
{
    emitByte(0x0F); emitByte(0xBE);
    emitModRM_m(dst, base, disp);
}

void Emitter::movsx_rw(Register dst, Register base, qint32 disp)
{
    emitByte(0x0F); emitByte(0xBF);
    emitModRM_m(dst, base, disp);
}

void Emitter::movzx_rb(Register dst, Register base, qint32 disp)
{
    emitByte(0x0F); emitByte(0xB6);
    emitModRM_m(dst, base, disp);
}

void Emitter::movzx_rw(Register dst, Register base, qint32 disp)
{
    emitByte(0x0F); emitByte(0xB7);
    emitModRM_m(dst, base, disp);
}

void Emitter::movsx_rb_reg(Register dst, Register src)
{
    emitByte(0x0F); emitByte(0xBE);
    emitModRM_rr(dst, src);
}

void Emitter::movsx_rw_reg(Register dst, Register src)
{
    emitByte(0x0F); emitByte(0xBF);
    emitModRM_rr(dst, src);
}

void Emitter::movzx_rb_reg(Register dst, Register src)
{
    emitByte(0x0F); emitByte(0xB6);
    emitModRM_rr(dst, src);
}

void Emitter::movzx_rw_reg(Register dst, Register src)
{
    emitByte(0x0F); emitByte(0xB7);
    emitModRM_rr(dst, src);
}

void Emitter::mov_mr8(Register base, qint32 disp, Register src)
{
    emitByte(0x88);
    emitModRM_m(src, base, disp);
}

void Emitter::mov_mr16(Register base, qint32 disp, Register src)
{
    emitByte(0x66); // operand-size prefix
    emitByte(0x89);
    emitModRM_m(src, base, disp);
}

void Emitter::lea(Register dst, Register base, qint32 disp)
{
    emitByte(0x8D);
    emitModRM_m(dst, base, disp);
}

void Emitter::push_r(Register r)
{
    emitByte(0x50 + (r & 7));
}

void Emitter::push_i(quint32 imm)
{
    emitByte(0x68);
    emitLE32(imm);
}

void Emitter::push_m(Register base, qint32 disp)
{
    emitByte(0xFF);
    emitModRM_m(6, base, disp);
}

void Emitter::pop_r(Register r)
{
    emitByte(0x58 + (r & 7));
}

void Emitter::xchg(Register a, Register b)
{
    if (a == EAX)
        emitByte(0x90 + (b & 7));
    else if (b == EAX)
        emitByte(0x90 + (a & 7));
    else {
        emitByte(0x87);
        emitModRM_rr(a, b);
    }
}

void Emitter::cdq()
{
    emitByte(0x99);
}

void Emitter::add_rr(Register dst, Register src)
{
    emitByte(0x01);
    emitModRM_rr(src, dst);
}

void Emitter::add_ri(Register dst, qint32 imm)
{
    if (imm >= -128 && imm <= 127) {
        emitByte(0x83);
        emitModRM_rr(0, dst);
        emitByte(quint8(imm));
    } else if (dst == EAX) {
        emitByte(0x05);
        emitLE32(quint32(imm));
    } else {
        emitByte(0x81);
        emitModRM_rr(0, dst);
        emitLE32(quint32(imm));
    }
}

void Emitter::add_mi(Register base, qint32 disp, qint32 imm)
{
    if (imm >= -128 && imm <= 127) {
        emitByte(0x83);
        emitModRM_m(0, base, disp);
        emitByte(quint8(imm));
    } else {
        emitByte(0x81);
        emitModRM_m(0, base, disp);
        emitLE32(quint32(imm));
    }
}

void Emitter::sub_rr(Register dst, Register src)
{
    emitByte(0x29);
    emitModRM_rr(src, dst);
}

void Emitter::sub_ri(Register dst, qint32 imm)
{
    if (imm >= -128 && imm <= 127) {
        emitByte(0x83);
        emitModRM_rr(5, dst);
        emitByte(quint8(imm));
    } else if (dst == EAX) {
        emitByte(0x2D);
        emitLE32(quint32(imm));
    } else {
        emitByte(0x81);
        emitModRM_rr(5, dst);
        emitLE32(quint32(imm));
    }
}

void Emitter::adc_rr(Register dst, Register src)
{
    emitByte(0x11);
    emitModRM_rr(src, dst);
}

void Emitter::sbb_rr(Register dst, Register src)
{
    emitByte(0x19);
    emitModRM_rr(src, dst);
}

void Emitter::imul_rr(Register dst, Register src)
{
    emitByte(0x0F); emitByte(0xAF);
    emitModRM_rr(dst, src);
}

void Emitter::mul_r(Register src)
{
    emitByte(0xF7);
    emitModRM_rr(4, src);
}

void Emitter::imul_r(Register src)
{
    emitByte(0xF7);
    emitModRM_rr(5, src);
}

void Emitter::idiv_r(Register src)
{
    emitByte(0xF7);
    emitModRM_rr(7, src);
}

void Emitter::div_r(Register src)
{
    emitByte(0xF7);
    emitModRM_rr(6, src);
}

void Emitter::neg_r(Register r)
{
    emitByte(0xF7);
    emitModRM_rr(3, r);
}

void Emitter::not_r(Register r)
{
    emitByte(0xF7);
    emitModRM_rr(2, r);
}

void Emitter::and_rr(Register dst, Register src)
{
    emitByte(0x21);
    emitModRM_rr(src, dst);
}

void Emitter::and_ri(Register dst, quint32 imm)
{
    if (dst == EAX) {
        emitByte(0x25);
        emitLE32(imm);
    } else {
        emitByte(0x81);
        emitModRM_rr(4, dst);
        emitLE32(imm);
    }
}

void Emitter::or_rr(Register dst, Register src)
{
    emitByte(0x09);
    emitModRM_rr(src, dst);
}

void Emitter::xor_rr(Register dst, Register src)
{
    emitByte(0x31);
    emitModRM_rr(src, dst);
}

void Emitter::shl_cl(Register r)
{
    emitByte(0xD3);
    emitModRM_rr(4, r);
}

void Emitter::shr_cl(Register r)
{
    emitByte(0xD3);
    emitModRM_rr(5, r);
}

void Emitter::sar_cl(Register r)
{
    emitByte(0xD3);
    emitModRM_rr(7, r);
}

void Emitter::shl_ri(Register r, quint8 count)
{
    if (count == 1) {
        emitByte(0xD1);
        emitModRM_rr(4, r);
    } else {
        emitByte(0xC1);
        emitModRM_rr(4, r);
        emitByte(count);
    }
}

void Emitter::shr_ri(Register r, quint8 count)
{
    if (count == 1) {
        emitByte(0xD1);
        emitModRM_rr(5, r);
    } else {
        emitByte(0xC1);
        emitModRM_rr(5, r);
        emitByte(count);
    }
}

void Emitter::sar_ri(Register r, quint8 count)
{
    if (count == 1) {
        emitByte(0xD1);
        emitModRM_rr(7, r);
    } else {
        emitByte(0xC1);
        emitModRM_rr(7, r);
        emitByte(count);
    }
}

void Emitter::shld_cl(Register dst, Register src)
{
    emitByte(0x0F); emitByte(0xA5);
    emitModRM_rr(src, dst);
}

void Emitter::shrd_cl(Register dst, Register src)
{
    emitByte(0x0F); emitByte(0xAD);
    emitModRM_rr(src, dst);
}

void Emitter::cmp_rr(Register a, Register b)
{
    emitByte(0x39);
    emitModRM_rr(b, a);
}

void Emitter::cmp_ri(Register r, qint32 imm)
{
    if (imm >= -128 && imm <= 127) {
        emitByte(0x83);
        emitModRM_rr(7, r);
        emitByte(quint8(imm));
    } else if (r == EAX) {
        emitByte(0x3D);
        emitLE32(quint32(imm));
    } else {
        emitByte(0x81);
        emitModRM_rr(7, r);
        emitLE32(quint32(imm));
    }
}

void Emitter::test_rr(Register a, Register b)
{
    emitByte(0x85);
    emitModRM_rr(b, a);
}

void Emitter::setcc(Condition cc, Register r)
{
    emitByte(0x0F);
    emitByte(0x90 + (cc & 0xF));
    emitModRM_rr(0, r);
}

void Emitter::jmp_rel32(qint32 offset)
{
    emitByte(0xE9);
    emitLE32(quint32(offset));
}

void Emitter::jcc_rel32(Condition cc, qint32 offset)
{
    emitByte(0x0F);
    emitByte(0x80 + (cc & 0xF));
    emitLE32(quint32(offset));
}

void Emitter::jmp(Label& lbl)
{
    if (lbl.boundOffset >= 0) {
        // Backward jump target is known
        qint32 rel = lbl.boundOffset - (currentPosition() + 5);
        jmp_rel32(rel);
    } else {
        // Forward jump: emit placeholder 0, record for patching
        emitByte(0xE9);
        lbl.pendingPatches.append(currentPosition());
        emitLE32(0);
    }
}

void Emitter::jcc(Condition cc, Label& lbl)
{
    if (lbl.boundOffset >= 0) {
        qint32 rel = lbl.boundOffset - (currentPosition() + 6);
        jcc_rel32(cc, rel);
    } else {
        emitByte(0x0F);
        emitByte(0x80 + (cc & 0xF));
        lbl.pendingPatches.append(currentPosition());
        emitLE32(0);
    }
}

void Emitter::bind(Label& lbl)
{
    lbl.boundOffset = currentPosition();
    // Resolve all pending forward patches
    for (int i = 0; i < lbl.pendingPatches.size(); i++) {
        int patchOff = lbl.pendingPatches[i];
        qint32 rel = lbl.boundOffset - (patchOff + 4); // rel = target - next_instruction
        patchLE32(patchOff, quint32(rel));
    }
    lbl.pendingPatches.clear();
}

quint32 Emitter::call_rel32(qint32 offset)
{
    emitByte(0xE8);
    quint32 rel32Offset = currentPosition();
    emitLE32(quint32(offset));
    return rel32Offset;
}

void Emitter::call_r(Register r)
{
    emitByte(0xFF);
    emitModRM_rr(2, r);
}

void Emitter::ret()
{
    emitByte(0xC3);
}

void Emitter::nop()
{
    emitByte(0x90);
}

void Emitter::int80()
{
    emitByte(0xCD);
    emitByte(0x80);
}

void Emitter::fld_s(Register base, qint32 disp)
{
    emitByte(0xD9);
    emitModRM_m(0, base, disp);
}

void Emitter::fld_d(Register base, qint32 disp)
{
    emitByte(0xDD);
    emitModRM_m(0, base, disp);
}

void Emitter::fstp_s(Register base, qint32 disp)
{
    emitByte(0xD9);
    emitModRM_m(3, base, disp);
}

void Emitter::fstp_d(Register base, qint32 disp)
{
    emitByte(0xDD);
    emitModRM_m(3, base, disp);
}

void Emitter::fild_s(Register base, qint32 disp)
{
    emitByte(0xDB);
    emitModRM_m(0, base, disp);
}

void Emitter::fistp_s(Register base, qint32 disp)
{
    emitByte(0xDB);
    emitModRM_m(3, base, disp);
}

void Emitter::fisttp_s(Register base, qint32 disp)
{
    // FISTTP dword [base + disp]: 0xDB /1 (SSE3 truncating store)
    emitByte(0xDB);
    emitModRM_m(1, base, disp);
}

void Emitter::faddp()
{
    emitByte(0xDE); emitByte(0xC1);
}

void Emitter::fsubp()
{
    emitByte(0xDE); emitByte(0xE9);
}

void Emitter::fmulp()
{
    emitByte(0xDE); emitByte(0xC9);
}

void Emitter::fdivp()
{
    emitByte(0xDE); emitByte(0xF9);
}

void Emitter::fchs()
{
    emitByte(0xD9); emitByte(0xE0);
}

void Emitter::fabs_()
{
    emitByte(0xD9); emitByte(0xE1);
}

void Emitter::fcomip()
{
    emitByte(0xDF); emitByte(0xF1);
}

void Emitter::fstp_st0()
{
    emitByte(0xDD); emitByte(0xD8);
}

void Emitter::fxch()
{
    emitByte(0xD9); emitByte(0xC9);
}

void Emitter::fld_st0()
{
    emitByte(0xD9); emitByte(0xC0);
}
