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

#include "MilArmv7Emitter.h"
using namespace Mil;
using namespace Arm;


Operand2::Operand2(quint32 imm) : d_type(Immediate), d_imm(imm), d_rm(InvalidReg), d_shift(LSL, 0) {
    Q_ASSERT_X(isValidImmediate(imm), "Operand2", "Immediate cannot be encoded in ARMv7 (8-bit rotated by even number)");
}

Operand2::Operand2(Register rm) : d_type(Reg), d_imm(0), d_rm(rm), d_shift(LSL, 0) {
}

Operand2::Operand2(Register rm, const Shift& shift) : d_type(ShiftedReg), d_imm(0), d_rm(rm), d_shift(shift) {
}

// ARMv7 Immediate Encoding: An 8-bit value rotated right by an EVEN number of bits (0 to 30)
bool Operand2::isValidImmediate(quint32 val, quint32* encodedOut) {
    if (val <= 0xFF) {
        if (encodedOut) *encodedOut = val; // Rotation is 0
        return true;
    }

    // Try to find a valid rotation
    for (int rot = 2; rot < 32; rot += 2) {
        // Rotate left to see if it fits in 8 bits
        quint32 rotated = (val << rot) | (val >> (32 - rot));
        if (rotated <= 0xFF) {
            if (encodedOut) {
                *encodedOut = ((rot / 2) << 8) | rotated;
            }
            return true;
        }
    }
    return false;
}

MemOp::MemOp(Register rn, qint32 offset, AddrMode mode)
    : d_rn(rn), d_rm(InvalidReg), d_offset(offset), d_isRegOffset(false), d_mode(mode), d_shift(LSL, 0) {
}

MemOp::MemOp(Register rn, Register rm, AddrMode mode)
    : d_rn(rn), d_rm(rm), d_offset(0), d_isRegOffset(true), d_mode(mode), d_shift(LSL, 0) {
}

MemOp::MemOp(Register rn, Register rm, const Shift& shift, AddrMode mode)
    : d_rn(rn), d_rm(rm), d_offset(0), d_isRegOffset(true), d_mode(mode), d_shift(shift) {
}

Emitter::Emitter() {
    d_buffer.reserve(4096);
}

void Emitter::emit32(quint32 inst) {
    // ARM instructions are 32-bit Little Endian
    d_buffer.append(static_cast<char>(inst & 0xFF));
    d_buffer.append(static_cast<char>((inst >> 8) & 0xFF));
    d_buffer.append(static_cast<char>((inst >> 16) & 0xFF));
    d_buffer.append(static_cast<char>((inst >> 24) & 0xFF));
}

void Emitter::bind(Label& label) {
    Q_ASSERT(!label.isBound());
    int pos = currentPosition();
    label.bindTo(pos);

    // Patch all forward references that used this label before it was bound
    const QVector<int>& uses = label.unresolvedUses();
    for (int i = 0; i < uses.size(); ++i) {
        patchBranch(uses[i], pos);
    }
}

void Emitter::patchWord(int offset, quint32 word) {
    Q_ASSERT(offset >= 0 && offset + 3 < d_buffer.size());
    d_buffer[offset] = static_cast<char>(word & 0xFF);
    d_buffer[offset + 1] = static_cast<char>((word >> 8) & 0xFF);
    d_buffer[offset + 2] = static_cast<char>((word >> 16) & 0xFF);
    d_buffer[offset + 3] = static_cast<char>((word >> 24) & 0xFF);
}

void Emitter::patchBranch(int offset, int targetPosition) {
    // Read existing instruction at 'offset'
    quint8 b0 = d_buffer[offset];
    quint8 b1 = d_buffer[offset + 1];
    quint8 b2 = d_buffer[offset + 2];
    quint8 b3 = d_buffer[offset + 3];
    quint32 inst = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);

    // ARM branches are PC-relative. The PC reads as the address of the current instruction + 8 bytes.
    qint32 pc = offset + 8;
    qint32 relOffset = targetPosition - pc;

    Q_ASSERT((relOffset & 3) == 0); // Must be 4-byte aligned
    qint32 imm24 = relOffset >> 2;
    Q_ASSERT(imm24 >= -0x800000 && imm24 <= 0x7FFFFF); // Branch offset must fit in signed 24 bits
    imm24 &= 0x00FFFFFF;

    inst = (inst & 0xFF000000) | imm24; // Keep condition and opcode, replace offset

    // Write patched instruction back
    d_buffer[offset] = static_cast<char>(inst & 0xFF);
    d_buffer[offset + 1] = static_cast<char>((inst >> 8) & 0xFF);
    d_buffer[offset + 2] = static_cast<char>((inst >> 16) & 0xFF);
    d_buffer[offset + 3] = static_cast<char>((inst >> 24) & 0xFF);
}


void Emitter::emitDataProcessing(Condition cond, int op, bool s, Register rn, Register rd, const Operand2& op2) {
    quint32 inst = (cond << 28) | (op << 21);
    if (s) inst |= (1 << 20);
    if (rn != InvalidReg) inst |= (rn << 16);
    if (rd != InvalidReg) inst |= (rd << 12);

    if (op2.type() == Operand2::Immediate) {
        inst |= (1 << 25); // I-bit = 1
        quint32 encodedImm = 0;
        Operand2::isValidImmediate(op2.immediate(), &encodedImm);
        inst |= encodedImm;
    } else {
        // I-bit = 0 (Register)
        inst |= op2.rm();
        if (op2.type() == Operand2::ShiftedReg) {
            const Shift& sh = op2.shift();
            inst |= (sh.type << 5);
            if (sh.isRegShift) {
                inst |= (sh.rs << 8) | (1 << 4);
            } else {
                inst |= (sh.amount << 7);
            }
        }
    }
    emit32(inst);
}

void Emitter::and_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_AND, s, rn, rd, op2);
}

void Emitter::eor_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_EOR, s, rn, rd, op2);
}

void Emitter::sub_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_SUB, s, rn, rd, op2);
}

void Emitter::rsb_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_RSB, s, rn, rd, op2);
}

void Emitter::add_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_ADD, s, rn, rd, op2);
}

void Emitter::adc_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_ADC, s, rn, rd, op2);
}
void Emitter::sbc_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_SBC, s, rn, rd, op2);
}
void Emitter::rsc_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_RSC, s, rn, rd, op2);
}
void Emitter::tst_(Condition cond, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_TST, true, rn, InvalidReg, op2);
}
void Emitter::teq_(Condition cond, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_TEQ, true, rn, InvalidReg, op2);
}
void Emitter::cmp_(Condition cond, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_CMP, true, rn, InvalidReg, op2);
}

void Emitter::cmn_(Condition cond, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_CMN, true, rn, InvalidReg, op2);
}
void Emitter::orr_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_ORR, s, rn, rd, op2);
}

void Emitter::mov_(Condition cond, bool s, Register rd, const Operand2& op2) {
    emitDataProcessing(cond, OP_MOV, s, InvalidReg, rd, op2);
}

void Emitter::bic_(Condition cond, bool s, Register rd, Register rn, const Operand2& op2) {
    emitDataProcessing(cond, OP_BIC, s, rn, rd, op2);
}

void Emitter::mvn_(Condition cond, bool s, Register rd, const Operand2& op2) {
    emitDataProcessing(cond, OP_MVN, s, InvalidReg, rd, op2);
}

void Emitter::b_(Condition cond, Label& label) {
    emitBranch(cond, false, label);
}

void Emitter::bl_(Condition cond, Label& label) {
    emitBranch(cond, true, label);
}

void Emitter::emitBranch(Condition cond, bool link, Label& label) {
    quint32 inst = (cond << 28) | (0xA << 24); // 1010 opcode for B
    if (link) inst |= (1 << 24); // 1011 opcode for BL

    if (label.isBound()) {
        qint32 pc = currentPosition() + 8;
        qint32 relOffset = label.position() - pc;
        qint32 imm24 = (relOffset >> 2) & 0x00FFFFFF;
        inst |= imm24;
    } else {
        // Unresolved: Record the offset of THIS instruction
        label.addUnresolvedUse(currentPosition());
    }
    emit32(inst);
}

void Emitter::bx_(Condition cond, Register rm) {
    quint32 inst = (cond << 28) | 0x012FFF10 | rm;
    emit32(inst);
}

void Emitter::blx_(Condition cond, Register rm) {
    quint32 inst = (cond << 28) | 0x012FFF30 | rm;
    emit32(inst);
}

void Emitter::mul_(Condition cond, bool s, Register rd, Register rm, Register rs) {
    quint32 inst = (cond << 28) | (0x00000090);
    if (s) inst |= (1 << 20);
    inst |= (rd << 16) | (rs << 8) | rm;
    emit32(inst);
}

void Emitter::mla_(Condition cond, bool s, Register rd, Register rm, Register rs, Register rn) {
    quint32 inst = (cond << 28) | (0x00200090);
    if (s) inst |= (1 << 20);
    inst |= (rd << 16) | (rn << 12) | (rs << 8) | rm;
    emit32(inst);
}

void Emitter::umull_(Condition cond, bool s, Register rdLo, Register rdHi, Register rm, Register rs) {
    quint32 inst = (cond << 28) | (0x00800090);
    if (s) inst |= (1 << 20);
    inst |= (rdHi << 16) | (rdLo << 12) | (rs << 8) | rm;
    emit32(inst);
}

void Emitter::smull_(Condition cond, bool s, Register rdLo, Register rdHi, Register rm, Register rs) {
    quint32 inst = (cond << 28) | (0x00C00090);
    if (s) inst |= (1 << 20);
    inst |= (rdHi << 16) | (rdLo << 12) | (rs << 8) | rm;
    emit32(inst);
}

void Emitter::sdiv_(Condition cond, Register rd, Register rn, Register rm) {
    quint32 inst = (cond << 28) | 0x0710F010 | (rd << 16) | (rm << 8) | rn;
    emit32(inst);
}

void Emitter::udiv_(Condition cond, Register rd, Register rn, Register rm) {
    quint32 inst = (cond << 28) | 0x0730F010 | (rd << 16) | (rm << 8) | rn;
    emit32(inst);
}

void Emitter::emitLoadStore(Condition cond, bool load, bool byte, Register rd, const MemOp& op) {
    quint32 inst = (cond << 28) | (1 << 26); // 01 for Data Transfer
    bool u = true;
    quint32 offset = 0;

    if (op.isRegOffset()) {
        inst |= (1 << 25); // I=1 (inverse of Data Proc) means Register
        offset = op.rm();
        const Shift& sh = op.shift();
        offset |= (sh.type << 5) | (sh.amount << 7);
    } else {
        qint32 off = op.offset();
        if (off < 0) { u = false; off = -off; }
        Q_ASSERT(off <= 0xFFF); // Standard offset max 12-bits
        offset = off;
    }

    if (u) inst |= (1 << 23); // U-bit
    if (byte) inst |= (1 << 22); // B-bit
    if (load) inst |= (1 << 20); // L-bit

    if (op.mode() == MemOp::PreIndexed) inst |= (1 << 24) | (1 << 21); // P=1, W=1
    else if (op.mode() == MemOp::PostIndexed) inst |= (0 << 24) | (0 << 21); // P=0, W=0
    else inst |= (1 << 24); // Offset: P=1, W=0

    inst |= (op.rn() << 16) | (rd << 12) | offset;
    emit32(inst);
}

void Emitter::ldr_(Condition cond, Register rd, const MemOp& op) {
    emitLoadStore(cond, true, false, rd, op);
}

void Emitter::str_(Condition cond, Register rd, const MemOp& op) {
    emitLoadStore(cond, false, false, rd, op);
}

void Emitter::ldrb_(Condition cond, Register rd, const MemOp& op) {
    emitLoadStore(cond, true, true, rd, op);
}

void Emitter::strb_(Condition cond, Register rd, const MemOp& op) {
    emitLoadStore(cond, false, true, rd, op);
}

void Emitter::emitExtraLoadStore(Condition cond, int op1, int op2, Register rd, const MemOp& op) {
    quint32 inst = (cond << 28) | (1 << 7) | (1 << 4); // Extra load/store magic bits
    inst |= (op1 << 20); // L bit mapping
    inst |= (op2 << 5);  // bits 6:5 determine size/sign

    bool u = true;
    quint32 offset = 0;

    if (op.isRegOffset()) {
        inst |= (0 << 22); // I=0 means Register
        offset = op.rm();
    } else {
        inst |= (1 << 22); // I=1 means Immediate
        qint32 off = op.offset();
        if (off < 0) { u = false; off = -off; }
        Q_ASSERT(off <= 0xFF); // Extra offsets are max 8-bits
        offset = ((off & 0xF0) << 4) | (off & 0x0F); // Split into 4-bit halves
    }

    if (u) inst |= (1 << 23);

    if (op.mode() == MemOp::PreIndexed) inst |= (1 << 24) | (1 << 21);
    else if (op.mode() == MemOp::PostIndexed) inst |= (0 << 24) | (0 << 21);
    else inst |= (1 << 24);

    inst |= (op.rn() << 16) | (rd << 12) | offset;
    emit32(inst);
}

void Emitter::ldrh_(Condition cond, Register rd, const MemOp& op) {
    emitExtraLoadStore(cond, 1, 1, rd, op);
}
void Emitter::strh_(Condition cond, Register rd, const MemOp& op) {
    emitExtraLoadStore(cond, 0, 1, rd, op);
}
void Emitter::ldrsb_(Condition cond, Register rd, const MemOp& op) {
    emitExtraLoadStore(cond, 1, 2, rd, op);
}

void Emitter::ldrsh_(Condition cond, Register rd, const MemOp& op) {
    emitExtraLoadStore(cond, 1, 3, rd, op);
}

void Emitter::ldrd_(Condition cond, Register rdLo, Register /*rdHi*/, const MemOp& op) {
    Q_ASSERT((rdLo & 1) == 0); // LDRD requires even-numbered register
    emitExtraLoadStore(cond, 0, 3, rdLo, op);
}
void Emitter::strd_(Condition cond, Register rdLo, Register /*rdHi*/, const MemOp& op) {
    Q_ASSERT((rdLo & 1) == 0); // STRD requires even-numbered register
    emitExtraLoadStore(cond, 0, 2, rdLo, op);
}

void Emitter::ldmia_(Condition cond, Register rn, bool writeback, quint16 regList) {
    quint32 inst = (cond << 28) | (0x08900000) | (rn << 16) | regList; // P=0, U=1, L=1
    if (writeback) inst |= (1 << 21);
    emit32(inst);
}

void Emitter::stmdb_(Condition cond, Register rn, bool writeback, quint16 regList) {
    quint32 inst = (cond << 28) | (0x09000000) | (rn << 16) | regList; // P=1, U=0, L=0
    if (writeback) inst |= (1 << 21);
    emit32(inst);
}

static inline quint32 encodeSReg(SRegister r, int shift1, int shift2) {
    return (((r >> 1) & 0xF) << shift1) | ((r & 1) << shift2);
}
static inline quint32 encodeDReg(DRegister r, int shift1, int shift2) {
    return ((r & 0xF) << shift1) | (((r >> 4) & 1) << shift2);
}

void Emitter::vadds(Condition cond, SRegister sd, SRegister sn, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0E300A00 | encodeSReg(sd, 12, 22) | encodeSReg(sn, 16, 7) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vaddd(Condition cond, DRegister dd, DRegister dn, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0E300B00 | encodeDReg(dd, 12, 22) | encodeDReg(dn, 16, 7) | encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vsubs(Condition cond, SRegister sd, SRegister sn, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0E300A40 | encodeSReg(sd, 12, 22) | encodeSReg(sn, 16, 7) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vsubd(Condition cond, DRegister dd, DRegister dn, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0E300B40 | encodeDReg(dd, 12, 22) | encodeDReg(dn, 16, 7) | encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vmuls(Condition cond, SRegister sd, SRegister sn, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0E200A00 | encodeSReg(sd, 12, 22) | encodeSReg(sn, 16, 7) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vmuld(Condition cond, DRegister dd, DRegister dn, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0E200B00 | encodeDReg(dd, 12, 22) | encodeDReg(dn, 16, 7) | encodeDReg(dm, 0, 5);
    emit32(inst);
}

void Emitter::vdivs(Condition cond, SRegister sd, SRegister sn, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0E800A00 | encodeSReg(sd, 12, 22) | encodeSReg(sn, 16, 7) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vdivd(Condition cond, DRegister dd, DRegister dn, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0E800B00 | encodeDReg(dd, 12, 22) | encodeDReg(dn, 16, 7) | encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vnegs(Condition cond, SRegister sd, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0EB10A40 | encodeSReg(sd, 12, 22) | encodeSReg(sm, 0, 5);
    emit32(inst);
}

void Emitter::vnegd(Condition cond, DRegister dd, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0EB10B40 | encodeDReg(dd, 12, 22) |  encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vcvtsd(Condition cond, SRegister sd, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0EB70BC0 | encodeSReg(sd, 12, 22) | encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vcvtds(Condition cond, DRegister dd, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0EB70AC0 | encodeDReg(dd, 12, 22) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vcvts_i32(Condition cond, SRegister sd, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0EBD0AC0 | encodeSReg(sd, 12, 22) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vcvti32_s(Condition cond, SRegister sd, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0EB80AC0 | encodeSReg(sd, 12, 22) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vcvtd_i32(Condition cond, SRegister sd, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0EBD0BC0 | encodeSReg(sd, 12, 22) | encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vcvti32_d(Condition cond, DRegister dd, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0EB80BC0 | encodeDReg(dd, 12, 22) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vcmps(Condition cond, SRegister sd, SRegister sm) {
    quint32 inst = (cond << 28) | 0x0EB40A40 | encodeSReg(sd, 12, 22) | encodeSReg(sm, 0, 5);
    emit32(inst);
}
void Emitter::vcmpd(Condition cond, DRegister dd, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0EB40B40 | encodeDReg(dd, 12, 22) | encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vmrs_apsr(Condition cond) {
    // VMRS APSR_nzcv, FPSCR
    quint32 inst = (cond << 28) | 0x0EF1FA10;
    emit32(inst);
}
void Emitter::vldrs(Condition cond, SRegister sd, Register rn, qint32 offset) {
    Q_ASSERT((offset & 3) == 0); // Must be word-aligned
    bool u = true;
    if (offset < 0) { u = false; offset = -offset; }
    Q_ASSERT(offset <= 1020); // 8-bit * 4
    quint32 imm8 = offset >> 2;
    quint32 inst = (cond << 28) | 0x0D100A00 | encodeSReg(sd, 12, 22) | (rn << 16) | imm8;
    if (u) inst |= (1 << 23);
    emit32(inst);
}
void Emitter::vstrs(Condition cond, SRegister sd, Register rn, qint32 offset) {
    Q_ASSERT((offset & 3) == 0);
    bool u = true;
    if (offset < 0) { u = false; offset = -offset; }
    Q_ASSERT(offset <= 1020);
    quint32 imm8 = offset >> 2;
    quint32 inst = (cond << 28) | 0x0D000A00 | encodeSReg(sd, 12, 22)  | (rn << 16) | imm8;
    if (u) inst |= (1 << 23);
    emit32(inst);
}
void Emitter::vldrd(Condition cond, DRegister dd, Register rn, qint32 offset) {
    Q_ASSERT((offset & 3) == 0);
    bool u = true;
    if (offset < 0) { u = false; offset = -offset; }
    Q_ASSERT(offset <= 1020);
    quint32 imm8 = offset >> 2;
    quint32 inst = (cond << 28) | 0x0D100B00 |  encodeDReg(dd, 12, 22) | (rn << 16) | imm8;
    if (u) inst |= (1 << 23);
    emit32(inst);
}
void Emitter::vstrd(Condition cond, DRegister dd, Register rn, qint32 offset) {
    Q_ASSERT((offset & 3) == 0);
    bool u = true;
    if (offset < 0) { u = false; offset = -offset; }
    Q_ASSERT(offset <= 1020);
    quint32 imm8 = offset >> 2;
    quint32 inst = (cond << 28) | 0x0D000B00 | encodeDReg(dd, 12, 22) | (rn << 16) | imm8;
    if (u) inst |= (1 << 23);
    emit32(inst);
}
void Emitter::vmov_s_r(Condition cond, SRegister sn, Register rt) {
    quint32 inst = (cond << 28) | 0x0E000A10 | encodeSReg(sn, 16, 7) | (rt << 12);
    emit32(inst);
}
void Emitter::vmov_r_s(Condition cond, Register rt, SRegister sn) {
    quint32 inst = (cond << 28) | 0x0E100A10  | encodeSReg(sn, 16, 7) | (rt << 12);
    emit32(inst);
}
void Emitter::vmov_d_rr(Condition cond, DRegister dm, Register rtLo, Register rtHi) {
    quint32 inst = (cond << 28) | 0x0C400B10 |  (rtHi << 16) | (rtLo << 12) | encodeDReg(dm, 0, 5);
    emit32(inst);
}
void Emitter::vmov_rr_d(Condition cond, Register rtLo, Register rtHi, DRegister dm) {
    quint32 inst = (cond << 28) | 0x0C500B10 | (rtHi << 16) | (rtLo << 12) | encodeDReg(dm, 0, 5);
    emit32(inst);
}


void Emitter::nop_() {
    emit32(0xE320F000); // Standard ARMv7 NOP
}

void Emitter::svc_(Condition cond, quint32 imm24) {
    Q_ASSERT(imm24 <= 0xFFFFFF);
    emit32((cond << 28) | 0x0F000000 | imm24);
}

void Emitter::cpsid_(quint8 flags) {
    emit32(0xF10C0000 | (flags << 6));
}

void Emitter::cpsie_(quint8 flags) {
    emit32(0xF1080000 | (flags << 6));
}

void Emitter::mrs_(Condition cond, Register rd, StatusRegister sr) {
    quint32 inst = (cond << 28) | 0x010F0000 | (sr << 22) | (rd << 12);
    emit32(inst);
}

void Emitter::msr_(Condition cond, StatusRegister sr, const Operand2& op2) {
    quint32 inst = (cond << 28) | 0x0120F000 | (sr << 22);
    inst |= (0xF << 16); // Write all fields (cxsf) by default

    if (op2.type() == Operand2::Immediate) {
        inst |= (1 << 25);
        quint32 enc = 0;
        Operand2::isValidImmediate(op2.immediate(), &enc);
        inst |= enc;
    } else {
        inst |= op2.rm();
    }
    emit32(inst);
}

void Emitter::mcr_(Condition cond, Coprocessor coproc, quint8 opc1, Register rt, quint8 crn, quint8 crm, quint8 opc2) {
    // Move to Coprocessor from Register
    quint32 inst = (cond << 28) | 0x0E000010 | (opc1 << 21) | (crn << 16) | (rt << 12) | (coproc << 8) | (opc2 << 5) | crm;
    emit32(inst);
}

void Emitter::mrc_(Condition cond, Coprocessor coproc, quint8 opc1, Register rt, quint8 crn, quint8 crm, quint8 opc2) {
    // Move to Register from Coprocessor
    quint32 inst = (cond << 28) | 0x0E100010 | (opc1 << 21) | (crn << 16) | (rt << 12) | (coproc << 8) | (opc2 << 5) | crm;
    emit32(inst);
}

void Emitter::dmb_(BarrierOption option) {
    emit32(0xF57FF050 | option);
}

void Emitter::dsb_(BarrierOption option) {
    emit32(0xF57FF040 | option);
}

void Emitter::isb_(BarrierOption option) {
    emit32(0xF57FF060 | option);
}

void Emitter::ldrex_(Condition cond, Register rt, Register rn) {
    emit32((cond << 28) | 0x01900F9F | (rn << 16) | (rt << 12));
}

void Emitter::strex_(Condition cond, Register rd, Register rt, Register rn) {
    emit32((cond << 28) | 0x01800F90 | (rn << 16) | (rd << 12) | rt);
}

void Emitter::clrex_() {
    // CLREX is unconditional (0xF prefix)
    emit32(0xF57FF01F);
}


void Emitter::movw_(Condition cond, Register rd, quint16 imm16) {
    quint32 imm4 = (imm16 >> 12) & 0xF;
    quint32 imm12 = imm16 & 0xFFF;
    quint32 inst = (cond << 28) | 0x03000000 | (imm4 << 16) | (rd << 12) | imm12;
    emit32(inst);
}

void Emitter::movt_(Condition cond, Register rd, quint16 imm16) {
    quint32 imm4 = (imm16 >> 12) & 0xF;
    quint32 imm12 = imm16 & 0xFFF;
    quint32 inst = (cond << 28) | 0x03400000 | (imm4 << 16) | (rd << 12) | imm12;
    emit32(inst);
}

void Emitter::movImm32(Register rd, quint32 imm32) {
    movw_(AL, rd, static_cast<quint16>(imm32 & 0xFFFF));
    if (imm32 > 0xFFFF)
        movt_(AL, rd, static_cast<quint16>((imm32 >> 16) & 0xFFFF));
}


void Emitter::sxtb_(Condition cond, Register rd, Register rm) {
    quint32 inst = (cond << 28) | 0x06AF0070 | (rd << 12) | rm;
    emit32(inst);
}

void Emitter::sxth_(Condition cond, Register rd, Register rm) {
    quint32 inst = (cond << 28) | 0x06BF0070 | (rd << 12) | rm;
    emit32(inst);
}

void Emitter::uxtb_(Condition cond, Register rd, Register rm) {
    quint32 inst = (cond << 28) | 0x06EF0070 | (rd << 12) | rm;
    emit32(inst);
}

void Emitter::uxth_(Condition cond, Register rd, Register rm) {
    quint32 inst = (cond << 28) | 0x06FF0070 | (rd << 12) | rm;
    emit32(inst);
}


