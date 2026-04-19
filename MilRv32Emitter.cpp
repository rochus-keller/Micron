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

#include "MilRv32Emitter.h"
using namespace Mil;
using namespace Rv32;

// RISC-V Base Opcodes
const quint8 OPC_LOAD     = 0x03;
const quint8 OPC_LOAD_FP  = 0x07;
const quint8 OPC_MISC_MEM = 0x0F;
const quint8 OPC_OP_IMM   = 0x13;
const quint8 OPC_AUIPC    = 0x17;
const quint8 OPC_STORE    = 0x23;
const quint8 OPC_STORE_FP = 0x27;
const quint8 OPC_AMO      = 0x2F;
const quint8 OPC_OP       = 0x33;
const quint8 OPC_LUI      = 0x37;
const quint8 OPC_OP_FP    = 0x53;
const quint8 OPC_BRANCH   = 0x63;
const quint8 OPC_JALR     = 0x67;
const quint8 OPC_JAL      = 0x6F;
const quint8 OPC_SYSTEM   = 0x73;

Emitter::Emitter() {
    d_buffer.reserve(4096);
}

void Emitter::emit32(quint32 inst) {
    // RISC-V instructions are 32-bit Little Endian
    d_buffer.append(char(inst & 0xFF));
    d_buffer.append(char((inst >> 8) & 0xFF));
    d_buffer.append(char((inst >> 16) & 0xFF));
    d_buffer.append(char((inst >> 24) & 0xFF));
}

void Emitter::emitR(quint8 opcode, quint8 funct3, quint8 funct7, quint8 rd, quint8 rs1, quint8 rs2) {
    quint32 inst = (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode;
    emit32(inst);
}

void Emitter::emitI(quint8 opcode, quint8 funct3, quint8 rd, quint8 rs1, qint32 imm12) {
    Q_ASSERT(imm12 >= -2048 && imm12 <= 2047);
    quint32 uimm = static_cast<quint32>(imm12) & 0xFFF;
    quint32 inst = (uimm << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode;
    emit32(inst);
}

void Emitter::emitS(quint8 opcode, quint8 funct3, quint8 rs1, quint8 rs2, qint32 imm12) {
    Q_ASSERT(imm12 >= -2048 && imm12 <= 2047);
    quint32 uimm = static_cast<quint32>(imm12) & 0xFFF;
    quint32 imm11_5 = (uimm >> 5) & 0x7F;
    quint32 imm4_0  = uimm & 0x1F;
    quint32 inst = (imm11_5 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (imm4_0 << 7) | opcode;
    emit32(inst);
}

void Emitter::emitB(quint8 opcode, quint8 funct3, quint8 rs1, quint8 rs2, Label& label) {
    quint32 inst = (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | opcode;
    if (label.isBound()) {
        qint32 offset = label.position() - currentPosition();
        Q_ASSERT(offset >= -4096 && offset <= 4094 && (offset & 1) == 0);
        quint32 uoff = static_cast<quint32>(offset);
        quint32 b12   = (uoff >> 12) & 0x1;
        quint32 b11   = (uoff >> 11) & 0x1;
        quint32 b10_5 = (uoff >> 5) & 0x3F;
        quint32 b4_1  = (uoff >> 1) & 0xF;
        inst |= (b12 << 31) | (b10_5 << 25) | (b4_1 << 8) | (b11 << 7);
    } else {
        label.addUnresolvedUse(currentPosition());
    }
    emit32(inst);
}

void Emitter::emitU(quint8 opcode, quint8 rd, qint32 imm20) {
    quint32 uimm = static_cast<quint32>(imm20) & 0xFFFFF;
    quint32 inst = (uimm << 12) | (rd << 7) | opcode;
    emit32(inst);
}

void Emitter::emitJ(quint8 opcode, quint8 rd, Label& label) {
    quint32 inst = (rd << 7) | opcode;
    if (label.isBound()) {
        qint32 offset = label.position() - currentPosition();
        Q_ASSERT(offset >= -1048576 && offset <= 1048574 && (offset & 1) == 0);
        quint32 uoff = static_cast<quint32>(offset);
        quint32 j20    = (uoff >> 20) & 0x1;
        quint32 j10_1  = (uoff >> 1) & 0x3FF;
        quint32 j11    = (uoff >> 11) & 0x1;
        quint32 j19_12 = (uoff >> 12) & 0xFF;
        inst |= (j20 << 31) | (j10_1 << 21) | (j11 << 20) | (j19_12 << 12);
    } else {
        label.addUnresolvedUse(currentPosition());
    }
    emit32(inst);
}

void Emitter::bind(Label& label) {
    Q_ASSERT(!label.isBound());
    int pos = currentPosition();
    label.bindTo(pos);

    const QVector<int>& uses = label.unresolvedUses();
    for (int i = 0; i < uses.size(); ++i) {
        patchBranch(uses[i], pos);
    }
}

void Emitter::patchBranch(int codeOffset, int targetPosition) {
    quint8 b0 = d_buffer[codeOffset];
    quint8 b1 = d_buffer[codeOffset + 1];
    quint8 b2 = d_buffer[codeOffset + 2];
    quint8 b3 = d_buffer[codeOffset + 3];
    quint32 inst = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);

    qint32 offset = targetPosition - codeOffset;
    quint32 uoff = static_cast<quint32>(offset);
    quint8 opcode = inst & 0x7F;

    if (opcode == OPC_BRANCH) { // B-Type
        quint32 b12   = (uoff >> 12) & 0x1;
        quint32 b11   = (uoff >> 11) & 0x1;
        quint32 b10_5 = (uoff >> 5) & 0x3F;
        quint32 b4_1  = (uoff >> 1) & 0xF;
        inst &= 0x01FFF07F; // Clear immediate bits
        inst |= (b12 << 31) | (b10_5 << 25) | (b4_1 << 8) | (b11 << 7);
    }
    else if (opcode == OPC_JAL) { // J-Type
        quint32 j20    = (uoff >> 20) & 0x1;
        quint32 j10_1  = (uoff >> 1) & 0x3FF;
        quint32 j11    = (uoff >> 11) & 0x1;
        quint32 j19_12 = (uoff >> 12) & 0xFF;
        inst &= 0x00000FFF; // Clear immediate bits
        inst |= (j20 << 31) | (j10_1 << 21) | (j11 << 20) | (j19_12 << 12);
    }

    d_buffer[codeOffset]     = char(inst & 0xFF);
    d_buffer[codeOffset + 1] = char((inst >> 8) & 0xFF);
    d_buffer[codeOffset + 2] = char((inst >> 16) & 0xFF);
    d_buffer[codeOffset + 3] = char((inst >> 24) & 0xFF);
}

void Emitter::addi(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_OP_IMM, 0, rd, rs1, imm12);
}
void Emitter::slti(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_OP_IMM, 2, rd, rs1, imm12);
}
void Emitter::sltiu(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_OP_IMM, 3, rd, rs1, imm12);
}
void Emitter::xori(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_OP_IMM, 4, rd, rs1, imm12);
}
void Emitter::ori(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_OP_IMM, 6, rd, rs1, imm12);
}
void Emitter::andi(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_OP_IMM, 7, rd, rs1, imm12);
}

void Emitter::slli(Register rd, Register rs1, quint8 shamt) {
    emitI(OPC_OP_IMM, 1, rd, rs1, shamt & 0x1F);
}
void Emitter::srli(Register rd, Register rs1, quint8 shamt) {
    emitI(OPC_OP_IMM, 5, rd, rs1, shamt & 0x1F);
}
void Emitter::srai(Register rd, Register rs1, quint8 shamt) {
    emitI(OPC_OP_IMM, 5, rd, rs1, (shamt & 0x1F) | 0x400); // 0x400 sets the arithmetic flag
}

void Emitter::add(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 0, 0x00, rd, rs1, rs2);
}
void Emitter::sub(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 0, 0x20, rd, rs1, rs2);
}
void Emitter::sll(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 1, 0x00, rd, rs1, rs2);
}
void Emitter::slt(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 2, 0x00, rd, rs1, rs2);
}
void Emitter::sltu(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 3, 0x00, rd, rs1, rs2);
}
void Emitter::xor_(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 4, 0x00, rd, rs1, rs2);
}
void Emitter::srl(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 5, 0x00, rd, rs1, rs2);
}
void Emitter::sra(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 5, 0x20, rd, rs1, rs2);
}
void Emitter::or_(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 6, 0x00, rd, rs1, rs2);
}
void Emitter::and_(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 7, 0x00, rd, rs1, rs2);
}

void Emitter::lui(Register rd, qint32 imm20) {
    emitU(OPC_LUI, rd, imm20);
}
void Emitter::auipc(Register rd, qint32 imm20) {
    emitU(OPC_AUIPC, rd, imm20);
}

void Emitter::lb(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_LOAD, 0, rd, rs1, imm12);
}
void Emitter::lh(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_LOAD, 1, rd, rs1, imm12);
}
void Emitter::lw(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_LOAD, 2, rd, rs1, imm12);
}
void Emitter::lbu(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_LOAD, 4, rd, rs1, imm12);
}
void Emitter::lhu(Register rd, Register rs1, qint32 imm12) {
    emitI(OPC_LOAD, 5, rd, rs1, imm12);
}
void Emitter::sb(Register rs2, Register rs1, qint32 imm12) {
    emitS(OPC_STORE, 0, rs1, rs2, imm12);
}
void Emitter::sh(Register rs2, Register rs1, qint32 imm12) {
    emitS(OPC_STORE, 1, rs1, rs2, imm12);
}
void Emitter::sw(Register rs2, Register rs1, qint32 imm12) {
    emitS(OPC_STORE, 2, rs1, rs2, imm12);
}

void Emitter::beq(Register rs1, Register rs2, Label & l) {
    emitB(OPC_BRANCH, 0, rs1, rs2, l);
}
void Emitter::bne(Register rs1, Register rs2, Label & l) {
    emitB(OPC_BRANCH, 1, rs1, rs2, l);
}
void Emitter::blt(Register rs1, Register rs2, Label & l) {
    emitB(OPC_BRANCH, 4, rs1, rs2, l);
}
void Emitter::bge(Register rs1, Register rs2, Label & l) {
    emitB(OPC_BRANCH, 5, rs1, rs2, l);
}
void Emitter::bltu(Register rs1, Register rs2, Label & l) {
    emitB(OPC_BRANCH, 6, rs1, rs2, l);
}
void Emitter::bgeu(Register rs1, Register rs2, Label & l) {
    emitB(OPC_BRANCH, 7, rs1, rs2, l);
}

void Emitter::jal(Register rd, Label & label) {
    emitJ(OPC_JAL, rd, label);
}
void Emitter::jalr(Register rd, Register rs1, qint32 imm) {
    emitI(OPC_JALR, 0, rd, rs1, imm);
}

void Emitter::mul(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 0, 0x01, rd, rs1, rs2);
}
void Emitter::mulh(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 1, 0x01, rd, rs1, rs2);
}
void Emitter::mulhsu(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 2, 0x01, rd, rs1, rs2);
}
void Emitter::mulhu(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 3, 0x01, rd, rs1, rs2);
}
void Emitter::div(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 4, 0x01, rd, rs1, rs2);
}
void Emitter::divu(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 5, 0x01, rd, rs1, rs2);
}
void Emitter::rem(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 6, 0x01, rd, rs1, rs2);
}
void Emitter::remu(Register rd, Register rs1, Register rs2) {
    emitR(OPC_OP, 7, 0x01, rd, rs1, rs2);
}

void Emitter::lr_w(Register rd, Register rs1) {
    emitR(OPC_AMO, 2, 0x08, rd, rs1, Zero);
}
void Emitter::sc_w(Register rd, Register rs1, Register rs2) {
    emitR(OPC_AMO, 2, 0x0C, rd, rs1, rs2);
}
void Emitter::amoswap_w(Register rd, Register rs1, Register rs2) {
    emitR(OPC_AMO, 2, 0x04, rd, rs1, rs2);
}
void Emitter::amoadd_w(Register rd, Register rs1, Register rs2) {
    emitR(OPC_AMO, 2, 0x00, rd, rs1, rs2);
}

void Emitter::flw(FRegister rd, Register rs1, qint32 imm12) {
    emitI(OPC_LOAD_FP, 2, rd, rs1, imm12);
}
void Emitter::fsw(FRegister rs2, Register rs1, qint32 imm12) {
    emitS(OPC_STORE_FP, 2, rs1, rs2, imm12);
}

void Emitter::fadd_s(FRegister rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 7, 0x00, rd, rs1, rs2);
}
void Emitter::fsub_s(FRegister rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 7, 0x04, rd, rs1, rs2);
}
void Emitter::fmul_s(FRegister rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 7, 0x08, rd, rs1, rs2);
}
void Emitter::fdiv_s(FRegister rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 7, 0x0C, rd, rs1, rs2);
}
void Emitter::fsqrt_s(FRegister rd, FRegister rs1) {
    emitR(OPC_OP_FP, 7, 0x2C, rd, rs1, 0);
}

void Emitter::fcvtw_s(Register rd, FRegister rs1) {
    emitR(OPC_OP_FP, 7, 0x60, rd, rs1, 0); // Float to int
}
void Emitter::fcvts_w(FRegister rd, Register rs1) {
    emitR(OPC_OP_FP, 7, 0x68, rd, rs1, 0); // Int to float
}

void Emitter::fmvw_x(FRegister rd, Register rs1) {
    emitR(OPC_OP_FP, 0, 0x78, rd, rs1, 0); // Move bits directly
}
void Emitter::fmvx_w(Register rd, FRegister rs1) {
    emitR(OPC_OP_FP, 0, 0x70, rd, rs1, 0);
}

void Emitter::fsgnj_s(FRegister rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 0, 0x10, rd, rs1, rs2);
}
void Emitter::fsgnjn_s(FRegister rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 1, 0x10, rd, rs1, rs2);
}
void Emitter::fsgnjx_s(FRegister rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 2, 0x10, rd, rs1, rs2);
}
void Emitter::fcvtwu_s(Register rd, FRegister rs1) {
    // FCVT.WU.S: rs2 field = 1 (unsigned), rounding mode RTZ (funct3=1)
    emitR(OPC_OP_FP, 1, 0x60, rd, rs1, 1);
}
void Emitter::fcvts_wu(FRegister rd, Register rs1) {
    // FCVT.S.WU: rs2 field = 1 (unsigned), rounding mode RNE (funct3=7)
    emitR(OPC_OP_FP, 7, 0x68, rd, rs1, 1);
}
void Emitter::feq_s(Register rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 2, 0x50, rd, rs1, rs2);
}
void Emitter::flt_s(Register rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 1, 0x50, rd, rs1, rs2);
}
void Emitter::fle_s(Register rd, FRegister rs1, FRegister rs2) {
    emitR(OPC_OP_FP, 0, 0x50, rd, rs1, rs2);
}

void Emitter::ecall() {
    emitI(OPC_SYSTEM, 0, Zero, Zero, 0);
}
void Emitter::ebreak() {
    emitI(OPC_SYSTEM, 0, Zero, Zero, 1);
}
void Emitter::mret() {
    emitI(OPC_SYSTEM, 0, Zero, Zero, 0x302);
}
void Emitter::sret() {
    emitI(OPC_SYSTEM, 0, Zero, Zero, 0x102);
}
void Emitter::wfi() {
    emitI(OPC_SYSTEM, 0, Zero, Zero, 0x105);
}

void Emitter::fence() {
    emitI(OPC_MISC_MEM, 0, Zero, Zero, 0x0FF); // Full barrier
}
void Emitter::fence_i() {
    emitI(OPC_MISC_MEM, 1, Zero, Zero, 0); // Instruction cache flush
}

void Emitter::csrrw(Register rd, CSR csr, Register rs1) {
    emitI(OPC_SYSTEM, 1, rd, rs1, csr);
}
void Emitter::csrrs(Register rd, CSR csr, Register rs1) {
    // Generates LUI + ADDI appropriately
    emitI(OPC_SYSTEM, 2, rd, rs1, csr);
}
void Emitter::csrrc(Register rd, CSR csr, Register rs1) {
    emitI(OPC_SYSTEM, 3, rd, rs1, csr);
}
void Emitter::csrrwi(Register rd, CSR csr, quint8 zimm) {
    emitI(OPC_SYSTEM, 5, rd, zimm & 0x1F, csr); // rs1 acts as 5-bit zero-extended immediate
}
void Emitter::csrrsi(Register rd, CSR csr, quint8 zimm) {
    emitI(OPC_SYSTEM, 6, rd, zimm & 0x1F, csr);
}
void Emitter::csrrci(Register rd, CSR csr, quint8 zimm) {
    emitI(OPC_SYSTEM, 7, rd, zimm & 0x1F, csr);
}

void Emitter::emitWord(quint32 word) {
    emit32(word);
}

void Emitter::patchWord(int offset, quint32 word) {
    // Patch a 32-bit word at a given offset in the code buffer
    Q_ASSERT(offset >= 0 && offset + 3 < d_buffer.size());
    d_buffer[offset]     = char(word & 0xFF);
    d_buffer[offset + 1] = char((word >> 8) & 0xFF);
    d_buffer[offset + 2] = char((word >> 16) & 0xFF);
    d_buffer[offset + 3] = char((word >> 24) & 0xFF);
}

void Emitter::li(Register rd, quint32 value) {
    // Sign-extend the lower 12 bits to safely isolate them for ADDI
    qint32 lo = static_cast<qint32>(value << 20) >> 20;

    // The difference must be provided by LUI (which shifts its immediate left by 12).
    qint32 hi = value - static_cast<quint32>(lo);

    if (hi != 0) {
        lui(rd, hi >> 12);
        if (lo != 0) {
            addi(rd, rd, lo);
        }
    } else {
        addi(rd, Zero, lo);
    }
}


