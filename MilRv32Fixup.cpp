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

#include "MilRv32Fixup.h"
#include "MilRv32Emitter.h"
#include "MilElfWriter.h"

using namespace Mil;
using namespace Rv32;

// RISC-V relocation types (from elf.h / RISC-V ELF psABI)
enum RiscvRelocType {
    R_RISCV_NONE     = 0,
    R_RISCV_32       = 1,   // Absolute 32-bit
    R_RISCV_CALL     = 18,  // AUIPC+JALR pair (PC-relative call)
    R_RISCV_HI20     = 26,  // Absolute high 20 bits (LUI)
    R_RISCV_LO12_I   = 27,  // Absolute low 12 bits (I-type: ADDI, LW, etc.)
    R_RISCV_LO12_S   = 28,  // Absolute low 12 bits (S-type: SW, SB, etc.)
    R_RISCV_RELAX    = 51   // Linker relaxation hint
};

void FixupTracker::recordAbsAddr(quint32 codeOffset, quint32 symbolIdx, qint32 addend)
{
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.addend = addend;
    reloc.kind = Reloc_AbsAddr;
    d_textRelocs.append(reloc);
}

void FixupTracker::recordAbsLoad(quint32 codeOffset, quint32 symbolIdx, qint32 addend)
{
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.addend = addend;
    reloc.kind = Reloc_AbsLoad;
    d_textRelocs.append(reloc);
}

void FixupTracker::recordAbsStore(quint32 codeOffset, quint32 symbolIdx, qint32 addend)
{
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.addend = addend;
    reloc.kind = Reloc_AbsStore;
    d_textRelocs.append(reloc);
}

void FixupTracker::recordCall(quint32 codeOffset, quint32 symbolIdx)
{
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.addend = 0;
    reloc.kind = Reloc_Call;
    d_textRelocs.append(reloc);
}

void FixupTracker::recordDataRef(quint32 dataOffset, quint32 symbolIdx, qint32 addend)
{
    DataReloc reloc;
    reloc.dataOffset = dataOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.addend = addend;
    d_dataRelocs.append(reloc);
}

void FixupTracker::generateRelocations(ElfWriter& elf, quint32 relTextSection, quint32 relDataSection)
{
    // Text relocations
    for (int i = 0; i < d_textRelocs.size(); i++) {
        const TextReloc& r = d_textRelocs[i];
        switch (r.kind) {
        case Reloc_AbsAddr:
            // LUI at codeOffset -> R_RISCV_HI20
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_RISCV_HI20, r.addend);
            // ADDI at codeOffset+4 -> R_RISCV_LO12_I
            elf.addRelocation(relTextSection, r.codeOffset + 4, r.symbolIdx, R_RISCV_LO12_I, r.addend);
            break;
        case Reloc_AbsLoad:
            // LUI at codeOffset -> R_RISCV_HI20
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_RISCV_HI20, r.addend);
            // LW/LB/LH at codeOffset+4 -> R_RISCV_LO12_I (loads are I-type)
            elf.addRelocation(relTextSection, r.codeOffset + 4, r.symbolIdx, R_RISCV_LO12_I, r.addend);
            break;
        case Reloc_AbsStore:
            // LUI at codeOffset -> R_RISCV_HI20
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_RISCV_HI20, r.addend);
            // SW/SB/SH at codeOffset+4 -> R_RISCV_LO12_S (stores are S-type)
            elf.addRelocation(relTextSection, r.codeOffset + 4, r.symbolIdx, R_RISCV_LO12_S, r.addend);
            break;
        case Reloc_Call:
            // AUIPC+JALR pair -> R_RISCV_CALL (single reloc covers both)
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_RISCV_CALL, r.addend);
            // NOTE: Do NOT emit R_RISCV_RELAX here. Linker relaxation shrinks
            // AUIPC+JALR (8 bytes) to JAL (4 bytes), which shifts all subsequent
            // code. Internal branch offsets (bnez/beq/bne/etc.) are fully resolved
            // in the .o file and have no relocations, so the linker cannot adjust
            // them — causing branches to land at wrong targets after relaxation.
            break;
        }
    }

    // Data relocations (always R_RISCV_32)
    for (int i = 0; i < d_dataRelocs.size(); i++) {
        const DataReloc& r = d_dataRelocs[i];
        elf.addRelocation(relDataSection, r.dataOffset, r.symbolIdx, R_RISCV_32, r.addend);
    }
}

void FixupTracker::reset()
{
    d_textRelocs.clear();
    d_dataRelocs.clear();
}
