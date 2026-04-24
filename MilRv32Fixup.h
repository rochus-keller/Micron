#ifndef MILRV32FIXUP_H
#define MILRV32FIXUP_H

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
#include <QMap>
#include <QPair>
#include <QtGlobal>

namespace Mil
{
    class ElfWriter;

namespace Rv32
{
    class Emitter;

    // The Rv32 FixupTracker bridges the gap between the Rv32::Emitter and the ElfWriter.
    // It records relocation requests during code generation and emits them as
    // RISC-V ELF relocations (RELA) when finalizing the object file.
    //
    // RISC-V address materialization uses LUI+ADDI (or LUI+load/store) pairs:
    //   LUI  rd, %hi(symbol)        -> R_RISCV_HI20
    //   ADDI rd, rd, %lo(symbol)    -> R_RISCV_LO12_I
    //
    // Calls use AUIPC+JALR pairs:
    //   AUIPC rd, 0                 -> R_RISCV_CALL (covers both instructions)
    //   JALR  ra, rd, 0
    //
    // Data references use R_RISCV_32 (absolute 32-bit).

    class FixupTracker
    {
    public:
        FixupTracker() {}

        // Record a LUI+ADDI pair at codeOffset that loads an absolute address.
        // Two relocations: R_RISCV_HI20 at codeOffset, R_RISCV_LO12_I at codeOffset+4.
        void recordAbsAddr(quint32 codeOffset, quint32 symbolIdx, qint32 addend = 0);

        // Record a LUI+LW pair at codeOffset that loads from an absolute address.
        // R_RISCV_HI20 at codeOffset, R_RISCV_LO12_I at codeOffset+4.
        void recordAbsLoad(quint32 codeOffset, quint32 symbolIdx, qint32 addend = 0);

        // Record a LUI+SW pair at codeOffset that stores to an absolute address.
        // R_RISCV_HI20 at codeOffset, R_RISCV_LO12_S at codeOffset+4.
        void recordAbsStore(quint32 codeOffset, quint32 symbolIdx, qint32 addend = 0);

        // Record an AUIPC+JALR call pair at codeOffset.
        // Single R_RISCV_CALL relocation covers both instructions.
        void recordCall(quint32 codeOffset, quint32 symbolIdx);

        // Record an absolute 32-bit data reference at dataOffset in .data section.
        void recordDataRef(quint32 dataOffset, quint32 symbolIdx, qint32 addend = 0);

        // Generate ELF RELA relocations for all recorded references.
        void generateRelocations(ElfWriter& elf, quint32 relTextSection, quint32 relDataSection);

        void reset();

    private:
        enum RelocKind {
            Reloc_AbsAddr,    // LUI+ADDI -> R_RISCV_HI20 + R_RISCV_LO12_I
            Reloc_AbsLoad,    // LUI+LW   -> R_RISCV_HI20 + R_RISCV_LO12_I
            Reloc_AbsStore,   // LUI+SW   -> R_RISCV_HI20 + R_RISCV_LO12_S
            Reloc_Call         // AUIPC+JALR -> R_RISCV_CALL (+ R_RISCV_RELAX)
        };

        struct TextReloc {
            quint32 codeOffset;
            quint32 symbolIdx;
            qint32 addend;
            RelocKind kind;
        };

        struct DataReloc {
            quint32 dataOffset;
            quint32 symbolIdx;
            qint32 addend;
        };

        QList<TextReloc> d_textRelocs;
        QList<DataReloc> d_dataRelocs;
    };

} // Rv32
} // Mil

#endif // MILRV32FIXUP_H
