#ifndef MILARMFIXUP_H
#define MILARMFIXUP_H

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

namespace Arm
{
    class Emitter;

    // The ArmFixupTracker bridges the gap between the Arm::Emitter and the ElfWriter
    class FixupTracker
    {
    public:
        FixupTracker() {}

        void recordBranch(quint32 codeOffset, quint32 labelId);

        void defineLabel(quint32 labelId, quint32 codeOffset);

        void resolveForwardBranches(Arm::Emitter& emitter);

        void resetLabels();

        void recordExternalCall(quint32 codeOffset, quint32 symbolIdx);

        void recordExternalAddr(quint32 codeOffset, quint32 symbolIdx);

        void recordDataRef(quint32 dataOffset, quint32 symbolIdx);

        void recordPCRelRef(quint32 codeOffset, quint32 symbolIdx);

        void recordLocalCall(quint32 codeOffset, quint32 procSymbolIdx);

        void generateRelocations(ElfWriter& elf, quint32 relTextSection, quint32 relDataSection);

        void reset();

    private:
        struct BranchRef {
            // branch instruction offset -> target label ID
            quint32 codeOffset;
            quint32 labelId;
        };
        QList<BranchRef> d_branches;

        QMap<quint32, quint32> d_labels; // label ID -> code offset

        enum RelocKind {
            Reloc_Call,       // BL -> R_ARM_CALL
            Reloc_MovwMovt,   // MOVW/MOVT pair -> R_ARM_MOVW_ABS_NC + R_ARM_MOVT_ABS
            Reloc_Abs32,      // Direct 32-bit -> R_ARM_ABS32
            Reloc_Rel32,      // PC-relative 32-bit -> R_ARM_REL32
            Reloc_LocalCall   // BL within module -> R_ARM_CALL (with local symbol)
        };

        struct TextReloc {
            quint32 codeOffset;
            quint32 symbolIdx;
            RelocKind kind;
        };

        struct DataReloc {
            quint32 dataOffset;
            quint32 symbolIdx;
        };

        QList<TextReloc> d_textRelocs;
        QList<DataReloc> d_dataRelocs;
    };

} // Arm
} // Mil

#endif // MILARMFIXUP_H
