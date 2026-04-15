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

#include "MilArmFixup.h"
#include "MilArmv7Emitter.h"
#include "MilElfWriter.h"

using namespace Mil;
using namespace Arm;

enum ArmRelocType {
    R_ARM_NONE = 0,
    R_ARM_ABS32 = 2, // Direct 32-bit
    R_ARM_REL32 = 3, // PC-relative 32-bit
    R_ARM_CALL = 28, // BL/BLX immediate
    R_ARM_JUMP24 = 29, // B/BL<cond> immediate
    R_ARM_MOVW_ABS_NC = 43, // MOVW immediate (lower 16 bits, no check)
    R_ARM_MOVT_ABS  = 44  // MOVT immediate (upper 16 bits)
};

// Record a branch instruction at codeOffset that targets a label ID.
void FixupTracker::recordBranch(quint32 codeOffset, quint32 labelId)
{
    // The label may not yet be defined (forward reference).
    BranchRef ref;
    ref.codeOffset = codeOffset;
    ref.labelId = labelId;
    d_branches.append(ref);
}

void FixupTracker::defineLabel(quint32 labelId, quint32 codeOffset)
{
    d_labels.insert(labelId, codeOffset);
}

// Resolve all forward branches for the current procedure.
void FixupTracker::resolveForwardBranches(Arm::Emitter& emitter)
{
    // Must be called after all labels in the procedure are defined.
    // Patches the branch instructions directly in the Emitter's buffer.

    // Note: For intra-procedure branches, the code generator should use the
    // Emitter's own Label/bind mechanism, which handles forward references
    // automatically. This method resolves any additional cross-procedure
    // forward branches recorded via recordBranch().

     // To patch branches, we directly manipulate the emitter's code buffer.
    // The Emitter's patchBranch is protected, so we read/write the buffer
    // through machineCode() and patchWord().

    for (int i = 0; i < d_branches.size(); i++) {
        const BranchRef& ref = d_branches[i];
        QMap<quint32, quint32>::const_iterator it = d_labels.find(ref.labelId);
        if (it == d_labels.end()) {
            Q_ASSERT_X(false, "ArmFixupTracker::resolveForwardBranches", "Unresolved label");
            continue;
        }

        quint32 targetOffset = it.value();
        quint32 branchOffset = ref.codeOffset;

        // Read existing instruction from emitter buffer
        const QByteArray& code = emitter.machineCode();
        Q_ASSERT(branchOffset + 3 < static_cast<quint32>(code.size()));
        quint8 b0 = static_cast<quint8>(code[branchOffset]);
        quint8 b1 = static_cast<quint8>(code[branchOffset + 1]);
        quint8 b2 = static_cast<quint8>(code[branchOffset + 2]);
        quint8 b3 = static_cast<quint8>(code[branchOffset + 3]);
        quint32 inst = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);

        // ARM branches: PC = branchOffset + 8 (pipeline)
        qint32 pc = branchOffset + 8;
        qint32 relOffset = static_cast<qint32>(targetOffset) - pc;
        Q_ASSERT((relOffset & 3) == 0);
        qint32 imm24 = relOffset >> 2;
        Q_ASSERT(imm24 >= -0x800000 && imm24 <= 0x7FFFFF);
        imm24 &= 0x00FFFFFF;

        inst = (inst & 0xFF000000) | imm24;

        // Write patched instruction back via patchWord
        emitter.patchWord(branchOffset, inst);
    }
}

// Clear label state for the next procedure.
void FixupTracker::resetLabels()
{
    d_branches.clear();
    d_labels.clear();
}

// Record a BL instruction at codeOffset that calls an external procedure.
void FixupTracker::recordExternalCall(quint32 codeOffset, quint32 symbolIdx)
{
    // module+proc identify the target. The ELF symbol for the target must
    // be created separately; this just records the fixup location.
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.kind = Reloc_Call;
    d_textRelocs.append(reloc);
}

// Record a MOVW/MOVT pair at codeOffset that loads the address of an
// external variable or procedure. Two relocations are generated:
//   MOVW at codeOffset     -> R_ARM_MOVW_ABS_NC
//   MOVT at codeOffset + 4 -> R_ARM_MOVT_ABS
void FixupTracker::recordExternalAddr(quint32 codeOffset, quint32 symbolIdx)
{
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.kind = Reloc_MovwMovt;
    d_textRelocs.append(reloc);
}

void FixupTracker::recordDataRef(quint32 dataOffset, quint32 symbolIdx)
{
    // Used for initialized pointers, vtable entries, etc.
    DataReloc reloc;
    reloc.dataOffset = dataOffset;
    reloc.symbolIdx = symbolIdx;
    d_dataRelocs.append(reloc);
}

// Record a PC-relative 32-bit reference (e.g., for literal pools)
void FixupTracker::recordPCRelRef(quint32 codeOffset, quint32 symbolIdx)
{
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = symbolIdx;
    reloc.kind = Reloc_Rel32;
    d_textRelocs.append(reloc);
}

// Record a BL instruction at codeOffset that calls a procedure within sthe same module.
void FixupTracker::recordLocalCall(quint32 codeOffset, quint32 procSymbolIdx)
{
    // procOffset is the offset of the target procedure
    // within .text. These can be resolved without ELF relocations if the
    // module is self-contained, or emitted as local relocations.
    TextReloc reloc;
    reloc.codeOffset = codeOffset;
    reloc.symbolIdx = procSymbolIdx;
    reloc.kind = Reloc_LocalCall;
    d_textRelocs.append(reloc);
}

// Generate ELF relocations for all inter-module and local references.
void FixupTracker::generateRelocations(ElfWriter& elf, quint32 relTextSection, quint32 relDataSection)
{
    // relTextSection: index of .rel.text section in the ElfWriter
    // relDataSection: index of .rel.data section in the ElfWriter
    // Text relocations
    for (int i = 0; i < d_textRelocs.size(); i++) {
        const TextReloc& r = d_textRelocs[i];
        switch (r.kind) {
        case Reloc_Call:
        case Reloc_LocalCall:
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_ARM_CALL);
            break;
        case Reloc_MovwMovt:
            // MOVW at codeOffset
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_ARM_MOVW_ABS_NC);
            // MOVT at codeOffset + 4
            elf.addRelocation(relTextSection, r.codeOffset + 4, r.symbolIdx, R_ARM_MOVT_ABS);
            break;
        case Reloc_Rel32:
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_ARM_REL32);
            break;
        case Reloc_Abs32:
            elf.addRelocation(relTextSection, r.codeOffset, r.symbolIdx, R_ARM_ABS32);
            break;
        }
    }

    // Data relocations (always ABS32)
    for (int i = 0; i < d_dataRelocs.size(); i++) {
        const DataReloc& r = d_dataRelocs[i];
        elf.addRelocation(relDataSection, r.dataOffset, r.symbolIdx, R_ARM_ABS32);
    }
}

void FixupTracker::reset()
{
    d_branches.clear();
    d_labels.clear();
    d_textRelocs.clear();
    d_dataRelocs.clear();
}
