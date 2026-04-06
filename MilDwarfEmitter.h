#ifndef MILDWARFEMITTER_H
#define MILDWARFEMITTER_H

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

#include "MilElfWriter.h"
#include <QByteArray>
#include <QList>
#include <QMap>
#include <QString>
#include <QtGlobal>

namespace Mil
{
    class Type;
    class Declaration;

    // Generates DWARF v4 debug information for the backend.
    //
    // Produces the following ELF sections:
    // .debug_info    - Compilation unit, subprograms, variables, types
    // .debug_abbrev  - Abbreviation table (templates for DIEs)
    // .debug_line    - Line number program (source line -> address mapping)
    // .debug_frame   - Call frame information (CFA = R11 + 8 for all frames)
    // .debug_str     - String table (indirect strings referenced by DIEs)
    class DwarfEmitter
    {
    public:
        DwarfEmitter(ElfWriter& elf, ElfWriter::Architecture arch = ElfWriter::ArchX86);

        // Begin a new compilation unit (one per module).
        void beginCompilationUnit(const QByteArray& filename,
                                  const QByteArray& directory,
                                  const QByteArray& producer = "micc (Micron Compiler)");

        void endCompilationUnit();

        void addSubprogram(const QByteArray& name, quint32 lowPC, quint32 highPC, quint32 line,
                           quint32 prologuePC = 0);
        void addParameter(const QByteArray& name, qint32 fpOffset, quint32 typeRef);
        void addLocalVariable(const QByteArray& name, qint32 fpOffset, quint32 typeRef);
        void endSubprogram();

        void addModuleVariable(const QByteArray& name, quint32 address, quint32 typeRef, quint32 line);

        quint32 addBaseType(const QByteArray& name, quint8 encoding, quint8 byteSize);
        quint32 addPointerType(quint32 baseTypeRef);

        quint32 addStructType(const QByteArray& name, quint32 byteSize);
        void addStructMember(const QByteArray& name, quint32 typeRef, quint32 byteOffset); // 0 for unions
        void endStructType();

        quint32 addArrayType(quint32 elemTypeRef, quint32 count);

        void addLineEntry(quint32 address, quint32 line, quint32 column = 0);

        // Write all DWARF sections (.debug_info, .debug_abbrev, .debug_line,
        // .debug_frame, .debug_str) to the ElfWriter.
        void finalize(quint32 textSymIdx, quint32 dataSymIdx);

        // Get or create a DWARF type DIE for a MIL Type.
        quint32 getOrCreateType(Type* t);

    private:
        void buildAbbrevTable();
        void buildDebugFrame();
        void buildDebugLine();

        static void appendULEB128(QByteArray& buf, quint32 value);
        static void appendSLEB128(QByteArray& buf, qint32 value);
        void appendString(QByteArray& buf, const QByteArray& str);
        quint32 addDebugString(const QByteArray& str);

        // Each abbreviation code identifies a DIE template in .debug_abbrev
        enum AbbrevCode {
            ABBREV_COMPILE_UNIT = 1,
            ABBREV_SUBPROGRAM = 2,
            ABBREV_PARAMETER = 3,
            ABBREV_VARIABLE = 4,
            ABBREV_BASE_TYPE = 5,
            ABBREV_POINTER_TYPE = 6,
            ABBREV_STRUCT_TYPE = 7,
            ABBREV_MEMBER = 8,
            ABBREV_ARRAY_TYPE = 9,
            ABBREV_SUBRANGE = 10,
            ABBREV_MODULE_VAR = 11  // DW_TAG_variable at CU scope (uses DW_OP_addr)
        };

        ElfWriter& d_elf;
        ElfWriter::Architecture d_arch;

        // Pre-created ELF section indices and their section symbol indices.
        // Created in the constructor so that debug section symbols are added
        // to the symbol table BEFORE any global (code) symbols, avoiding
        // index shifts that would break existing .rel.text relocations.
        quint32 d_abbrevSecIdx;
        quint32 d_infoSecIdx;
        quint32 d_strSecIdx;
        quint32 d_lineSecIdx;
        quint32 d_frameSecIdx;
        quint32 d_abbrevSymIdx;
        quint32 d_strSymIdx;
        quint32 d_lineSymIdx;

        QByteArray d_info; // .debug_info
        QByteArray d_abbrev; // .debug_abbrev
        QByteArray d_str; // .debug_str
        QByteArray d_line; // .debug_line
        QByteArray d_frame; // .debug_frame

        QMap<QByteArray, quint32> d_strOffsets; // string -> offset in .debug_str
        QMap<Type*, quint32> d_typeCache; // Type* -> .debug_info offset

        quint32 d_cuStartOffset; // offset in d_info where the CU header starts
        bool d_inCU; // true between beginCompilationUnit/endCompilationUnit

        struct LineEntry {
            quint32 address;
            quint32 line;
            quint32 column;
        };
        QList<LineEntry> d_lineEntries;
        static bool sortLineEntry(const DwarfEmitter::LineEntry& a, const DwarfEmitter::LineEntry& b) {
            return a.address < b.address;
        }
        // Source file info for line table
        QByteArray d_filename;
        QByteArray d_directory;

        quint32 d_lowPC;
        quint32 d_highPC;

        enum RelocTarget { RELOC_TEXT, RELOC_DATA, RELOC_STR, RELOC_ABBREV, RELOC_LINE };
        struct DwarfReloc {
            quint32 offset; // offset within the section (d_info or d_line)
            RelocTarget target; // which section symbol to relocate against
        };
        QList<DwarfReloc> d_infoRelocs; // for .debug_info
        QList<DwarfReloc> d_lineRelocs; // for .debug_line
        QList<DwarfReloc> d_frameRelocs; // for .debug_frame

        // Subprogram bounds for FDE generation
        struct SubprogramBounds {
            quint32 lowPC;
            quint32 highPC;
            quint32 prologuePC; // PC where push ebp; mov ebp, esp starts
        };
        QList<SubprogramBounds> d_fdes;

        void appendStringRef(const QByteArray& str);
    };

} // namespace Mil

#endif // MILDWARFEMITTER_H
