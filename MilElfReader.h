#ifndef MILELFREADER_H
#define MILELFREADER_H

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
#include <QString>
#include <QList>
#include <QtGlobal>

namespace Mil
{
    // Lightweight ELF32 reader for extracting sections from relocatable object files.
    // only handles ELF32 little-endian (ARM32 & RV32) relocatable objects
    // Mostly used so far to get MIL specs from micron.mod sections

    class ElfReader
    {
    public:
        enum Architecture {
            ArchUnknown,
            ArchARM,
            ArchRISCV,
            ArchX86
        };

        ElfReader();

        // Load an ELF file from disk. Returns true on success.
        bool loadFromFile(const QString& filename);

        // Load from an in-memory buffer. Returns true on success.
        bool loadFromData(const QByteArray& data);

        bool isValid() const { return d_valid; }

        Architecture machine() const { return d_arch; }

        QString errorMessage() const { return d_error; }

        int sectionCount() const { return d_sections.size(); }

        QByteArray sectionName(int index) const;

        // Get a section's raw data
        QByteArray sectionData(int index) const;

        // Get a section's type (SHT_PROGBITS, SHT_SYMTAB, etc.)
        quint32 sectionType(int index) const;

        // Get a section's flags (SHF_ALLOC, SHF_WRITE, etc.)
        quint32 sectionFlags(int index) const;

        // Find a section by name. Returns the section index, or -1 if not found.
        int findSection(const QByteArray& name) const;

        // Read the .micron.mod section and return its contents as a QByteArray (UTF-8).
        // Returns an empty QByteArray if the section is not present.
        QByteArray readMicronModSection() const;

        struct Symbol {
            QByteArray name;
            quint32 value;
            quint32 size;
            quint8 binding;   // STB_LOCAL, STB_GLOBAL, STB_WEAK
            quint8 type;      // STT_NOTYPE, STT_OBJECT, STT_FUNC, STT_SECTION
            quint16 shndx;    // Section index
        };

        // Get all symbols from the .symtab section.
        QList<Symbol> symbols() const { return d_symbols; }

        // Find a symbol by name. Returns the index in the symbol list, or -1.
        int findSymbol(const QByteArray& name) const;

        struct Relocation {
            quint32 offset;
            quint32 symbolIdx;
            quint8 type;
            quint32 section;   // Which .rel.xxx section this came from
            quint32 targetSection; // The section being relocated (from sh_info)
            qint32 addend;
        };

        QList<Relocation> relocations() const { return d_relocations; }

    private:
        bool parse(const QByteArray& data);
        bool parseSymbols(const QByteArray& data, quint32 symtabOff, quint32 symtabSize,
                          quint32 strtabOff, quint32 strtabSize);
        bool parseRelocations(const QByteArray& data, int sectionIdx,
                              quint32 relOff, quint32 relSize, quint32 targetSection, quint32 relType);

        static inline quint32 readLE32(const char* buf);
        static inline quint16 readLE16(const char* buf);

        struct SectionInfo {
            QByteArray name;
            quint32 type;
            quint32 flags;
            quint32 offset;
            quint32 size;
            quint32 link;
            quint32 info;
            quint32 alignment;
            quint32 entsize;
        };

        QList<SectionInfo> d_sections;
        QList<Symbol> d_symbols;
        QList<Relocation> d_relocations;
        QByteArray d_fileData;  // Retain the file data for section access
        bool d_valid;
        QString d_error;
        Architecture d_arch;
    };

} // namespace Mil

#endif // MILELFREADER_H
