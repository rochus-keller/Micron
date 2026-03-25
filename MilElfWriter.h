#ifndef MILELFWRITER_H
#define MILELFWRITER_H

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
#include <QString>
#include <QtGlobal>

namespace Mil
{
    // ELF symbol binding
    enum ElfSymBinding {
        STB_LOCAL = 0,
        STB_GLOBAL = 1,
        STB_WEAK  = 2
    };

    // ELF symbol type
    enum ElfSymType {
        STT_NOTYPE = 0,
        STT_OBJECT = 1,
        STT_FUNC = 2,
        STT_SECTION = 3,
        STT_FILE = 4
    };

    // ELF section flags
    enum ElfSectionFlags {
        SHF_WRITE = 0x1,
        SHF_ALLOC = 0x2,
        SHF_EXECINSTR = 0x4,
        SHF_STRINGS = 0x20
    };

    // ELF section types
    enum ElfSectionType {
        SHT_NULL = 0,
        SHT_PROGBITS = 1,
        SHT_SYMTAB = 2,
        SHT_STRTAB = 3,
        SHT_REL  = 9,
        SHT_NOBITS = 8
    };

    class ElfWriter
    {
    public:
        enum Architecture {
            ArchUnknown,
            ArchARM,
            ArchRISCV,
            ArchX86
        };

        ElfWriter(Architecture);

        // Section management
        // Returns 1-based section index (0 is always SHN_UNDEF)
        quint32 addSection(const QByteArray& name, quint32 type, quint32 flags,
                           quint32 alignment = 4);
        void appendToSection(quint32 sectionIdx, const char* data, quint32 len);
        void appendToSection(quint32 sectionIdx, const QByteArray& data);
        quint32 sectionSize(quint32 sectionIdx) const;
        void patchWord(quint32 sectionIdx, quint32 offset, quint32 value);

        // Convenience: create standard sections and return their indices
        struct StandardSections {
            quint32 text;  // .text
            quint32 data;  // .data
            quint32 bss;   // .bss
            quint32 rodata; // .rodata
            quint32 relText; // .rel.text
            quint32 relData; // .rel.data
            quint32 micronMod; // .micron.mod (optional, 0 if not created)
        };
        StandardSections createStandardSections();

        // Symbol management
        // Returns symbol index in .symtab
        quint32 addSymbol(const QByteArray& name, quint32 section,
                          quint32 value, quint32 size,
                          quint8 binding, quint8 type);

        // Add a section symbol (STT_SECTION) for the given section index
        quint32 addSectionSymbol(quint32 sectionIdx);

        // Update the value (offset within section) of an existing symbol.
        // Used by the renderer to set procedure offsets after code generation.
        void setSymbolValue(quint32 symbolIdx, quint32 value);

        // Relocation management
        // Adds a REL entry to the appropriate .rel.xxx section
        void addRelocation(quint32 relSection, quint32 offset,
                           quint32 symbolIdx, quint8 relocType, qint32 addend = 0);

        // Configure a SHT_REL section's link (symtab), info (target section), entsize.
        // Call this after addSection() for custom .rel.* sections (e.g. .rel.debug_info).
        void configureRelSection(quint32 relSectionIdx, quint32 targetSectionIdx);

        // Write the complete ELF file
        bool write(const QString& filename);

        // Write to a QByteArray (for testing)
        QByteArray toByteArray();

    private:
        struct Section {
            QByteArray name;
            quint32 type;
            quint32 flags;
            quint32 alignment;
            QByteArray data;
            quint32 link;   // sh_link (symtab -> strtab)
            quint32 info;   // sh_info (symtab -> first global, rel -> target section)
            quint32 entsize; // sh_entsize, sizeof(Elf32_Sym), sizeof(Elf32_Rel)
        };

        struct Symbol {
            QByteArray name;
            quint32 value;
            quint32 size;
            quint8 info; // ELF32_ST_INFO(bind, type)
            quint8 other;
            quint16 shndx;
        };

        struct Relocation {
            quint32 offset;
            quint32 info; // ELF32_R_INFO(sym, type)
        };

        // Internal section indices for .symtab and .strtab (created in constructor)
        quint32 d_symtabIdx;
        quint32 d_strtabIdx;
        quint32 d_shstrtabIdx;

        QList<Section> d_sections; // index 0 = SHN_UNDEF (null section)
        QList<Symbol> d_symbols; // index 0 = STN_UNDEF
        quint32 d_firstGlobal; // index of first global symbol

        // String table helpers
        quint32 addToStrtab(const QByteArray& str);
        quint32 addToShstrtab(const QByteArray& str);

        // Build the final binary
        QByteArray buildElf();

        Architecture d_arch;
        quint32 d_elfFlags; // Allows overriding processor-specific flags
    };

} // namespace Mil

#endif // MILELFWRITER_H
