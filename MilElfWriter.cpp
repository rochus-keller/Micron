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
#include <QFile>
#include <QtEndian>

using namespace Mil;

// ELF32 structure sizes (we write these manually to avoid host <elf.h> dependency
// and to ensure correct cross-compilation from any host)
static const quint32 ELF32_EHDR_SIZE = 52;
static const quint32 ELF32_SHDR_SIZE = 40;
static const quint32 ELF32_SYM_SIZE  = 16;
static const quint32 ELF32_REL_SIZE  =  8;
static const quint16 EM_ARM = 40;
static const quint16 EM_RISCV = 243;
static const quint16 EM_386 = 3;
static const quint32 SHT_RELA = 4;
static const quint32 ELF32_RELA_SIZE = 12;

// ELF magic and identification
static const unsigned char ELF_IDENT[16] = {
    0x7f, 'E', 'L', 'F', // EI_MAG
    1, // EI_CLASS = ELFCLASS32
    1, // EI_DATA = ELFDATA2LSB (little-endian)
    1, // EI_VERSION = EV_CURRENT
    0, // EI_OSABI = ELFOSABI_NONE
    0, 0, 0, 0, 0, 0, 0, 0  // EI_ABIVERSION + padding
};

// Helper: write a little-endian 32-bit value into a buffer
static inline void putLE32(char* buf, quint32 val)
{
    buf[0] = char(val & 0xFF);
    buf[1] = char((val >> 8) & 0xFF);
    buf[2] = char((val >> 16) & 0xFF);
    buf[3] = char((val >> 24) & 0xFF);
}

// Helper: write a little-endian 16-bit value into a buffer
static inline void putLE16(char* buf, quint16 val)
{
    buf[0] = char(val & 0xFF);
    buf[1] = char((val >> 8) & 0xFF);
}

ElfWriter::ElfWriter(Architecture arch):d_arch(arch), d_elfFlags(0),
    d_firstGlobal(1) // STN_UNDEF (index 0) counts as a local symbol
{   
    // Default ABI flags
    // ARM typically uses 0x05000000 (EF_ARM_ABI_VER5)
    // RISC-V typically uses 0 (or 1 if RVC compressed instructions are strictly enforced)
    if (d_arch == ArchARM)
        d_elfFlags = 0x05000000;
    else
        d_elfFlags = 0x00000000;

    // Section 0: SHN_UNDEF (null section, required by ELF)
    Section nullSec;
    nullSec.type = SHT_NULL;
    nullSec.flags = 0;
    nullSec.alignment = 0;
    nullSec.link = 0;
    nullSec.info = 0;
    nullSec.entsize = 0;
    d_sections.append(nullSec);

    // Symbol 0: STN_UNDEF (null symbol, required by ELF)
    Symbol nullSym;
    nullSym.value = 0;
    nullSym.size = 0;
    nullSym.info = 0;
    nullSym.other = 0;
    nullSym.shndx = 0;
    d_symbols.append(nullSym);

    // Create .shstrtab (section header string table) - always present
    d_shstrtabIdx = addSection(QByteArray(".shstrtab"), SHT_STRTAB, 0, 1);

    // Create .strtab (symbol string table)
    d_strtabIdx = addSection(QByteArray(".strtab"), SHT_STRTAB, 0, 1);

    // Create .symtab (symbol table)
    d_symtabIdx = addSection(QByteArray(".symtab"), SHT_SYMTAB, 0, 4);
    d_sections[d_symtabIdx].link = d_strtabIdx;
    d_sections[d_symtabIdx].entsize = ELF32_SYM_SIZE;
    // info will be set to first global symbol index at write time
}

quint32 ElfWriter::addSection(const QByteArray& name, quint32 type, quint32 flags, quint32 alignment)
{
    Section sec;
    sec.name = name;
    sec.type = type;
    sec.flags = flags;
    sec.alignment = alignment;
    sec.link = 0;
    sec.info = 0;
    sec.entsize = 0;

    // Add the name to .shstrtab (if .shstrtab already exists)
    if (d_shstrtabIdx > 0 && d_shstrtabIdx < quint32(d_sections.size())) {
        // The name offset will be resolved at build time
    }

    d_sections.append(sec);
    return d_sections.size() - 1;
}

void ElfWriter::appendToSection(quint32 sectionIdx, const char* data, quint32 len)
{
    Q_ASSERT(sectionIdx > 0 && sectionIdx < quint32(d_sections.size()));
    d_sections[sectionIdx].data.append(data, len);
}

void ElfWriter::appendToSection(quint32 sectionIdx, const QByteArray& data)
{
    Q_ASSERT(sectionIdx > 0 && sectionIdx < quint32(d_sections.size()));
    d_sections[sectionIdx].data.append(data);
}

quint32 ElfWriter::sectionSize(quint32 sectionIdx) const
{
    Q_ASSERT(sectionIdx < quint32(d_sections.size()));
    return d_sections[sectionIdx].data.size();
}

void ElfWriter::patchWord(quint32 sectionIdx, quint32 offset, quint32 value)
{
    Q_ASSERT(sectionIdx > 0 && sectionIdx < quint32(d_sections.size()));
    Q_ASSERT(offset + 4 <= quint32(d_sections[sectionIdx].data.size()));
    memcpy(d_sections[sectionIdx].data.data() + offset, &value, 4);
}

ElfWriter::StandardSections ElfWriter::createStandardSections()
{
    StandardSections ss;
    ss.text = addSection(QByteArray(".text"), SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 4);
    ss.data = addSection(QByteArray(".data"), SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, 4);
    ss.bss = addSection(QByteArray(".bss"), SHT_NOBITS,   SHF_ALLOC | SHF_WRITE, 4);
    ss.rodata = addSection(QByteArray(".rodata"), SHT_PROGBITS, SHF_ALLOC, 4);

    const bool useRela = (d_arch == ArchRISCV);
    QByteArray relPrefix = useRela ? ".rela" : ".rel";
    quint32 relType = useRela ? SHT_RELA : SHT_REL;
    quint32 relEntSize = useRela ? ELF32_RELA_SIZE : ELF32_REL_SIZE;

    // Relocations for text
    ss.relText = addSection(relPrefix + ".text", relType, 0, 4);
    d_sections[ss.relText].link = d_symtabIdx;
    d_sections[ss.relText].info = ss.text;
    d_sections[ss.relText].entsize = relEntSize;

    // Relocations for data
    ss.relData = addSection(relPrefix + ".data", relType, 0, 4);
    d_sections[ss.relData].link = d_symtabIdx;
    d_sections[ss.relData].info = ss.data;
    d_sections[ss.relData].entsize = relEntSize;

    ss.micronMod = 0; // not created by default
    return ss;
}

quint32 ElfWriter::addSymbol(const QByteArray& name, quint32 section,
                              quint32 value, quint32 size,
                              quint8 binding, quint8 type)
{
    Symbol sym;
    sym.name = name;
    sym.value = value;
    sym.size = size;
    sym.info = quint8((binding << 4) | (type & 0xF));
    sym.other = 0;
    sym.shndx = quint16(section);

    // ELF requires all local symbols before global symbols.
    // We insert locals at the current position, globals at the end.
    if (binding == STB_LOCAL) {
        // Insert before the first global symbol
        d_symbols.insert(d_firstGlobal, sym);
        d_firstGlobal++;
        return d_firstGlobal - 1;
    } else {
        d_symbols.append(sym);
        return d_symbols.size() - 1;
    }
}

quint32 ElfWriter::addSectionSymbol(quint32 sectionIdx)
{
    return addSymbol(QByteArray(), sectionIdx, 0, 0, STB_LOCAL, STT_SECTION);
}

void ElfWriter::setSymbolValue(quint32 symbolIdx, quint32 value)
{
    Q_ASSERT(symbolIdx < quint32(d_symbols.size()));
    d_symbols[symbolIdx].value = value;
}

void ElfWriter::configureRelSection(quint32 relSectionIdx, quint32 targetSectionIdx)
{
    Q_ASSERT(relSectionIdx > 0 && relSectionIdx < quint32(d_sections.size()));
    quint32 type = d_sections[relSectionIdx].type;
    Q_ASSERT(type == SHT_REL || type == SHT_RELA); // Allow SHT_RELA

    d_sections[relSectionIdx].link = d_symtabIdx;
    d_sections[relSectionIdx].info = targetSectionIdx;
    d_sections[relSectionIdx].entsize = (type == SHT_RELA) ? ELF32_RELA_SIZE : ELF32_REL_SIZE;
}

void ElfWriter::addRelocation(quint32 relSection, quint32 offset,
                               quint32 symbolIdx, quint8 relocType, qint32 addend)
{
    Q_ASSERT(relSection > 0 && relSection < quint32(d_sections.size()));
    const quint32 type = d_sections[relSection].type;
    Q_ASSERT(type == SHT_REL || type == SHT_RELA);

    const bool isRela = (type == SHT_RELA);
    char rel[12]; // Max size is 12 for RELA

    putLE32(rel, offset);
    putLE32(rel + 4, (quint32(symbolIdx) << 8) | (relocType & 0xFF));

    if (isRela) {
        // Write the extra 4 bytes for the explicit addend
        putLE32(rel + 8, quint32(addend));
        d_sections[relSection].data.append(rel, 12);
    } else {
        d_sections[relSection].data.append(rel, 8);
    }
}

quint32 ElfWriter::addToStrtab(const QByteArray& str)
{
    QByteArray& strtab = d_sections[d_strtabIdx].data;
    if (strtab.isEmpty()) {
        strtab.append('\0'); // First byte must be null
    }
    quint32 offset = strtab.size();
    strtab.append(str);
    strtab.append('\0');
    return offset;
}

quint32 ElfWriter::addToShstrtab(const QByteArray& str)
{
    QByteArray& shstrtab = d_sections[d_shstrtabIdx].data;
    if (shstrtab.isEmpty()) {
        shstrtab.append('\0'); // First byte must be null
    }
    quint32 offset = shstrtab.size();
    shstrtab.append(str);
    shstrtab.append('\0');
    return offset;
}

bool ElfWriter::write(const QString& filename)
{
    QByteArray elf = buildElf();
    if (elf.isEmpty())
        return false;

    QFile file(filename);
    if (!file.open(QIODevice::WriteOnly))
        return false;

    qint64 written = file.write(elf);
    file.close();
    return written == elf.size();
}

QByteArray ElfWriter::toByteArray()
{
    return buildElf();
}

QByteArray ElfWriter::buildElf()
{
    // Build .shstrtab content (section names)
    // Clear and rebuild so offsets are correct
    d_sections[d_shstrtabIdx].data.clear();
    QList<quint32> shNameOffsets;
    for (int i = 0; i < d_sections.size(); i++) {
        if (i == 0) {
            shNameOffsets.append(addToShstrtab(QByteArray(""))); // null section name = ""
            // addToShstrtab adds a leading \0 on first call, so "" at offset 0 works
            // Actually we want offset 0 which is the initial \0
            shNameOffsets[0] = 0;
        } else {
            shNameOffsets.append(addToShstrtab(d_sections[i].name));
        }
    }

    // Build .strtab content (symbol names)
    d_sections[d_strtabIdx].data.clear();
    QList<quint32> symNameOffsets;
    for (int i = 0; i < d_symbols.size(); i++) {
        if (i == 0 || d_symbols[i].name.isEmpty()) {
            symNameOffsets.append(0); // null/empty name -> offset 0
        } else {
            symNameOffsets.append(addToStrtab(d_symbols[i].name));
        }
    }

    // Build .symtab content
    d_sections[d_symtabIdx].data.clear();
    d_sections[d_symtabIdx].info = d_firstGlobal; // sh_info = index of first global symbol
    for (int i = 0; i < d_symbols.size(); i++) {
        char sym[ELF32_SYM_SIZE];
        putLE32(sym + 0, symNameOffsets[i]); // st_name
        putLE32(sym + 4, d_symbols[i].value); // st_value
        putLE32(sym + 8, d_symbols[i].size); // st_size
        sym[12] = char(d_symbols[i].info); // st_info
        sym[13] = char(d_symbols[i].other); // st_other
        putLE16(sym + 14, d_symbols[i].shndx); // st_shndx
        d_sections[d_symtabIdx].data.append(sym, ELF32_SYM_SIZE);
    }

    // Compute file layout
    // Layout: ELF header | section data (aligned) | section header table
    quint32 offset = ELF32_EHDR_SIZE;
    QList<quint32> sectionOffsets;

    for (int i = 0; i < d_sections.size(); i++) {
        if (i == 0 || d_sections[i].type == SHT_NOBITS) {
            sectionOffsets.append(0);
            continue;
        }
        // Align offset
        quint32 align = d_sections[i].alignment;
        if (align < 1) align = 1;
        offset = (offset + align - 1) & ~(align - 1);
        sectionOffsets.append(offset);
        offset += d_sections[i].data.size();
    }

    // Align section header table to 4 bytes
    offset = (offset + 3) & ~3;
    quint32 shoff = offset;

    // Build the ELF binary
    quint32 totalSize = shoff + d_sections.size() * ELF32_SHDR_SIZE;
    QByteArray result;
    result.resize(totalSize);
    result.fill('\0');
    char* out = result.data();

    // ELF Header
    quint16 e_machine;
    switch (d_arch) {
    case ArchX86:  e_machine = EM_386;   break;
    case ArchRISCV: e_machine = EM_RISCV; break;
    default:       e_machine = EM_ARM;   break;
    }
    memcpy(out, ELF_IDENT, 16); // e_ident
    putLE16(out + 16, 1); // e_type = ET_REL
    putLE16(out + 18, e_machine); // Dynamic machine type
    putLE32(out + 20, 1);  // e_version = EV_CURRENT
    putLE32(out + 24, 0);  // e_entry = 0 (relocatable)
    putLE32(out + 28, 0);  // e_phoff = 0 (no program headers)
    putLE32(out + 32, shoff); // e_shoff
    putLE32(out + 36, d_elfFlags); // Dynamic flags (0 for RV32, 0x05000000 for ARM)
    putLE16(out + 40, ELF32_EHDR_SIZE); // e_ehsize
    putLE16(out + 42, 0);  // e_phentsize
    putLE16(out + 44, 0);  // e_phnum
    putLE16(out + 46, ELF32_SHDR_SIZE);  // e_shentsize
    putLE16(out + 48, d_sections.size());  // e_shnum
    putLE16(out + 50, d_shstrtabIdx);  // e_shstrndx

    // Section data
    for (int i = 1; i < d_sections.size(); i++) {
        if (d_sections[i].type == SHT_NOBITS)
            continue;
        if (sectionOffsets[i] > 0 && !d_sections[i].data.isEmpty()) {
            memcpy(out + sectionOffsets[i], d_sections[i].data.constData(),
                   d_sections[i].data.size());
        }
    }

    // Section Header Table
    char* shdr = out + shoff;
    for (int i = 0; i < d_sections.size(); i++) {
        char* sh = shdr + i * ELF32_SHDR_SIZE;
        putLE32(sh + 0,  (i < shNameOffsets.size()) ? shNameOffsets[i] : 0); // sh_name
        putLE32(sh + 4,  d_sections[i].type);      // sh_type
        putLE32(sh + 8,  d_sections[i].flags);     // sh_flags
        putLE32(sh + 12, 0);                        // sh_addr (0 for relocatable)
        putLE32(sh + 16, sectionOffsets[i]);         // sh_offset
        if (d_sections[i].type == SHT_NOBITS) {
            putLE32(sh + 20, 0);                     // sh_size for BSS: set by user via bssSize if needed
        } else {
            putLE32(sh + 20, d_sections[i].data.size()); // sh_size
        }
        putLE32(sh + 24, d_sections[i].link);     // sh_link
        putLE32(sh + 28, d_sections[i].info);        // sh_info
        putLE32(sh + 32, d_sections[i].alignment);   // sh_addralign
        putLE32(sh + 36, d_sections[i].entsize);     // sh_entsize
    }

    return result;
}
