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

#include "MilElfReader.h"
#include <QFile>
using namespace Mil;

// ELF32 structure sizes
static const quint32 ELF32_EHDR_SIZE = 52;
static const quint32 ELF32_SHDR_SIZE = 40;
static const quint32 ELF32_SYM_SIZE = 16;
static const quint32 ELF32_REL_SIZE = 8;
static const quint32 ELF32_RELA_SIZE = 12;

// ELF identification bytes
static const unsigned char ELF_MAG0 = 0x7f;
static const unsigned char ELF_MAG1 = 'E';
static const unsigned char ELF_MAG2 = 'L';
static const unsigned char ELF_MAG3 = 'F';

// ELF constants
static const quint16 ET_REL = 1; // Relocatable file
static const quint16 EM_RISCV = 243; // Machine ID for RISC-V
static const quint16 EM_ARM = 40; // Machine ID for ARM
static const quint16 EM_386 = 3;
static const quint8 ELFCLASS32 = 1;
static const quint8 ELFDATA2LSB = 1; // Little-endian

// Section types
static const quint32 SHT_SYMTAB = 2;
static const quint32 SHT_STRTAB = 3;
static const quint32 SHT_RELA = 4;
static const quint32 SHT_REL = 9;


inline quint32 ElfReader::readLE32(const char* buf)
{
    const unsigned char* p = reinterpret_cast<const unsigned char*>(buf);
    return quint32(p[0]) | (quint32(p[1]) << 8)
         | (quint32(p[2]) << 16) | (quint32(p[3]) << 24);
}

inline quint16 ElfReader::readLE16(const char* buf)
{
    const unsigned char* p = reinterpret_cast<const unsigned char*>(buf);
    return quint16(p[0]) | (quint16(p[1]) << 8);
}

ElfReader::ElfReader(): d_valid(false),d_arch(ArchUnknown),d_elfFlags(0)
{
}

bool ElfReader::loadFromFile(const QString& filename)
{
    QFile file(filename);
    if (!file.open(QIODevice::ReadOnly)) {
        d_error = QString("Cannot open file: %1").arg(filename);
        d_valid = false;
        return false;
    }
    QByteArray data = file.readAll();
    file.close();
    return loadFromData(data);
}

bool ElfReader::loadFromData(const QByteArray& data)
{
    d_sections.clear();
    d_symbols.clear();
    d_relocations.clear();
    d_fileData.clear();
    d_valid = false;
    d_error.clear();

    if (!parse(data)) {
        return false;
    }

    d_fileData = data;
    d_valid = true;
    return true;
}

bool ElfReader::parse(const QByteArray& data)
{
    // Check minimum size for ELF header
    if (quint32(data.size()) < ELF32_EHDR_SIZE) {
        d_error = "File too small for ELF header";
        return false;
    }

    const char* raw = data.constData();

    // Verify ELF magic
    if (quint8(raw[0]) != ELF_MAG0 ||
        quint8(raw[1]) != ELF_MAG1 ||
        quint8(raw[2]) != ELF_MAG2 ||
        quint8(raw[3]) != ELF_MAG3) {
        d_error = "Not an ELF file (bad magic)";
        return false;
    }

    // Check ELF class (32-bit)
    if (quint8(raw[4]) != ELFCLASS32) {
        d_error = "Not a 32-bit ELF file";
        return false;
    }

    // Check endianness (little-endian)
    if (quint8(raw[5]) != ELFDATA2LSB) {
        d_error = "Not a little-endian ELF file";
        return false;
    }

    // Check file type (relocatable)
    quint16 e_type = readLE16(raw + 16);
    if (e_type != ET_REL) {
        d_error = QString("Not a relocatable ELF file (type=%1)").arg(e_type);
        return false;
    }

    // Check machine
    quint16 e_machine = readLE16(raw + 18);
    if (e_machine == EM_ARM) {
        d_arch = ArchARM;
    } else if (e_machine == EM_RISCV) {
        d_arch = ArchRISCV;
    } else if (e_machine == EM_386) {
        d_arch = ArchX86;
    } else {
        d_error = QString("Unsupported machine architecture (id=%1)").arg(e_machine);
        return false;
    }

    // Read ELF flags (e.g. RV32 ISA+ABI flags)
    d_elfFlags = readLE32(raw + 36);

    // Read section header table info
    quint32 e_shoff = readLE32(raw + 32);      // Section header table offset
    quint16 e_shentsize = readLE16(raw + 46);   // Size of each section header
    quint16 e_shnum = readLE16(raw + 48);       // Number of section headers
    quint16 e_shstrndx = readLE16(raw + 50);    // Section header string table index

    if (e_shentsize < ELF32_SHDR_SIZE) {
        d_error = "Invalid section header entry size";
        return false;
    }

    // Validate section header table fits in file
    quint32 shTableEnd = e_shoff + quint32(e_shnum) * e_shentsize;
    if (shTableEnd > quint32(data.size())) {
        d_error = "Section header table extends beyond file";
        return false;
    }

    // Read all section headers
    for (quint16 i = 0; i < e_shnum; i++) {
        const char* sh = raw + e_shoff + i * e_shentsize;
        SectionInfo sec;
        sec.type = readLE32(sh + 4);
        sec.flags = readLE32(sh + 8);
        sec.offset = readLE32(sh + 16);
        sec.size  = readLE32(sh + 20);
        sec.link  = readLE32(sh + 24);
        sec.info  = readLE32(sh + 28);
        sec.alignment = readLE32(sh + 32);
        sec.entsize   = readLE32(sh + 36);
        // Name will be resolved after we have the string table
        d_sections.append(sec);
    }

    // Resolve section names from .shstrtab
    if (e_shstrndx < e_shnum) {
        const SectionInfo& shstrtab = d_sections[e_shstrndx];
        if (shstrtab.offset + shstrtab.size <= quint32(data.size())) {
            for (int i = 0; i < d_sections.size(); i++) {
                const char* sh = raw + e_shoff + i * e_shentsize;
                quint32 nameOff = readLE32(sh + 0);
                if (nameOff < shstrtab.size) {
                    const char* namePtr = raw + shstrtab.offset + nameOff;
                    // Find null terminator within bounds
                    quint32 maxLen = shstrtab.size - nameOff;
                    quint32 len = 0;
                    while (len < maxLen && namePtr[len] != '\0')
                        len++;
                    d_sections[i].name = QByteArray(namePtr, len);
                }
            }
        }
    }

    // Find and parse .symtab and its associated .strtab
    for (int i = 0; i < d_sections.size(); i++) {
        if (d_sections[i].type == SHT_SYMTAB) {
            quint32 strtabIdx = d_sections[i].link;
            if (strtabIdx < quint32(d_sections.size()) &&
                d_sections[strtabIdx].type == SHT_STRTAB) {
                parseSymbols(data,
                             d_sections[i].offset, d_sections[i].size,
                             d_sections[strtabIdx].offset, d_sections[strtabIdx].size);
            }
            break; // Only one .symtab expected
        }
    }

    // Find and parse .rel.* / .rela.* sections
    for (int i = 0; i < d_sections.size(); i++) {
        quint32 type = d_sections[i].type;
        if (type == SHT_REL || type == SHT_RELA) {
            parseRelocations(data, i,
                             d_sections[i].offset, d_sections[i].size,
                             d_sections[i].info, type);
        }
    }

    return true;
}

bool ElfReader::parseSymbols(const QByteArray& data, quint32 symtabOff, quint32 symtabSize,
                              quint32 strtabOff, quint32 strtabSize)
{
    if (symtabOff + symtabSize > quint32(data.size()))
        return false;
    if (strtabOff + strtabSize > quint32(data.size()))
        return false;

    const char* raw = data.constData();
    quint32 numSyms = symtabSize / ELF32_SYM_SIZE;

    for (quint32 i = 0; i < numSyms; i++) {
        const char* sym = raw + symtabOff + i * ELF32_SYM_SIZE;
        Symbol s;
        quint32 nameOff = readLE32(sym + 0);
        s.value   = readLE32(sym + 4);
        s.size    = readLE32(sym + 8);
        quint8 info = quint8(sym[12]);
        s.binding = info >> 4;
        s.type    = info & 0xF;
        s.shndx   = readLE16(sym + 14);

        // Resolve name from .strtab
        if (nameOff < strtabSize) {
            const char* namePtr = raw + strtabOff + nameOff;
            quint32 maxLen = strtabSize - nameOff;
            quint32 len = 0;
            while (len < maxLen && namePtr[len] != '\0')
                len++;
            s.name = QByteArray(namePtr, len);
        }

        d_symbols.append(s);
    }
    return true;
}

bool ElfReader::parseRelocations(const QByteArray& data, int sectionIdx,
                                  quint32 relOff, quint32 relSize,  quint32 targetSection, quint32 relType)
{
    if (relOff + relSize > quint32(data.size()))
        return false;

    const char* raw = data.constData();
    const bool isRela = (relType == SHT_RELA);
    const quint32 entrySize = isRela ? ELF32_RELA_SIZE : ELF32_REL_SIZE;
    const quint32 numRels = relSize / entrySize;

    for (quint32 i = 0; i < numRels; i++) {
        const char* rel = raw + relOff + i * entrySize;
        Relocation r;
        r.offset = readLE32(rel + 0);
        const quint32 info = readLE32(rel + 4);
        r.symbolIdx = info >> 8;
        r.type = (info & 0xFF);
        r.section = quint32(sectionIdx);
        r.targetSection = targetSection;

        // Parse explicit addend for RISC-V/RELA, default to 0 for ARM/REL
        if (isRela) {
            r.addend = static_cast<qint32>(readLE32(rel + 8));
        } else {
            r.addend = 0; // Addend is stored inline in the instruction for REL
        }

        d_relocations.append(r);
    }
    return true;
}

QByteArray ElfReader::sectionName(int index) const
{
    if (index < 0 || index >= d_sections.size())
        return QByteArray();
    return d_sections[index].name;
}

QByteArray ElfReader::sectionData(int index) const
{
    if (index < 0 || index >= d_sections.size())
        return QByteArray();
    if (!d_valid || d_fileData.isEmpty())
        return QByteArray();

    const SectionInfo& sec = d_sections[index];
    if (sec.offset + sec.size > quint32(d_fileData.size()))
        return QByteArray();

    return QByteArray(d_fileData.constData() + sec.offset, sec.size);
}

quint32 ElfReader::sectionType(int index) const
{
    if (index < 0 || index >= d_sections.size())
        return 0;
    return d_sections[index].type;
}

quint32 ElfReader::sectionFlags(int index) const
{
    if (index < 0 || index >= d_sections.size())
        return 0;
    return d_sections[index].flags;
}

int ElfReader::findSection(const QByteArray& name) const
{
    for (int i = 0; i < d_sections.size(); i++) {
        if (d_sections[i].name == name)
            return i;
    }
    return -1;
}

QByteArray ElfReader::readMicronModSection() const
{
    int idx = findSection(QByteArray(".micron.mod"));
    if (idx < 0)
        return QByteArray();
    return sectionData(idx);
}

int ElfReader::findSymbol(const QByteArray& name) const
{
    for (int i = 0; i < d_symbols.size(); i++) {
        if (d_symbols[i].name == name)
            return i;
    }
    return -1;
}
