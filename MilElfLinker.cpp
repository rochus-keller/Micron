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

#include "MilElfLinker.h"
#include <QFile>
#include <QSet>
#include <QStringList>
#include <QDateTime>
#include <QFileInfo>
#include <QCryptographicHash>
using namespace Mil;

// ELF Section flags 
#define SHF_WRITE 0x1
#define SHF_ALLOC 0x2
#define SHF_EXECINSTR 0x4
#define SHT_PROGBITS 1
#define SHT_NOBITS 8
#define SHT_STRTAB 3
#define STB_GLOBAL 1
#define STB_WEAK 2
#define SHN_ABS 0xFFF1
#define SHN_COMMON 0xFFF2


ElfLinker::ElfLinker() : d_arch(ElfReader::ArchUnknown), d_baseAddress(0x08048000), d_bssSize(0), d_gotAddr(0) {}

ElfLinker::~ElfLinker() {
    for (int i = 0; i < d_files.size(); ++i) {
        delete d_files[i]->reader;
        delete d_files[i];
    }
    for (int i = 0; i < d_archives.size(); ++i) {
        delete d_archives[i];
    }
}

bool ElfLinker::addFile(const QString& filename) {
    QFile f(filename);
    if (!f.open(QIODevice::ReadOnly)) {
        d_error = "Cannot open file: " + filename;
        return false;
    }
    return addFileFromData(f.readAll(), filename);
}

bool ElfLinker::addFileFromData(const QByteArray& data, const QString& identifier) {
    ElfReader* reader = new ElfReader();
    if (!reader->loadFromData(data)) {
        d_error = "Invalid ELF object: " + identifier + " (" + reader->errorMessage() + ")";
        delete reader;
        return false;
    }

    if (d_arch == ElfReader::ArchUnknown) {
        d_arch = reader->machine();
    } else if (d_arch != reader->machine()) {
        d_error = "Architecture mismatch in " + identifier;
        delete reader;
        return false;
    }

    InputFile* file = new InputFile();
    file->reader = reader;
    file->filename = identifier;
    d_files.append(file);
    return true;
}

bool ElfLinker::addArchive(const QString& filename) {
    QFile f(filename);
    if (!f.open(QIODevice::ReadOnly)) {
        d_error = "Cannot open archive: " + filename;
        return false;
    }
    QByteArray data = f.readAll();
    if (!data.startsWith("!<arch>\n")) {
        d_error = "Not a valid static archive: " + filename;
        return false;
    }

    Archive* arc = new Archive();
    arc->filename = filename;
    arc->data = data;

    // First file in GNU/SysV archives is usually the symbol index '/'
    const char* ptr = data.constData() + 8;
    if (qstrncmp(ptr, "/", 1) == 0 || qstrncmp(ptr, "//", 2) == 0) {
        int size = QByteArray(ptr + 48, 10).trimmed().toInt();
        const char* symData = ptr + 60; // Skip 60-byte header
        
        quint32 numSyms = readBE32(symData);
        const char* offsets = symData + 4;
        const char* strings = symData + 4 + (numSyms * 4);

        for (quint32 i = 0; i < numSyms; ++i) {
            quint32 offset = readBE32(offsets + (i * 4));
            arc->symbolOffsets[QByteArray(strings)].append(offset);
            strings += qstrlen(strings) + 1; // Advance past null terminator
        }
    }
    d_archives.append(arc);
    return true;
}

bool ElfLinker::extractFromArchives() {
    bool extractedNew = false;
    do {
        extractedNew = false;
        // Gather undefined and weak-only symbols
        QHash<QByteArray, bool> undefined;   // truly undefined (shndx==0)
        QHash<QByteArray, bool> weakOnly;    // defined only as weak, no strong def yet
        QSet<QByteArray> strongDefined;      // has a strong (global) definition

        for (int i = 0; i < d_files.size(); ++i) {
            QList<ElfReader::Symbol> syms = d_files[i]->reader->symbols();
            for (int s = 0; s < syms.size(); ++s) {
                const ElfReader::Symbol& sym = syms[s];
                if (sym.name.isEmpty())
                    continue;
                if (sym.shndx == 0) {
                    if (!strongDefined.contains(sym.name))
                        undefined[sym.name] = true;
                } else if (sym.binding == STB_GLOBAL) {
                    strongDefined.insert(sym.name);
                    undefined.remove(sym.name);
                    weakOnly.remove(sym.name);
                } else if (sym.binding == STB_WEAK && sym.shndx != 0) {
                    if (!strongDefined.contains(sym.name)) {
                        weakOnly[sym.name] = true;
                        undefined.remove(sym.name);
                    }
                }
            }
        }

        // Extract the first non-yet-extracted member for a symbol
        auto extractSymbol = [&](const QByteArray& symName) -> bool {
            for (int a = 0; a < d_archives.size(); ++a) {
                Archive* arc = d_archives[a];
                if (!arc->symbolOffsets.contains(symName))
                    continue;
                const QList<quint32>& offsets = arc->symbolOffsets[symName];
                for (int j = 0; j < offsets.size(); ++j) {
                    quint32 offset = offsets[j];
                    if (arc->extractedMembers.contains(offset))
                        continue;
                    arc->extractedMembers.insert(offset);

                    const char* header = arc->data.constData() + offset;
                    int size = QByteArray(header + 48, 10).trimmed().toInt();
                    QByteArray objData = QByteArray(header + 60, size);

                    if (!addFileFromData(objData, arc->filename + "(" + symName + ")"))
                        return false;
                    extractedNew = true;
                    return true;
                }
            }
            return true; // not found is not an error
        };

        // Search archives for undefined symbols
        QHashIterator<QByteArray, bool> it(undefined);
        while (it.hasNext()) {
            it.next();
            if (!extractSymbol(it.key()))
                return false;
        }

        // Search archives for strong overrides of weak-only symbols
        QHashIterator<QByteArray, bool> wit(weakOnly);
        while (wit.hasNext()) {
            wit.next();
            if (!extractSymbol(wit.key()))
                return false;
        }
    } while (extractedNew);

    return true;
}

bool ElfLinker::link(const QString& outPath) {
    if (!extractFromArchives())
        return false;

    d_text.clear(); d_rodata.clear(); d_data.clear();
    d_bssSize = 0; d_globalSymbols.clear();
    d_debugSecs.clear(); d_debugMap.clear();

    // Merge sections & compute sizes
    for (int i = 0; i < d_files.size(); ++i) {
        InputFile* file = d_files[i];
        const int secCount = file->reader->sectionCount();
        file->secBucket.fill(-1, secCount);
        file->secOffset.fill(0, secCount);

        for (int sec = 1; sec < secCount; ++sec) {
            const quint32 flags = file->reader->sectionFlags(sec);
            const quint32 type = file->reader->sectionType(sec);
            const QByteArray name = file->reader->sectionName(sec);

            if (!(flags & SHF_ALLOC)) {
                // Collect DWARF debug sections (e.g., .debug_info, .debug_line)
                if (name.startsWith(".debug_") || name.startsWith(".zdebug_") || name.startsWith(".line")) {
                    int dbgIdx = d_debugMap.value(name, d_debugSecs.size());
                    if (dbgIdx == d_debugSecs.size()) {
                        d_debugMap[name] = dbgIdx;
                        d_debugSecs.append({name, QByteArray()});
                    }
                    file->secOffset[sec] = d_debugSecs[dbgIdx].data.size();
                    d_debugSecs[dbgIdx].data.append(file->reader->sectionData(sec));
                    file->secBucket[sec] = 4 + dbgIdx; // Map to bucket 4+
                }
                continue;
            }

            if (name == ".text" || (flags & SHF_EXECINSTR)) {
                file->secOffset[sec] = alignTo(d_text.size(), 4);
                d_text.append(QByteArray(file->secOffset[sec] - d_text.size(), '\0'));
                d_text.append(file->reader->sectionData(sec));
                file->secBucket[sec] = 0;
            } else if (type == SHT_PROGBITS && !(flags & SHF_WRITE)) {
                file->secOffset[sec] = alignTo(d_rodata.size(), 4);
                d_rodata.append(QByteArray(file->secOffset[sec] - d_rodata.size(), '\0'));
                d_rodata.append(file->reader->sectionData(sec));
                file->secBucket[sec] = 1;
            } else if (type == SHT_PROGBITS && (flags & SHF_WRITE)) {
                file->secOffset[sec] = alignTo(d_data.size(), 4);
                d_data.append(QByteArray(file->secOffset[sec] - d_data.size(), '\0'));
                d_data.append(file->reader->sectionData(sec));
                file->secBucket[sec] = 2;
            } else if (type == SHT_NOBITS) {
                quint32 secSize = 0;
                QList<ElfReader::Symbol> syms = file->reader->symbols();
                for (int s = 0; s < syms.size(); ++s) {
                    if (syms[s].shndx == sec && (syms[s].value + syms[s].size > secSize))
                        secSize = syms[s].value + syms[s].size;
                }
                d_bssSize = alignTo(d_bssSize, 4);
                file->secBucket[sec] = 3;
                file->secOffset[sec] = d_bssSize;
                d_bssSize += secSize;
            } else if (name == ".multiboot") {
                // Ensure Multiboot header is the absolute first thing in the executable
                QByteArray temp = file->reader->sectionData(sec);
                temp.append(d_text); // Prepend it to the existing text
                d_text = temp;

                file->secOffset[sec] = 0; // It sits at offset 0
                file->secBucket[sec] = 0; // It belongs in the RX text segment
            }
        }
    }

    // Generate ESP-IDF App Descriptor as a separate .rodata_desc section
    QByteArray espAppDesc;
    if (d_isEsp32) {
        espAppDesc = generateEspAppDescriptor(outPath);
#if 0 // unnecessary
        // Shift all .rodata section offsets by descriptor size so they follow .rodata_desc
        const quint32 descSize = espAppDesc.size();
        for (int i = 0; i < d_files.size(); ++i) {
            InputFile* file = d_files[i];
            for (int sec = 0; sec < file->secBucket.size(); ++sec) {
                if (file->secBucket[sec] == 1)
                    file->secOffset[sec] += descSize;
            }
        }
#endif
    }

    // Reserve GOT space, scan relocations for GOT-referencing types
    d_got.clear();
    d_gotEntries.clear();
    d_gotAddr = 0;
    if (d_arch == ElfReader::ArchX86) {
        for (int i = 0; i < d_files.size(); ++i) {
            InputFile* file = d_files[i];
            QList<ElfReader::Relocation> rels = file->reader->relocations();
            QList<ElfReader::Symbol> syms = file->reader->symbols();
            for (int r = 0; r < rels.size(); ++r) {
                const ElfReader::Relocation& rel = rels[r];
                if (rel.type == 43) { // R_386_GOT32X
                    QByteArray symName = syms[rel.symbolIdx].name;
                    if (!d_gotEntries.contains(symName)) {
                        const quint32 off = d_got.size();
                        d_gotEntries[symName] = off;
                        emit32(d_got, 0); // placeholder, filled after symbol resolution
                    }
                }
            }
        }
        if (!d_got.isEmpty()) {
            const quint32 gotDataOffset = alignTo(d_data.size(), 4);
            d_data.append(QByteArray(gotDataOffset - d_data.size(), '\0'));
            d_gotAddr = gotDataOffset; // store data-relative offset; convert to vaddr after memory map
            d_data.append(d_got);
        }
    }

    // Compute Memory Map
    const quint16 phnum = d_isEsp32 ? 3 : 2;
    const quint32 headerSize = 52 + phnum * 32; // Ehdr + N * Phdr

    quint32 textFileOffset, rodataFileOffset, dataFileOffset;
    quint32 dataLmaAddr; // physical address (differs from VMA on ESP32)
    quint32 rxFileSize;

    quint32 rodataDescFileOffset = 0;
    quint32 rodataDescAddr = 0;

    if (d_isEsp32) {
        // ESP32 Harvard: Headers -> .rodata_desc -> .rodata (DROM) -> .text (IROM) -> .data (SRAM VMA / Flash LMA)
        rodataDescFileOffset = headerSize;
        rodataDescAddr = d_dromAddress + headerSize;
        rodataFileOffset = rodataDescFileOffset + espAppDesc.size();
        d_rodataAddr = rodataDescAddr + espAppDesc.size();

        textFileOffset = alignTo(rodataFileOffset + d_rodata.size(), 4);
        d_textAddr = d_iromAddress;

        dataFileOffset = alignTo(textFileOffset + d_text.size(), 0x1000);
        dataLmaAddr = d_dromAddress + dataFileOffset;
        d_dataAddr = d_dataAddress;
        d_bssAddr = d_dataAddr + d_data.size();
        rxFileSize = textFileOffset + d_text.size(); // not used for ESP32 PT_LOAD
    } else {
        // Standard x86/ARM/RV32-Linux layout: Headers -> .text -> .rodata -> .data -> .bss
        textFileOffset = headerSize;
        d_textAddr = d_baseAddress + headerSize;
        rodataFileOffset = headerSize + d_text.size();
        d_rodataAddr = d_textAddr + d_text.size();
        rxFileSize = headerSize + d_text.size() + d_rodata.size();

        dataFileOffset = alignTo(rxFileSize, 0x1000);
        d_dataAddr = d_baseAddress + dataFileOffset;
        dataLmaAddr = d_dataAddr;
        d_bssAddr = d_dataAddr + d_data.size();
    }

    // Convert GOT data-relative offset to virtual address
    if (d_gotAddr != 0)
        d_gotAddr = d_dataAddr + d_gotAddr;

    // Allocate SHN_COMMON symbols in BSS (deduplicate by name)
    QHash<QByteArray, quint32> commonBssMap; // name -> BSS offset
    for (int i = 0; i < d_files.size(); ++i) {
        InputFile* file = d_files[i];
        QList<ElfReader::Symbol> syms = file->reader->symbols();
        for (int s = 0; s < syms.size(); ++s) {
            const ElfReader::Symbol& sym = syms[s];
            if (sym.shndx == SHN_COMMON) {
                if (!commonBssMap.contains(sym.name)) {
                    quint32 align = sym.value > 0 ? sym.value : 4;
                    d_bssSize = alignTo(d_bssSize, align);
                    commonBssMap[sym.name] = d_bssSize;
                    d_bssSize += sym.size;
                }
                file->commonBssOffset[s] = commonBssMap[sym.name];
            }
        }
    }

    // Symbol Resolution
    for (int i = 0; i < d_files.size(); ++i) {
        InputFile* file = d_files[i];
        QList<ElfReader::Symbol> syms = file->reader->symbols();
        file->symAddr.fill(0, syms.size());

        // Export defined globals
        for (int s = 0; s < syms.size(); ++s) {
            const ElfReader::Symbol& sym = syms[s];

            if (sym.shndx == SHN_ABS) {
                file->symAddr[s] = sym.value;
                 if ((sym.binding == STB_GLOBAL || sym.binding == STB_WEAK) && !sym.name.isEmpty()) {
                    if (!d_globalSymbols.contains(sym.name) || sym.binding == STB_GLOBAL)
                        d_globalSymbols[sym.name] = sym.value;
                }
            } else if (sym.shndx == SHN_COMMON) {
                file->symAddr[s] = d_bssAddr + file->commonBssOffset[s];
                if ((sym.binding == STB_GLOBAL || sym.binding == STB_WEAK) && !sym.name.isEmpty()) {
                    if (!d_globalSymbols.contains(sym.name) || sym.binding == STB_GLOBAL)
                        d_globalSymbols[sym.name] = file->symAddr[s];
                }
            } else if (sym.shndx != 0 && sym.shndx < file->secBucket.size() && file->secBucket[sym.shndx] != -1) {
                const int b = file->secBucket[sym.shndx];

                const quint32 base = (b==0)? d_textAddr : (b==1)? d_rodataAddr : (b==2)? d_dataAddr : (b==3)? d_bssAddr : 0;
                file->symAddr[s] = base + file->secOffset[sym.shndx] + sym.value;
                if ((sym.binding == STB_GLOBAL || sym.binding == STB_WEAK) && !sym.name.isEmpty()) {
                    if (!d_globalSymbols.contains(sym.name) || sym.binding == STB_GLOBAL)
                        d_globalSymbols[sym.name] = file->symAddr[s];
                }
            }
        }
    }

    // Provide standard linker-defined symbols if not already defined
    const quint32 gotSymAddr = d_gotAddr ? d_gotAddr : d_dataAddr;
    auto defineSym = [&](const QByteArray& name, quint32 val) {
        if (!d_globalSymbols.contains(name))
            d_globalSymbols[name] = val;
    };
    defineSym("_GLOBAL_OFFSET_TABLE_", gotSymAddr);
    defineSym("__ehdr_start", d_baseAddress);
    defineSym("_DYNAMIC", 0);
    defineSym("__init_array_start", 0);
    defineSym("__init_array_end", 0);
    defineSym("__fini_array_start", 0);
    defineSym("__fini_array_end", 0);
    defineSym("__preinit_array_start", 0);
    defineSym("__preinit_array_end", 0);
    defineSym("_init", 0);
    defineSym("_fini", 0);
    defineSym("__heap_start", d_bssAddr + d_bssSize);
    defineSym("__heap_end", d_bssAddr + d_bssSize);

    // Bind undefined symbols and override weak definitions with strong ones
    QSet<QByteArray> missing;
    for (int i = 0; i < d_files.size(); ++i) {
        InputFile* file = d_files[i];
        QList<ElfReader::Symbol> syms = file->reader->symbols();

        for (int s = 0; s < syms.size(); ++s) {
            const ElfReader::Symbol& sym = syms[s];
            if (sym.name.isEmpty())
                continue;
            if (sym.shndx == 0) {
                if (d_globalSymbols.contains(sym.name)) {
                    file->symAddr[s] = d_globalSymbols[sym.name];
                } else {
                    missing.insert(sym.name);
                }
            } else if (sym.binding == STB_WEAK && d_globalSymbols.contains(sym.name)) {
                // Override local weak address with the global (possibly strong) address
                file->symAddr[s] = d_globalSymbols[sym.name];
            }
        }
    }
    if (!missing.isEmpty()) {
        QStringList names;
        foreach (const QByteArray& n, missing)
            names << QString::fromUtf8(n);
        names.sort();
        d_error = "Undefined references:\n  " + names.join("\n  ");
        return false;
    }

    if (!buildGot())
        return false;

    if (!applyRelocations())
        return false;

    // Build Executable Image with Debug Info
    QByteArray elf;
    elf.append("\x7f""ELF\1\1\1\0\0\0\0\0\0\0\0\0", 16);

    const quint16 e_machine = (d_arch == ElfReader::ArchARM) ? 40 : (d_arch == ElfReader::ArchRISCV) ? 243 : 3;
    const quint32 entryAddr = d_globalSymbols.value("_start", d_textAddr);

    // Build the String Table for Section Headers (.shstrtab)
    QByteArray shstrtab;
    shstrtab.append('\0'); // string index 0 is empty
    auto addStr = [&](const QByteArray& str) -> quint32 {
        quint32 idx = shstrtab.size();
        shstrtab.append(str).append('\0');
        return idx;
    };

    const quint32 n_text = addStr(".text"), n_rodata = addStr(".rodata");
    const quint32 n_data = addStr(".data"), n_bss = addStr(".bss");
    const quint32 n_rodata_desc = d_isEsp32 ? addStr(".rodata_desc") : 0;
    QVector<quint32> debugNames;
    for (int i=0; i < d_debugSecs.size(); ++i) debugNames.append(addStr(d_debugSecs[i].name));
    quint32 n_shstrtab = addStr(".shstrtab");

    // Calculate Final File Offsets
    quint32 currentFileOff = dataFileOffset + d_data.size();
    QVector<quint32> debugOffsets;
    for (int i = 0; i < d_debugSecs.size(); ++i) {
        debugOffsets.append(currentFileOff);
        currentFileOff += d_debugSecs[i].data.size();
    }
    quint32 shstrtabOff = currentFileOff;
    currentFileOff += shstrtab.size();
    quint32 shtOff = alignTo(currentFileOff, 4);

    const quint16 shnum = 6 + d_debugSecs.size() + (d_isEsp32 ? 1 : 0); // null, text, rodata, data, bss, shstrtab + debugs + rodata_desc
    const quint16 shstrndx = shnum - 1; // shstrtab is the last section

    // Write ELF Header
    emit16(elf, 2);           // e_type = ET_EXEC
    emit16(elf, e_machine);
    emit32(elf, 1);
    emit32(elf, entryAddr);
    emit32(elf, 52);          // e_phoff
    emit32(elf, shtOff);      // e_shoff (Points to the end of the file where SHT lives)
    quint32 e_flags = 0;

    if (d_arch == ElfReader::ArchARM)
        e_flags = 0x05000000;
    else if (d_arch == ElfReader::ArchRISCV && !d_files.isEmpty())
        e_flags = d_files[0]->reader->elfFlags();

    emit32(elf, e_flags);
    emit16(elf, 52); emit16(elf, 32); emit16(elf, phnum);
    emit16(elf, 40); emit16(elf, shnum); emit16(elf, shstrndx);

    if (d_isEsp32) {
        // PT_LOAD 1: DROM (.rodata_desc + .rodata) — Read-only data in Flash
        quint32 dromSize = rodataFileOffset + d_rodata.size();
        emit32(elf, 1); emit32(elf, 0); emit32(elf, d_dromAddress); emit32(elf, d_dromAddress);
        emit32(elf, dromSize); emit32(elf, dromSize); emit32(elf, 4); emit32(elf, 0x1000);

        // PT_LOAD 2: IROM (.text) — Executable code in Flash
        emit32(elf, 1); emit32(elf, textFileOffset); emit32(elf, d_textAddr); emit32(elf, d_textAddr);
        emit32(elf, d_text.size()); emit32(elf, d_text.size()); emit32(elf, 5); emit32(elf, 0x1000);

        // PT_LOAD 3: SRAM (.data+.bss) — VMA in SRAM, LMA in Flash
        emit32(elf, 1); emit32(elf, dataFileOffset); emit32(elf, d_dataAddr); emit32(elf, dataLmaAddr);
        emit32(elf, d_data.size()); emit32(elf, d_data.size() + d_bssSize); emit32(elf, 6); emit32(elf, 0x1000);
    } else {
        // PT_LOAD RX Segment (.text + .rodata)
        emit32(elf, 1); emit32(elf, 0); emit32(elf, d_baseAddress); emit32(elf, d_baseAddress);
        emit32(elf, rxFileSize); emit32(elf, rxFileSize); emit32(elf, 5); emit32(elf, 0x1000);

        // PT_LOAD RW Segment (.data + .bss)
        emit32(elf, 1); emit32(elf, dataFileOffset); emit32(elf, d_dataAddr); emit32(elf, d_dataAddr);
        emit32(elf, d_data.size()); emit32(elf, d_data.size() + d_bssSize); emit32(elf, 6); emit32(elf, 0x1000);
    }

    // Write Segments to file buffer in physical order
    if (d_isEsp32) {
        elf.append(espAppDesc);
        elf.append(d_rodata);
        elf.append(QByteArray(textFileOffset - elf.size(), '\0')); // align
        elf.append(d_text);
    } else {
        elf.append(d_text);
        elf.append(d_rodata);
    }
    elf.append(QByteArray(dataFileOffset - elf.size(), '\0')); // align
    elf.append(d_data);

    // Write Debug Sections to file buffer (Not loaded into memory)
    for (int i = 0; i < d_debugSecs.size(); ++i) {
        elf.append(d_debugSecs[i].data);
    }

    // Write Section String Table
    elf.append(shstrtab);
    elf.append(QByteArray(shtOff - elf.size(), '\0')); // align SHT

    // Build Section Header Table (SHT) for debuggers
    emitShdr(elf, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0); // Null section
    emitShdr(elf, n_text, SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, d_textAddr, textFileOffset, d_text.size(), 0, 0, 4, 0);
    if (d_isEsp32)
        emitShdr(elf, n_rodata_desc, SHT_PROGBITS, SHF_ALLOC, rodataDescAddr, rodataDescFileOffset, espAppDesc.size(), 0, 0, 4, 0);
    emitShdr(elf, n_rodata, SHT_PROGBITS, SHF_ALLOC, d_rodataAddr, rodataFileOffset, d_rodata.size(), 0, 0, 4, 0);
    emitShdr(elf, n_data, SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, d_dataAddr, dataFileOffset, d_data.size(), 0, 0, 4, 0);
    emitShdr(elf, n_bss, SHT_NOBITS, SHF_ALLOC | SHF_WRITE, d_bssAddr, dataFileOffset + d_data.size(), d_bssSize, 0, 0, 4, 0);

    for (int i = 0; i < d_debugSecs.size(); ++i) {
        // Debug sections have Virtual Address = 0, because they aren't loaded to RAM.
        emitShdr(elf, debugNames[i], SHT_PROGBITS, 0, 0, debugOffsets[i], d_debugSecs[i].data.size(), 0, 0, 1, 0);
    }

    emitShdr(elf, n_shstrtab, SHT_STRTAB, 0, 0, shstrtabOff, shstrtab.size(), 0, 0, 1, 0);

    // Patch the ESP-IDF App Descriptor SHA256 hash with the hash of the final ELF
    if (d_isEsp32) {
        QByteArray hash = QCryptographicHash::hash(elf, QCryptographicHash::Sha256);
        const int sha256Offset = rodataDescFileOffset + 144; // app_elf_sha256 is at byte 144 in the descriptor
        memcpy(elf.data() + sha256Offset, hash.constData(), 32);
    }

    QFile outFile(outPath);
    if (!outFile.open(QIODevice::WriteOnly))
        return false;
    outFile.write(elf);
    outFile.close();
    outFile.setPermissions(outFile.permissions() | QFile::ExeUser);

    return true;
}

bool ElfLinker::buildGot() {
    if (d_gotEntries.isEmpty())
        return true;

    // Compute data-relative offset where GOT starts
    const quint32 gotDataStart = d_gotAddr - d_dataAddr;

    // Fill GOT entries with resolved symbol addresses
    QHashIterator<QByteArray, quint32> it(d_gotEntries);
    while (it.hasNext()) {
        it.next();
        const quint32 addr = d_globalSymbols.value(it.key(), 0);
        const quint32 entryDataOff = gotDataStart + it.value();
        quint8* p = reinterpret_cast<quint8*>(d_data.data() + entryDataOff);
        writeLE32(p, addr);
    }

    return true;
}

bool ElfLinker::applyRelocations() {
    for (int i = 0; i < d_files.size(); ++i) {
        InputFile* file = d_files[i];
        QList<ElfReader::Relocation> rels = file->reader->relocations();

        for (int r = 0; r < rels.size(); ++r) {
            const ElfReader::Relocation& rel = rels[r];
            const int b = file->secBucket[rel.targetSection];
            if (b < 0 || b == 3)
                continue; // Ignore unmapped or BSS patches

            QByteArray* targetData;
            quint32 targetBaseAddr = 0;

            if (b == 0) {
                targetData = &d_text; targetBaseAddr = d_textAddr;
            } else if (b == 1) {
                targetData = &d_rodata; targetBaseAddr = d_rodataAddr;
            } else if (b == 2) {
                targetData = &d_data; targetBaseAddr = d_dataAddr;
            } else {
                targetData = &d_debugSecs[b - 4].data;
                targetBaseAddr = 0; // Debug sections are not loaded (vaddr=0)
            }

            const quint32 patchOffset = file->secOffset[rel.targetSection] + rel.offset;
            const quint32 patchVAddr = targetBaseAddr + patchOffset;
            const quint32 symAddr = file->symAddr[rel.symbolIdx];

            quint8* p = reinterpret_cast<quint8*>(targetData->data() + patchOffset);
            quint32 insn = readLE32(p);
            const qint32 addend = rel.addend ? rel.addend : (qint32)insn;

            if (d_arch == ElfReader::ArchX86) {
                const quint32 gotAddr = d_globalSymbols.value("_GLOBAL_OFFSET_TABLE_");
                if (rel.type == 1) { // R_386_32
                    writeLE32(p, symAddr + addend);
                } else if (rel.type == 2 || rel.type == 4) { // R_386_PC32, R_386_PLT32
                    writeLE32(p, symAddr + addend - patchVAddr);
                } else if (rel.type == 9) { // R_386_GOTOFF
                    writeLE32(p, symAddr + addend - gotAddr);
                } else if (rel.type == 10) { // R_386_GOTPC
                    writeLE32(p, gotAddr + addend - patchVAddr);
                } else if (rel.type == 43) { // R_386_GOT32X
                    // Patch with offset from GOT base to the symbol's GOT entry
                    QList<ElfReader::Symbol> syms = file->reader->symbols();
                    const QByteArray symName = syms[rel.symbolIdx].name;
                    const quint32 gotEntryOff = d_gotEntries.value(symName, 0);
                    writeLE32(p, gotEntryOff + addend);
                } else {
                    d_error = QString("Unsupported x86 relocation type %1 in %2").arg(rel.type).arg(file->filename);
                    return false;
                }
            }
            else if (d_arch == ElfReader::ArchARM) {
                if (rel.type == 0) { // R_ARM_NONE — ignore
                } else if (rel.type == 2 || rel.type == 38) { // R_ARM_ABS32 / R_ARM_TARGET1
                    writeLE32(p, symAddr + addend);
                } else if (rel.type == 28 || rel.type == 29) {
                    // R_ARM_CALL / R_ARM_JUMP24
                    qint32 implicitAddend = insn & 0x00FFFFFF;
                    if (implicitAddend & 0x00800000)
                        implicitAddend |= 0xFF000000;
                    const qint32 offset = symAddr + (implicitAddend << 2) - patchVAddr;
                    insn = (insn & 0xFF000000) | ((offset >> 2) & 0x00FFFFFF);
                    writeLE32(p, insn);
                } else if (rel.type == 43 || rel.type == 44) {
                    // R_ARM_MOVW_ABS_NC / R_ARM_MOVT_ABS
                    // Extract implicit 16-bit addend from MOVW/MOVT encoding: imm4=[19:16], imm12=[11:0]
                    const qint32 movAddend = ((insn >> 4) & 0xF000) | (insn & 0x0FFF);
                    const quint32 val = symAddr + movAddend;
                    const quint16 imm16 = (rel.type == 44) ? ((val >> 16) & 0xFFFF) : (val & 0xFFFF);
                    insn = (insn & 0xFFF0F000) | ((imm16 >> 12) << 16) | (imm16 & 0x0FFF);
                    writeLE32(p, insn);
                } else if (rel.type == 42) {
                    // R_ARM_PREL31
                    const qint32 offset = symAddr + addend - patchVAddr;
                    writeLE32(p, (insn & 0x80000000) | (offset & 0x7FFFFFFF));
                } else if (rel.type == 3) {
                    // R_ARM_REL32
                    writeLE32(p, symAddr + addend - patchVAddr);
                } else if (rel.type == 40) {
                    // R_ARM_V4BX — ARMv4 BX compatibility, treat as no-op
                } else if (rel.type == 104 || rel.type == 105 || rel.type == 106 ||
                           rel.type == 107 || rel.type == 108) {
                    // R_ARM_TLS_DTPMOD32(104), TLS_DTPOFF32(105), TLS_GD32(106),
                    // TLS_LE32(107), TLS_IE32(108)
                    // Bare-metal single-threaded: treat all TLS as absolute offsets
                    writeLE32(p, symAddr + addend);
                } else if (rel.type == 10) {
                    // R_ARM_THM_CALL (Thumb BL)
                    qint32 S  = (readLE16(p) >> 10) & 1;
                    const qint32 hi = readLE16(p) & 0x3FF;
                    qint32 J1 = (readLE16(p+2) >> 13) & 1;
                    qint32 J2 = (readLE16(p+2) >> 11) & 1;
                    const qint32 lo = readLE16(p+2) & 0x7FF;
                    qint32 I1 = !(J1 ^ S);
                    qint32 I2 = !(J2 ^ S);
                    const qint32 existOff = (S ? 0xFF000000 : 0) | (I1 << 23) | (I2 << 22) | (hi << 12) | (lo << 1);
                    const qint32 offset = symAddr + existOff - patchVAddr;
                    S  = (offset >> 24) & 1;
                    I1 = (offset >> 23) & 1;
                    I2 = (offset >> 22) & 1;
                    J1 = !(I1 ^ S);
                    J2 = !(I2 ^ S);
                    const quint16 hw0 = 0xF000 | (S << 10) | ((offset >> 12) & 0x3FF);
                    const quint16 hw1 = 0xD000 | (J1 << 13) | (J2 << 11) | ((offset >> 1) & 0x7FF);
                    writeLE16(p, hw0);
                    writeLE16(p+2, hw1);
                } else if (rel.type == 30) {
                    // R_ARM_THM_JUMP24 (Thumb B.W)
                    qint32 S  = (readLE16(p) >> 10) & 1;
                    const qint32 hi = readLE16(p) & 0x3FF;
                    qint32 J1 = (readLE16(p+2) >> 13) & 1;
                    qint32 J2 = (readLE16(p+2) >> 11) & 1;
                    const qint32 lo = readLE16(p+2) & 0x7FF;
                    qint32 I1 = !(J1 ^ S);
                    qint32 I2 = !(J2 ^ S);
                    const qint32 existOff = (S ? 0xFF000000 : 0) | (I1 << 23) | (I2 << 22) | (hi << 12) | (lo << 1);
                    const qint32 offset = symAddr + existOff - patchVAddr;
                    S  = (offset >> 24) & 1;
                    I1 = (offset >> 23) & 1;
                    I2 = (offset >> 22) & 1;
                    J1 = !(I1 ^ S);
                    J2 = !(I2 ^ S);
                    const quint16 hw0 = 0xF000 | (S << 10) | ((offset >> 12) & 0x3FF);
                    const quint16 hw1 = 0x9000 | (J1 << 13) | (J2 << 11) | ((offset >> 1) & 0x7FF);
                    writeLE16(p, hw0);
                    writeLE16(p+2, hw1);
                } else {
                    d_error = QString("Unsupported ARM relocation type %1 in %2").arg(rel.type).arg(file->filename);
                    return false;
                }
            }
            else if (d_arch == ElfReader::ArchRISCV) {
                if (rel.type == 1) {
                    // R_RISCV_32
                    writeLE32(p, symAddr + rel.addend);
                } else if (rel.type == 18 || rel.type == 19) {
                    // R_RISCV_CALL / R_RISCV_CALL_PLT (auipc + jalr)
                    const qint32 offset = symAddr - patchVAddr;
                    const qint32 hi20 = (offset + 0x800) & 0xFFFFF000;
                    const qint32 lo12 = offset - hi20;

                    quint32 auipc = readLE32(p);
                    quint32 jalr = readLE32(p + 4);
                    auipc = (auipc & 0x00000FFF) | hi20;
                    jalr  = (jalr & 0x000FFFFF) | ((lo12 & 0xFFF) << 20);

                    writeLE32(p, auipc);
                    writeLE32(p + 4, jalr);
                } else if (rel.type == 23) {
                    // R_RISCV_PCREL_HI20 (AUIPC)
                    const qint32 offset = (symAddr + rel.addend) - patchVAddr;
                    const quint32 hi = (offset + 0x800) & 0xFFFFF000;
                    insn = (insn & 0x00000FFF) | hi;
                    writeLE32(p, insn);
                } else if (rel.type == 24 || rel.type == 25) { // R_RISCV_PCREL_LO12_I / _S
                    // The symbol+addend of a PCREL_LO12 points to the AUIPC instruction
                    // (a local label). We find the PCREL_HI20 reloc at that offset to
                    // compute the actual low 12 bits of the PC-relative address.
                    const quint32 hiVAddr = symAddr + rel.addend; // VMA of the AUIPC
                    qint32 lo12 = 0;
                    bool found = false;
                    for (int rr2 = 0; rr2 < rels.size(); ++rr2) {
                        const ElfReader::Relocation& r2 = rels[rr2];
                        if (r2.type != 23)
                            continue; // only PCREL_HI20
                        if (r2.targetSection != rel.targetSection)
                            continue;
                        const quint32 r2VAddr = targetBaseAddr + file->secOffset[r2.targetSection] + r2.offset;
                        if (r2VAddr != hiVAddr)
                            continue;
                        quint32 hiSym = 0;
                        if (r2.symbolIdx < file->symAddr.size())
                            hiSym = file->symAddr[r2.symbolIdx];
                        const qint32 fullOffset = (hiSym + r2.addend) - r2VAddr;
                        lo12 = fullOffset - ((fullOffset + 0x800) & 0xFFFFF000);
                        found = true;
                        break;
                    }
                    if (!found) {
                        d_error = QString("Cannot find paired PCREL_HI20 for PCREL_LO12 at offset 0x%1 in %2")
                                    .arg(rel.offset, 0, 16).arg(file->filename);
                        return false;
                    }
                    if (rel.type == 24) {
                        // I-type
                        insn = (insn & 0x000FFFFF) | ((lo12 & 0xFFF) << 20);
                    } else {
                        // S-type
                        insn = (insn & 0x01FFF07F) | ((lo12 & 0x1F) << 7) | (((lo12 >> 5) & 0x7F) << 25);
                    }
                    writeLE32(p, insn);
                } else if (rel.type == 26) {
                    // R_RISCV_HI20 (LUI)
                    const quint32 val = symAddr + rel.addend;
                    const quint32 hi = (val + 0x800) & 0xFFFFF000;
                    insn = (insn & 0x00000FFF) | hi;
                    writeLE32(p, insn);
                } else if (rel.type == 27) {
                    // R_RISCV_LO12_I (ADDI, LW, etc.)
                    const quint32 val = symAddr + rel.addend;
                    const quint32 lo = val & 0xFFF;
                    insn = (insn & 0x000FFFFF) | (lo << 20);
                    writeLE32(p, insn);
                } else if (rel.type == 28) {
                    // R_RISCV_LO12_S (SW, SB, etc.)
                    const quint32 val = symAddr + rel.addend;
                    const quint32 lo = val & 0xFFF;
                    insn = (insn & 0x01FFF07F) | ((lo & 0x1F) << 7) | ((lo >> 5) << 25);
                    writeLE32(p, insn);
                } else if (rel.type == 16) {
                    // R_RISCV_BRANCH
                    qint32 offset = symAddr - patchVAddr;
                    quint32 b_imm = ((offset >> 12) & 1) << 31 | ((offset >> 5) & 0x3f) << 25 |
                                    ((offset >> 1) & 0xf) << 8 | ((offset >> 11) & 1) << 7;
                    writeLE32(p, (insn & 0x1FFF07F) | b_imm);
                } else if (rel.type == 17) {
                    // R_RISCV_JAL
                    const qint32 offset = symAddr - patchVAddr;
                    const quint32 j_imm = ((offset >> 20) & 1) << 31 | ((offset >> 1) & 0x3ff) << 21 |
                                    ((offset >> 11) & 1) << 20 | ((offset >> 12) & 0xff) << 12;
                    writeLE32(p, (insn & 0xFFF) | j_imm);
                } else if (rel.type == 29) {
                    // R_RISCV_TPREL_HI20 (LUI for TLS)
                    const quint32 val = symAddr + rel.addend;
                    const quint32 hi = (val + 0x800) & 0xFFFFF000;
                    insn = (insn & 0x00000FFF) | hi;
                    writeLE32(p, insn);
                } else if (rel.type == 30) {
                    // R_RISCV_TPREL_LO12_I (TLS I-type)
                    const quint32 val = symAddr + rel.addend;
                    const quint32 lo = val & 0xFFF;
                    insn = (insn & 0x000FFFFF) | (lo << 20);
                    writeLE32(p, insn);
                } else if (rel.type == 31) {
                    // R_RISCV_TPREL_LO12_S (TLS S-type)
                    const quint32 val = symAddr + rel.addend;
                    const quint32 lo = val & 0xFFF;
                    insn = (insn & 0x01FFF07F) | ((lo & 0x1F) << 7) | ((lo >> 5) << 25);
                    writeLE32(p, insn);
                } else if (rel.type == 32) {
                    // R_RISCV_TPREL_ADD — hint, ignore
                } else if (rel.type == 51) {
                    // R_RISCV_RELAX — linker relaxation hint, ignore
                } else if (rel.type == 35 || rel.type == 39) {
                    // R_RISCV_ADD32 / R_RISCV_SUB32
                    quint32 cur = readLE32(p);
                    if (rel.type == 35)
                        cur += symAddr + rel.addend;
                    else
                        cur -= symAddr + rel.addend;
                    writeLE32(p, cur);
                } else if (rel.type == 34 || rel.type == 38) {
                    // R_RISCV_ADD16 / R_RISCV_SUB16
                    quint16 cur = readLE16(p);
                    if (rel.type == 34)
                        cur += (quint16)(symAddr + rel.addend);
                    else
                        cur -= (quint16)(symAddr + rel.addend);
                    writeLE16(p, cur);
                } else if (rel.type == 33 || rel.type == 37) {
                    // R_RISCV_ADD8 / R_RISCV_SUB8
                    quint8 cur = (quint8)(*p);
                    if (rel.type == 33)
                        cur += (quint8)(symAddr + rel.addend);
                    else
                        cur -= (quint8)(symAddr + rel.addend);
                    *p = (char)cur;
                } else if (rel.type == 53 || rel.type == 52) {
                    // R_RISCV_SET6 (53) / R_RISCV_SUB6 (52)
                    quint8 cur = (quint8)(*p);
                    if (rel.type == 53)
                        cur = (cur & 0xC0) | ((symAddr + rel.addend) & 0x3F);
                    else
                        cur = (cur & 0xC0) | (((cur & 0x3F) - (symAddr + rel.addend)) & 0x3F);
                    *p = (char)cur;
                } else if (rel.type == 54) {
                    // R_RISCV_SET8
                    *p = (char)(quint8)(symAddr + rel.addend);
                } else if (rel.type == 55) {
                    // R_RISCV_SET16
                    writeLE16(p, (quint16)(symAddr + rel.addend));
                } else if (rel.type == 56) {
                    // R_RISCV_SET32
                    writeLE32(p, symAddr + rel.addend);
                } else {
                    d_error = QString("Unsupported RISC-V relocation type %1 in %2").arg(rel.type).arg(file->filename);
                    return false;
                }
            }
        }
    }
    return true;
}

void ElfLinker::emit32(QByteArray& out, quint32 val) {
    out.append(char(val & 0xFF)).append(char((val >> 8) & 0xFF));
    out.append(char((val >> 16) & 0xFF)).append(char((val >> 24) & 0xFF));
}

void ElfLinker::emit16(QByteArray& out, quint16 val) {
    out.append(char(val & 0xFF)).append(char((val >> 8) & 0xFF));
}

void ElfLinker::emitShdr(QByteArray& out, quint32 name, quint32 type, quint32 flags, quint32 addr, quint32 off, quint32 size,
                         quint32 link, quint32 info, quint32 align, quint32 entsize) {
    emit32(out, name); emit32(out, type); emit32(out, flags); emit32(out, addr);
    emit32(out, off);  emit32(out, size); emit32(out, link);  emit32(out, info);
    emit32(out, align); emit32(out, entsize);
}

QByteArray ElfLinker::generateEspAppDescriptor(const QString& outPath) {
    // ESP-IDF esp_app_desc_t: 256-byte structure expected by espflash/bootloader
    // See https://docs.espressif.com/projects/esp-idf/en/stable/esp32/api-reference/system/app_image_format.html
    QByteArray desc(256, '\0');
    quint8* p = reinterpret_cast<quint8*>(desc.data());

    // magic_word (offset 0, 4 bytes)
    writeLE32(p + 0, 0xABCD5432);

    // secure_version (offset 4, 4 bytes) — 0 for bare-metal
    writeLE32(p + 4, 0);

    // reserv1[2] (offset 8, 8 bytes) — zeroed

    // version[32] (offset 16, 32 bytes)
    QByteArray version = "1.0.0";
    memcpy(p + 16, version.constData(), qMin(version.size(), 31));

    // project_name[32] (offset 48, 32 bytes) — derived from output filename
    QByteArray projName = QFileInfo(outPath).baseName().toUtf8();
    memcpy(p + 48, projName.constData(), qMin(projName.size(), 31));

    // time[16] (offset 80, 16 bytes) — current compile time
    QByteArray timeStr = QDateTime::currentDateTime().toString("hh:mm:ss").toUtf8();
    memcpy(p + 80, timeStr.constData(), qMin(timeStr.size(), 15));

    // date[16] (offset 96, 16 bytes) — current compile date
    QByteArray dateStr = QDateTime::currentDateTime().toString("MMM dd yyyy").toUtf8();
    memcpy(p + 96, dateStr.constData(), qMin(dateStr.size(), 15));

    // idf_ver[32] (offset 112, 32 bytes)
    QByteArray idfVer = "standalone";
    memcpy(p + 112, idfVer.constData(), qMin(idfVer.size(), 31));

    // app_elf_sha256[32] (offset 144, 32 bytes) — zeroed now, patched after ELF assembly

    // Remaining fields (offset 176..255) — zeroed (min/max efuse, mmu_page_size, reserv2/3)

    return desc;
}
