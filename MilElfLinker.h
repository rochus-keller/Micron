#ifndef MILELFLINKER_H
#define MILELFLINKER_H

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

#include <QString>
#include <QList>
#include <QByteArray>
#include <QHash>
#include <QSet>
#include <QVector>
#include "MilElfReader.h"

namespace Mil
{
    class ElfLinker
    {
    public:
        ElfLinker();
        ~ElfLinker();

        // Configure the base virtual load address (e.g., 0x08048000 for Linux, 0x8000 for Raspi bare-metal)
        void setBaseAddress(quint32 addr) { d_baseAddress = addr; }

        bool addFile(const QString& filename);
        bool addArchive(const QString& filename);
        bool link(const QString& outPath);

        QString errorMessage() const { return d_error; }

    private:
        struct InputFile {
            ElfReader* reader;
            QString filename;
            QVector<int> secBucket;    // 0=text, 1=rodata, 2=data, 3=bss, 4+=debug sections, -1=ignore
            QVector<quint32> secOffset; 
            QVector<quint32> symAddr;
            QHash<int, quint32> commonBssOffset; // sym index -> BSS offset for SHN_COMMON symbols
        };

        struct Archive {
            QString filename;
            QByteArray data;
            QHash<QByteArray, QList<quint32>> symbolOffsets; // symbol -> list of member offsets
            QSet<quint32> extractedMembers; // already-extracted member offsets
        };

        struct DebugSection {
            QByteArray name;
            QByteArray data;
        };

        QList<InputFile*> d_files;
        QList<Archive*> d_archives;
        QHash<QByteArray, quint32> d_globalSymbols;
        QString d_error;
        ElfReader::Architecture d_arch;
        quint32 d_baseAddress;

        // Computed segment virtual addresses (set during link())
        quint32 d_textAddr = 0;
        quint32 d_rodataAddr = 0;
        quint32 d_dataAddr = 0;
        quint32 d_bssAddr = 0;

        // Combined standard sections
        QByteArray d_text;
        QByteArray d_rodata;
        QByteArray d_data;
        quint32 d_bssSize;

        // Global Offset Table for PIC code
        QByteArray d_got;
        QHash<QByteArray, quint32> d_gotEntries; // symName -> offset in d_got
        quint32 d_gotAddr; // virtual address of GOT

        // Combined DWARF/Debug sections
        QVector<DebugSection> d_debugSecs;
        QHash<QByteArray, int> d_debugMap;

        bool addFileFromData(const QByteArray& data, const QString& identifier);
        bool extractFromArchives();
        bool buildGot();
        bool applyRelocations();

        static inline quint32 alignTo(quint32 val, quint32 align) {
            return (val + align - 1) & ~(align - 1);
        }
        static inline quint32 readBE32(const char* p) {
            return ((quint8)p[0] << 24) | ((quint8)p[1] << 16) | ((quint8)p[2] << 8) | (quint8)p[3];
        }
        static inline quint32 readLE32(const quint8* p) {
            return (p[3] << 24) | (p[2] << 16) | (p[1] << 8) | p[0];
        }
        static inline void writeLE32(quint8* p, quint32 val) {
            p[0] = val & 0xFF; p[1] = (val >> 8) & 0xFF; p[2] = (val >> 16) & 0xFF; p[3] = (val >> 24) & 0xFF;
        }
        static inline quint16 readLE16(const quint8* p) {
            return (p[1] << 8) | p[0];
        }
        static inline void writeLE16(quint8* p, quint16 val) {
            p[0] = val & 0xFF; p[1] = (val >> 8) & 0xFF;
        }

        void emit32(QByteArray& out, quint32 val);
        void emit16(QByteArray& out, quint16 val);
        void emitShdr(QByteArray& out, quint32 name, quint32 type, quint32 flags, quint32 addr, quint32 off,
                      quint32 size, quint32 link, quint32 info, quint32 align, quint32 entsize);
    };

} // namespace Mil

#endif // MILELFLINKER_H
