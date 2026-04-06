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

#include "MilDwarfEmitter.h"
#include "MilElfWriter.h"
#include "MilAst.h"
#include <QtDebug>
#include <algorithm>

using namespace Mil;

static const quint16 DW_TAG_array_type = 0x01;
static const quint16 DW_TAG_compile_unit     = 0x11;
static const quint16 DW_TAG_formal_parameter = 0x05;
static const quint16 DW_TAG_member     = 0x0D;
static const quint16 DW_TAG_pointer_type     = 0x0F;
static const quint16 DW_TAG_structure_type   = 0x13;
static const quint16 DW_TAG_base_type  = 0x24;
static const quint16 DW_TAG_subprogram = 0x2E;
static const quint16 DW_TAG_subrange_type    = 0x21;
static const quint16 DW_TAG_variable   = 0x34;

static const quint8 DW_CHILDREN_no  = 0x00;
static const quint8 DW_CHILDREN_yes = 0x01;

static const quint16 DW_AT_name = 0x03;
static const quint16 DW_AT_byte_size  = 0x0B;
static const quint16 DW_AT_stmt_list  = 0x10;
static const quint16 DW_AT_low_pc   = 0x11;
static const quint16 DW_AT_high_pc  = 0x12;
static const quint16 DW_AT_language   = 0x13;
static const quint16 DW_AT_comp_dir   = 0x1B;
static const quint16 DW_AT_producer   = 0x25;
static const quint16 DW_AT_decl_line  = 0x3B;
static const quint16 DW_AT_encoding   = 0x3E;
static const quint16 DW_AT_frame_base = 0x40;
static const quint16 DW_AT_type = 0x49;
static const quint16 DW_AT_upper_bound= 0x2F;
static const quint16 DW_AT_data_member_location = 0x38;
static const quint16 DW_AT_location   = 0x02;
static const quint16 DW_AT_external   = 0x3F;
static const quint16 DW_AT_count= 0x37;

static const quint8 DW_FORM_addr = 0x01;
static const quint8 DW_FORM_data1= 0x0B;
static const quint8 DW_FORM_data2= 0x05;
static const quint8 DW_FORM_data4= 0x06;
static const quint8 DW_FORM_strp = 0x0E;  // offset into .debug_str
static const quint8 DW_FORM_udata= 0x0F;  // unsigned LEB128
static const quint8 DW_FORM_ref4 = 0x13;  // 4-byte offset within CU
static const quint8 DW_FORM_flag_present = 0x19; // implicit flag (DWARF4)
static const quint8 DW_FORM_exprloc    = 0x18;  // DWARF expression with length prefix
static const quint8 DW_FORM_sec_offset = 0x17;  // offset into another section

static const quint8 DW_ATE_address  = 0x01;
static const quint8 DW_ATE_boolean  = 0x02;
static const quint8 DW_ATE_float    = 0x04;
static const quint8 DW_ATE_signed   = 0x05;
static const quint8 DW_ATE_unsigned = 0x07;
static const quint8 DW_ATE_unsigned_char = 0x08;

static const quint16 DW_LANG_Pascal83 = 0x02;
static const quint16 DW_LANG_lo_user = 0x8000;

static const quint8 DW_OP_addr = 0x03; // 4-byte address
static const quint8 DW_OP_reg5  = 0x55; // EBP (x86 frame pointer)
static const quint8 DW_OP_reg11 = 0x5B; // R11 (ARM frame pointer)
static const quint8 DW_OP_breg11  = 0x7B; // R11 + SLEB128 offset
static const quint8 DW_OP_fbreg= 0x91; // frame base + SLEB128 offset

static const quint8 DW_CFA_def_cfa = 0x0C;
static const quint8 DW_CFA_offset = 0x80; // high 2 bits = 10, low 6 = register
static const quint8 DW_CFA_advance_loc = 0x40; // high 2 bits = 01, low 6 = delta
static const quint8 DW_CFA_nop  = 0x00;
static const quint8 DW_CFA_def_cfa_register = 0x0D;
static const quint8 DW_CFA_def_cfa_offset   = 0x0E;

static const quint32 SHT_PROGBITS_VAL = 1;
static const quint32 SHT_REL_VAL = 9;
static const quint32 ELF32_REL_SIZE = 8;
static const quint8  R_ARM_ABS32_VAL = 2;
static const quint8  R_386_32_VAL = 1;


static void appendLE16(QByteArray& buf, quint16 val)
{
    char b[2];
    b[0] = static_cast<char>(val & 0xFF);
    b[1] = static_cast<char>((val >> 8) & 0xFF);
    buf.append(b, 2);
}

static void appendLE32(QByteArray& buf, quint32 val)
{
    char b[4];
    b[0] = static_cast<char>(val & 0xFF);
    b[1] = static_cast<char>((val >> 8) & 0xFF);
    b[2] = static_cast<char>((val >> 16) & 0xFF);
    b[3] = static_cast<char>((val >> 24) & 0xFF);
    buf.append(b, 4);
}

static void patchLE32(QByteArray& buf, int offset, quint32 val)
{
    char* p = buf.data() + offset;
    p[0] = static_cast<char>(val & 0xFF);
    p[1] = static_cast<char>((val >> 8) & 0xFF);
    p[2] = static_cast<char>((val >> 16) & 0xFF);
    p[3] = static_cast<char>((val >> 24) & 0xFF);
}

DwarfEmitter::DwarfEmitter(ElfWriter& elf, ElfWriter::Architecture arch)
    : d_elf(elf), d_arch(arch), d_cuStartOffset(0), d_inCU(false),
      d_lowPC(0xFFFFFFFF), d_highPC(0)
{
    d_str.append('\0');

    // Pre-create all debug sections and their section symbols NOW, before
    // any global (code) symbols are added.  ElfWriter::addSymbol inserts
    // LOCAL symbols before globals, which shifts global indices.  If we
    // waited until finalize() to create these, every .rel.text relocation
    // that was already emitted against a global symbol would reference the
    // wrong symbol after the shift.
    d_abbrevSecIdx = d_elf.addSection(QByteArray(".debug_abbrev"), SHT_PROGBITS_VAL, 0, 1);
    d_abbrevSymIdx = d_elf.addSectionSymbol(d_abbrevSecIdx);

    d_infoSecIdx = d_elf.addSection(QByteArray(".debug_info"), SHT_PROGBITS_VAL, 0, 1);

    d_strSecIdx = d_elf.addSection(QByteArray(".debug_str"), SHT_PROGBITS_VAL, SHF_STRINGS, 1);
    d_strSymIdx = d_elf.addSectionSymbol(d_strSecIdx);

    d_lineSecIdx = d_elf.addSection(QByteArray(".debug_line"), SHT_PROGBITS_VAL, 0, 1);
    d_lineSymIdx = d_elf.addSectionSymbol(d_lineSecIdx);

    d_frameSecIdx = d_elf.addSection(QByteArray(".debug_frame"), SHT_PROGBITS_VAL, 0, 4);
}

void DwarfEmitter::appendULEB128(QByteArray& buf, quint32 value)
{
    do {
        quint8 byte = value & 0x7F;
        value >>= 7;
        if (value != 0)
            byte |= 0x80; // more bytes to come
        buf.append(static_cast<char>(byte));
    } while (value != 0);
}

void DwarfEmitter::appendSLEB128(QByteArray& buf, qint32 value)
{
    bool more = true;
    while (more) {
        quint8 byte = value & 0x7F;
        value >>= 7;
        // sign bit of byte is second high order bit (0x40)
        if ((value == 0 && !(byte & 0x40)) || (value == -1 && (byte & 0x40))) {
            more = false;
        } else {
            byte |= 0x80;
        }
        buf.append(static_cast<char>(byte));
    }
}

void DwarfEmitter::appendString(QByteArray& buf, const QByteArray& str)
{
    buf.append(str);
    buf.append('\0');
}

quint32 DwarfEmitter::addDebugString(const QByteArray& str)
{
    QMap<QByteArray, quint32>::const_iterator it = d_strOffsets.find(str);
    if (it != d_strOffsets.end())
        return it.value();

    quint32 offset = d_str.size();
    d_str.append(str);
    d_str.append('\0');
    d_strOffsets.insert(str, offset);
    return offset;
}

void DwarfEmitter::appendStringRef(const QByteArray& str)
{
    DwarfReloc r;
    r.offset = d_info.size();
    r.target = RELOC_STR;
    d_infoRelocs.append(r);
    appendLE32(d_info, addDebugString(str));
}

void DwarfEmitter::beginCompilationUnit(const QByteArray& filename,
                                        const QByteArray& directory,
                                        const QByteArray& producer)
{
    // filename: source file name
    // directory: source directory

    Q_ASSERT(!d_inCU);
    d_inCU = true;
    d_filename = filename;
    d_directory = directory;

    d_cuStartOffset = d_info.size();

    appendLE32(d_info, 0);    // unit_length placeholder
    appendLE16(d_info, 4);    // DWARF version 4

    // Abbrev offset - needs relocation against .debug_abbrev
    {
        DwarfReloc rAbbrev;
        rAbbrev.offset = d_info.size();
        rAbbrev.target = RELOC_ABBREV;
        d_infoRelocs.append(rAbbrev);
        appendLE32(d_info, 0);    // abbrev offset (relocated)
    }
    d_info.append(static_cast<char>(4)); // address_size = 4 bytes

    appendULEB128(d_info, ABBREV_COMPILE_UNIT);

    // DW_AT_producer needs relocation against .debug_str
    appendStringRef(producer);

    // Use DW_LANG_Pascal83 as closest standard language to Oberon/Micron.
    // This enables GDB's info args/info locals to work with the DWARF entries.
    appendLE16(d_info, DW_LANG_Pascal83);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(filename);

    // DW_AT_comp_dir needs relocation against .debug_str
    appendStringRef(directory);

    // DW_AT_low_pc placeholder, patched in endCompilationUnit
    // Record relocation for this address (will be patched to actual .text offset)
    DwarfReloc r; r.offset = d_info.size(); r.target = RELOC_TEXT;
    d_infoRelocs.append(r);
    appendLE32(d_info, 0);

    // DW_AT_high_pc length, no relocation needed
    appendLE32(d_info, 0);

    // DW_AT_stmt_list offset into .debug_line
    {
        DwarfReloc rLine;
        rLine.offset = d_info.size();
        rLine.target = RELOC_LINE;
        d_infoRelocs.append(rLine);
        appendLE32(d_info, 0); // relocated to .debug_line
    }
}

void DwarfEmitter::endCompilationUnit()
{
    Q_ASSERT(d_inCU);

    d_info.append(static_cast<char>(0));

    // Patch the CU header
    quint32 cuSize = d_info.size() - d_cuStartOffset - 4;
    patchLE32(d_info, d_cuStartOffset, cuSize);

    // Patch low_pc and high_pc
    // CU header 11 bytes, tABBREV_COMPILE_UNIT ULEB128 1 byte,
    // producer(4) + language(2) + name(4) + comp_dir(4) = 14 bytes
    // low_pc at offset +11 +1 +14 = +26 from CU start
    // high_pc at offset +30 from CU start
    quint32 lowPcOffset = d_cuStartOffset + 11 + 1 + 14;
    quint32 highPcOffset = lowPcOffset + 4;
    if (d_lowPC <= d_highPC) {
        patchLE32(d_info, lowPcOffset, d_lowPC);
        patchLE32(d_info, highPcOffset, d_highPC - d_lowPC); // high_pc is length (FORM_data4)
    }

    d_inCU = false;
}

void DwarfEmitter::addSubprogram(const QByteArray& name, quint32 lowPC, quint32 highPC,
                                 quint32 line, quint32 prologuePC)
{
    // lowPC/highPC: start/end address in .text
    // prologuePC: address where push ebp; mov ebp, esp starts (may differ from lowPC
    //   for begin$ functions that have a guard check before the prologue)

    Q_ASSERT(d_inCU);

    if (lowPC < d_lowPC) d_lowPC = lowPC;
    if (highPC > d_highPC) d_highPC = highPC;

    appendULEB128(d_info, ABBREV_SUBPROGRAM);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(name);

    // DW_AT_low_pc needs relocation against .text
    DwarfReloc r; r.offset = d_info.size(); r.target = RELOC_TEXT;
    d_infoRelocs.append(r);
    appendLE32(d_info, lowPC);

    // DW_AT_high_pc length, no relocation needed
    appendLE32(d_info, highPC - lowPC);

    // Record bounds for FDE generation
    SubprogramBounds bounds;
    bounds.lowPC = lowPC;
    bounds.highPC = highPC;
    bounds.prologuePC = (prologuePC > 0) ? prologuePC : lowPC;
    d_fdes.append(bounds);

    appendULEB128(d_info, line);

    QByteArray expr;
    expr.append(static_cast<char>(d_arch == ElfWriter::ArchX86 ? DW_OP_reg5 : DW_OP_reg11));
    appendULEB128(d_info, expr.size());
    d_info.append(expr);

    // DW_AT_external always present for exported procs
}

void DwarfEmitter::addParameter(const QByteArray& name, qint32 fpOffset, quint32 typeRef)
{
    // fpOffset: offset relative to frame pointer (FP + 8 + paramOffset)

    Q_ASSERT(d_inCU);

    appendULEB128(d_info, ABBREV_PARAMETER);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(name);

    // DW_AT_type offset within CU
    appendLE32(d_info, typeRef);

    // DW_AT_location (DW_FORM_exprloc)
    QByteArray expr;
    expr.append(static_cast<char>(DW_OP_fbreg));
    appendSLEB128(expr, fpOffset);
    appendULEB128(d_info, expr.size());
    d_info.append(expr);

    appendULEB128(d_info, 0); // line info not always available for params
}

void DwarfEmitter::addLocalVariable(const QByteArray& name, qint32 fpOffset, quint32 typeRef)
{
    // fpOffset: offset relative to frame pointer (FP - localsSize + localOffset)

    Q_ASSERT(d_inCU);

    appendULEB128(d_info, ABBREV_VARIABLE);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(name);

    appendLE32(d_info, typeRef);

    QByteArray expr;
    expr.append(static_cast<char>(DW_OP_fbreg));
    appendSLEB128(expr, fpOffset);
    appendULEB128(d_info, expr.size());
    d_info.append(expr);

    appendULEB128(d_info, 0);
}

void DwarfEmitter::endSubprogram()
{
    d_info.append(static_cast<char>(0));
}

void DwarfEmitter::addModuleVariable(const QByteArray& name, quint32 address,
                                     quint32 typeRef, quint32 line)
{
    // address: absolute address in .data/.bss, relocated

    Q_ASSERT(d_inCU);

    appendULEB128(d_info, ABBREV_MODULE_VAR);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(name);

    appendLE32(d_info, typeRef);

    // The ULEB128 length prefix 1 (opcode) + 4 (address) = 5 bytes
    appendULEB128(d_info, 5); // exprloc length
    d_info.append(static_cast<char>(DW_OP_addr));
    // Record relocation for this address (against .data section)
    DwarfReloc r; r.offset = d_info.size(); r.target = RELOC_DATA;
    d_infoRelocs.append(r);
    appendLE32(d_info, address);

    appendULEB128(d_info, line);
}

quint32 DwarfEmitter::addBaseType(const QByteArray& name, quint8 encoding, quint8 byteSize)
{
    // Returns the .debug_info offset of the type DIE.

    quint32 offset = d_info.size() - d_cuStartOffset;

    appendULEB128(d_info, ABBREV_BASE_TYPE);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(name);

    d_info.append(static_cast<char>(encoding));

    d_info.append(static_cast<char>(byteSize));

    return offset;
}

quint32 DwarfEmitter::addPointerType(quint32 baseTypeRef)
{
    quint32 offset = d_info.size() - d_cuStartOffset;

    appendULEB128(d_info, ABBREV_POINTER_TYPE);

    d_info.append(static_cast<char>(4));

    // DW_AT_type pointed-to type
    appendLE32(d_info, baseTypeRef);

    return offset;
}

quint32 DwarfEmitter::addStructType(const QByteArray& name, quint32 byteSize)
{
    quint32 offset = d_info.size() - d_cuStartOffset;

    // has children: members
    appendULEB128(d_info, ABBREV_STRUCT_TYPE);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(name);

    appendULEB128(d_info, byteSize);

    return offset;
}

void DwarfEmitter::addStructMember(const QByteArray& name, quint32 typeRef, quint32 byteOffset)
{
    appendULEB128(d_info, ABBREV_MEMBER);

    // DW_AT_name needs relocation against .debug_str
    appendStringRef(name);

    appendLE32(d_info, typeRef);

    appendULEB128(d_info, byteOffset);
}

void DwarfEmitter::endStructType()
{
    d_info.append(static_cast<char>(0));
}

quint32 DwarfEmitter::addArrayType(quint32 elemTypeRef, quint32 count)
{
    quint32 offset = d_info.size() - d_cuStartOffset;

    // children: subrange
    appendULEB128(d_info, ABBREV_ARRAY_TYPE);

    // DW_AT_type element type
    appendLE32(d_info, elemTypeRef);

    if (count > 0) {
        appendULEB128(d_info, ABBREV_SUBRANGE);
        appendULEB128(d_info, count);
    }

    d_info.append(static_cast<char>(0));

    return offset;
}

void DwarfEmitter::addLineEntry(quint32 address, quint32 line, quint32 column)
{
    LineEntry entry;
    entry.address = address;
    entry.line = line;
    entry.column = column;
    d_lineEntries.append(entry);
}

quint32 DwarfEmitter::getOrCreateType(Type* t)
{
    // Caches results to avoid duplicate type DIEs.

    if (!t)
        return 0;

    // Check cache
    QMap<Type*, quint32>::const_iterator it = d_typeCache.find(t);
    if (it != d_typeCache.end())
        return it.value();

    quint32 ref = 0;

    switch (t->kind) {
    case Type::BOOL:
        ref = addBaseType("bool", DW_ATE_boolean, 1);
        break;
    case Type::CHAR:
        ref = addBaseType("char", DW_ATE_unsigned_char, 1);
        break;
    case Type::INT8:
        ref = addBaseType("int8", DW_ATE_signed, 1);
        break;
    case Type::INT16:
        ref = addBaseType("int16", DW_ATE_signed, 2);
        break;
    case Type::INT32:
        ref = addBaseType("int32", DW_ATE_signed, 4);
        break;
    case Type::INT64:
        ref = addBaseType("int64", DW_ATE_signed, 8);
        break;
    case Type::UINT8:
        ref = addBaseType("uint8", DW_ATE_unsigned, 1);
        break;
    case Type::UINT16:
        ref = addBaseType("uint16", DW_ATE_unsigned, 2);
        break;
    case Type::UINT32:
        ref = addBaseType("uint32", DW_ATE_unsigned, 4);
        break;
    case Type::UINT64:
        ref = addBaseType("uint64", DW_ATE_unsigned, 8);
        break;
    case Type::FLOAT32:
        ref = addBaseType("float32", DW_ATE_float, 4);
        break;
    case Type::FLOAT64:
        ref = addBaseType("float64", DW_ATE_float, 8);
        break;
    case Type::INTPTR:
        ref = addBaseType("intptr", DW_ATE_address, 4);
        break;
    case Type::DBLINTPTR:
        ref = addBaseType("dblintptr", DW_ATE_unsigned, 8);
        break;
    case Type::Pointer: {
        quint32 baseRef = 0;
        if (t->getType())
            baseRef = getOrCreateType(t->getType());
        ref = addPointerType(baseRef);
        break;
    }
    case Type::Struct:
    case Type::Object:
    case Type::Union: {
        QByteArray name;
        if (t->decl)
            name = t->decl->name;
        else
            name = "<anon>";
        ref = addStructType(name, t->getByteSize(4));
        d_typeCache.insert(t, ref);
        foreach (Declaration* field, t->subs) {
            if (field->kind == Declaration::Field) {
                quint32 fieldTypeRef = 0;
                if (field->getType())
                    fieldTypeRef = getOrCreateType(field->getType());
                addStructMember(field->name, fieldTypeRef, field->f.off);
            }
        }
        endStructType();
        return ref; // already cached
    }
    case Type::Array: {
        quint32 elemRef = 0;
        if (t->getType())
            elemRef = getOrCreateType(t->getType());
        ref = addArrayType(elemRef, t->len);
        break;
    }
    default:
        // For unhandled types, emit a void-like base type
        ref = addBaseType("void", DW_ATE_address, 0);
        break;
    }

    d_typeCache.insert(t, ref);
    return ref;
}

void DwarfEmitter::buildAbbrevTable()
{
    d_abbrev.clear();

    appendULEB128(d_abbrev, ABBREV_COMPILE_UNIT);
    appendULEB128(d_abbrev, DW_TAG_compile_unit);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_yes));
    appendULEB128(d_abbrev, DW_AT_producer);   appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_language);   appendULEB128(d_abbrev, DW_FORM_data2);
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_comp_dir);   appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_low_pc);     appendULEB128(d_abbrev, DW_FORM_addr);
    appendULEB128(d_abbrev, DW_AT_high_pc);    appendULEB128(d_abbrev, DW_FORM_data4);
    appendULEB128(d_abbrev, DW_AT_stmt_list);  appendULEB128(d_abbrev, DW_FORM_sec_offset);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0); // end of attrs

    appendULEB128(d_abbrev, ABBREV_SUBPROGRAM);
    appendULEB128(d_abbrev, DW_TAG_subprogram);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_yes));
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_low_pc);     appendULEB128(d_abbrev, DW_FORM_addr);
    appendULEB128(d_abbrev, DW_AT_high_pc);    appendULEB128(d_abbrev, DW_FORM_data4);
    appendULEB128(d_abbrev, DW_AT_decl_line);  appendULEB128(d_abbrev, DW_FORM_udata);
    appendULEB128(d_abbrev, DW_AT_frame_base); appendULEB128(d_abbrev, DW_FORM_exprloc);
    appendULEB128(d_abbrev, DW_AT_external);   appendULEB128(d_abbrev, DW_FORM_flag_present);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_PARAMETER);
    appendULEB128(d_abbrev, DW_TAG_formal_parameter);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_no));
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_type); appendULEB128(d_abbrev, DW_FORM_ref4);
    appendULEB128(d_abbrev, DW_AT_location);   appendULEB128(d_abbrev, DW_FORM_exprloc);
    appendULEB128(d_abbrev, DW_AT_decl_line);  appendULEB128(d_abbrev, DW_FORM_udata);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_VARIABLE);
    appendULEB128(d_abbrev, DW_TAG_variable);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_no));
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_type); appendULEB128(d_abbrev, DW_FORM_ref4);
    appendULEB128(d_abbrev, DW_AT_location);   appendULEB128(d_abbrev, DW_FORM_exprloc);
    appendULEB128(d_abbrev, DW_AT_decl_line);  appendULEB128(d_abbrev, DW_FORM_udata);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_BASE_TYPE);
    appendULEB128(d_abbrev, DW_TAG_base_type);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_no));
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_encoding);   appendULEB128(d_abbrev, DW_FORM_data1);
    appendULEB128(d_abbrev, DW_AT_byte_size);  appendULEB128(d_abbrev, DW_FORM_data1);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_POINTER_TYPE);
    appendULEB128(d_abbrev, DW_TAG_pointer_type);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_no));
    appendULEB128(d_abbrev, DW_AT_byte_size);  appendULEB128(d_abbrev, DW_FORM_data1);
    appendULEB128(d_abbrev, DW_AT_type); appendULEB128(d_abbrev, DW_FORM_ref4);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_STRUCT_TYPE);
    appendULEB128(d_abbrev, DW_TAG_structure_type);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_yes));
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_byte_size);  appendULEB128(d_abbrev, DW_FORM_udata);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_MEMBER);
    appendULEB128(d_abbrev, DW_TAG_member);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_no));
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_type); appendULEB128(d_abbrev, DW_FORM_ref4);
    appendULEB128(d_abbrev, DW_AT_data_member_location); appendULEB128(d_abbrev, DW_FORM_udata);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_ARRAY_TYPE);
    appendULEB128(d_abbrev, DW_TAG_array_type);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_yes));
    appendULEB128(d_abbrev, DW_AT_type); appendULEB128(d_abbrev, DW_FORM_ref4);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_SUBRANGE);
    appendULEB128(d_abbrev, DW_TAG_subrange_type);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_no));
    appendULEB128(d_abbrev, DW_AT_count);appendULEB128(d_abbrev, DW_FORM_udata);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    appendULEB128(d_abbrev, ABBREV_MODULE_VAR);
    appendULEB128(d_abbrev, DW_TAG_variable);
    d_abbrev.append(static_cast<char>(DW_CHILDREN_no));
    appendULEB128(d_abbrev, DW_AT_name); appendULEB128(d_abbrev, DW_FORM_strp);
    appendULEB128(d_abbrev, DW_AT_type); appendULEB128(d_abbrev, DW_FORM_ref4);
    appendULEB128(d_abbrev, DW_AT_location);   appendULEB128(d_abbrev, DW_FORM_exprloc);
    appendULEB128(d_abbrev, DW_AT_decl_line);  appendULEB128(d_abbrev, DW_FORM_udata);
    appendULEB128(d_abbrev, DW_AT_external);   appendULEB128(d_abbrev, DW_FORM_flag_present);
    appendULEB128(d_abbrev, 0); appendULEB128(d_abbrev, 0);

    d_abbrev.append(static_cast<char>(0));
}

void DwarfEmitter::buildDebugFrame()
{
    d_frame.clear();

    // Frame layout depends on architecture:
    // ARM: PUSH {R11, LR} -> CFA = R11 + 8, R11 at CFA-8, LR at CFA-4
    // x86: PUSH EBP; MOV EBP,ESP -> CFA = EBP + 8, EBP at CFA-8, EIP at CFA-4
    //
    // The CIE defines the initial rules valid at function entry (before the
    // prologue): CFA = ESP + 4, return address at [CFA-4] = [ESP].
    // Each FDE then advances through the prologue to describe the state after
    // push ebp; mov ebp, esp.

    const bool isX86 = (d_arch == ElfWriter::ArchX86);
    const quint32 spReg = isX86 ? 4 : 13;        // ESP=4, SP=13
    const quint32 fpReg = isX86 ? 5 : 11;         // EBP=5, R11=11
    const quint32 raReg = isX86 ? 8 : 14;         // EIP=8, LR=14
    const quint32 codeAlign = isX86 ? 1 : 4;      // x86 variable-length, ARM 4-byte

    int cieStart = d_frame.size();
    appendLE32(d_frame, 0); // length placeholder

    appendLE32(d_frame, 0xFFFFFFFF); // marks this as CIE, not FDE

    d_frame.append(static_cast<char>(4)); // v4

    d_frame.append('\0'); // augmentation (empty)

    d_frame.append(static_cast<char>(4)); // address size 4

    d_frame.append(static_cast<char>(0)); // segment size

    appendULEB128(d_frame, codeAlign);

    appendSLEB128(d_frame, -4); // data_alignment_factor: stack grows down, word-sized slots

    appendULEB128(d_frame, raReg);

    // CIE initial instructions: state at function entry (before prologue).
    // CFA = ESP + 4 (return address is at [ESP], so CFA is just above it).
    // Return address (EIP) is saved at CFA - 4 = [ESP].
    d_frame.append(static_cast<char>(DW_CFA_def_cfa));
    appendULEB128(d_frame, spReg);
    appendULEB128(d_frame, 4);  // CFA = ESP + 4

    d_frame.append(static_cast<char>(DW_CFA_offset | raReg));
    appendULEB128(d_frame, 1); // EIP at CFA - 1*4 = CFA - 4 = [ESP]

    while ((d_frame.size() - cieStart) % 4 != 0)
        d_frame.append(static_cast<char>(DW_CFA_nop));

    quint32 cieLen = d_frame.size() - cieStart - 4;
    patchLE32(d_frame, cieStart, cieLen);

    // Emit FDEs for each subprogram
    for (int i = 0; i < d_fdes.size(); i++) {
        int fdeStart = d_frame.size();
        appendLE32(d_frame, 0); // length placeholder

        // CIE pointer: in .debug_frame this is the byte offset to the CIE
        // from the beginning of the section (which is 0 = cieStart).
        appendLE32(d_frame, cieStart);

        // Relocated start address of the function
        DwarfReloc r;
        r.offset = d_frame.size();
        r.target = RELOC_TEXT;
        d_frameRelocs.append(r);
        appendLE32(d_frame, d_fdes[i].lowPC);

        // Length of the function
        appendLE32(d_frame, d_fdes[i].highPC - d_fdes[i].lowPC);

        // Emit prologue CFA instructions.
        // The prologue (push ebp; mov ebp, esp) may not start at lowPC for
        // begin$ functions that have a guard check.  We first advance to
        // prologuePC, then describe the prologue.
        quint32 prologuePC = d_fdes[i].prologuePC;
        quint32 lowPC = d_fdes[i].lowPC;

        // Advance from lowPC to the push ebp instruction (if there is a gap)
        quint32 guardLen = prologuePC - lowPC;
        if (guardLen > 0) {
            // Use DW_CFA_advance_loc variants depending on delta size
            if (guardLen <= 63) {
                d_frame.append(static_cast<char>(DW_CFA_advance_loc | guardLen));
            } else {
                d_frame.append(static_cast<char>(0x02)); // DW_CFA_advance_loc1
                d_frame.append(static_cast<char>(guardLen & 0xFF));
            }
        }

        // After push ebp (1 byte on x86): CFA = ESP + 8, EBP saved at CFA - 8
        quint32 pushSize = isX86 ? 1 : 4;
        if (pushSize <= 63)
            d_frame.append(static_cast<char>(DW_CFA_advance_loc | pushSize));
        else
            d_frame.append(static_cast<char>(DW_CFA_advance_loc | 1)); // fallback

        d_frame.append(static_cast<char>(DW_CFA_def_cfa_offset));
        appendULEB128(d_frame, 8); // CFA = ESP + 8

        d_frame.append(static_cast<char>(DW_CFA_offset | fpReg));
        appendULEB128(d_frame, 2); // EBP at CFA - 2*4 = CFA - 8

        // After mov ebp, esp (2 bytes on x86): CFA = EBP + 8
        quint32 movSize = isX86 ? 2 : 4;
        if (movSize <= 63)
            d_frame.append(static_cast<char>(DW_CFA_advance_loc | movSize));
        else
            d_frame.append(static_cast<char>(DW_CFA_advance_loc | 1)); // fallback

        d_frame.append(static_cast<char>(DW_CFA_def_cfa_register));
        appendULEB128(d_frame, fpReg); // CFA = EBP + 8 (offset unchanged)

        while ((d_frame.size() - fdeStart) % 4 != 0)
            d_frame.append(static_cast<char>(DW_CFA_nop));

        quint32 fdeLen = d_frame.size() - fdeStart - 4;
        patchLE32(d_frame, fdeStart, fdeLen);
    }
}

void DwarfEmitter::buildDebugLine()
{
    d_line.clear();

    int headerStart = d_line.size();
    appendLE32(d_line, 0); // unit_length placeholder

    appendLE16(d_line, 4); // version = 4

    int headerLenOffset = d_line.size();
    appendLE32(d_line, 0); // header_length placeholder

    int headerBodyStart = d_line.size();

    d_line.append(static_cast<char>(d_arch == ElfWriter::ArchX86 ? 1 : 4));

    d_line.append(static_cast<char>(1));

    d_line.append(static_cast<char>(1));

    d_line.append(static_cast<char>(static_cast<qint8>(-5))); // line base

    d_line.append(static_cast<char>(14)); // line range

    d_line.append(static_cast<char>(13)); // opcode base

    d_line.append(static_cast<char>(0)); // DW_LNS_copy
    d_line.append(static_cast<char>(1)); // DW_LNS_advance_pc
    d_line.append(static_cast<char>(1)); // DW_LNS_advance_line
    d_line.append(static_cast<char>(1)); // DW_LNS_set_file
    d_line.append(static_cast<char>(1)); // DW_LNS_set_column
    d_line.append(static_cast<char>(0)); // DW_LNS_negate_stmt
    d_line.append(static_cast<char>(0)); // DW_LNS_set_basic_block
    d_line.append(static_cast<char>(0)); // DW_LNS_const_add_pc
    d_line.append(static_cast<char>(1)); // DW_LNS_fixed_advance_pc
    d_line.append(static_cast<char>(0)); // DW_LNS_set_prologue_end
    d_line.append(static_cast<char>(0)); // DW_LNS_set_epilogue_begin
    d_line.append(static_cast<char>(1)); // DW_LNS_set_isa

    // Include the compilation directory
    if (!d_directory.isEmpty()) {
        d_line.append(d_directory);
        d_line.append('\0');
    }
    d_line.append('\0');

    if (!d_filename.isEmpty()) {
        d_line.append(d_filename);
        d_line.append('\0');
        appendULEB128(d_line, d_directory.isEmpty() ? 0 : 1); // directory index
        appendULEB128(d_line, 0); // last modification time
        appendULEB128(d_line, 0); // file size
    }
    d_line.append('\0');

    quint32 headerLen = d_line.size() - headerBodyStart;
    patchLE32(d_line, headerLenOffset, headerLen);

    std::sort(d_lineEntries.begin(), d_lineEntries.end(), sortLineEntry);

    quint32 curAddr = 0;
    quint32 curLine = 1;

    static const quint8 DW_LNS_copy = 1;
    static const quint8 DW_LNS_advance_pc = 2;
    static const quint8 DW_LNS_advance_line = 3;
    static const quint8 DW_LNS_set_file = 4;
    static const quint8 DW_LNE_end_sequence = 1;
    static const quint8 DW_LNE_set_address = 2;

    for (int i = 0; i < d_lineEntries.size(); i++) {
        const LineEntry& entry = d_lineEntries[i];

        if (i == 0) {
            d_line.append(static_cast<char>(0)); // extended opcode marker
            appendULEB128(d_line, 5); // length: 1 (opcode) + 4 (address)
            d_line.append(static_cast<char>(DW_LNE_set_address));
            DwarfReloc lr; lr.offset = d_line.size(); lr.target = RELOC_TEXT;
            d_lineRelocs.append(lr);
            appendLE32(d_line, entry.address);
            curAddr = entry.address;
        } else {
            quint32 addrDelta = entry.address - curAddr;
            if (addrDelta > 0) {
                d_line.append(static_cast<char>(DW_LNS_advance_pc));
                quint32 minInsnLen = (d_arch == ElfWriter::ArchX86) ? 1 : 4;
                appendULEB128(d_line, addrDelta / minInsnLen); // in min_instruction_length units
                curAddr = entry.address;
            }
        }

        qint32 lineDelta = (qint32)entry.line - (qint32)curLine;
        if (lineDelta != 0) {
            d_line.append(static_cast<char>(DW_LNS_advance_line));
            appendSLEB128(d_line, lineDelta);
            curLine = entry.line;
        }

        if (i == 0) {
            d_line.append(static_cast<char>(DW_LNS_set_file));
            appendULEB128(d_line, 1);
        }

        d_line.append(static_cast<char>(DW_LNS_copy));
    }

    // Advance PC to the end of the compilation unit before emitting end_sequence
    // so the debugger knows the last statement's address range extends to highPC.
    {
        quint32 addrDelta = d_highPC - curAddr;
        if (addrDelta > 0) {
            d_line.append(static_cast<char>(DW_LNS_advance_pc));
            quint32 minInsnLen = (d_arch == ElfWriter::ArchX86) ? 1 : 4;
            appendULEB128(d_line, addrDelta / minInsnLen);
        }
    }

    d_line.append(static_cast<char>(0));
    appendULEB128(d_line, 1); // length = 1
    d_line.append(static_cast<char>(DW_LNE_end_sequence));

    quint32 unitLen = d_line.size() - headerStart - 4;
    patchLE32(d_line, headerStart, unitLen);
}

void DwarfEmitter::finalize(quint32 textSymIdx, quint32 dataSymIdx)
{
    // Must be called after all DIEs and line entries have been added.
    // textSymIdx and dataSymIdx are the ELF section symbol indices for .text
    // and .data, used to generate relocations in .rel.debug_info and .rel.debug_line.
    //
    // The debug sections and their section symbols were already created in the
    // constructor (to avoid shifting global symbol indices).  Here we just
    // build the content and append it to the pre-created sections.

    buildAbbrevTable();
    buildDebugFrame();
    buildDebugLine();

    d_elf.appendToSection(d_abbrevSecIdx, d_abbrev);
    d_elf.appendToSection(d_infoSecIdx, d_info);
    d_elf.appendToSection(d_strSecIdx, d_str);
    d_elf.appendToSection(d_lineSecIdx, d_line);
    d_elf.appendToSection(d_frameSecIdx, d_frame);

    if (!d_infoRelocs.isEmpty()) {
        quint32 relInfoIdx = d_elf.addSection(QByteArray(".rel.debug_info"), SHT_REL_VAL, 0, 4);
        d_elf.configureRelSection(relInfoIdx, d_infoSecIdx);
        for (int i = 0; i < d_infoRelocs.size(); i++) {
            const DwarfReloc& r = d_infoRelocs[i];
            quint32 symIdx = 0;
            if (r.target == RELOC_TEXT) symIdx = textSymIdx;
            else if (r.target == RELOC_DATA) symIdx = dataSymIdx;
            else if (r.target == RELOC_STR) symIdx = d_strSymIdx;
            else if (r.target == RELOC_ABBREV) symIdx = d_abbrevSymIdx;
            else if (r.target == RELOC_LINE) symIdx = d_lineSymIdx;
            quint8 relType = (d_arch == ElfWriter::ArchX86) ? R_386_32_VAL : R_ARM_ABS32_VAL;
            d_elf.addRelocation(relInfoIdx, r.offset, symIdx, relType);
        }
    }

    if (!d_lineRelocs.isEmpty()) {
        quint32 relLineIdx = d_elf.addSection(QByteArray(".rel.debug_line"), SHT_REL_VAL, 0, 4);
        d_elf.configureRelSection(relLineIdx, d_lineSecIdx);
        quint8 relType = (d_arch == ElfWriter::ArchX86) ? R_386_32_VAL : R_ARM_ABS32_VAL;
        for (int i = 0; i < d_lineRelocs.size(); i++) {
            const DwarfReloc& r = d_lineRelocs[i];
            quint32 symIdx = (r.target == RELOC_TEXT) ? textSymIdx : dataSymIdx;
            d_elf.addRelocation(relLineIdx, r.offset, symIdx, relType);
        }
    }

    if (!d_frameRelocs.isEmpty()) {
        quint32 relFrameIdx = d_elf.addSection(QByteArray(".rel.debug_frame"), SHT_REL_VAL, 0, 4);
        d_elf.configureRelSection(relFrameIdx, d_frameSecIdx);
        quint8 relType = (d_arch == ElfWriter::ArchX86) ? R_386_32_VAL : R_ARM_ABS32_VAL;
        for (int i = 0; i < d_frameRelocs.size(); i++) {
            const DwarfReloc& r = d_frameRelocs[i];
            quint32 symIdx = (r.target == RELOC_TEXT) ? textSymIdx : dataSymIdx;
            d_elf.addRelocation(relFrameIdx, r.offset, symIdx, relType);
        }
    }
}
