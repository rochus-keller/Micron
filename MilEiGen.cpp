/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MilEiGen.h"
#include "EiGen/layout.hpp"
#include "EiGen/cdemitter.hpp"
#include "EiGen/cdemittercontext.hpp"
#include "EiGen/stdcharset.hpp"
#include "EiGen/stringpool.hpp"
#include "EiGen/strdiagnostics.hpp"
#include "EiGen/cdgenerator.hpp"
#include <iostream>
#include <deque>
#include <QTextStream>
#include <QCoreApplication>
#include <QDateTime>
#include <QtDebug>
#include "MilProject.h"
#include "MilValidator.h"
using namespace Mil;
using namespace ECS;

struct TargetData
{
    const char*const name;
    const char*const backend;
    EiGen::ProcessorCode architecture;
    bool has_linkregister; // 0, 1
    unsigned char pointer_width; // 2, 4, 8
    unsigned char stack_align; // 4, 8, 16
    unsigned char int_width; // usually 4
    const bool executable;
    const char*const description;
    const char*const converter;
};
static const struct TargetData target_data[] = {
    {"", "none", EiGen::NoProcessor, false, 0, 0, 0, true, "undefined", 0},
    {"amd32linux", "amd32", EiGen::Amd32, false, 4, 4, 4, true, "Linux-based 32-bit systems", "dbgdwarf"}, // amd32run.obf amd32linuxrun.obf
    {"amd64linux", "amd64", EiGen::Amd64, false, 8, 8, 4, true, "Linux-based 64-bit systems", "dbgdwarf"}, // amd64run.obf amd64linuxrun.obf
    {"arma32linux", "arma32", EiGen::Arma32, true, 4, 4, 4, true, "Linux-based systems", "dbgdwarf"}, // arma32run.obf arma32linuxrun.obf
    {"arma64linux", "arma64", EiGen::Arma64, true, 8, 16, 4, true, "Linux-based systems", "dbgdwarf"},
    {"armt32linux", "armt32", EiGen::Armt32, true, 4, 4, 4, true, "Linux-based systems", "dbgdwarf"},
    {"armt32fpelinux", "armt32fpe", EiGen::Armt32, true, 4, 4, 4, true, "Linux-based systems", "dbgdwarf"}, // armt32fperun.obf armt32fpelinuxrun.obf
    {"bios16", "amd16", EiGen::Amd16, false, 2, 2, 2, false, "BIOS-based 16-bit systems", 0}, // amd16run.obf bios16run.obf
    {"bios32", "amd32", EiGen::Amd32, false, 4, 4, 4, false, "BIOS-based 32-bit systems", 0},
    {"bios64", "amd64", EiGen::Amd64, false, 8, 8, 4, false, "BIOS-based 64-bit systems", 0},
    {"dos", "amd16", EiGen::Amd16, false, 2, 2, 2, false, "DOS systems", 0},
    {"efi32", "amd32", EiGen::Amd32, false, 4, 4, 4, false, "EFI-based 32-bit systems", 0},
    {"efi64", "amd64", EiGen::Amd64, false, 8, 8, 4, false, "EFI-based 64-bit systems", 0},
    {"osx32", "amd32", EiGen::Amd32, false, 4, 4, 4, true, "32-bit OS X systems", "dbgdwarf"},
    {"osx64", "amd64", EiGen::Amd64, false, 8, 8, 4, true, "64-bit OS X systems", "dbgdwarf"},
    {"rpi2b", "arma32", EiGen::Arma32, true, 4, 4, 4, false, "Raspberry Pi 2 Model B", 0},
    {"win32", "amd32", EiGen::Amd32, false, 4, 4, 4, false, "32-bit Windows systems", 0},
    {"win64", "amd64", EiGen::Amd64, false, 8, 8, 4, false, "64-bit Windows systems", 0},
    {"amd16", "amd16", EiGen::Amd16, false, 2, 2, 2, false, "barebone x86 real mode", 0},
    {"amd32", "amd32", EiGen::Amd32, false, 4, 4, 4, false, "barebone x86 protected mode", 0},
    {"amd64", "amd64", EiGen::Amd64, false, 8, 8, 4, false, "barebone x86 long mode", 0},
    {"arma32", "arma32", EiGen::Arma32, true, 4, 4, 4, false, "barebone ARMv7", 0},
    {"armt32", "armt32", EiGen::Armt32, true, 4, 4, 4, false, "barebone ARMv7 thumb", 0},
    {"arma64", "arma64", EiGen::Arma64, true, 8, 16, 4, false, "barebone ARMv8", 0},
};

static const struct EcsTypeInfo {
    uint8_t width; // number of bits
    const char* name;
    const char* dbgname;
} ecsTypes[] = {
    { 8, "u1", "byte" },
    { 16, "u2", "ushort" },
    { 32, "u4", "uint" },
    { 64, "u8", "ulong" },
    { 8, "s1", "char" },
    { 16, "s2", "short" },
    { 32, "s4", "int" },
    { 64, "s8", "long" },
    { 32, "f4", "float" },
    { 64, "f8", "double" },
    { 0, "ptr", "pointer" },
    { 0, "fun", "function" },
};

class MyEmitter : public Code::Emitter
{
public:
    MyEmitter(Diagnostics& d, StringPool& sp, Charset& c, Code::Platform& p):
        Code::Emitter(d,sp,c,p),ctx(*this,s) {}

    using Smop = Emitter::Context::SmartOperand;
    using Label = Emitter::Context::Label;
    using Context = Code::Emitter::Context;
    using RestoreRegisterState = Code::Emitter::Context::RestoreRegisterState;

    Code::Sections s;
    Context ctx;
};

typedef MyEmitter::Smop Smop;

class EiGen::Imp
{
public:
    enum EcsType { u1, u2, u4, u8, s1, s2, s4, s8, f4, f8, ptr, fun, MaxType };

    AstModel* mdl;
    Declaration* curMod;
    Declaration* curProc;
    ASCIICharset charset;
    StringPool stringPool;
    StreamDiagnostics diagnostics;
    Layout layout;
    Code::Platform platform;
    MyEmitter emitter;
    TargetCode target;
    Code::Type types[MaxType];
    QHash<QByteArray, const char*> strings;
    QList<MyEmitter::Label*> loopStack;
    std::vector<MyEmitter::Label> labels;
    QHash<const char*, MyEmitter::Label*> labelRefs;
    MyEmitter::Label* endOfProc;
    bool hasDebugInfo;

    Imp(AstModel* mdl, TargetCode tg);

    EcsType getEcsType(Type*) const;
    Code::Type getCodeType(Type*) const;
    void loc( const Mic::RowCol& ) const;
    static EcsType ilToEcsType(quint8 op);
    Smop ldind(Type * ty, const Smop& addr, const Mic::RowCol& pos);

    bool generate(Declaration* module);
    void visitModule();
    void visitProcedure(Declaration*);
    void visitMetaDecl(Declaration*);
    QByteArray typeRef(Type*);
    void locPar(Declaration* param);
    void variable(Declaration* var);
    void constValue(QTextStream& out, Constant* c);
    void statementSeq(Statement* s);
    Smop expression(Expression* e);
    Smop emitBinOP(Expression* e);
    void emitSoapInit(const QByteArray& name, Type* t, int level);
    void emitSoaInit(const QByteArray& name, bool nameIsPtr, Type* t, int level);
    Type* deref(Type* t);
    void emitInitializer(Type*);
    void typeDecl(Type*);

    // Round up `n` to the nearest multiple of `align`. For instance,
    // align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
    int align_to(int n, int align) {

        return (n + align - 1) / align * align;
    }

    int push(Type *valueType, const Smop& reg) {
        if( valueType->isSUOA() || (valueType->kind == Type::Proc && valueType->typebound) )
        {
            const int sizeOnStack = align_to(valueType->getByteSize(target_data[target].pointer_width),
                                    target_data[target].stack_align);
            // make enough space on stack
            emitter.ctx.Subtract(Code::Reg(types[ptr],Code::RSP),
                                 Code::Reg(types[ptr],Code::RSP),
                        Code::Imm(types[ptr],sizeOnStack));
            // copy the data to stack
            emitter.ctx.Copy(Code::Reg(types[ptr],Code::RSP), reg, Code::Imm(types[ptr],sizeOnStack));
            return sizeOnStack;
        }

        const Code::Type type = getCodeType(valueType);
        emitter.ctx.Push(emitter.ctx.Convert(type,reg));
        const int aligned_size = align_to(type.size,target_data[target].stack_align);
        return aligned_size;
    }

    void store(Type *valueType, const Smop& lhs, const Smop& rhs, const Mic::RowCol& pos) {
        if( valueType->isSUOA() || (valueType->kind == Type::Proc && valueType->typebound) )
        {
            loc(pos);
            emitter.ctx.Copy(lhs, rhs, Code::Imm(types[ptr],valueType->getByteSize(target_data[target].pointer_width)));
            return;
        }

        const Code::Type type = getCodeType(valueType);
        loc(pos);
        emitter.ctx.Move(emitter.ctx.MakeMemory(type,lhs), emitter.ctx.Convert(type,rhs));
    }

    void release(Smop& smop)
    {
        smop = Smop();
    }

    static inline ECS::Position toPos(const Mic::RowCol& pos)
    {
        return ECS::Position(pos.d_row, pos.d_col);
    }
};

EiGen::EiGen(AstModel* mdl, TargetCode tg)
{
    imp = new Imp(mdl, tg);
}

EiGen::~EiGen()
{
    delete imp;
}

// modified version of http://www.josuttis.com/cppcode/fdstream.hpp
class fdoutbuf : public std::streambuf {
  protected:
    QIODevice* fd;    // file descriptor
  public:
    // constructor
    fdoutbuf (QIODevice* _fd) : fd(_fd) {}
  protected:
    // write one character
    virtual int_type overflow (int_type c) {
        if( !fd->putChar(c) )
            return EOF;
        else
            return c;
    }
    // write multiple characters
    virtual
    std::streamsize xsputn (const char* s, std::streamsize num) {
        return fd->write(s, num);
    }
};

class fdostream : public std::ostream {
  protected:
    fdoutbuf buf;
  public:
    fdostream (QIODevice* fd) : std::ostream(0), buf(fd) {
        rdbuf(&buf);
    }
};

bool EiGen::generate(Declaration* module, QIODevice* out)
{
    Q_ASSERT(module && module->kind == Declaration::Module);

    // TODO: calc offsets in AST of module

    const bool res = imp->generate(module);
    if( res && out )
    {
        fdostream out_stream(out);

        out_stream << "; this is Eigen intermediate code, generated by the Micron compiler" << std::endl;
        out_stream << "; see https://github.com/rochus-keller/Micron for more information" << std::endl;
        out_stream << "; assuming pointer size " << (int)target_data[imp->target].pointer_width
          << ", int size " << (int)target_data[imp->target].int_width
          << ", stack alignment " << (int)target_data[imp->target].stack_align << " bytes, "
          << (target_data[imp->target].has_linkregister ? "with" : "no") << " link reg" << std::endl;

        Code::Generator cdgen(imp->layout,imp->platform);

        cdgen.Generate(imp->emitter.s, target_data[imp->target].name,out_stream);
    }
    return res;
}

EiGen::TargetCode EiGen::translate(const char* name, bool defaultToHost)
{
    for( int i = 1; i < MaxTarget; i++ )
    {
        if( strcmp(target_data[i].name, name) == 0 )
            return TargetCode(i);
    }
    if( defaultToHost )
    {
#ifdef Q_PROCESSOR_ARM
#ifdef Q_OS_LINUX
        return sizeof(void*) == 8 ? ARMA64Linux : ARMA32Linux;
#elif defined Q_OS_MACOS
        return NoTarget; // TODO
#endif
#elif defined Q_PROCESSOR_X86
#ifdef Q_OS_LINUX
        return sizeof(void*) == 8 ? AMD64Linux : AMD32Linux;
#elif defined Q_OS_WIN32
        return sizeof(void*) == 8 ? Win64 : Win32;
#elif defined Q_OS_MACOS
        return sizeof(void*) == 8 ? OSX64 : OSX32;
#endif
#endif
    }
    return NoTarget;
}

quint8 EiGen::pointer_width(EiGen::TargetCode t)
{
    return target_data[t].pointer_width;
}

quint8 EiGen::stack_align(EiGen::TargetCode t)
{
    return target_data[t].stack_align;
}

static QByteArray qualident(Declaration* d)
{
    if( d->outer )
        return qualident(d->outer) + "$" + d->forwardToProc()->name;
    else
        return d->name;
}

static uint32_t MurmurOAAT_32(const char* str, uint32_t h)
{
    // One-byte-at-a-time hash based on Murmur's mix
    // Source: https://github.com/aappleby/smhasher/blob/master/src/Hashes.cpp
    for (; *str; ++str) {
        h ^= *str;
        h *= 0x5bd1e995;
        h ^= h >> 15;
    }
    return h;
}

void string_hash(const char* in, char* out, int outlen ) // reusable
{
    const char* code = "0123456789"
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            "abcdefghijklmnopqrstuvwxyz";
    const int codelen = strlen(code);

    const int inlen = strlen(in);
    int i;
    out[outlen] = 0;
    for( i = 0; i < outlen; i++ )
        out[i] = '_';
    if( inlen < outlen )
        for( i = 0; i < inlen; i++ )
        {
            union { uint32_t h; char s[4]; };
            h = MurmurOAAT_32(in+i,0x12345678);
            for( int j = 0; j < 4; j++ )
                out[(i+j) % outlen] ^= s[j];
        }
    else
        for( i = 0; i < inlen; i++ )
        {
            const int j = i % outlen;
            out[j] ^= in[i];
        }
    for( i = 0; i < outlen; i++ )
    {
        const uint8_t ch = (uint8_t)out[i];
        char c = code[ch % codelen];
        out[i] = c;
    }
}

#if 0
static void cast(Type* from, Type* to, Smop& reg) {

    if (to->kind == Type::Any)
        return;

    Code::Type fromType = getCodeType(from);
    Code::Type toType = getCodeType(to);

    if (to->kind == TYPE_ATOMIC && to->atomic.akind == ATOMIC_TYPE_BOOL) {
        Label label = e->CreateLabel();
        e->BranchNotEqual(label,Code::Imm(reg.type,0),reg);
        reg = e->Set (label, Code::Imm(toType,0), Code::Imm(toType,1));
        return;
    }

    if( fromType != toType )
        reg = e->Convert(toType,reg);
}
#endif

EiGen::Imp::EcsType EiGen::Imp::ilToEcsType(quint8 op)
{
    switch(op)
    {
    case IL_ldelem_i1:
    case IL_stelem_i1:
    case IL_ldind_i1:
    case IL_stind_i1:
    case IL_conv_i1:
        return s1;
    case IL_ldelem_i2:
    case IL_stelem_i2:
    case IL_ldind_i2:
    case IL_conv_i2:
        return s2;
    case IL_ldelem_i4:
    case IL_stelem_i4:
    case IL_ldind_i4:
    case IL_stind_i4:
    case IL_conv_i4:
    case IL_ldc_i4_0:
    case IL_ldc_i4_1:
    case IL_ldc_i4_2:
    case IL_ldc_i4_3:
    case IL_ldc_i4_4:
    case IL_ldc_i4_5:
    case IL_ldc_i4_6:
    case IL_ldc_i4_7:
    case IL_ldc_i4_8:
    case IL_ldc_i4_m1:
    case IL_ldc_i4_s:
    case IL_ldc_i4:
        return s4;
    case IL_ldelem_i8:
    case IL_stelem_i8:
    case IL_ldind_i8:
    case IL_stind_i8:
    case IL_conv_i8:
    case IL_ldc_i8:
        return s8;
    case IL_ldelem_ip:
    case IL_stelem_ip:
    case IL_ldind_ip:
    case IL_stind_ip:
        return ptr;
    case IL_ldelem_r4:
    case IL_stelem_r4:
    case IL_ldind_r4:
    case IL_stind_r4:
    case IL_conv_r4:
    case IL_ldc_r4:
        return f4;
    case IL_ldelem_r8:
    case IL_stelem_r8:
    case IL_ldind_r8:
    case IL_stind_r8:
    case IL_conv_r8:
    case IL_ldc_r8:
        return f8;
    case IL_ldelem_u1:
    case IL_ldind_u1:
    case IL_conv_u1:
        return u1;
    case IL_ldelem_u2:
    case IL_ldind_u2:
    case IL_conv_u2:
        return u2;
    case IL_ldelem_u4:
    case IL_ldind_u4:
    case IL_conv_u4:
        return u4;
    case IL_ldelem_u8:
    case IL_ldind_u8:
    case IL_conv_u8:
        return u8;
    default:
        Q_ASSERT(false);
    }
    return ptr;
}

Smop EiGen::Imp::ldind(Type* lhsValueType, const Smop& addr, const Mic::RowCol& pos)
{
     Q_ASSERT(lhsValueType && lhsValueType->kind != Type::NameRef); // lhsValueType is already derefed

     if( lhsValueType->isSUO() || lhsValueType->kind == Type::Array ) {
         // If it is a structured value, do not attempt to load the value to the
         // register because in general we can't load an entire array or struct to a
         // register. As a result, the result of an evaluation of an array or struct
         // becomes the address of the array or struct.
         return addr;
     }

#if 0
     // TODO ?
     if( lhsValueType->kind == Type::Pointer && lhsValueType->getType()->deref()->kind == Type::Array )
         return addr; // not really a pointer, but actually an array
#endif

     loc(pos);
     const Code::Type type = getCodeType(lhsValueType);
     return emitter.ctx.MakeMemory(type,addr);
}

EiGen::Imp::Imp(AstModel* mdl, TargetCode tg):mdl(mdl),curMod(0), curProc(0),
    diagnostics(std::cerr), endOfProc(0), hasDebugInfo(false), target(tg),
    layout(
        {target_data[tg].int_width, 1, 8},
        {4, 4, 8},
        target_data[tg].pointer_width,
        target_data[tg].pointer_width,
        {0, target_data[tg].stack_align, target_data[tg].stack_align}, true),
    platform(layout, target_data[tg].has_linkregister),
    emitter(diagnostics,stringPool,charset, platform)
{
    types[u1] = Code::Unsigned(ecsTypes[u1].width/8);
    types[u2] = Code::Unsigned(ecsTypes[u2].width/8);
    types[u4] = Code::Unsigned(ecsTypes[u4].width/8);
    types[u8] = Code::Unsigned(ecsTypes[u8].width/8);
    types[s1] = Code::Signed(ecsTypes[s1].width/8);
    types[s2] = Code::Signed(ecsTypes[s2].width/8);
    types[s4] = Code::Signed(ecsTypes[s4].width/8);
    types[s8] = Code::Signed(ecsTypes[s8].width/8);
    types[f4] = Code::Float(ecsTypes[f4].width/8);
    types[f8] = Code::Float(ecsTypes[f8].width/8);
    types[ptr] = Code::Pointer(target_data[tg].pointer_width);
    types[fun] = Code::Function(target_data[tg].pointer_width);
}

EiGen::Imp::EcsType EiGen::Imp::getEcsType(Type* t) const
{
    if( t == 0 )
        return MaxType;
    switch( t->kind )
    {
    case Type::BOOL:
    case Type::CHAR:
    case Type::UINT8:
        return u1;
    case Type::INT8:
        return s1;
    case Type::INT16:
        return s2;
    case Type::INT32:
        return s4;
    case Type::INT64:
        return s8;
    case Type::UINT16:
        return u2;
    case Type::UINT32:
        return u4;
    case Type::UINT64:
        return u8;
    case Type::FLOAT32:
        return f4;
    case Type::FLOAT64:
        return f8;
    case Type::Pointer:
    case Type::INTPTR:
        return ptr;
    case Type::Proc:
        return fun;
    default:
        return MaxType;
    }
}

Code::Type EiGen::Imp::getCodeType(Type* t) const
{
    EcsType et = getEcsType(t);
    if( et == MaxType )
        return types[ptr];
    else
        return types[et];
}

void EiGen::Imp::loc(const Mic::RowCol&) const
{
    // TODO
}

bool EiGen::Imp::generate(Declaration* module)
{
    Q_ASSERT( module );
    curMod = module;

    visitModule();

    QHash<QByteArray, const char*>::const_iterator i;
    for( i = strings.begin(); i != strings.end(); ++i )
    {
        emitter.ctx.Begin(Code::Section::Data, i.key().toStdString(),
                 1,
                 false, // required
                 true, // duplicable
                 false // replaceable
                 );
        std::string str(i.value());
        for( int j = 0; j < str.size(); j++ )
        {
            if( str[j] == '\n' || str[j] == '\r' )
                str[j] = ' ';
        }
        const std::string cmt = "\"" + str + "\"";
        emitter.ctx.Comment(cmt.c_str());
        for( int j = 0; j <= str.size(); j++ )
            emitter.ctx.Define(Code::Imm(types[s1],i.value()[j]));
    }

    return true;
}

void EiGen::Imp::visitModule()
{
   Declaration* sub = curMod->subs;
#if 0
   // not necessary
   while( sub )
   {
       if( sub->kind == Declaration::Import )
       {
            hout << "#include \"" << Project::escapeFilename(sub->name) << ".h\"" << endl;
       }
       sub = sub->next;
   }
#endif

   sub = curMod->subs;
   while( sub )
   {
       if( hasDebugInfo && sub->kind == Declaration::TypeDecl )
       {
           emitter.ctx.Begin(Code::Section::TypeSection, qualident(sub).toStdString());
           emitter.ctx.Locate(curMod->name.toStdString(), toPos(sub->pos));
           typeDecl(sub->getType());

           if( sub->getType()->objectInit && sub->getType()->isSOA() )
               emitInitializer(sub->getType());
       }
       sub = sub->next;
   }

   if( hasDebugInfo )
       for( int i = 0; i < ptr; i++ )
       {
           emitter.ctx.Begin(Code::Section::TypeSection,ecsTypes[i].dbgname);
           emitter.ctx.DeclareType(types[i]);
       }

   emitter.ctx.Begin(Code::Section::Data, curMod->name.toStdString() + "$init_run$",
            1,
            false, // required
            true, // duplicable
            false // replaceable
            );
   emitter.ctx.Define(Code::Imm(types[s1],0));

#if 0
   // not necessary
   sub = curMod->subs;
   while( sub )
   {
       if( sub->kind == Declaration::ConstDecl )
       {
           hout << "#define " << qualident(sub);
           constValue(hout, sub->c);
           hout << endl << endl;
       }
       sub = sub->next;
   }
#endif

   bool initFound = false;
   sub = curMod->subs;
   while( sub )
   {
       switch( sub->kind )
       {
       case Declaration::TypeDecl:
           if( hasDebugInfo )
           {
               Type* t = deref(sub->getType());
               if( t && t->kind == Type::Object )
               {
                   foreach( Declaration* p, t->subs )
                   {
                       if( p->kind == Declaration::Procedure )
                           visitProcedure(p);
                   }
                   visitMetaDecl(sub);
               }
           }
           break;
       case Declaration::VarDecl:
           variable(sub);
           break;
       case Declaration::Procedure:
           visitProcedure(sub);
           if( sub->entryPoint )
               initFound = true;
           break;
       }

       sub = sub->next;
   }

   if( !initFound )
   {
        Declaration proc;
        proc.kind = Declaration::Procedure;
        proc.entryPoint = true;
        proc.outer = curMod;
        proc.name = "begin$";
        visitProcedure(&proc);
   }

   if( curMod->entryPoint )
   {
       // generate the entry point
       emitter.ctx.Begin(Code::Section::Code, "main" );
       emitter.ctx.Enter(0);

       Smop f = Code::Adr(types[fun],curMod->name.toStdString() + "$begin$");
       emitter.ctx.Call(f, 0);
       emitter.ctx.Leave();
       emitter.ctx.Push(Code::Reg(types[s4], Code::RRes));
       emitter.ctx.Call(Code::Adr(types[fun],"_Exit"),0);

       // TODO: how do we handle argc and argv?
   }
}

void EiGen::Imp::visitProcedure(Declaration* proc)
{
    labels.clear();
    labelRefs.clear();
    curProc = proc;

    if( !proc->forward && !proc->extern_ )
    {
        emitter.ctx.Begin(Code::Section::Code, qualident(proc->forwardToProc()).toStdString() );
        MyEmitter::Label lbl = emitter.ctx.CreateLabel();
        endOfProc = &lbl;

        int stacksize = 0;
        Declaration* sub = proc->subs;
        while(sub)
        {
            if( sub->kind == Declaration::LocalDecl )
            {
                int size = sub->getType()->getByteSize(target_data[target].pointer_width);
                size = AstModel::align(size, target_data[target].stack_align);
                stacksize += size;
                if( sub->off >= 0 )
                {
                    // update locals offset if not yet done
                    Q_ASSERT( sub->off >= 0);
                    // off allocated so far starts with 0 and assumes addresses growing from low to high;
                    // EiGen assumes a stack which grows from higher to lower addresses;
                    // so we have to adjust each off that it points "to the other end" of the variable slot, and
                    // then make it negative, so it gets the right address relative to the frame pointer;
                    // if off is negative up front, we assume that this change has already be done.
                    sub->off = -1 * (sub->off + size);
                }
            }
            sub = sub->next;
        }


        if( hasDebugInfo )
        {
            emitter.ctx.Locate(proc->name.toStdString(),toPos(proc->pos));
            Type* retType = deref(proc->getType());
            if( retType->kind != Type::Undefined )
            {
                if( retType->isSUO() || retType->kind == Type::Array )
                    emitter.ctx.DeclareVoid(); // instead we have a hidden parameter
                else
                    emitter.ctx.DeclareType(getCodeType(retType));
            }else
                emitter.ctx.DeclareVoid();

            DeclList params = proc->getParams();
            for( int i = 0; i < params.size(); i++ )
                locPar(params[i]);

            sub = proc->subs;
            while(sub)
            {
                if( sub->kind == Declaration::LocalDecl )
                {
                    locPar(sub);
                }
                sub = sub->next;
            }
        }

        // Prologue
        if( !proc->entryPoint && target_data[target].has_linkregister )
            emitter.ctx.Push(Code::Reg(types[fun],Code::RLink));
        emitter.ctx.Enter(stacksize);

        if( proc->entryPoint && !curMod->nobody )
        {
            // if( !module$init_run$ )
            MyEmitter::Label afterend = emitter.ctx.CreateLabel();
            Smop to = emitter.ctx.MakeMemory(types[u1], Code::Adr(types[ptr], curMod->name.toStdString() + "$init_run$"));
            emitter.ctx.BranchNotEqual(afterend, to, Code::Imm(types[u1], 0) );
            emitter.ctx.Move(to, Code::Imm(types[u1],1));
            release(to);

            Declaration* sub = curMod->subs;
            while( sub )
            {
                if( sub->kind == Declaration::Import && !sub->imported->nobody )
                {
                     Smop f = Code::Adr(types[fun], sub->imported->name.toStdString() + "$begin$");
                     emitter.ctx.Call(f, 0);
                }
                else if( sub->kind == Declaration::VarDecl )
                    emitSoapInit( qualident(sub), sub->getType(), 1 );
                sub = sub->next;
            }

            afterend();
        }

        sub = proc->subs;
        while(sub)
        {
            if( sub->kind == Declaration::LocalDecl )
            {
                emitSoapInit( sub->name, sub->getType(), 0 );
            }
            sub = sub->next;
        }
        statementSeq(proc->body);

        // Epilogue
        lbl();

        // TODO if( return_type && is_type_compound(return_type) )
        //    e->Move(Code::Reg(types[s4],Code::RRes),Code::Imm(types[s4],0));

        emitter.ctx.Leave();
        emitter.ctx.Return();
    }

    endOfProc = 0;
    curProc = 0;
}

void EiGen::Imp::visitMetaDecl(Declaration* d)
{
#if 0
    // TODO
    const QByteArray className = qualident(d);
    hout << "struct " << className << "$Class$ {" << endl;
    bout << "struct " << className << "$Class$ " << className << "$class$ = { " << endl;

    Type* t = deref(d->getType());
    if( t->getType() )
    {
        hout << ws(0) << typeRef(t->getType()) << "$Class$* super$;" << endl;
        bout << ws(0) << "&" << typeRef(t->getType()) << "$class$," << endl;
    }else
    {
        hout << ws(0) << "void* super$;" << endl;
        bout << ws(0) << "0," << endl;
    }

    DeclList methods = t->getMethodTable();
    foreach( Declaration* p, methods )
    {
        Q_ASSERT( p->kind == Declaration::Procedure && !p->forward );
        bout << ws(0) << qualident(p) << ", " << endl;

        hout << ws(0) << typeRef(p->getType()) << " (*" << p->name << ")";
        hout << "(";
        DeclList params = p->getParams();
        for( int i = 0; i < params.size(); i++ )
        {
            if( i != 0 )
                hout << ", ";
            parameter(hout, params[i]);
        }
        hout << ");" << endl;
    }

    bout << "};" << endl << endl;
    hout << "};" << endl;
    hout << "extern struct " << className << "$Class$ " << className << "$class$;" << endl;
#endif
}

QByteArray EiGen::Imp::typeRef(Type* t)
{
    if( t == 0 )
        return "void";
    t = t->deref();
    switch(t->kind)
    {
    case Type::Any:
        return "void";
    case Type::StringLit:
        return "const char*";
    case Type::ByteArrayLit:
        return "const unsigned char*";
    case Type::NIL:
        return "NULL";
    case Type::BOOL:
        return "unsigned char";
    case Type::CHAR:
        return "char";
    case Type::INT8:
        return "char";
    case Type::INT16:
        return "short";
    case Type::INT32:
        return "int";
    case Type::INT64:
        return "long long";
    case Type::UINT8:
        return "unsigned char";
    case Type::UINT16:
        return "unsigned short";
    case Type::UINT32:
        return "unsigned int";
    case Type::UINT64:
        return "unsigned long long";
    case Type::FLOAT32:
        return "float";
    case Type::FLOAT64:
        return "double";
    case Type::INTPTR:
        return "void*";
    case Type::DBLINTPTR:
        return "DBLINTPTR"; // TODO
    }

    if( t->kind == Type::Pointer )
    {
        Type* tt = deref(t->getType());
        if( tt->kind == Type::Array )
            tt = deref(tt->getType()); // we treat an array as a pointer to its elements
        QByteArray prefix;
        if( tt->kind == Type::Struct || tt->kind == Type::Object )
            prefix = "struct ";
        else if( tt->kind == Type::Union )
            prefix = "union ";
        return prefix + typeRef(tt) + "*";
    }else if( t->decl )
        return qualident(t->decl);
    else
        return "?TYPE";
}

void EiGen::Imp::locPar(Declaration* var)
{
    emitter.ctx.DeclareSymbol(*endOfProc, var->name.toStdString(),
                     Code::Mem(types[ptr],Code::RFP, var->off));
    emitter.ctx.Locate(var->name.toStdString(),toPos(var->pos));
    typeDecl(deref(var->getType()));
}

void EiGen::Imp::variable(Declaration* var)
{
    Type* t = deref(var->getType());
    emitter.ctx.Begin(Code::Section::Data, qualident(var).toStdString(),
             t->getAlignment(target_data[target].pointer_width),
             false, // required
             true, // duplicable
             false // replaceable
             );
    if( hasDebugInfo )
    {
        emitter.ctx.Locate(curMod->name.toStdString(),
                  toPos(var->pos));
        typeDecl(var->getType());
    }

    emitter.ctx.Reserve(t->getByteSize(target_data[target].pointer_width));
}

void EiGen::Imp::constValue(QTextStream& out, Constant* c)
{
    switch( c->kind )
    {
    case Constant::D:
        out << c->d;
        break;
    case Constant::I:
        out << c->i;
        break;
    case Constant::S:
        out << "\"" << c->s << "\"";
        break;
    case Constant::B:
        {
            const ByteString* ba = c->b;
            out << "{";
            for( int i = 0; i < ba->len; i++ )
            {
                if( i != 0 && i % 16 == 0 )
                    out << endl << "\t";
                out << "0x" << QByteArray::number(quint8(ba->b[i]),16) << ", ";
            }
            out << "}";
        }
        break;
    case Constant::R:
        constValue(out, c->r->c);
        break;
    case Constant::C:
        {
            if( c->c->type )
                out << "(" << typeRef(c->c->type) << ")";
            out << "{";
            Component* i = c->c->c;
            if( !i->name.isEmpty() )
                out << "." << i->name << "=";
            constValue(out, i->c);
            while( i->next )
            {
                out << ", ";
                i = i->next;
                if( !i->name.isEmpty() )
                    out << "." << i->name << "=";
                constValue(out, i->c);
            }
            out << "}";
        }
        break;
    }
}

void EiGen::Imp::statementSeq(Statement* s)
{
    while(s)
    {
        switch( s->kind )
        {
        case Statement::ExprStat:
            if( s->args )
                expression(s->args);
            break;

        case IL_if: {
                Smop cond = expression(s->args);
                const Code::Type type = getCodeType(deref(s->args->getType()));
                MyEmitter::Label lelse = emitter.ctx.CreateLabel();
                MyEmitter::Label lend = emitter.ctx.CreateLabel();
                emitter.ctx.BranchEqual(lelse,Code::Imm(type,0),emitter.ctx.Convert(type,cond));
                release(cond);
                statementSeq(s->body);
                emitter.ctx.Branch(lend);
                lelse();
                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    statementSeq(s->body);
                }
                lend();
            }
            break;

        case IL_loop: {
                MyEmitter::Label lbegin = emitter.ctx.CreateLabel();
                MyEmitter::Label lend = emitter.ctx.CreateLabel();
                loopStack.push_back(&lend);
                lbegin();
                statementSeq(s->body);
                emitter.ctx.Branch(lbegin);
                lend();
                loopStack.pop_back();
            }
            break;

        case IL_repeat: {
                MyEmitter::Label lbegin = emitter.ctx.CreateLabel();
                lbegin();
                statementSeq(s->body);
                Smop cond = expression(s->args);
                const Code::Type type = getCodeType(deref(s->args->getType()));
                emitter.ctx.BranchEqual(lbegin,Code::Imm(type,0),emitter.ctx.Convert(type,cond));
            }
            break;

        case IL_switch: {
                Smop cond = expression(s->args);

                const Code::Type cond_type = getCodeType(deref(s->args->getType()));
                cond = emitter.ctx.Convert(cond_type,cond);

                Statement* stat = s;

                // TODO: create jump table instead
                std::deque<MyEmitter::Label> case_labels;
                while( s->next && s->next->kind == IL_case )
                {
                    s = s->next;
                    case_labels.push_back(emitter.ctx.CreateLabel());
                    Expression* e = s->e;
                    while(e)
                    {
                        emitter.ctx.BranchEqual(case_labels.back(), Code::Imm(cond_type,e->i), cond);
                        e = e->next;
                    }
                }
                release(cond);

                MyEmitter::Label lend = emitter.ctx.CreateLabel();

                // if none of the jumps were taken, we get here
                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    statementSeq(s->body);
                }
                emitter.ctx.Branch(lend);

                Statement* next = s;

                s = stat;
                int i = 0;
                while( s->next && s->next->kind == IL_case )
                {
                    s = s->next;
                    case_labels[i++]();
                    statementSeq(s->body);
                    emitter.ctx.Branch(lend);
                }

                lend();
                s = next;
            }
            break;

        case IL_while: {
                MyEmitter::Label lbegin = emitter.ctx.CreateLabel();
                MyEmitter::Label lbreak = emitter.ctx.CreateLabel();
                lbegin();
                Smop cond = expression(s->args);
                const Code::Type type = getCodeType(deref(s->args->getType()));
                emitter.ctx.BranchEqual(lbreak,Code::Imm(type,0),emitter.ctx.Convert(type,cond));
                statementSeq(s->body);
                emitter.ctx.Branch(lbegin);
                lbreak();
            }
            break;

        case IL_exit:
            emitter.ctx.Branch(*loopStack.back());
            break;

        case IL_stloc:
        case IL_stloc_s:
        case IL_stloc_0:
        case IL_stloc_1:
        case IL_stloc_2:
        case IL_stloc_3: {
                Smop value = expression(s->args);
                DeclList locals = curProc->getLocals();
                Q_ASSERT(s->id < locals.size());
                Smop addr = Code::Reg(types[ptr],Code::RFP, locals[s->id]->off);
                loc(s->pos);
                store(deref(s->args->getType()), addr, value, s->pos);
            }
            break;

        case IL_starg: {
                Smop value = expression(s->args);
                DeclList params = curProc->getParams();
                Q_ASSERT(s->id < params.size());
                Smop addr = Code::Reg(types[ptr],Code::RFP, params[s->id]->off);
                loc(s->pos);
                store(deref(s->args->getType()), addr, value, s->pos);
            }
            break;

        case IL_stind:
        case IL_stind_i1:
        case IL_stind_i4:
        case IL_stind_i8:
        case IL_stind_r4:
        case IL_stind_r8:
        case IL_stind_ip:
        case IL_stind_ipp: {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );
                Smop lhs = expression(s->args->lhs);
                emitter.ctx.SaveRegister(lhs);
                Smop rhs = expression(s->args->rhs);
                emitter.ctx.RestoreRegister(lhs);
                loc(s->pos);
                Type* ptrT = deref(s->args->lhs->getType());
                Q_ASSERT(ptrT->kind == Type::Pointer);
                Type* base = deref(ptrT->getType());
                store(base, lhs, rhs, s->pos);
            }
            break;

        case IL_stelem:
        case IL_stelem_i1:
        case IL_stelem_i2:
        case IL_stelem_i4:
        case IL_stelem_i8:
        case IL_stelem_r4:
        case IL_stelem_r8:
        case IL_stelem_ip:
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument &&
                          s->args->lhs && s->args->rhs &&
                          s->args->next && s->args->next->kind == Expression::Argument &&
                          s->args->next->rhs && s->args->next->lhs == 0);

                Smop arr = expression(s->args->next->rhs);
                emitter.ctx.SaveRegister(arr);
                Smop idx = expression(s->args->lhs);
                emitter.ctx.RestoreRegister(arr);

                Type* aptr = deref(s->args->next->rhs->getType());
                Q_ASSERT( aptr->kind == Type::Pointer );
                Type* aos = deref(aptr->getType());
                Q_ASSERT( aos->kind == Type::Array );
                Type* et = deref(aos->getType());

                idx = emitter.ctx.Multiply(idx, Code::Imm(idx.type,et->getByteSize(target_data[target].pointer_width)));
                loc(s->pos);
                Smop lhs = emitter.ctx.Add(arr,emitter.ctx.Convert(types[ptr],idx));
                release(arr);
                release(idx);
                emitter.ctx.SaveRegister(lhs);
                Smop rhs = expression(s->args->rhs);
                emitter.ctx.RestoreRegister(lhs);
                store(et, lhs, rhs, s->pos);
            }
            break;

        case IL_stfld: {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );

                Smop tmp = expression(s->args->lhs);
                loc(s->pos);
                tmp = emitter.ctx.Add(emitter.ctx.Convert(types[ptr],tmp), Code::Imm(types[ptr],s->d->f.off));

                emitter.ctx.SaveRegister(tmp);
                Smop rhs = expression(s->args->rhs);
                emitter.ctx.RestoreRegister(tmp);

                store(deref(s->d->getType()), tmp, rhs, s->pos);
            }
            break;

        case IL_stvar: {
                Smop value = expression(s->args);
                Smop var = Code::Adr(types[ptr],qualident(s->d).toStdString());
                loc(s->pos);
                store(deref(s->args->getType()), var, value, s->pos);
            } break;

        case IL_ret:
            if( s->args )
            {
                Smop rhs = expression(s->args);
                Type* rt = deref(s->args->getType());
                if( !rt->isSUOA() )
                {
                    Code::Type type = getCodeType(rt);
                    emitter.ctx.Move (Code::Reg(type, Code::RRes), emitter.ctx.Convert(type,rhs));
                } // else TODO
            }
            loc(s->pos);
            Q_ASSERT(endOfProc);
            emitter.ctx.Branch(*endOfProc);
            break;

        case IL_pop:
            expression(s->args);
            break;

        case IL_free: {
                const MyEmitter::RestoreRegisterState restore(emitter.ctx);
                Q_ASSERT(deref(s->args->getType())->kind == Type::Pointer);
                Smop f = Code::Adr(types[fun],"free");
                emitter.ctx.Push(expression(s->args));
                const int aligned_size = align_to(types[ptr].size,target_data[target].stack_align);
                emitter.ctx.Call(f,aligned_size);
            }
            break;

        case IL_label: {
                MyEmitter::Label* l = labelRefs.value(s->name);
                if(l == 0)
                {
                    labels.push_back(emitter.ctx.CreateLabel());
                    labelRefs.insert(s->name, &labels.back());
                    labels.back()();
                }else {
                    (*l)();
                }
            }
            break;

        case IL_goto: {
                MyEmitter::Label* l = labelRefs.value(s->name);
                if(l == 0)
                {
                    labels.push_back(emitter.ctx.CreateLabel());
                    labelRefs.insert(s->name, &labels.back());
                    loc(s->pos);
                    emitter.ctx.Branch(labels.back());
                }else {
                    loc(s->pos);
                    emitter.ctx.Branch(*l);
                }
            }
            break;

        case IL_line:
            // TODO
            break;

        default:
            Q_ASSERT(false);
        }

        s = s->next;
    }
}

Smop EiGen::Imp::emitBinOP(Expression* e)
{
    Smop lhs = expression(e->lhs);
    emitter.ctx.SaveRegister(lhs);
    Smop rhs = expression(e->rhs);
    emitter.ctx.RestoreRegister(lhs);

    const Code::Type lhsT = getCodeType(e->getType());

    rhs = emitter.ctx.Convert(lhsT,rhs);
    lhs = emitter.ctx.Convert(lhsT,lhs);

    Smop lhs2 = lhs;
    if( target_data[target].architecture == Amd32 && lhs2.type.size == 8 &&
            ( lhs2.type.model == Code::Type::Signed || lhs2.type.model == Code::Type::Unsigned ) &&
            lhs2.model == Code::Operand::Memory )
        // TODO: work-around add issue with 64 bit numbers on 32 bit systems
        // see https://software.openbrace.org/issues/663
       lhs2 = emitter.ctx.Move(lhs2);


    loc(e->pos);

    switch(e->kind)
    {
    case IL_add:
        return emitter.ctx.Add(lhs2,rhs);
    case IL_div_un: // TODO un
    case IL_div:
        if( rhs.model == Code::Operand::Immediate && rhs.type.model == Code::Type::Float && rhs.fimm == 0.0 ) {
            if( lhs2.model == Code::Operand::Immediate && lhs2.type.model == Code::Type::Float && lhs2.fimm == 0.0)
                return Code::FImm(lhs2.type,NAN);
            else
                return Code::FImm(lhs2.type,HUGE_VALF);
        }else
            return emitter.ctx.Divide(lhs2,rhs);
    case IL_mul:
        return emitter.ctx.Multiply(lhs2,rhs);
    case IL_rem:
    case IL_rem_un:// TODO un
        return emitter.ctx.Modulo(lhs2,rhs);
    case IL_sub:
        return emitter.ctx.Subtract(lhs2,rhs);
    case IL_and:
        return emitter.ctx.And(lhs2,rhs);
    case IL_or:
        return emitter.ctx.Or(lhs2,rhs);
    case IL_xor:
        return emitter.ctx.ExclusiveOr(lhs2,rhs);
    case IL_shl:
    case IL_shr_un:
        return emitter.ctx.ShiftLeft(lhs2,rhs);
    case IL_shr:
        return emitter.ctx.ShiftRight(lhs2,rhs);
    case IL_ceq:
    case IL_cgt_un:
    case IL_cgt:
    case IL_clt_un:
    case IL_clt: {
            MyEmitter::Label ltrue = emitter.ctx.CreateLabel();
            switch(e->kind)
            {
            case IL_ceq:
                emitter.ctx.BranchEqual(ltrue,lhs2,rhs);
                break;
            case IL_cgt_un: // TODO: UN
            case IL_cgt:
                emitter.ctx.BranchLessThan(ltrue,rhs,lhs2);
                break;
            case IL_clt_un: // TODO: UN
            case IL_clt:
                emitter.ctx.BranchLessThan(ltrue,lhs2,rhs);
                break;
            }
            return emitter.ctx.Set(ltrue,Code::Imm(types[s4],0),Code::Imm(types[s4],1));
        }break;
    default:
        Q_ASSERT(false);
    }

    return Smop();
}

void EiGen::Imp::emitSoapInit(const QByteArray& name, Type* t, int level)
{
#if 0
    // TODO
    t = deref(t);
    if( t->kind == Type::Pointer && t->pointerInit )
        out << ws(level) << name << " = NULL;" << endl;
    else if( t->isSUOA() )
    {
        if( t->pointerInit ) // it's cheaper to directly zero the whole thing
            out << ws(level) << "memset(" << (t->isSUO() ? "&" : "") << name << ", 0, sizeof(" << typeRef(t) << "));" << endl;
    }
    //emitSoaInit(out, name, false, t);
#endif
}

void EiGen::Imp::emitSoaInit(const QByteArray& name, bool nameIsPtr, Type* t, int level)
{
#if 0
    // TODO
    t = deref(t);
    if( !t->objectInit )
        return;
    if( t->isSO() )
        out << ws(level) << qualident(t->decl) << "$init$(" << (nameIsPtr ? "" : "&") << name << ", 1);" << endl;
    else if( t->kind == Type::Array && t->len && deref(t->getType())->isSO() )
        out << ws(level) << qualident(deref(t->getType())->decl) << "$init$(" << name << ", " << t->len << ");" << endl;
#endif
}

static void collectArgs(Expression* e, QList<Expression*>& args)
{
    if( e && e->kind == Expression::Argument )
    {
        if( e->next )
            collectArgs(e->next, args);
        if( e->lhs )
            args << e->lhs;
        args << e->rhs;
    }else if(e)
        args << e;
}

Type*EiGen::Imp::deref(Type* t)
{
    if( t && t->kind == Type::NameRef )
        return deref(t->getType());
    else if( t )
        return t;
    else
        return mdl->getBasicType(Type::Undefined);
}

void EiGen::Imp::emitInitializer(Type* t)
{
#if 0
    // TODO
    t = deref(t);
    hout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n);" << endl;
    bout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n) {" << endl;
    bout << ws(0) << "int i;" << endl;
    bout << ws(0) << "for( i = 0; i < n; i++ ) {" << endl;

    if( t->kind == Type::Object )
        bout << ws(1) << "obj[i].class$ = &" << qualident(t->decl) << "$class$;" << endl;
    else if( t->kind == Type::Array && t->len && t->objectInit )
    {
        Type* et = deref(t->getType());
        bout << ws(1) << qualident(et->decl) << "$init$(obj, " << t->len << ");" << endl;
    }

    foreach( Declaration* field, t->subs )
    {
        if( field->kind != Declaration::Field )
            continue;
        Type* tt = deref(field->getType());
        if( tt->isSO() )
        {
            bout << ws(1) << qualident(tt->decl) << "$init$(&obj->" << field->name << ", 1);" << endl;
        }else if( tt->kind == Type::Array && tt->len )
        {
            Type* et = deref(tt->getType());
            if( et->isSO() )
                bout << ws(1) << qualident(tt->decl) << "$init$(obj->" << field->name << ", " << tt->len << ");" << endl;
        }
    }

    bout << ws(0) << "}" << endl;
    bout << "}" << endl << endl;
#endif
}

void EiGen::Imp::typeDecl(Type* ty)
{
    if( ty == 0 )
    {
        emitter.ctx.DeclareVoid();
        return;
    }

    if( ty->kind == Type::NameRef )
    {
        Quali q = ty->toQuali();
        if( q.first.isEmpty() )
            q.first = curMod->name;
        emitter.ctx.DeclareType(q.first.toStdString() + "$" + q.second.toStdString());
        return;
    }

    if( ty->kind > Type::NIL && ty->kind < Type::MaxBasicType )
    {
        emitter.ctx.DeclareType(std::string(ecsTypes[getEcsType(ty)].dbgname));
        return;
    }

    switch(ty->kind)
    {
    case Type::Undefined:
        emitter.ctx.DeclareVoid();
        break;
    case Type::Pointer:
        emitter.ctx.DeclarePointer();
        typeDecl(ty->getType());
       break;
    case Type::Proc: {
        MyEmitter::Label ext = emitter.ctx.CreateLabel();
        emitter.ctx.DeclareFunction(ext);
        typeDecl(ty->getType());
        foreach(Declaration* param, ty->subs)
            // TODO: typebound
            typeDecl(param->getType());
        ext();
        break;
    }
    case Type::Array:
        emitter.ctx.DeclareArray(0,ty->len);
        typeDecl(ty->getType());
        break;
    case Type::Struct:
    case Type::Union:
    case Type::Object: {
            MyEmitter::Label ext = emitter.ctx.CreateLabel();
            emitter.ctx.DeclareRecord(ext,ty->getByteSize(target_data[target].pointer_width));
            // TODO: vptr ref for Object
            QList<Declaration*> fields = ty->getFieldList(true);
            foreach(Declaration* field, fields)
                ; // TODO print_member(mem);
            ext();
        break;
    }
    default:
        assert(0);
    }
}

Smop EiGen::Imp::expression(Expression* e)
{
    switch(e->kind)
    {
    case IL_add:
    case IL_div_un:
    case IL_div:
    case IL_mul:
    case IL_rem:
    case IL_rem_un:
    case IL_sub:
    case IL_and:
    case IL_or:
    case IL_xor:
    case IL_shl:
    case IL_shr_un:
    case IL_shr:
        return emitBinOP(e);

    case IL_neg: {
            Smop tmp = expression(e->lhs);
            const Code::Type type = getCodeType(deref(e->lhs->getType()));
            loc(e->pos);
            return emitter.ctx.Negate(emitter.ctx.Convert(type,tmp));
        }

    case IL_abs: {
            const MyEmitter::RestoreRegisterState restore(emitter.ctx);
            Smop value = expression(e->lhs);
            Type* t = deref(e->lhs->getType());
            Smop f;
            Code::Type ct;
            if( t->isFloat() )
            {
                ct = types[f8];
                f = Code::Adr(types[fun],"fabs");
            }else if( t->isInt32OnStack() )
            {
                ct = types[s4];
                f = Code::Adr(types[fun],"abs");
            }else
            {
                ct = types[s8];
                f = Code::Adr(types[fun],"llabs");
            }
            emitter.ctx.Push(emitter.ctx.Convert(ct, value));
            const int aligned_size = align_to(ct.size,target_data[target].stack_align);
            return emitter.ctx.Call(ct, f,aligned_size);
        }

    case IL_not: {
            Smop tmp = expression(e->lhs);
            const Code::Type type = getCodeType(deref(e->lhs->getType()));
            loc(e->pos);
            return emitter.ctx.Complement(emitter.ctx.Convert(type,tmp));
        }

    case IL_ldc_i4_0:
    case IL_ldc_i4_1:
    case IL_ldc_i4_2:
    case IL_ldc_i4_3:
    case IL_ldc_i4_4:
    case IL_ldc_i4_5:
    case IL_ldc_i4_6:
    case IL_ldc_i4_7:
    case IL_ldc_i4_8:
    case IL_ldc_i4_m1:
    case IL_ldc_i4_s:
    case IL_ldc_i4:
        return Code::Imm(types[s4],e->i);
    case IL_ldc_i8:
        return Code::Imm(types[s8],e->i);

    case IL_ldc_r4:
        return Code::FImm(types[f4],e->f);
    case IL_ldc_r8:
        return Code::FImm(types[f8],e->f);

    case IL_ldnull:
        return Code::Imm(types[ptr],0);

    case IL_ldstr: {
            QByteArray id;
            id.resize(10);
            id[0] = '.';
            string_hash(e->c->s, id.data()+1, 9);
            strings[id] = e->c->s;
            return Code::Adr(types[ptr],id.toStdString());
        }

    case IL_conv_i1:
    case IL_conv_i2:
    case IL_conv_i4:
    case IL_conv_u1:
    case IL_conv_u2:
    case IL_conv_u4:
    case IL_conv_i8:
    case IL_conv_u8:
    case IL_conv_r4:
    case IL_conv_r8: {
            Smop tmp = expression(e->lhs);
            tmp = emitter.ctx.Convert(types[ilToEcsType(e->kind)],tmp);
            switch(e->kind)
            {
            case IL_conv_i1:
            case IL_conv_i2:
                tmp = emitter.ctx.Convert(types[s4],tmp);
                break;
            case IL_conv_u1:
            case IL_conv_u2:
                tmp = emitter.ctx.Convert(types[u4],tmp);
                break;
            default:
                break;
            }
            return tmp;
        }

    case IL_ceq:
    case IL_cgt_un: // TODO: UN
    case IL_cgt:
    case IL_clt_un: // TODO: UN
    case IL_clt:
        return emitBinOP(e);

    case IL_ldvar: {
            Smop tmp = Code::Adr(types[ptr],qualident(e->d).toStdString());
            Type* varType = deref(e->getType());
            return ldind(varType, tmp, e->pos);
        }

    case IL_ldvara:
        return Code::Adr(types[ptr],qualident(e->d).toStdString());

    case IL_ldarg_0:
    case IL_ldarg_1:
    case IL_ldarg_2:
    case IL_ldarg_3:
    case IL_ldarg_s:
    case IL_ldarg:
    case IL_ldarga_s:
    case IL_ldarga: {
            DeclList params = curProc->getParams();
            Q_ASSERT( e->id < params.size() );
            Smop argAddr = Code::Reg(types[ptr],Code::RFP, params[e->id]->off);
            Type* argVarType = deref(e->getType());
            if( e->kind != IL_ldarga_s && e->kind != IL_ldarga )
                return ldind(argVarType, argAddr, e->pos);
            else
                return argAddr;
        }

    case IL_ldloc_0:
    case IL_ldloc_1:
    case IL_ldloc_2:
    case IL_ldloc_3:
    case IL_ldloc_s:
    case IL_ldloc:
    case IL_ldloca_s:
    case IL_ldloca:{
            DeclList locals = curProc->getLocals();
            Q_ASSERT( e->id < locals.size() );
            Smop localAddr = Code::Reg(types[ptr],Code::RFP, locals[e->id]->off);
            Type* localVarType = deref(e->getType());
            if( e->kind != IL_ldloca_s && e->kind != IL_ldloca )
                return ldind(localVarType, localAddr, e->pos);
            else
                return localAddr;
        }

    case IL_ldind_i1:
    case IL_ldind_i2:
    case IL_ldind_i4:
    case IL_ldind_i8:
    case IL_ldind_ip:
    case IL_ldind_ipp:
    case IL_ldind_r4:
    case IL_ldind_r8:
    case IL_ldind_u1:
    case IL_ldind_u2:
    case IL_ldind_u4:
    case IL_ldind_u8:
    case IL_ldind: {
            Smop addr = expression(e->lhs);
            Type* baseType = deref(e->getType());
            return ldind(baseType, addr, e->pos);
        }

    case IL_ldelem_i1:
    case IL_ldelem_i2:
    case IL_ldelem_i4:
    case IL_ldelem_i8:
    case IL_ldelem_ip:
    case IL_ldelem_r4:
    case IL_ldelem_r8:
    case IL_ldelem_u1:
    case IL_ldelem_u2:
    case IL_ldelem_u4:
    case IL_ldelem_u8:
    case IL_ldelem:
    case IL_ldelema: {
            Smop arrayAddr = expression(e->lhs);
            emitter.ctx.SaveRegister(arrayAddr);
            Smop idx = expression(e->rhs);
            emitter.ctx.RestoreRegister(arrayAddr);
            Type* ptrToArrT = deref(e->lhs->getType());
            Type* arrayT = deref(ptrToArrT->getType());
            Type* elementT = deref(arrayT->getType());
            Smop offset = emitter.ctx.Multiply(idx,
                        Code::Imm(idx.type,elementT->getByteSize(target_data[target].pointer_width)));
            release(idx);
            loc(e->pos);
            Smop elemAddr = emitter.ctx.Add(arrayAddr,emitter.ctx.Convert(types[ptr],offset));
            release(arrayAddr);
            release(offset);
            if( e->kind == IL_ldelema )
                return elemAddr;
            else
                return ldind(elementT, elemAddr, e->pos);
        }

    case IL_ldfld:
    case IL_ldflda: {
            Smop structAddr = expression(e->lhs);
            loc(e->pos);
            Smop fieldAddr = emitter.ctx.Add(emitter.ctx.Convert(types[ptr],structAddr),
                                             Code::Imm(types[ptr],e->d->f.off));
            if( e->kind == IL_ldflda )
                return fieldAddr;
            else
            {
                Type* fieldType = deref(e->d->getType());
                return ldind(fieldType, fieldAddr, e->pos);
            }
        }

    case IL_ldproc:
        return Code::Adr(types[fun],qualident(e->d).toStdString());

    case IL_castptr:
        return expression(e->lhs);

    case IL_call:
    case IL_calli: {
            const MyEmitter::RestoreRegisterState restore(emitter.ctx);

            QList<Expression*> args;
            collectArgs(e->rhs, args);

            // push args in reverse order
            int aligned_size = 0;
            for( int i = args.size()-1; i >= 0; i-- )
            {
                Smop actual = expression(args[i]);
                Type* argT = deref(args[i]->getType());
                aligned_size += push(argT, actual);
                // TODO: convert types of varargs
            }
#if 0 // TODO
            // If the return type is a large struct/union, the caller passes
            // a pointer to a buffer as if it were the first argument.
            if (expr->call.return_buffer) {
                Smop res;
                if( expr->call.return_buffer->lhs )
                    res = var_address(expr->call.return_buffer->lhs);
                else
                    res = Code::Reg(types[ptr], Code::RFP,expr->call.return_buffer->offset);
                aligned_size += pushRes(types[ptr], res);
            }
#endif
            Type* retType = 0;
            Smop f;
            if(e->kind == IL_calli)
            {
                retType = deref(e->lhs->getType());
                f = expression(e->lhs);
            }else
            {
                retType = deref(e->d->getType());
                f = Code::Adr(types[fun],qualident(e->d).toStdString());
            }
            if(retType && retType->kind == Type::Undefined)
                retType = 0;

            f = emitter.ctx.Convert(types[fun],f);

            Smop res;
            loc(e->pos);
#if 0 // TODO
            if( expr->call.return_buffer )
            {
                emitter.ctx.Call(f,aligned_size);
                if( expr->call.return_buffer->lhs )
                    res = var_address(expr->call.return_buffer->lhs);
                else
                    res = Code::Reg(types[ptr], Code::RFP,expr->call.return_buffer->offset);
            }else
#endif
                if( retType )
            {
                res = emitter.ctx.Call(getCodeType(retType), f,aligned_size);
                // TODO
            }else
                emitter.ctx.Call(f,aligned_size);

            return res;
        }

    case IL_newobj: {
            const MyEmitter::RestoreRegisterState restore(emitter.ctx);
            Q_ASSERT(e->getType()->kind == Type::Pointer);
            // TODO if( deref(e->getType())->objectInit ) $init$
            // calloc(1, sizeof(e->getType()->getType()))
            Smop f = Code::Adr(types[fun],"calloc");
            emitter.ctx.Push(Code::Imm(types[ptr],e->getType()->getType()->getByteSize(target_data[target].pointer_width)));
            emitter.ctx.Push(Code::Imm(types[ptr],1));
            const int aligned_size = 2 * align_to(types[ptr].size,target_data[target].stack_align);
            return emitter.ctx.Call(types[ptr], f,aligned_size);
        }

    case IL_newarr: {
            const MyEmitter::RestoreRegisterState restore(emitter.ctx);
            // TODO if( deref(e->getType())->objectInit ) $init$
            // calloc(n, sizeof(element_type))
            Type* et = deref(e->d->getType());
            Smop len = expression(e->lhs);
            Smop f = Code::Adr(types[fun],"calloc");
            emitter.ctx.Push(Code::Imm(types[ptr],et->getByteSize(target_data[target].pointer_width)));
            emitter.ctx.Push(len);
            const int aligned_size = 2 * align_to(types[ptr].size,target_data[target].stack_align);
            return emitter.ctx.Call(types[ptr], f,aligned_size);
        }

    case IL_dup:
        return expression(e->lhs);

    case IL_nop:
        break;

    case IL_iif: {
            Expression* if_ = e->e;
            Q_ASSERT(if_ && if_->kind == IL_if && if_->next->kind == IL_then && if_->next->next->kind == IL_else &&
                     if_->next->next->next == 0); // no IL_end
            Expression* then_ = if_->next;
            Expression* else_ = if_->next->next;

            Smop cond = expression(if_->lhs);
            const Code::Type type = getCodeType(deref(if_->lhs->getType()));
            MyEmitter::Label lelse = emitter.ctx.CreateLabel();
            MyEmitter::Label lend = emitter.ctx.CreateLabel();
            loc(if_->pos);
            emitter.ctx.BranchEqual(lelse,Code::Imm(type,0),emitter.ctx.Convert(type,cond));
            release(cond);
            Smop res;
            Smop lhs = expression(then_->lhs);
            // apparently C allows x ? (void)a : (void)b or mixed
            if( lhs.model != Code::Operand::Void ) {
                res = emitter.ctx.MakeRegister(lhs);
                emitter.ctx.Fix(res);
            }
            loc(then_->pos);
            emitter.ctx.Branch(lend);
            lelse();
            Smop rhs = expression(else_->lhs);
            if( rhs.model == Code::Operand::Void || lhs.model == Code::Operand::Void ) {
                if( res.model == Code::Operand::Register )
                    emitter.ctx.Unfix(res);
                res = rhs;
            } else {
                rhs = emitter.ctx.Convert(res.type,rhs);
                loc(else_->pos);
                emitter.ctx.Move(res,rhs);
                if( res.model == Code::Operand::Register )
                    emitter.ctx.Unfix(res);
            }
            lend();
            return res;
        }

    case IL_ldobj:
        qWarning() << "TODO not implemented: " << s_opName[e->kind];
        //constValue(out, e->c); // TODO
        break;

    case IL_initobj:
        {
            qWarning() << "TODO not implemented: " << s_opName[e->kind];
            Type* t = deref(e->d->getType());
            const QByteArray name = typeRef(t);
            //out << "_ptr$ = ";
            expression(e->lhs);
            //out << "; memset(_ptr$, 0, sizeof(" << name << "));";
            //emitSoaInit(out, "((" + name + "*)_ptr$)", true, t, level );
        }
        break;

    case IL_ldmeth:
        qWarning() << "TODO not implemented: " << s_opName[e->kind];
        //out << "{(_ptr$ = ";
        return expression(e->lhs);
        //out << ", _ptr$), ((" << typeRef(e->lhs->getType()) << ")_ptr$)";
        //out << "->class$->" << e->d->name << "}";
        break;

    case IL_callvirt:
        {
            qWarning() << "TODO not implemented: " << s_opName[e->kind];
            //out << "(_ptr$ = ";
            expression(e->lhs);
            //out << ", ((" << typeRef(e->lhs->getType()) << ")_ptr$)";
            //out << "->class$->" << e->d->name << "(_ptr$";
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for( int i = 0; i < args.size(); i++ )
            {
               //out << ", ";
                expression(args[i]);
            }
            //out << "))";
        }
        break;

    case IL_callvi:
        {
            qWarning() << "TODO not implemented: " << s_opName[e->kind];
            //out << "(_ptr$ = &";
            expression(e->lhs);
            //out << ", ((" << typeRef(e->lhs->getType()) << "*)_ptr$)->proc(((";
            //out << typeRef(e->lhs->getType()) << "*)_ptr$)->self";
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for( int i = 0; i < args.size(); i++ )
            {
                //out << ", ";
                expression(args[i]);
            }
            //out << "))";
        }
        break;

    case IL_ptroff:
        qWarning() << "TODO not implemented: " << s_opName[e->kind];
        expression(e->lhs);
        //out << " += ";
        expression(e->rhs);
        //out << " * sizeof(" << typeRef(e->d->getType()) << ")";
        break;

    case IL_sizeof:
    case IL_newvla:
    case IL_isinst:
        qWarning() << "TODO not implemented: " << s_opName[e->kind];
        break;
    default:
        Q_ASSERT(false);
    }
}

