/*
* Copyright 2019-2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MicRowCol.h"
#include "MilEmitter.h"
#include "MilOps.h"
#include "MicAtom.h"
#include <QtDebug>
using namespace Mil;

static const char* bool_sym = 0;
static const char* char_sym = 0;

Emitter::Emitter(AbstractRenderer* r, DbgInfo di):d_out(r),d_typeKind(0),ops(0),lastLine(0), dbgInfo(di)
{
    Q_ASSERT( r );
    bool_sym = Mic::Atom::getAtom("bool").constData();
    char_sym = Mic::Atom::getAtom("char").constData();
}

void Emitter::beginModule(const QByteArray& fullName, const QString& sourceFile, const RowCol & pos)
{
    Q_ASSERT( !fullName.isEmpty() );
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
#if 0
    QByteArrayList names;
    bool fullyInstantiated = true;
    foreach( const MilMetaParam& m, mp )
    {
        if( m.isGeneric )
            fullyInstantiated = false;
        names << m.name;
    }
    if( fullyInstantiated )
        names.clear();
    d_out->beginModule(fullName,sourceFile, names);
    foreach( const MilMetaParam& m, mp )
    {
        if( !m.isGeneric )
            continue; // otherwise the client directly calls addType/beginType or addConst
        if( m.isConst )
            d_out->addConst(MilQuali(),m.name,QVariant());
        else
            d_out->addType(m.name,false,MilQuali(),Generic,0);
    }
#else
    lineout(pos);
    QString source;
    if( dbgInfo != None )
        source = sourceFile;
    d_out->beginModule(fullName,source);
#endif
}

void Emitter::endModule(const RowCol& pos)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    lineout(pos);
    d_out->endModule();
}

void Emitter::addImport(const QByteArray& fullName, const RowCol & pos)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    lineout(pos);
    d_out->addImport(fullName);
}

void Emitter::addVariable(const Quali& typeRef, QByteArray name, const RowCol & pos, bool isPublic)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    lineout(pos);
    d_out->addVariable(typeRef,name, isPublic);
}

void Emitter::addConst(const Quali& typeRef, const QByteArray& name, const RowCol & pos, const QVariant& val)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    lineout(pos);
    d_out->addConst(typeRef, name, val);
}

void Emitter::beginProc(const QByteArray& procName, const RowCol & pos, bool isPublic, quint8 kind, const QByteArray& objectType)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT(!procName.isEmpty());

    firstLine = lineset(pos);
    d_proc.append(ProcData());
    d_proc.back().name = procName;
    d_proc.back().isPublic = isPublic;
    d_proc.back().kind = kind;
    d_proc.back().binding = objectType;
    d_stackDepth = 0;
    d_maxStackDepth = 0;
    d_proc.back().isVararg = false;
    ops = &d_proc.back().body;
}

void Emitter::toFinallySection(bool yes, const RowCol & pos)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0);
    if( yes )
        ops = &d_proc.back().finally;
    else
        ops = &d_proc.back().body;
    lastLine = 0;
}

void Emitter::endProc(const RowCol & pos)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    d_proc.back().endLine = lineset(pos); // mark the end of proc in the bytecode
    if( firstLine )
        d_out->line(firstLine); // mark the begin of proc
    d_out->addProcedure(d_proc.back());
    d_proc.pop_back();
    ops = 0;
}

void Emitter::beginType(const QByteArray& name, const RowCol & pos, bool isPublic, quint8 typeKind, const Quali& super)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT( typeKind == EmiTypes::Struct || typeKind == EmiTypes::Union || typeKind == EmiTypes::Object ||
              typeKind == EmiTypes::ProcType || typeKind == EmiTypes::MethType);
    d_typeKind = typeKind;
    if( typeKind == EmiTypes::Struct || typeKind == EmiTypes::Union || typeKind == EmiTypes::Object )
    {
        lineout(pos);
        d_out->beginType(name, isPublic, typeKind, super);
    }else
    {
        firstLine = lineset(pos);
        Q_ASSERT(typeKind == EmiTypes::ProcType || d_typeKind == EmiTypes::MethType);
        d_proc.append(ProcData());
        d_proc.back().name = name;
        d_proc.back().isPublic = isPublic;
        d_proc.back().kind = typeKind == EmiTypes::ProcType ? ProcData::ProcType : ProcData::MethType;
    }
}

void Emitter::endType()
{
    Q_ASSERT( d_typeKind == EmiTypes::Struct || d_typeKind == EmiTypes::Union || d_typeKind == EmiTypes::Object ||
              d_typeKind == EmiTypes::ProcType || d_typeKind == EmiTypes::MethType);
    if( d_typeKind == EmiTypes::Struct || d_typeKind == EmiTypes::Union || d_typeKind == EmiTypes::Object )
        d_out->endType();
    else
    {
        if( firstLine )
            d_out->line(firstLine);
        d_out->addProcedure(d_proc.back());
        d_proc.pop_back();
    }
    d_typeKind = 0;
}

void Emitter::addType(const QByteArray& name, const RowCol & pos, bool isPublic, const Quali& baseType, quint8 typeKind, quint32 len)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT( typeKind == EmiTypes::Alias || typeKind == EmiTypes::Pointer || typeKind == EmiTypes::Array);
    lineout(pos);
    d_out->addType(name,isPublic,baseType,typeKind,len);
}

void Emitter::addField(const QByteArray& fieldName, const RowCol & pos, const Quali& typeRef, bool isPublic, quint8 bits)
{
    Q_ASSERT( d_typeKind == EmiTypes::Struct || d_typeKind == EmiTypes::Union || d_typeKind == EmiTypes::Object );
    lineout(pos);
    d_out->addField(fieldName, typeRef, isPublic, bits );
}

quint32 Emitter::addLocal(const Quali& typeRef, QByteArray name, const RowCol & pos)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 );
    Q_ASSERT( !typeRef.second.isEmpty() );
    d_proc.back().locals.append(ProcData::Var(typeRef,name, lineset(pos)));
    return d_proc.back().locals.size()-1;
}

quint32 Emitter::addArgument(const Quali& typeRef, QByteArray name, const RowCol & pos)
{
    Q_ASSERT( !d_proc.isEmpty() || d_typeKind == EmiTypes::ProcType || d_typeKind == EmiTypes::MethType );
    if( typeRef.second.isEmpty() )
        return 0; // error reported elsewhere
    d_proc.back().params.append(ProcData::Var(typeRef,name, lineset(pos)));
    return 0;
}

void Emitter::setReturnType(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty()  || d_typeKind == EmiTypes::ProcType || d_typeKind == EmiTypes::MethType );
    Q_ASSERT( d_proc.back().retType.second.isEmpty() );
    d_proc.back().retType = typeRef;
}

void Emitter::setExtern(const QByteArray& origName)
{
    Q_ASSERT( d_proc.back().kind == ProcData::Extern );
    d_origName = origName;
}

void Emitter::setVararg()
{
    Q_ASSERT( !d_proc.isEmpty() || d_typeKind == EmiTypes::ProcType || d_typeKind == EmiTypes::MethType );
    d_proc.back().isVararg = true;
}

QByteArray Emitter::typeSymbol1(EmiTypes::Basic t)
{
    static QByteArray symbols[EmiTypes::IntPtr];
    if( symbols[0].isEmpty() )
    {
        symbols[EmiTypes::U1] = Mic::Atom::getAtom("uint8");
        symbols[EmiTypes::U2] = Mic::Atom::getAtom("uint16");
        symbols[EmiTypes::U4] = Mic::Atom::getAtom("uint32");
        symbols[EmiTypes::U8] = Mic::Atom::getAtom("uint64");
        symbols[EmiTypes::I1] = Mic::Atom::getAtom("int8");
        symbols[EmiTypes::I2] = Mic::Atom::getAtom("int16");
        symbols[EmiTypes::I4] = Mic::Atom::getAtom("int32");
        symbols[EmiTypes::I8] = Mic::Atom::getAtom("int64");
        symbols[EmiTypes::R4] = Mic::Atom::getAtom("float32");
        symbols[EmiTypes::R8] = Mic::Atom::getAtom("float64");
    }
    if( t < EmiTypes::IntPtr )
        return symbols[t];
    else
        return QByteArray();
}

QByteArray Emitter::typeSymbol2(EmiTypes::Basic t)
{
    static QByteArray symbols[EmiTypes::IntPtr];
    if( symbols[0].isEmpty() )
    {
        symbols[EmiTypes::U1] = Mic::Atom::getAtom("u1");
        symbols[EmiTypes::U2] = Mic::Atom::getAtom("u2");
        symbols[EmiTypes::U4] = Mic::Atom::getAtom("u4");
        symbols[EmiTypes::U8] = Mic::Atom::getAtom("u8");
        symbols[EmiTypes::I1] = Mic::Atom::getAtom("i1");
        symbols[EmiTypes::I2] = Mic::Atom::getAtom("i2");
        symbols[EmiTypes::I4] = Mic::Atom::getAtom("i4");
        symbols[EmiTypes::I8] = Mic::Atom::getAtom("i8");
        symbols[EmiTypes::R4] = Mic::Atom::getAtom("r4");
        symbols[EmiTypes::R8] = Mic::Atom::getAtom("r8");
    }
    if( t < EmiTypes::IntPtr )
        return symbols[t];
    else
        return QByteArray();
}

EmiTypes::Basic Emitter::fromSymbol(const QByteArray& name)
{
    for( int i = EmiTypes::I1; i <= EmiTypes::IntPtr; i++ )
        if( typeSymbol1(EmiTypes::Basic(i)).constData() == name.constData() ||
                typeSymbol2(EmiTypes::Basic(i)).constData() == name.constData() )
            return EmiTypes::Basic(i);
    return EmiTypes::Unknown;
}

bool Emitter::equals(const QByteArray& str, EmiTypes::Basic t)
{
    return str.constData() == typeSymbol1(t).constData() || str.constData() == typeSymbol2(t).constData();
}

QByteArray Emitter::toString(const Quali& q)
{
    QByteArray res = q.first;
    if( res.isEmpty() )
        res = q.second;
    else
        res += "!" + q.second;
    return res;
}

QByteArray Emitter::toString(const Trident& td)
{
    return toString(td.first) + "." + td.second;
}

void Emitter::add_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_add) );
    delta(-2+1);
}

void Emitter::abs_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_abs ) );
    delta(-1+1);
}

void Emitter::and_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_and) );
    delta(-2+1);
}

void Emitter::call_(const Quali& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_call,QVariant::fromValue(methodRef)) );
    delta(-argCount + (hasRet?1:0) );
}

void Emitter::calli_(const Quali& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_calli,QVariant::fromValue(methodRef)) );
    delta(-argCount + (hasRet?1:0) );
}

void Emitter::callvi_(const Quali& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_callvi,QVariant::fromValue(methodRef)) );
    delta(-argCount + (hasRet?1:0) );
}

void Emitter::callvirt_(const Trident& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_callvirt,QVariant::fromValue(methodRef)) );
    delta(-argCount - 1 + (hasRet?1:0) ); // this + args
}

void Emitter::case_(const CaseLabelList& l)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_case, QVariant::fromValue(l)) );
    delta(0);
}

void Emitter::castptr_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_castptr,QVariant::fromValue(typeRef)) );
    delta(-1+1);
}

void Emitter::ceq_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ceq));
    delta(-2+1);
}

void Emitter::cgt_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(ProcData::Op(IL_cgt_un) );
    else
        ops->append(ProcData::Op(IL_cgt) );
    delta(-2+1);
}

void Emitter::clt_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(ProcData::Op(IL_clt_un) );
    else
        ops->append(ProcData::Op(IL_clt) );
    delta(-2+1);
}

void Emitter::conv_(EmiTypes::Basic t)
{
    // NOTE that the value on the stack is at least int32 or uint32
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch( t )
    {
    case EmiTypes::I1:
        ops->append(ProcData::Op(IL_conv_i1));
        break;
    case EmiTypes::I2:
        ops->append(ProcData::Op(IL_conv_i2));
        break;
    case EmiTypes::I4:
        ops->append(ProcData::Op(IL_conv_i4));
        break;
    case EmiTypes::I8:
        ops->append(ProcData::Op(IL_conv_i8));
        break;
    case EmiTypes::R4:
        ops->append(ProcData::Op(IL_conv_r4));
        break;
    case EmiTypes::R8:
        ops->append(ProcData::Op(IL_conv_r8));
        break;
    case EmiTypes::U1:
        ops->append(ProcData::Op(IL_conv_u1));
        break;
    case EmiTypes::U2:
        ops->append(ProcData::Op(IL_conv_u2));
        break;
    case EmiTypes::U4:
        ops->append(ProcData::Op(IL_conv_u4));
        break;
    case EmiTypes::U8:
        ops->append(ProcData::Op(IL_conv_u8));
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    delta(-1+1);
}

void Emitter::div_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(ProcData::Op(IL_div_un ) );
    else
        ops->append(ProcData::Op(IL_div ) );
    delta(-2+1);
}

void Emitter::free_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_free) );
    delta(-1);
}

void Emitter::do_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_do) );
    delta(0);
}

void Emitter::dup_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_dup ) );
    delta(+1);
}

void Emitter::else_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_else) );
    delta(0);
}

void Emitter::end_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_end) );
    delta(0);
}

void Emitter::exit_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_exit) );
    delta(0);
}

void Emitter::goto_(const QByteArray& label)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_goto, label) );
    delta(0);
}

void Emitter::if_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_if) );
    delta(0);
}

void Emitter::iif_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_iif) );
    delta(0);
}

void Emitter::initobj(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_initobj,QVariant::fromValue(typeRef)));
    delta(0);
}

void Emitter::isinst_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_isinst,QVariant::fromValue(typeRef)));
    delta(0);
}

void Emitter::label_(const QByteArray& name)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_label,name) );
    delta(0);
}

void Emitter::ldarg_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(arg)
    {
    case 0:
        ops->append(ProcData::Op(IL_ldarg_0) );
        break;
    case 1:
        ops->append(ProcData::Op(IL_ldarg_1) );
        break;
    case 2:
        ops->append(ProcData::Op(IL_ldarg_2) );
        break;
    case 3:
        ops->append(ProcData::Op(IL_ldarg_3) );
        break;
    default:
        if( arg >= 4 && arg <= 255 )
            ops->append(ProcData::Op(IL_ldarg_s,arg) );
        else
            ops->append(ProcData::Op(IL_ldarg,arg) );
    }
    delta(+1);
}

void Emitter::ldarga_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( arg <= 255 )
        ops->append(ProcData::Op(IL_ldarga_s,arg) );
    else
        ops->append(ProcData::Op(IL_ldarga,arg) );
    delta(+1);
}

void Emitter::ldc_i4(qint32 v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(v)
    {
    case 0:
        ops->append(ProcData::Op(IL_ldc_i4_0));
        break;
    case 1:
        ops->append(ProcData::Op(IL_ldc_i4_1));
        break;
    case 2:
        ops->append(ProcData::Op(IL_ldc_i4_2));
        break;
    case 3:
        ops->append(ProcData::Op(IL_ldc_i4_3));
        break;
    case 4:
        ops->append(ProcData::Op(IL_ldc_i4_4));
        break;
    case 5:
        ops->append(ProcData::Op(IL_ldc_i4_5));
        break;
    case 6:
        ops->append(ProcData::Op(IL_ldc_i4_6));
        break;
    case 7:
        ops->append(ProcData::Op(IL_ldc_i4_7));
        break;
    case 8:
        ops->append(ProcData::Op(IL_ldc_i4_8));
        break;
    case -1:
        ops->append(ProcData::Op(IL_ldc_i4_m1));
        break;
    default:
        if( v >= -128 && v <= 127 )
            ops->append(ProcData::Op(IL_ldc_i4_s,v) );
        else
            ops->append(ProcData::Op(IL_ldc_i4,v) );
    }
    delta(+1);
}

void Emitter::ldc_i8(qint64 v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldc_i8,v) );
    delta(+1);
}

void Emitter::ldc_r4(double v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldc_r4,v) );
    delta(+1);
}

void Emitter::ldc_r8(double v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldc_r8,v) );
    delta(+1);
}

void Emitter::ldobj(const ConstrLiteral& v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldobj,QVariant::fromValue(v) ));
    delta(+1);
}

void Emitter::ldelem_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( typeRef.first.isEmpty() )
    {
        if( equals( typeRef.second,  EmiTypes::I1) )
            ops->append(ProcData::Op(IL_ldelem_i1));
        else if( equals( typeRef.second,  EmiTypes::I2) )
            ops->append(ProcData::Op(IL_ldelem_i2));
        else if( equals( typeRef.second,  EmiTypes::I4) )
            ops->append(ProcData::Op(IL_ldelem_i4));
        else if( equals( typeRef.second,  EmiTypes::I8) )
            ops->append(ProcData::Op(IL_ldelem_i8));
        else if( equals( typeRef.second, EmiTypes::R4 ) )
            ops->append(ProcData::Op(IL_ldelem_r4));
        else if( equals( typeRef.second,  EmiTypes::R8 ) )
            ops->append(ProcData::Op(IL_ldelem_r8));
        else if( equals( typeRef.second,  EmiTypes::U1 ) ||
                 typeRef.second.constData() == char_sym || typeRef.second.constData() == bool_sym)
            ops->append(ProcData::Op(IL_ldelem_u1));
        else if( equals( typeRef.second,  EmiTypes::U2 ) )
            ops->append(ProcData::Op(IL_ldelem_u2));
        else if( equals( typeRef.second,  EmiTypes::U4 ) )
            ops->append(ProcData::Op(IL_ldelem_u4));
        else if( equals( typeRef.second,  EmiTypes::U8 ) )
            ops->append(ProcData::Op(IL_ldelem_u8));
        else if( equals( typeRef.second,  EmiTypes::IntPtr ) )
            ops->append(ProcData::Op(IL_ldelem_ip));
        else if( equals( typeRef.second,  EmiTypes::IPP ) )
            ops->append(ProcData::Op(IL_ldelem_ipp));
        else
            ops->append(ProcData::Op(IL_ldelem,QVariant::fromValue(typeRef)));
    }else
        ops->append(ProcData::Op(IL_ldelem,QVariant::fromValue(typeRef)));
    delta(-2+1);
}

void Emitter::ldelema_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldelema,QVariant::fromValue(typeRef)));
    delta(-2+1);
}

void Emitter::ldfld_(const Trident& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldfld,QVariant::fromValue(fieldRef)));
    delta(-1+1);
}

void Emitter::ldflda_(const Trident& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldflda,QVariant::fromValue(fieldRef)));
    delta(-1+1);
}

void Emitter::ldmeth_(const Trident& methodRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldmeth,QVariant::fromValue(methodRef)));
    delta(-1+1);
}

void Emitter::ldproc_(const Quali& methodRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldproc,QVariant::fromValue(methodRef)));
    delta(+1);
}

void Emitter::ldind_(EmiTypes::Basic t)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    IL_op i = IL_invalid;
    switch( t )
    {
    case EmiTypes::I1:
        i = IL_ldind_i1;
        break;
    case EmiTypes::I2:
        i = IL_ldind_i2;
        break;
    case EmiTypes::I4:
        i = IL_ldind_i4;
        break;
    case EmiTypes::I8:
        i = IL_ldind_i8;
        break;
    case EmiTypes::U1:
        i = IL_ldind_u1;
        break;
    case EmiTypes::U2:
        i = IL_ldind_u2;
        break;
    case EmiTypes::U4:
        i = IL_ldind_u4;
        break;
    case EmiTypes::U8:
        i = IL_ldind_u8;
        break;
    case EmiTypes::R4:
        i = IL_ldind_r4;
        break;
    case EmiTypes::R8:
        i = IL_ldind_r8;
        break;
    case EmiTypes::IntPtr:
        i = IL_ldind_ip;
        break;
    case EmiTypes::IPP:
        i = IL_ldind_ipp;
        break;
    default:
        Q_ASSERT( false );
    }
    ops->append(ProcData::Op(i));
    delta(-1+1);
}

void Emitter::ldloc_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(loc)
    {
    case 0:
        ops->append(ProcData::Op(IL_ldloc_0) );
        break;
    case 1:
        ops->append(ProcData::Op(IL_ldloc_1) );
        break;
    case 2:
        ops->append(ProcData::Op(IL_ldloc_2) );
        break;
    case 3:
        ops->append(ProcData::Op(IL_ldloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            ops->append(ProcData::Op(IL_ldloc_s,loc) );
        else
            ops->append(ProcData::Op(IL_ldloc,loc) );
    }
    delta(+1);
}

void Emitter::ldloca_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( loc <= 255 )
        ops->append(ProcData::Op(IL_ldloca_s,loc));
    else
        ops->append(ProcData::Op(IL_ldloca,loc));
    delta(+1);
}

void Emitter::ldnull_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldnull));
    delta(+1);
}

void Emitter::ldind_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldind,QVariant::fromValue(typeRef)));
    delta(-1+1);
}

void Emitter::ldvar_(const Quali& memberRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldvar, QVariant::fromValue(memberRef)));
    delta(+1);
}

void Emitter::ldvara_(const Quali& memberRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldvara,QVariant::fromValue(memberRef)));
    delta(+1);
}

void Emitter::ldstr_(const QByteArray& str)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ldstr,str));
    delta(+1);
}

void Emitter::line_(const RowCol& pos)
{
    if( dbgInfo == None )
        return;
    if( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 )
    {
        quint32 cur = (dbgInfo == RowsOnly ? pos.d_row : pos.packed());
        if( cur != lastLine )
        {
            lastLine = cur;
            ops->append( ProcData::Op(IL_line,cur) );
        }
        delta(0);
    }else
        lineout(pos);
}

void Emitter::loop_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( d_proc.back().body.isEmpty() )
        ops->append(ProcData::Op(IL_nop));
    ops->append(ProcData::Op(IL_loop) );
    delta(0);
}

void Emitter::mul_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_mul ) );
    delta(-2+1);
}

void Emitter::neg_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_neg ) );
    delta(-1+1);
}

void Emitter::newarr_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_newarr,QVariant::fromValue(typeRef)));
    delta(-1+1);
}

void Emitter::newvla_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_newvla,QVariant::fromValue(typeRef)) );
    delta(0);
}

void Emitter::newobj_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_newobj,QVariant::fromValue(typeRef)));
    delta(-0+1);
}

void Emitter::nop_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_nop));
    delta(0);
}

void Emitter::not_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_not));
    delta(-1+1);
}

void Emitter::or_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_or));
    delta(-2+1);
}

void Emitter::pop_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_pop));
    delta(-1);
}

void Emitter::ptroff_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ptroff,QVariant::fromValue(typeRef)));
    delta(-1);
}

void Emitter::rem_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(ProcData::Op(IL_rem_un ) );
    else
        ops->append(ProcData::Op(IL_rem ) );
    delta(-2+1);
}

void Emitter::repeat_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( d_proc.back().body.isEmpty() )
        ops->append(ProcData::Op(IL_nop));
    ops->append(ProcData::Op(IL_repeat) );
    delta(0);
}

void Emitter::ret_(bool pop)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_ret));
    delta( pop ? -1 : 0);
}

void Emitter::shl_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_shl));
    delta(-2+1);
}

void Emitter::shr_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(ProcData::Op(IL_shr_un ) );
    else
        ops->append(ProcData::Op(IL_shr ) );
    delta(-2+1);
}

void Emitter::sizeof_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_sizeof,QVariant::fromValue(typeRef) ) );
    delta(+1);
}

void Emitter::starg_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( arg <= 255 )
        ops->append(ProcData::Op(IL_starg_s,arg ) );
    else
        ops->append(ProcData::Op(IL_starg,arg ) );
    delta(-1);
}

void Emitter::stelem_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( typeRef.first.isEmpty() )
    {
        if( equals( typeRef.second,  EmiTypes::I1) ||
                typeRef.second.constData() == char_sym || typeRef.second.constData() == bool_sym)
            ops->append(ProcData::Op(IL_stelem_i1));
        else if( equals( typeRef.second,  EmiTypes::I2) )
            ops->append(ProcData::Op(IL_stelem_i2));
        else if( equals( typeRef.second,  EmiTypes::I4) )
            ops->append(ProcData::Op(IL_stelem_i4));
        else if( equals( typeRef.second,  EmiTypes::I8) )
            ops->append(ProcData::Op(IL_stelem_i8));
        else if( equals( typeRef.second,  EmiTypes::R4) )
            ops->append(ProcData::Op(IL_stelem_r4));
        else if( equals( typeRef.second,  EmiTypes::R8) )
            ops->append(ProcData::Op(IL_stelem_r8));
        else if( equals( typeRef.second,  EmiTypes::IntPtr) )
            ops->append(ProcData::Op(IL_stelem_ip));
        else if( equals( typeRef.second,  EmiTypes::IPP) )
            ops->append(ProcData::Op(IL_stelem_ipp));
        else
            ops->append(ProcData::Op(IL_stelem,QVariant::fromValue(typeRef)));
    }else
        ops->append(ProcData::Op(IL_stelem,QVariant::fromValue(typeRef)));
    delta(-3);
}

void Emitter::stfld_(const Trident& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_stfld,QVariant::fromValue(fieldRef) ) );
    delta(-2);
}

void Emitter::stind_(EmiTypes::Basic t)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    IL_op i = IL_invalid;
    switch( t )
    {
    case EmiTypes::I1:
    case EmiTypes::U1:
        i = IL_stind_i1;
        break;
    case EmiTypes::I2:
    case EmiTypes::U2:
        i = IL_stind_i2;
        break;
    case EmiTypes::I4:
    case EmiTypes::U4:
        i = IL_stind_i4;
        break;
    case EmiTypes::I8:
    case EmiTypes::U8:
        i = IL_stind_i8;
        break;
    case EmiTypes::R4:
        i = IL_stind_r4;
        break;
    case EmiTypes::R8:
        i = IL_stind_r8;
        break;
    case EmiTypes::IntPtr:
        i = IL_stind_ip;
        break;
    case EmiTypes::IPP:
        i = IL_stind_ipp;
        break;
    default:
        Q_ASSERT( false );
    }
    ops->append(ProcData::Op(i));
    delta(-2);
}

void Emitter::stloc_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(loc)
    {
    case 0:
        ops->append(ProcData::Op(IL_stloc_0) );
        break;
    case 1:
        ops->append(ProcData::Op(IL_stloc_1) );
        break;
    case 2:
        ops->append(ProcData::Op(IL_stloc_2) );
        break;
    case 3:
        ops->append(ProcData::Op(IL_stloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            ops->append(ProcData::Op(IL_stloc_s,loc) );
        else
            ops->append(ProcData::Op(IL_stloc,loc) );
    }
    delta(-1);
}

void Emitter::strcpy_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_strcpy));
    delta(-2);
}

void Emitter::stind_(const Quali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_stind,QVariant::fromValue(typeRef)) );
    delta(-2);
}

void Emitter::stvar_(const Quali& memberRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_stvar,QVariant::fromValue(memberRef)) );
    delta(-1);
}

void Emitter::sub_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_sub ) );
    delta(-2+1);
}

void Emitter::switch_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_switch) );
    delta(0);
}

void Emitter::then_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_then) );
    delta(0);
}

void Emitter::until_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_until) );
    delta(0);
}

void Emitter::while_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( d_proc.back().body.isEmpty() )
        ops->append(ProcData::Op(IL_nop));
    ops->append(ProcData::Op(IL_while) );
    delta(0);
}

void Emitter::xor_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(ProcData::Op(IL_xor) );
    delta(-2+1);
}

void Emitter::delta(int d)
{
    int s = d_stackDepth;
    s += d;
    // TODO Q_ASSERT( s >= 0 );
    if( s >= 0 )
        d_stackDepth = s;
    else
        d_stackDepth = 0;
    if( d_stackDepth > d_maxStackDepth )
        d_maxStackDepth = d_stackDepth;
}

void Emitter::lineout(const RowCol & pos)
{
    const quint32 cur = lineset(pos);
    if( cur )
        d_out->line(cur);
}

quint32 Emitter::lineset(const RowCol & pos)
{
    if( dbgInfo == None )
        return 0;
    quint32 cur = (dbgInfo == RowsOnly ? pos.d_row : pos.packed());
    if( cur != lastLine )
    {
        lastLine = cur;
        return cur;
    }
    return 0;
}



