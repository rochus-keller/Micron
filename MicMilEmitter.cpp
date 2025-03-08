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
#include "MicMilEmitter.h"
#include "MicMilOp.h"
#include "MicToken.h"
#include <QCoreApplication>
#include <QDateTime>
#include <QtDebug>
using namespace Mic;

MilEmitter::MilEmitter(MilRenderer* r):d_out(r),d_typeKind(0),ops(0)
{
    Q_ASSERT( r );
}

void MilEmitter::beginModule(const QByteArray& fullName, const QString& sourceFile, const MilMetaParams& mp)
{
    Q_ASSERT( !fullName.isEmpty() );
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
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
}

void MilEmitter::endModule()
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->endModule();
}

void MilEmitter::addImport(const QByteArray& path)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->addImport(path);
}

void MilEmitter::addVariable(const MilQuali& typeRef, QByteArray name, bool isPublic)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->addVariable(typeRef,name, isPublic);
}

void MilEmitter::addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->addConst(typeRef, name, val);
}

void MilEmitter::beginProc(const QByteArray& procName, bool isPublic, quint8 kind, const QByteArray& objectType)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT(!procName.isEmpty());

    d_proc.append(MilProcedure());
    d_proc.back().name = procName;
    d_proc.back().isPublic = isPublic;
    d_proc.back().kind = kind;
    d_proc.back().binding = objectType;
    d_stackDepth = 0;
    d_maxStackDepth = 0;
    d_proc.back().isVararg = false;
    ops = &d_proc.back().body;
}

void MilEmitter::toFinallySection(bool yes)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( yes )
        ops = &d_proc.back().finally;
    else
        ops = &d_proc.back().body;
}

void MilEmitter::endProc()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    d_out->addProcedure(d_proc.back());
    d_proc.pop_back();
    ops = 0;
}

void MilEmitter::beginType(const QByteArray& name, bool isPublic, quint8 typeKind, const MilQuali& super)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT( typeKind == Struct || typeKind == Union || typeKind == Object ||
              typeKind == ProcType || typeKind == MethType);
    d_typeKind = typeKind;
    if( typeKind == Struct || typeKind == Union || typeKind == Object )
        d_out->beginType(name,isPublic, typeKind, super);
    else
    {
        Q_ASSERT(typeKind == ProcType || d_typeKind == MethType);
        d_proc.append(MilProcedure());
        d_proc.back().name = name;
        d_proc.back().isPublic = isPublic;
        d_proc.back().kind = typeKind == ProcType ? MilProcedure::ProcType : MilProcedure::MethType;
    }
}

void MilEmitter::endType()
{
    Q_ASSERT( d_typeKind == Struct || d_typeKind == Union || d_typeKind == Object ||
              d_typeKind == ProcType || d_typeKind == MethType);
    if( d_typeKind == Struct || d_typeKind == Union || d_typeKind == Object )
        d_out->endType();
    else
    {
        d_out->addProcedure(d_proc.back());
        d_proc.pop_back();
    }
    d_typeKind = 0;
}

void MilEmitter::addType(const QByteArray& name, bool isPublic, const MilQuali& baseType, quint8 typeKind, quint32 len)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT( typeKind == Alias || typeKind == Pointer || typeKind == Array);
    d_out->addType(name,isPublic,baseType,typeKind,len);
}

void MilEmitter::addField(const QByteArray& fieldName, const MilQuali& typeRef, bool isPublic, quint8 bits)
{
    Q_ASSERT( d_typeKind == Struct || d_typeKind == Union || d_typeKind == Object );
    d_out->addField(fieldName, typeRef, isPublic, bits );
}

quint32 MilEmitter::addLocal(const MilQuali& typeRef, QByteArray name)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 );
    Q_ASSERT( !typeRef.second.isEmpty() );
    d_proc.back().locals.append(MilVariable(typeRef,name));
    return d_proc.back().locals.size()-1;
}

quint32 MilEmitter::addArgument(const MilQuali& typeRef, QByteArray name)
{
    Q_ASSERT( !d_proc.isEmpty() || d_typeKind == ProcType || d_typeKind == MethType );
    if( typeRef.second.isEmpty() )
        return 0; // error reported elsewhere
    d_proc.back().params.append(MilVariable(typeRef,name));
    return 0;
}

void MilEmitter::setReturnType(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty()  || d_typeKind == ProcType || d_typeKind == MethType );
    Q_ASSERT( d_proc.back().retType.second.isEmpty() );
    d_proc.back().retType = typeRef;
}

void MilEmitter::setExtern(const QByteArray& origName)
{
    Q_ASSERT( d_proc.back().kind == MilProcedure::Extern );
    d_origName = origName;
}

void MilEmitter::setVararg()
{
    Q_ASSERT( !d_proc.isEmpty() || d_typeKind == ProcType || d_typeKind == MethType );
    d_proc.back().isVararg = true;
}

QByteArray MilEmitter::typeSymbol1(Type t)
{
    static QByteArray symbols[IntPtr];
    if( symbols[0].isEmpty() )
    {
        symbols[U1] = Token::getSymbol("uint8");
        symbols[U2] = Token::getSymbol("uint16");
        symbols[U4] = Token::getSymbol("uint32");
        symbols[U8] = Token::getSymbol("uint64");
        symbols[I1] = Token::getSymbol("int8");
        symbols[I2] = Token::getSymbol("int16");
        symbols[I4] = Token::getSymbol("int32");
        symbols[I8] = Token::getSymbol("int64");
        symbols[R4] = Token::getSymbol("float32");
        symbols[R8] = Token::getSymbol("float64");
    }
    if( t < IntPtr )
        return symbols[t];
    else
        return QByteArray();
}

QByteArray MilEmitter::typeSymbol2(Type t)
{
    static QByteArray symbols[IntPtr];
    if( symbols[0].isEmpty() )
    {
        symbols[U1] = Token::getSymbol("u1");
        symbols[U2] = Token::getSymbol("u2");
        symbols[U4] = Token::getSymbol("u4");
        symbols[U8] = Token::getSymbol("u8");
        symbols[I1] = Token::getSymbol("i1");
        symbols[I2] = Token::getSymbol("i2");
        symbols[I4] = Token::getSymbol("i4");
        symbols[I8] = Token::getSymbol("i8");
        symbols[R4] = Token::getSymbol("r4");
        symbols[R8] = Token::getSymbol("r8");
    }
    if( t < IntPtr )
        return symbols[t];
    else
        return QByteArray();
}

MilEmitter::Type MilEmitter::fromSymbol(const QByteArray& name)
{
    for( int i = I1; i <= IntPtr; i++ )
        if( typeSymbol1(Type(i)).constData() == name.constData() ||
                typeSymbol2(Type(i)).constData() == name.constData() )
            return Type(i);
    return Unknown;
}

bool MilEmitter::equals(const QByteArray& str, MilEmitter::Type t)
{
    return str.constData() == typeSymbol1(t).constData() || str.constData() == typeSymbol2(t).constData();
}

QByteArray MilEmitter::toString(const MilQuali& q)
{
    QByteArray res = q.first;
    if( res.isEmpty() )
        res = q.second;
    else
        res += "!" + q.second;
    return res;
}

QByteArray MilEmitter::toString(const MilTrident& td)
{
    return toString(td.first) + "." + td.second;
}

void MilEmitter::add_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_add) );
    delta(-2+1);
}

void MilEmitter::and_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_and) );
    delta(-2+1);
}

void MilEmitter::call_(const MilQuali& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_call,QVariant::fromValue(methodRef)) );
    delta(-argCount + (hasRet?1:0) );
}

void MilEmitter::calli_(const MilQuali& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_calli,QVariant::fromValue(methodRef)) );
    delta(-argCount + (hasRet?1:0) );
}

void MilEmitter::callvi_(const MilQuali& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_callvi,QVariant::fromValue(methodRef)) );
    delta(-argCount + (hasRet?1:0) );
}

void MilEmitter::callvirt_(const MilTrident& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_callvirt,QVariant::fromValue(methodRef)) );
    delta(-argCount - 1 + (hasRet?1:0) ); // this + args
}

void MilEmitter::case_(const CaseLabelList& l)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_case, QVariant::fromValue(l)) );
    delta(0);
}

void MilEmitter::castptr_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_castptr,QVariant::fromValue(typeRef)) );
    delta(-1+1);
}

void MilEmitter::ceq_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ceq));
    delta(-2+1);
}

void MilEmitter::cgt_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(MilOperation(IL_cgt_un) );
    else
        ops->append(MilOperation(IL_cgt) );
    delta(-2+1);
}

void MilEmitter::clt_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(MilOperation(IL_clt_un) );
    else
        ops->append(MilOperation(IL_clt) );
    delta(-2+1);
}

void MilEmitter::conv_(MilEmitter::Type t)
{
    // NOTE that the value on the stack is at least int32 or uint32
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch( t )
    {
    case I1:
        ops->append(MilOperation(IL_conv_i1));
        break;
    case I2:
        ops->append(MilOperation(IL_conv_i2));
        break;
    case I4:
        ops->append(MilOperation(IL_conv_i4));
        break;
    case I8:
        ops->append(MilOperation(IL_conv_i8));
        break;
    case R4:
        ops->append(MilOperation(IL_conv_r4));
        break;
    case R8:
        ops->append(MilOperation(IL_conv_r8));
        break;
    case U1:
        ops->append(MilOperation(IL_conv_u1));
        break;
    case U2:
        ops->append(MilOperation(IL_conv_u2));
        break;
    case U4:
        ops->append(MilOperation(IL_conv_u4));
        break;
    case U8:
        ops->append(MilOperation(IL_conv_u8));
        break;
    case IntPtr:
        ops->append(MilOperation(IL_conv_ip));
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    delta(-1+1);
}

void MilEmitter::div_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(MilOperation(IL_div_un ) );
    else
        ops->append(MilOperation(IL_div ) );
    delta(-2+1);
}

void MilEmitter::free_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_free) );
    delta(-1);
}

void MilEmitter::do_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_do) );
    delta(0);
}

void MilEmitter::dup_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_dup ) );
    delta(+1);
}

void MilEmitter::else_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_else) );
    delta(0);
}

void MilEmitter::end_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_end) );
    delta(0);
}

void MilEmitter::exit_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_exit) );
    delta(0);
}

void MilEmitter::goto_(const QByteArray& label)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_goto, label) );
    delta(0);
}

void MilEmitter::ifgoto_(const QByteArray& label)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ifgoto, label) );
    delta(0);
}

void MilEmitter::if_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_if) );
    delta(0);
}

void MilEmitter::iif_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_iif) );
    delta(0);
}

void MilEmitter::initobj(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_initobj,QVariant::fromValue(typeRef)));
    delta(0);
}

void MilEmitter::isinst_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_isinst,QVariant::fromValue(typeRef)));
    delta(0);
}

void MilEmitter::label_(const QByteArray& name)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_label,name) );
    delta(0);
}

void MilEmitter::ldarg_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(arg)
    {
    case 0:
        ops->append(MilOperation(IL_ldarg_0) );
        break;
    case 1:
        ops->append(MilOperation(IL_ldarg_1) );
        break;
    case 2:
        ops->append(MilOperation(IL_ldarg_2) );
        break;
    case 3:
        ops->append(MilOperation(IL_ldarg_3) );
        break;
    default:
        if( arg >= 4 && arg <= 255 )
            ops->append(MilOperation(IL_ldarg_s,arg) );
        else
            ops->append(MilOperation(IL_ldarg,arg) );
    }
    delta(+1);
}

void MilEmitter::ldarga_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( arg <= 255 )
        ops->append(MilOperation(IL_ldarga_s,arg) );
    else
        ops->append(MilOperation(IL_ldarga,arg) );
    delta(+1);
}

void MilEmitter::ldc_i4(qint32 v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(v)
    {
    case 0:
        ops->append(MilOperation(IL_ldc_i4_0));
        break;
    case 1:
        ops->append(MilOperation(IL_ldc_i4_1));
        break;
    case 2:
        ops->append(MilOperation(IL_ldc_i4_2));
        break;
    case 3:
        ops->append(MilOperation(IL_ldc_i4_3));
        break;
    case 4:
        ops->append(MilOperation(IL_ldc_i4_4));
        break;
    case 5:
        ops->append(MilOperation(IL_ldc_i4_5));
        break;
    case 6:
        ops->append(MilOperation(IL_ldc_i4_6));
        break;
    case 7:
        ops->append(MilOperation(IL_ldc_i4_7));
        break;
    case 8:
        ops->append(MilOperation(IL_ldc_i4_8));
        break;
    case -1:
        ops->append(MilOperation(IL_ldc_i4_m1));
        break;
    default:
        if( v >= -128 && v <= 127 )
            ops->append(MilOperation(IL_ldc_i4_s,v) );
        else
            ops->append(MilOperation(IL_ldc_i4,v) );
    }
    delta(+1);
}

void MilEmitter::ldc_i8(qint64 v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldc_i8,v) );
    delta(+1);
}

void MilEmitter::ldc_r4(double v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldc_r4,v) );
    delta(+1);
}

void MilEmitter::ldc_r8(double v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldc_r8,v) );
    delta(+1);
}

void MilEmitter::ldc_obj(const MilObject& v)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldc_obj,QVariant::fromValue(v) ));
    delta(+1);
}

void MilEmitter::ldelem_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( typeRef.first.isEmpty() )
    {
        if( equals( typeRef.second,  I1) )
            ops->append(MilOperation(IL_ldelem_i1));
        else if( equals( typeRef.second,  I2) )
            ops->append(MilOperation(IL_ldelem_i2));
        else if( equals( typeRef.second,  I4) )
            ops->append(MilOperation(IL_ldelem_i4));
        else if( equals( typeRef.second,  I8) )
            ops->append(MilOperation(IL_ldelem_i8));
        else if( equals( typeRef.second, R4 ) )
            ops->append(MilOperation(IL_ldelem_r4));
        else if( equals( typeRef.second,  R8 ) )
            ops->append(MilOperation(IL_ldelem_r8));
        else if( equals( typeRef.second,  U1 ) )
            ops->append(MilOperation(IL_ldelem_u1));
        else if( equals( typeRef.second,  U2 ) )
            ops->append(MilOperation(IL_ldelem_u2));
        else if( equals( typeRef.second,  U4 ) )
            ops->append(MilOperation(IL_ldelem_u4));
        else if( equals( typeRef.second,  U8 ) )
            ops->append(MilOperation(IL_ldelem_u8));
        else
            ops->append(MilOperation(IL_ldelem,QVariant::fromValue(typeRef)));
    }else
        ops->append(MilOperation(IL_ldelem,QVariant::fromValue(typeRef)));
    delta(-2+1);
}

void MilEmitter::ldelema_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldelema,QVariant::fromValue(typeRef)));
    delta(-2+1);
}

void MilEmitter::ldfld_(const MilTrident& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldfld,QVariant::fromValue(fieldRef)));
    delta(-1+1);
}

void MilEmitter::ldflda_(const MilTrident& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldflda,QVariant::fromValue(fieldRef)));
    delta(-1+1);
}

void MilEmitter::ldmeth_(const MilTrident& methodRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldmeth,QVariant::fromValue(methodRef)));
    delta(-1+1);
}

void MilEmitter::ldproc_(const MilQuali& methodRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldproc,QVariant::fromValue(methodRef)));
    delta(+1);
}

void MilEmitter::ldind_(MilEmitter::Type t)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    IL_op i = IL_invalid;
    switch( t )
    {
    case I1:
        i = IL_ldind_i1;
        break;
    case I2:
        i = IL_ldind_i2;
        break;
    case I4:
        i = IL_ldind_i4;
        break;
    case I8:
        i = IL_ldind_i8;
        break;
    case U1:
        i = IL_ldind_u1;
        break;
    case U2:
        i = IL_ldind_u2;
        break;
    case U4:
        i = IL_ldind_u4;
        break;
    case U8:
        i = IL_ldind_u8;
        break;
    case R4:
        i = IL_ldind_r4;
        break;
    case R8:
        i = IL_ldind_r8;
        break;
    case IntPtr:
        i = IL_ldind_ip;
        break;
    case IPP:
        i = IL_ldind_ipp;
        break;
    default:
        Q_ASSERT( false );
    }
    ops->append(MilOperation(i));
    delta(-1+1);
}

void MilEmitter::ldloc_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(loc)
    {
    case 0:
        ops->append(MilOperation(IL_ldloc_0) );
        break;
    case 1:
        ops->append(MilOperation(IL_ldloc_1) );
        break;
    case 2:
        ops->append(MilOperation(IL_ldloc_2) );
        break;
    case 3:
        ops->append(MilOperation(IL_ldloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            ops->append(MilOperation(IL_ldloc_s,loc) );
        else
            ops->append(MilOperation(IL_ldloc,loc) );
    }
    delta(+1);
}

void MilEmitter::ldloca_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( loc <= 255 )
        ops->append(MilOperation(IL_ldloca_s,loc));
    else
        ops->append(MilOperation(IL_ldloca,loc));
    delta(+1);
}

void MilEmitter::ldnull_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldnull));
    delta(+1);
}

void MilEmitter::ldobj_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldobj,QVariant::fromValue(typeRef)));
    delta(-1+1);
}

void MilEmitter::ldvar_(const MilQuali& memberRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldvar, QVariant::fromValue(memberRef)));
    delta(+1);
}

void MilEmitter::ldvara_(const MilQuali& memberRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldvara,QVariant::fromValue(memberRef)));
    delta(+1);
}

void MilEmitter::ldstr_(const QByteArray& str)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ldstr,str));
    delta(+1);
}

void MilEmitter::line_(quint32 l)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_line,l) );
    delta(0);
}

void MilEmitter::loop_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( d_proc.back().body.isEmpty() )
        ops->append(MilOperation(IL_nop));
    ops->append(MilOperation(IL_loop) );
    delta(0);
}

void MilEmitter::mul_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_mul ) );
    delta(-2+1);
}

void MilEmitter::neg_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_neg ) );
    delta(-1+1);
}

void MilEmitter::newarr_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_newarr,QVariant::fromValue(typeRef)));
    delta(-1+1);
}

void MilEmitter::newvla_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_newvla,QVariant::fromValue(typeRef)) );
    delta(0);
}

void MilEmitter::newobj_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_newobj,QVariant::fromValue(typeRef)));
    delta(-0+1);
}

void MilEmitter::nop_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_nop));
    delta(0);
}

void MilEmitter::not_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_not));
    delta(-1+1);
}

void MilEmitter::or_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_or));
    delta(-2+1);
}

void MilEmitter::pop_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_pop));
    delta(-1);
}

void MilEmitter::ptroff_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ptroff,QVariant::fromValue(typeRef)));
    delta(-1);
}

void MilEmitter::rem_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(MilOperation(IL_rem_un ) );
    else
        ops->append(MilOperation(IL_rem ) );
    delta(-2+1);
}

void MilEmitter::repeat_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( d_proc.back().body.isEmpty() )
        ops->append(MilOperation(IL_nop));
    ops->append(MilOperation(IL_repeat) );
    delta(0);
}

void MilEmitter::ret_(bool pop)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_ret));
    delta( pop ? -1 : 0);
}

void MilEmitter::shl_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_shl));
    delta(-2+1);
}

void MilEmitter::shr_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( withUnsigned )
        ops->append(MilOperation(IL_shr_un ) );
    else
        ops->append(MilOperation(IL_shr ) );
    delta(-2+1);
}

void MilEmitter::sizeof_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_sizeof,QVariant::fromValue(typeRef) ) );
    delta(+1);
}

void MilEmitter::starg_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( arg <= 255 )
        ops->append(MilOperation(IL_starg_s,arg ) );
    else
        ops->append(MilOperation(IL_starg,arg ) );
    delta(-1);
}

void MilEmitter::stelem_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( equals( typeRef.second,  I1) )
        ops->append(MilOperation(IL_stelem_i1));
    else if( equals( typeRef.second,  I2) )
        ops->append(MilOperation(IL_stelem_i2));
    else if( equals( typeRef.second,  I4) )
        ops->append(MilOperation(IL_stelem_i4));
    else if( equals( typeRef.second,  I8) )
        ops->append(MilOperation(IL_stelem_i8));
    else if( equals( typeRef.second,  R4) )
        ops->append(MilOperation(IL_stelem_r4));
    else if( equals( typeRef.second,  R8) )
        ops->append(MilOperation(IL_stelem_r8));
    else
        ops->append(MilOperation(IL_stelem,QVariant::fromValue(typeRef)));
    delta(-3);
}

void MilEmitter::stfld_(const MilTrident& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_stfld,QVariant::fromValue(fieldRef) ) );
    delta(-2);
}

void MilEmitter::stind_(MilEmitter::Type t)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    IL_op i = IL_invalid;
    switch( t )
    {
    case I1:
    case U1:
        i = IL_stind_i1;
        break;
    case I2:
    case U2:
        i = IL_stind_i2;
        break;
    case I4:
    case U4:
        i = IL_stind_i4;
        break;
    case I8:
    case U8:
        i = IL_stind_i8;
        break;
    case R4:
        i = IL_stind_r4;
        break;
    case R8:
        i = IL_stind_r8;
        break;
    case IntPtr:
        i = IL_stind_ip;
        break;
    case IPP:
        i = IL_stind_ipp;
        break;
    default:
        Q_ASSERT( false );
    }
    ops->append(MilOperation(i));
    delta(-2);
}

void MilEmitter::stloc_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    switch(loc)
    {
    case 0:
        ops->append(MilOperation(IL_stloc_0) );
        break;
    case 1:
        ops->append(MilOperation(IL_stloc_1) );
        break;
    case 2:
        ops->append(MilOperation(IL_stloc_2) );
        break;
    case 3:
        ops->append(MilOperation(IL_stloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            ops->append(MilOperation(IL_stloc_s,loc) );
        else
            ops->append(MilOperation(IL_stloc,loc) );
    }
    delta(-1);
}

void MilEmitter::stobj_(const MilQuali& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_stobj,QVariant::fromValue(typeRef)) );
    delta(-2);
}

void MilEmitter::stvar_(const MilQuali& memberRef)
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_stvar,QVariant::fromValue(memberRef)) );
    delta(-1);
}

void MilEmitter::sub_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_sub ) );
    delta(-2+1);
}

void MilEmitter::switch_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_switch) );
    delta(0);
}

void MilEmitter::then_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_then) );
    delta(0);
}

void MilEmitter::until_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_until) );
    delta(0);
}

void MilEmitter::while_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    if( d_proc.back().body.isEmpty() )
        ops->append(MilOperation(IL_nop));
    ops->append(MilOperation(IL_while) );
    delta(0);
}

void MilEmitter::xor_()
{
    Q_ASSERT( !d_proc.isEmpty() && d_typeKind == 0 && ops != 0 );
    ops->append(MilOperation(IL_xor) );
    delta(-2+1);
}

void MilEmitter::delta(int d)
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

IlAsmRenderer::IlAsmRenderer(QIODevice* dev):level(0),sourceRendered(false),state(Idle)
{
    // default is UTF-8, so no need to setCodec
    out.setDevice(dev);
}

void IlAsmRenderer::beginModule(const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& mp)
{
    source = sourceFile;
    d_moduleName = moduleName;
    sourceRendered = false;
    out << "// Generated by " << qApp->applicationName() << " " << qApp->applicationVersion() << " on "
        << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;

    out << "module " << moduleName;
    if( !mp.isEmpty() )
    {
        out << " (";
        for( int i = 0; i < mp.size(); i++ )
        {
            if( i != 0 )
                out << "; ";
            //if( !mp[i].type.isEmpty() )
            //    out << "const ";
            out << mp[i];
            //if( !mp[i].type.isEmpty() )
            //    out << ": " << mp[i].type;
        }
        out << ")";
    }
    out << endl;

    state = Module;
    level++;
}

void IlAsmRenderer::endModule()
{
    level--;
    out << ws() << "end " << d_moduleName << endl;
    state = Idle;
}

void IlAsmRenderer::addImport(const QByteArray& path)
{
    out << ws() << "import " << path;
    out << endl;
}

void IlAsmRenderer::addVariable(const MilQuali& typeRef, QByteArray name, bool isPublic)
{
    out << ws() << "var " << name << ": " << MilEmitter::toString(typeRef) << endl;
}

void IlAsmRenderer::addProcedure(const MilProcedure& m)
{
    if( m.kind == MilProcedure::Invalid )
        return;
    render(m);
}

void IlAsmRenderer::beginType(const QByteArray& className, bool isPublic, quint8 classKind, const MilQuali& super)
{
    Q_ASSERT(classKind == MilEmitter::Union || classKind == MilEmitter::Struct || classKind == MilEmitter::Object);
    state = Struct;
    out << ws() << "type " << className;
    if( isPublic )
        out << "*";

    out << " = ";
    if( classKind == MilEmitter::Union )
        out << "union ";
    else if( classKind == MilEmitter::Object )
    {
        out << "object ";
        if( !super.first.isEmpty() || !super.second.isEmpty() )
        {
            out << "(";
            if( !super.first.isEmpty() )
                out << super.first << ".";
            out << super.second;
            out << ")";
        }
    }
    else
        out << "struct ";

    out << endl;
    level++;
}

void IlAsmRenderer::endType()
{
    level--;
    out << ws();
    out << "end";
    out << endl;
    state = Module;
}

void IlAsmRenderer::addType(const QByteArray& name, bool isPublic, const MilQuali& baseType, quint8 typeKind, quint32 len)
{
    out << ws() << "type " << name;
    if( isPublic )
        out << "*";

    out << " = ";
    switch(typeKind)
    {
    case MilEmitter::Alias:
        out << MilEmitter::toString(baseType) << endl;
        break;
    case MilEmitter::Pointer:
        out << "pointer to " << MilEmitter::toString(baseType) << endl;
        break;
    case MilEmitter::Array:
        out << "array ";
        if( len )
            out << len << " ";
        out << "of " << MilEmitter::toString(baseType) << endl;
        break;
    }
}

void IlAsmRenderer::addField(const QByteArray& fieldName, const MilQuali& typeRef, bool isPublic, quint8 bits)
{
    out << ws();
    if( fieldName.isEmpty() && state == Struct )
    {
        out << ".. " << bits;
    }else
    {
        if( state == Module )
            out << "var ";
        out << fieldName;
        if( isPublic )
            out << "*";

        out << ": " << MilEmitter::toString(typeRef);
        if( bits && state == Struct )
            out << " : " << bits;
    }
    out << endl;
}

static inline bool isAllPrintable(const QString& str)
{
    for( int i = 0; i < str.size(); i++ )
    {
        if( !str[i].isPrint() )
            return false;
    }
    return true;
}

static void renderComponents( QTextStream& out, const QVariant& data )
{
    if( data.canConvert<MilRecordLiteral>() )
    {
        out << "{";
        MilRecordLiteral m = data.value<MilRecordLiteral>();
        for( int i = 0; i < m.size(); i++ )
        {
            out << m[i].first << "=";
            renderComponents(out,m[i].second);
        }
        out << "}";
        return;
    }
    switch( data.type() )
    {
    case QVariant::List:
        out << "{";
        foreach( const QVariant& v, data.toList() )
        {
            renderComponents(out,v);
            out << " ";
        }
        out << "}";
        break;
    case QVariant::ByteArray:
        out << "#" << data.toByteArray().toHex() << "#";
        break;
    case QVariant::String:
        {
            const QString str = data.toString();
            if(isAllPrintable(str))
            {
                if( str.contains('\"') )
                    out << "'" << str << "'";
                else
                    out << "\"" << str << "\"";
            }else
            {
                out << "#" << str.toLatin1().toHex() << "00#";
            }
        }
        break;
    case QVariant::LongLong:
    case QVariant::Int:
    case QVariant::Bool:
        out << data.toLongLong();
        break;
    case QVariant::ULongLong:
    case QVariant::UInt:
        out << data.toULongLong();
        break;
    case QVariant::Double:
        out << data.toDouble();
        break;
    default:
        Q_ASSERT(false);
    }
}

void IlAsmRenderer::render(const MilProcedure& m)
{
    State old = state;

    if( m.kind == MilProcedure::ProcType || m.kind == MilProcedure::MethType || m.kind == MilProcedure::Extern )
    {
        out << ws() << "type " << m.name;
        if( m.isPublic )
            out << "*";
        out << " = proc";
        if( m.kind == MilProcedure::MethType )
            out << "^";
    }else
    {
        out << ws() << "procedure ";
        if( !m.binding.isEmpty() )
            out << m.binding << ".";
        out << m.name;
        state = Proc;
        if( m.isPublic )
            out << "* ";
    }

    out << "(";

    for( int i = 0; i < m.params.size(); i++ )
    {
        if( i != 0 )
            out << "; ";
        if( !m.params[i].type.second.isEmpty() )
            out << m.params[i].name;
        else
            out << i;
        out << ": "<< MilEmitter::toString(m.params[i].type);
    }
    if( m.isVararg )
        out << ", .. ";

    out << ")";
    if( !m.retType.second.isEmpty() )
        out << ":" << MilEmitter::toString(m.retType);

    switch( m.kind)
    {
    case MilProcedure::Extern:
        out << " extern ";
        break;
    case MilProcedure::Inline:
        out << " inline ";
        break;
    case MilProcedure::Invar:
        out << " invar ";
        break;
    case MilProcedure::ModuleInit:
        out << " init ";
        break;
    }

    out << endl;

    if(m.kind == MilProcedure::ProcType || m.kind == MilProcedure::MethType || m.kind == MilProcedure::Extern )
        return;

    if( !m.locals.isEmpty() )
    {
        out << ws() << "var ";
        level++;
        for( int i = 0; i < m.locals.size(); i++ )
        {
            if( !m.locals[i].name.isEmpty() )
                out << m.locals[i].name;
            else
                out << i;
            out << ": " << MilEmitter::toString(m.locals[i].type) << "; ";
        }
        out << endl;
        level--;
    }
    out << ws() << "begin" << endl;

    level++;
    for( int i = 0; i < m.body.size(); i++ )
    {
        const MilOperation& op = m.body[i];
        switch( op.op )
        {
        case IL_invalid:
            break;
        case IL_if:
        case IL_iif:
        case IL_loop:
        case IL_repeat:
        case IL_switch:
        case IL_while:
            out << ws() << s_opName[op.op];
            if( op.index )
                out << " // " << op.index;
            out << endl;
            level++;
            break;
        case IL_then:
        case IL_else:
        case IL_until:
        case IL_do:
            level--;
            out << ws() << s_opName[op.op];
            if( op.index )
                out << " // " << op.index;
            out << endl;
            level++;
            break;
        case IL_end:
            level--;
            out << ws() << s_opName[op.op];
            if( op.index )
                out << " // " << op.index;
            out << endl;
            break;
        case IL_case:
            level--;
            out << ws() << s_opName[op.op];
            if( op.index )
                out << " // " << op.index;
            out << endl;
            level++;
            out << ws();
            foreach(qint64 i, op.arg.value<CaseLabelList>() )
                out << i << " ";
            out << endl;
            break;
        case IL_ldc_obj:
            {
                const MilObject obj = op.arg.value<MilObject>();
                out << ws() << s_opName[op.op] << " ";
                Q_ASSERT( obj.data.type() == QVariant::ByteArray ||
                          obj.data.type() == QVariant::List ||
                          obj.data.type() == QVariant::Map );
                if( obj.data.type() != QVariant::ByteArray )
                    out << MilEmitter::toString(obj.typeRef);
                renderComponents(out,obj.data);
                out << endl;
            }
            break;
        case IL_ldstr:
            {
                QByteArray bytes = op.arg.toByteArray();
                const QString str = QString::fromLatin1(bytes);
                bool isPrint = true;
                for( int i = 0; i < str.size(); i++ )
                    if( !str[i].isPrint() )
                    {
                        isPrint = false;
                        break;
                    }
                out << ws() << s_opName[op.op] << " ";
                if( isPrint )
                {
                    bytes = str.toLatin1(); // to get rid of explicit terminating zero
                    if( bytes.contains('"') )
                        out << "'" << bytes << "'";
                    else
                        out << "\"" << bytes << "\"";
                }else
                {
                    out << "#" << bytes.toHex();
                    if( !bytes.endsWith('\0') )
                        out << "00";
                    out << "#";
                }
                out << endl;
            }
            break;
        default:
            out << ws() << s_opName[op.op];
            if( !op.arg.isNull() )
            {
                out << " ";
                if( op.arg.canConvert<MilQuali>() )
                {
                    MilQuali q = op.arg.value<MilQuali>();
                    out << MilEmitter::toString(q);
                }else if( op.arg.canConvert<MilTrident>() )
                {
                    MilTrident td = op.arg.value<MilTrident>();
                    out << MilEmitter::toString(td);
                }else
                    out << op.arg.toByteArray();
            }
            out << endl;
            break;
        }
    }
    level--;
    out << ws() << "end " << m.name << endl;

    state = old;
}

void IlAsmRenderer::addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val)
{
    out << ws() << "const " << name;
    if( !typeRef.second.isEmpty() )
        out << " : " << MilEmitter::toString(typeRef);
    if( !val.isNull() )
    {
        out << " = ";
        if( val.canConvert<MilObject>())
        {
            const MilObject obj = val.value<MilObject>();
            if( obj.data.type() != QVariant::ByteArray )
                out << MilEmitter::toString(obj.typeRef);
            renderComponents(out,obj.data);
        }else
            renderComponents(out, val );
        out << endl;
    }
}


void MilSplitter::beginModule(const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& mp)
{
    foreach(MilRenderer* r, renderer)
        r->beginModule(moduleName, sourceFile, mp);
}

void MilSplitter::endModule()
{
    foreach(MilRenderer* r, renderer)
        r->endModule();
}

void MilSplitter::addImport(const QByteArray& path)
{
    foreach(MilRenderer* r, renderer)
        r->addImport(path);
}

void MilSplitter::addVariable(const MilQuali& typeRef, QByteArray name, bool isPublic)
{
    foreach(MilRenderer* r, renderer)
        r->addVariable(typeRef, name, isPublic);
}

void MilSplitter::addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val)
{
    foreach(MilRenderer* r, renderer)
        r->addConst(typeRef, name, val);
}

void MilSplitter::addProcedure(const MilProcedure& method)
{
    foreach(MilRenderer* r, renderer)
        r->addProcedure(method);
}

void MilSplitter::beginType(const QByteArray& name, bool isPublic, quint8 typeKind, const MilQuali& super)
{
    foreach(MilRenderer* r, renderer)
        r->beginType(name,isPublic,typeKind, super);
}

void MilSplitter::endType()
{
    foreach(MilRenderer* r, renderer)
        r->endType();
}

void MilSplitter::addType(const QByteArray& name, bool isPublic, const MilQuali& baseType, quint8 typeKind, quint32 len)
{
    foreach(MilRenderer* r, renderer)
        r->addType(name,isPublic,baseType, typeKind, len);
}

void MilSplitter::addField(const QByteArray& fieldName, const MilQuali& typeRef, bool isPublic, quint8 bits)
{
    foreach(MilRenderer* r, renderer)
        r->addField(fieldName,typeRef,isPublic, bits);
}


int MilType::indexOfField(const QByteArray& name) const
{
    for( int i = 0; i < fields.size(); i++ )
    {
        if( fields[i].name.constData() == name.constData() )
            return i;
    }
    return -1;
}

const MilVariable*MilType::findField(const QByteArray& name) const
{
    for( int i = 0; i < fields.size(); i++ )
    {
        if( fields[i].name.constData() == name.constData() )
            return &fields[i];
    }
    return 0;
}

int MilModule::indexOfVar(const QByteArray& name) const
{
    for( int i = 0; i < vars.size(); i++ )
    {
        if( vars[i].name.constData() == name.constData() )
            return i;
    }
    return -1;
}
