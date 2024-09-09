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
#include <QCoreApplication>
#include <QDateTime>
using namespace Mic;

MilEmitter::MilEmitter(MilRenderer* r):d_out(r),d_typeKind(0)
{
    Q_ASSERT( r );
}

void MilEmitter::beginModule(const QByteArray& moduleName, const QString& sourceFile, const MilMetaParams& mp)
{
    Q_ASSERT( !moduleName.isEmpty() );
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
    d_out->beginModule(moduleName,sourceFile, names);
    foreach( const MilMetaParam& m, mp )
    {
        if( !m.isGeneric )
            continue; // otherwise the client directly calls addType/beginType or addConst
        if( m.isConst )
            d_out->addConst("",m.name,QVariant());
        else
            d_out->addType(m.name,false,"",Generic,0);
    }
}

void MilEmitter::endModule()
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->endModule();
}

void MilEmitter::addImport(const QByteArray& path, const QByteArray& name)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->addImport(path, name);
}

void MilEmitter::addVariable(const QByteArray& typeRef, QByteArray name, bool isPublic)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->addVariable(typeRef,name, isPublic);
}

void MilEmitter::addConst(const QByteArray& typeRef, const QByteArray& name, const QVariant& val)
{
    Q_ASSERT( d_proc.isEmpty() && d_typeKind == 0 );
    d_out->addConst(typeRef, name, val);
}

void MilEmitter::beginProc(const QByteArray& procName, bool isPublic, quint8 kind)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT(!procName.isEmpty());

    d_proc.append(MilProcedure());
    d_proc.back().d_name = procName;
    d_proc.back().d_isPublic = isPublic;
    d_proc.back().d_kind = kind;
    d_stackDepth = 0;
    d_maxStackDepth = 0;
    d_proc.back().d_isVararg = false;
}

void MilEmitter::endProc()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_out->addProcedure(d_proc.back());
    d_proc.pop_back();
}

void MilEmitter::beginType(const QByteArray& name, bool isPublic, quint8 typeKind)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT( typeKind == Struct || typeKind == Union || typeKind == ProcType);
    d_typeKind = typeKind;
    if( typeKind == Struct || typeKind == Union )
        d_out->beginType(name,isPublic, typeKind);
    else
    {
        d_proc.append(MilProcedure());
        d_proc.back().d_name = name;
        d_proc.back().d_isPublic = isPublic;
        d_proc.back().d_kind = MilProcedure::ProcType;
    }
}

void MilEmitter::endType()
{
    Q_ASSERT( d_typeKind == Struct || d_typeKind == Union || d_typeKind == ProcType);
    if( d_typeKind == Struct || d_typeKind == Union )
        d_out->endType();
    else
    {
        d_out->addProcedure(d_proc.back());
        d_proc.pop_back();
    }
    d_typeKind = 0;
}

void MilEmitter::addType(const QByteArray& name, bool isPublic, const QByteArray& baseType, quint8 typeKind, quint32 len)
{
    Q_ASSERT( d_typeKind == 0 );
    Q_ASSERT( typeKind == Alias || typeKind == Pointer || typeKind == Array);
    d_out->addType(name,isPublic,baseType,typeKind,len);
}

void MilEmitter::addField(const QByteArray& fieldName, const QByteArray& typeRef, bool isPublic)
{
    Q_ASSERT( d_typeKind == Struct || d_typeKind == Union );
    d_out->addField(fieldName, typeRef, isPublic );
}

quint32 MilEmitter::addLocal(const QByteArray& typeRef, QByteArray name)
{
    Q_ASSERT( !d_proc.isEmpty() );
    Q_ASSERT( !typeRef.isEmpty() );
    d_proc.back().d_locals.append(qMakePair(typeRef,name));
    return d_proc.back().d_locals.size()-1;
}

quint32 MilEmitter::addArgument(const QByteArray& typeRef, QByteArray name)
{
    Q_ASSERT( !d_proc.isEmpty() || d_typeKind == ProcType );
    if( typeRef.isEmpty() )
        return 0; // error reported elsewhere
    d_proc.back().d_args.append(qMakePair(typeRef,name));
    return 0;
}

void MilEmitter::setReturnType(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty()  || d_typeKind == ProcType );
    Q_ASSERT( d_proc.back().d_retType.isEmpty() );
    d_proc.back().d_retType = typeRef;
}

void MilEmitter::setExtern(const QByteArray& origName)
{
    Q_ASSERT( d_proc.back().d_kind == MilProcedure::Extern );
    d_origName = origName;
}

void MilEmitter::setVararg()
{
    Q_ASSERT( !d_proc.isEmpty() || d_typeKind == ProcType );
    d_proc.back().d_isVararg = true;
}

void MilEmitter::add_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_add) );
    delta(-2+1);
}

void MilEmitter::and_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_and) );
    delta(-2+1);
}

void MilEmitter::call_(const QByteArray& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_call,methodRef) );
    delta(-argCount + (hasRet?1:0) );
}

void MilEmitter::calli_(const QByteArray& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_calli,methodRef) );
    delta(-argCount + (hasRet?1:0) );
}

void MilEmitter::case_(const CaseLabelList& l)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_case, QVariant::fromValue(l)) );
    delta(0);
}

void MilEmitter::castptr_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_castptr,typeRef) );
    delta(-1+1);
}

void MilEmitter::ceq_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ceq));
    delta(-2+1);
}

void MilEmitter::cgt_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( withUnsigned )
        d_proc.back().d_body.append(MilOperation(IL_cgt_un) );
    else
        d_proc.back().d_body.append(MilOperation(IL_cgt) );
    delta(-2+1);
}

void MilEmitter::clt_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( withUnsigned )
        d_proc.back().d_body.append(MilOperation(IL_clt_un) );
    else
        d_proc.back().d_body.append(MilOperation(IL_clt) );
    delta(-2+1);
}

void MilEmitter::conv_(MilEmitter::ToType t)
{
    // NOTE that the value on the stack is at least int32 or uint32
    Q_ASSERT( !d_proc.isEmpty() );
    switch( t )
    {
    case ToI1:
        d_proc.back().d_body.append(MilOperation(IL_conv_i1));
        break;
    case ToI2:
        d_proc.back().d_body.append(MilOperation(IL_conv_i2));
        break;
    case ToI4:
        d_proc.back().d_body.append(MilOperation(IL_conv_i4));
        break;
    case ToI8:
        d_proc.back().d_body.append(MilOperation(IL_conv_i8));
        break;
    case ToR4:
        d_proc.back().d_body.append(MilOperation(IL_conv_r4));
        break;
    case ToR8:
        d_proc.back().d_body.append(MilOperation(IL_conv_r8));
        break;
    case ToU1:
        d_proc.back().d_body.append(MilOperation(IL_conv_u1));
        break;
    case ToU2:
        d_proc.back().d_body.append(MilOperation(IL_conv_u2));
        break;
    case ToU4:
        d_proc.back().d_body.append(MilOperation(IL_conv_u4));
        break;
    case ToU8:
        d_proc.back().d_body.append(MilOperation(IL_conv_u8));
        break;
    case ToIp:
        d_proc.back().d_body.append(MilOperation(IL_conv_ip));
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    delta(-1+1);
}

void MilEmitter::div_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( withUnsigned )
        d_proc.back().d_body.append(MilOperation(IL_div_un ) );
    else
        d_proc.back().d_body.append(MilOperation(IL_div ) );
    delta(-2+1);
}

void MilEmitter::disp_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_disp) );
    delta(-1);
}

void MilEmitter::do_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_do) );
    delta(0);
}

void MilEmitter::dup_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_dup ) );
    delta(+1);
}

void MilEmitter::else_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_else) );
    delta(0);
}

void MilEmitter::end_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_end) );
    delta(0);
}

void MilEmitter::exit_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_exit) );
    delta(0);
}

void MilEmitter::goto_(const QByteArray& label)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_goto, label) );
    delta(0);
}

void MilEmitter::if_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_if) );
    delta(0);
}

void MilEmitter::initobj(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_initobj,typeRef));
    delta(0);
}

void MilEmitter::label_(const QByteArray& name)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_label,name) );
    delta(0);
}

void MilEmitter::ldarg_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() );
    switch(arg)
    {
    case 0:
        d_proc.back().d_body.append(MilOperation(IL_ldarg_0) );
        break;
    case 1:
        d_proc.back().d_body.append(MilOperation(IL_ldarg_1) );
        break;
    case 2:
        d_proc.back().d_body.append(MilOperation(IL_ldarg_2) );
        break;
    case 3:
        d_proc.back().d_body.append(MilOperation(IL_ldarg_3) );
        break;
    default:
        if( arg >= 4 && arg <= 255 )
            d_proc.back().d_body.append(MilOperation(IL_ldarg_s,arg) );
        else
            d_proc.back().d_body.append(MilOperation(IL_ldarg,arg) );
    }
    delta(+1);
}

void MilEmitter::ldarga_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( arg <= 255 )
        d_proc.back().d_body.append(MilOperation(IL_ldarga_s,arg) );
    else
        d_proc.back().d_body.append(MilOperation(IL_ldarga,arg) );
    delta(+1);
}

void MilEmitter::ldc_i4(qint32 v)
{
    Q_ASSERT( !d_proc.isEmpty() );
    switch(v)
    {
    case 0:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_0));
        break;
    case 1:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_1));
        break;
    case 2:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_2));
        break;
    case 3:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_3));
        break;
    case 4:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_4));
        break;
    case 5:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_5));
        break;
    case 6:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_6));
        break;
    case 7:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_7));
        break;
    case 8:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_8));
        break;
    case -1:
        d_proc.back().d_body.append(MilOperation(IL_ldc_i4_m1));
        break;
    default:
        if( v >= -128 && v <= 127 )
            d_proc.back().d_body.append(MilOperation(IL_ldc_i4_s,v) );
        else
            d_proc.back().d_body.append(MilOperation(IL_ldc_i4,v) );
    }
    delta(+1);
}

void MilEmitter::ldc_i8(qint64 v)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldc_i8,v) );
    delta(+1);
}

void MilEmitter::ldc_r4(double v)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldc_r4,v) );
    delta(+1);
}

void MilEmitter::ldc_r8(double v)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldc_r8,v) );
    delta(+1);
}

void MilEmitter::ldc_obj(const MilObject& v)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldc_obj,QVariant::fromValue(v) ));
    delta(+1);
}

void MilEmitter::ldelem_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( typeRef == "int8" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_i1));
    else if( typeRef == "int16" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_i2));
    else if( typeRef == "int32" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_i4));
    else if( typeRef == "int64" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_i8));
    else if( typeRef == "float32" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_r4));
    else if( typeRef == "float64" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_r8));
    else if( typeRef == "uint8" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_u1));
    else if( typeRef == "uint16" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_u2));
    else if( typeRef == "uint32" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_u4));
    else if( typeRef == "uint64" )
        d_proc.back().d_body.append(MilOperation(IL_ldelem_u8));
    else
        d_proc.back().d_body.append(MilOperation(IL_ldelem,typeRef));
    delta(-2+1);
}

void MilEmitter::ldelema_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldelema,typeRef));
    delta(-2+1);
}

void MilEmitter::ldfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldfld,fieldRef));
    delta(-1+1);
}

void MilEmitter::ldflda_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldflda,fieldRef));
    delta(-1+1);
}

void MilEmitter::ldproc_(const QByteArray& methodRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldproc,methodRef));
    delta(+1);
}

void MilEmitter::ldind_(MilEmitter::IndType t)
{
    Q_ASSERT( !d_proc.isEmpty() );
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
    default:
        Q_ASSERT( false );
    }
    d_proc.back().d_body.append(MilOperation(i));
    delta(-1+1);
}

void MilEmitter::ldloc_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() );
    switch(loc)
    {
    case 0:
        d_proc.back().d_body.append(MilOperation(IL_ldloc_0) );
        break;
    case 1:
        d_proc.back().d_body.append(MilOperation(IL_ldloc_1) );
        break;
    case 2:
        d_proc.back().d_body.append(MilOperation(IL_ldloc_2) );
        break;
    case 3:
        d_proc.back().d_body.append(MilOperation(IL_ldloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            d_proc.back().d_body.append(MilOperation(IL_ldloc_s,loc) );
        else
            d_proc.back().d_body.append(MilOperation(IL_ldloc,loc) );
    }
    delta(+1);
}

void MilEmitter::ldloca_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( loc <= 255 )
        d_proc.back().d_body.append(MilOperation(IL_ldloca_s,loc));
    else
        d_proc.back().d_body.append(MilOperation(IL_ldloca,loc));
    delta(+1);
}

void MilEmitter::ldnull_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldnull));
    delta(+1);
}

void MilEmitter::ldobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldobj,typeRef));
    delta(-1+1);
}

void MilEmitter::ldvar_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldvar, fieldRef));
    delta(+1);
}

void MilEmitter::ldvara_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ldvara,fieldRef));
    delta(+1);
}

void MilEmitter::ldstr_(const QByteArray& str)
{
    Q_ASSERT( !d_proc.isEmpty() );
    Q_ASSERT( !str.isEmpty() &&
              ( str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'') ||
                str.startsWith('#') && str.endsWith('#') ) );
    if( str.startsWith('#') )
        Q_ASSERT(QByteArray::fromHex(str.mid(1,str.size()-2)).endsWith(char(0)));
    d_proc.back().d_body.append(MilOperation(IL_ldstr,str));
    delta(+1);
}

void MilEmitter::line_(quint32 l)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_line,l) );
    delta(0);
}

void MilEmitter::loop_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_loop) );
    delta(0);
}

void MilEmitter::mul_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_mul ) );
    delta(-2+1);
}

void MilEmitter::neg_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_neg ) );
    delta(-1+1);
}

void MilEmitter::newarr_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_newarr,typeRef));
    delta(-1+1);
}

void MilEmitter::newvla_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_newvla,typeRef) );
    delta(0);
}

void MilEmitter::newobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_newobj,typeRef));
    delta(-0+1);
}

void MilEmitter::not_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_not));
    delta(-1+1);
}

void MilEmitter::or_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_or));
    delta(-2+1);
}

void MilEmitter::pop_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_pop));
    delta(-1);
}

void MilEmitter::rem_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( withUnsigned )
        d_proc.back().d_body.append(MilOperation(IL_rem_un ) );
    else
        d_proc.back().d_body.append(MilOperation(IL_rem ) );
    delta(-2+1);
}

void MilEmitter::repeat_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_repeat) );
    delta(0);
}

void MilEmitter::ret_(bool hasRet)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_ret));
    delta( hasRet ? -1: 0 );
}

void MilEmitter::shl_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_shl));
    delta(-2+1);
}

void MilEmitter::shr_(bool withUnsigned)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( withUnsigned )
        d_proc.back().d_body.append(MilOperation(IL_shr_un ) );
    else
        d_proc.back().d_body.append(MilOperation(IL_shr ) );
    delta(-2+1);
}

void MilEmitter::sizeof_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_sizeof,typeRef ) );
    delta(+1);
}

void MilEmitter::starg_(quint16 arg)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( arg <= 255 )
        d_proc.back().d_body.append(MilOperation(IL_starg_s,arg ) );
    else
        d_proc.back().d_body.append(MilOperation(IL_starg,arg ) );
    delta(-1);
}

void MilEmitter::stelem_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    if( typeRef == "int8" )
        d_proc.back().d_body.append(MilOperation(IL_stelem_i1));
    else if( typeRef == "int16" )
        d_proc.back().d_body.append(MilOperation(IL_stelem_i2));
    else if( typeRef == "int32" )
        d_proc.back().d_body.append(MilOperation(IL_stelem_i4));
    else if( typeRef == "int64" )
        d_proc.back().d_body.append(MilOperation(IL_stelem_i8));
    else if( typeRef == "float32" )
        d_proc.back().d_body.append(MilOperation(IL_stelem_r4));
    else if( typeRef == "float64" )
        d_proc.back().d_body.append(MilOperation(IL_stelem_r8));
    else
        d_proc.back().d_body.append(MilOperation(IL_stelem,typeRef));
    delta(-3);
}

void MilEmitter::stfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_stfld,fieldRef ) );
    delta(-2);
}

void MilEmitter::stind_(MilEmitter::IndType t)
{
    Q_ASSERT( !d_proc.isEmpty() );
    IL_op i = IL_invalid;
    switch( t )
    {
    case I1:
        i = IL_stind_i1;
        break;
    case I2:
        i = IL_stind_i2;
        break;
    case I4:
        i = IL_stind_i4;
        break;
    case I8:
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
    default:
        Q_ASSERT( false );
    }
    d_proc.back().d_body.append(MilOperation(i));
    delta(-2);
}

void MilEmitter::stloc_(quint16 loc)
{
    Q_ASSERT( !d_proc.isEmpty() );
    switch(loc)
    {
    case 0:
        d_proc.back().d_body.append(MilOperation(IL_stloc_0) );
        break;
    case 1:
        d_proc.back().d_body.append(MilOperation(IL_stloc_1) );
        break;
    case 2:
        d_proc.back().d_body.append(MilOperation(IL_stloc_2) );
        break;
    case 3:
        d_proc.back().d_body.append(MilOperation(IL_stloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            d_proc.back().d_body.append(MilOperation(IL_stloc_s,loc) );
        else
            d_proc.back().d_body.append(MilOperation(IL_stloc,loc) );
    }
    delta(-1);
}

void MilEmitter::stobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_stobj,typeRef) );
    delta(-2);
}

void MilEmitter::stvar_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_stvar,fieldRef) );
    delta(-1);
}

void MilEmitter::sub_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_sub ) );
    delta(-2+1);
}

void MilEmitter::switch_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_switch) );
    delta(0);
}

void MilEmitter::then_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_then) );
    delta(0);
}

void MilEmitter::until_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_until) );
    delta(0);
}

void MilEmitter::while_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_while) );
    delta(0);
}

void MilEmitter::xor_()
{
    Q_ASSERT( !d_proc.isEmpty() );
    d_proc.back().d_body.append(MilOperation(IL_xor) );
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

void IlAsmRenderer::addImport(const QByteArray& path, const QByteArray& name)
{
    out << ws() << "import ";
    if( !name.isEmpty() )
        out << name << " := " << path;
    else
        out << path;
    out << endl;
}

void IlAsmRenderer::addVariable(const QByteArray& typeRef, QByteArray name, bool isPublic)
{
    out << ws() << "var " << name << ": " << typeRef << endl;
}

void IlAsmRenderer::addProcedure(const MilProcedure& m)
{
    if( m.d_kind == MilProcedure::Invalid )
        return;
    render(m);
}

void IlAsmRenderer::beginType(const QByteArray& className, bool isPublic, quint8 classKind)
{
    state = Struct;
    out << ws() << "type " << className;
    if( isPublic )
        out << "*";

    out << " = ";
    if( classKind == MilEmitter::Union )
        out << "union ";
    else
        out << "struct ";

    out << endl;
    level++;
}

void IlAsmRenderer::endType()
{
    level--;
    out << ws() << "end" << endl;
    state = Module;
}

void IlAsmRenderer::addType(const QByteArray& name, bool isPublic, const QByteArray& baseType, quint8 typeKind, quint32 len)
{
    out << ws() << "type " << name;
    if( isPublic )
        out << "*";

    out << " = ";
    switch(typeKind)
    {
    case MilEmitter::Alias:
        out << baseType << endl;
        break;
    case MilEmitter::Pointer:
        out << "pointer to " << baseType << endl;
        break;
    case MilEmitter::Array:
        out << "array ";
        if( len )
            out << len << " ";
        out << "of " << baseType << endl;
        break;
    }
}

void IlAsmRenderer::addField(const QByteArray& fieldName, const QByteArray& typeRef, bool isPublic)
{
    out << ws();
    if( state == Module )
        out << "var ";
    out << fieldName;
    if( isPublic )
        out << "*";

    out << ": " << typeRef << endl;
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
    case QVariant::Map:
        {
            out << "{";
            QVariantMap m = data.toMap();
            QVariantMap::const_iterator i;
            for( i = m.begin(); i != m.end(); ++i )
            {
                out << i.key() << "=";
                renderComponents(out,i.value());
            }
            out << "}";
        }
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

    if( m.d_kind == MilProcedure::ProcType || m.d_kind == MilProcedure::Extern )
    {
        out << ws() << "type " << m.d_name;
        if( m.d_isPublic )
            out << "*";
        out << " = proc";
    }else
    {
        out << ws() << "procedure " << m.d_name;
        state = Proc;
        if( m.d_isPublic )
            out << "* ";
    }

    out << "(";

    for( int i = 0; i < m.d_args.size(); i++ )
    {
        if( i != 0 )
            out << "; ";
        if( !m.d_args[i].first.isEmpty() )
            out << m.d_args[i].second;
        else
            out << i;
        out << ": "<< m.d_args[i].first;
    }
    if( m.d_isVararg )
        out << ", .. ";

    out << ")";
    if( !m.d_retType.isEmpty() )
        out << ":" << m.d_retType;

    switch( m.d_kind)
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

    if(m.d_kind == MilProcedure::ProcType || m.d_kind == MilProcedure::Extern )
        return;

    if( !m.d_locals.isEmpty() )
    {
        out << ws() << "var ";
        level++;
        for( int i = 0; i < m.d_locals.size(); i++ )
        {
            if( !m.d_locals[i].second.isEmpty() )
                out << m.d_locals[i].second;
            else
                out << i;
            out << ": " << m.d_locals[i].first << "; ";
        }
        out << endl;
        level--;
    }
    out << ws() << "begin" << endl;

    level++;
    for( int i = 0; i < m.d_body.size(); i++ )
    {
        const MilOperation& op = m.d_body[i];
        switch( op.d_ilop )
        {
        case IL_invalid:
            break;
        case IL_if:
        case IL_loop:
        case IL_repeat:
        case IL_switch:
        case IL_while:
            out << ws() << s_opName[op.d_ilop] << endl;
            level++;
            break;
        case IL_then:
        case IL_else:
        case IL_until:
        case IL_do:
            level--;
            out << ws() << s_opName[op.d_ilop] << endl;
            level++;
            break;
        case IL_end:
            level--;
            out << ws() << s_opName[op.d_ilop] << endl;
            break;
        case IL_case:
            level--;
            out << ws() << s_opName[op.d_ilop] << endl;
            level++;
            out << ws();
            foreach(qint64 i, op.d_arg.value<CaseLabelList>() )
                out << i << " ";
            out << endl;
            break;
        case IL_ldc_obj:
            {
                const MilObject obj = op.d_arg.value<MilObject>();
                out << ws() << s_opName[op.d_ilop] << " ";
                Q_ASSERT( obj.data.type() == QVariant::ByteArray ||
                          obj.data.type() == QVariant::List ||
                          obj.data.type() == QVariant::Map );
                if( obj.data.type() != QVariant::ByteArray )
                    out << obj.typeRef;
                renderComponents(out,obj.data);
                out << endl;
            }
            break;
        default:
            out << ws() << s_opName[op.d_ilop];
            if( !op.d_arg.isNull() )
                out << " " << op.d_arg.toByteArray();
            out << endl;
            break;
        }
    }
    level--;
    out << ws() << "end " << m.d_name << endl;

    state = old;
}

void IlAsmRenderer::addConst(const QByteArray& typeRef, const QByteArray& name, const QVariant& val)
{
    out << ws() << "const " << name;
    if( !typeRef.isEmpty() )
        out << " : " << typeRef;
    if( !val.isNull() )
    {
        out << " = ";
        if( val.canConvert<MilObject>())
        {
            const MilObject obj = val.value<MilObject>();
            if( obj.data.type() != QVariant::ByteArray )
                out << obj.typeRef;
            renderComponents(out,obj.data);
        }else
            renderComponents(out, val );
        out << endl;
    }
}
