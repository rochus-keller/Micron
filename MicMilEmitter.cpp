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

MilEmitter::MilEmitter(MilRenderer* r):d_out(r)
{
    Q_ASSERT( r );
}

void MilEmitter::beginModule(const QByteArray& moduleName, const QString& sourceFile)
{
    Q_ASSERT( !moduleName.isEmpty() );
    d_out->beginModule(moduleName,sourceFile);
}

void MilEmitter::endModule()
{
    d_out->endModule();
}

void MilEmitter::addImport(const QByteArray& path, const QByteArray& name)
{
    Q_ASSERT( d_method.isEmpty() );
    d_out->addImport(path, name);
}

void MilEmitter::beginProc(const QByteArray& methodName, bool isPublic, quint8 kind)
{
    Q_ASSERT( d_method.isEmpty() );
    Q_ASSERT(!methodName.isEmpty());
    d_method = methodName;
    d_isPublic = isPublic;
    d_procKind = kind;
    d_body.clear();
    d_args.clear();
    d_locals.clear();
    d_stackDepth = 0;
    d_maxStackDepth = 0;
    d_retType.clear();
    d_library.clear();
    d_origName.clear();
    d_isVararg = false;
}

void MilEmitter::endProc()
{
    MilProcedure meth;
    meth.d_args = d_args;
    meth.d_body = d_body;
    meth.d_isPublic = d_isPublic;
    meth.d_isVararg = d_isVararg;
    meth.d_locals = d_locals;
    meth.d_name = d_method;
    meth.d_retType = d_retType;
    meth.d_library = d_library;
    meth.d_origName = d_origName;
    meth.d_stackDepth = d_maxStackDepth;
    meth.d_kind = d_procKind;
    d_out->addProcedure(meth);
    d_method.clear();
    d_body.clear();
    d_retType.clear();
    d_args.clear();
    d_locals.clear();
    d_library.clear();
    d_origName.clear();
}

void MilEmitter::beginStruct(const QByteArray& className, bool isPublic, quint8 classKind)
{
    d_out->beginStruct(className,isPublic, classKind);
}

void MilEmitter::endStruct()
{
    d_out->endStruct();
}

void MilEmitter::addField(const QByteArray& fieldName, const QByteArray& typeRef, bool isPublic)
{
    d_out->addField(fieldName, typeRef, isPublic );
}

quint32 MilEmitter::addLocal(const QByteArray& typeRef, QByteArray name)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( !typeRef.isEmpty() );
    d_locals.append(qMakePair(typeRef,name));
    return 0;
}

quint32 MilEmitter::addArgument(const QByteArray& typeRef, QByteArray name)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( !typeRef.isEmpty() );
    d_args.append(qMakePair(typeRef,name));
    return 0;
}

void MilEmitter::setReturnType(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( d_retType.isEmpty() );
    d_retType = typeRef;
}

void MilEmitter::setPinvoke(const QByteArray& lib, const QByteArray& origName)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( d_library.isEmpty() );
    d_library = lib;
    d_origName = origName;
}

void MilEmitter::setVararg()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_isVararg = true;
}

void MilEmitter::add_()
{
    Q_ASSERT( !d_method.isEmpty() );
        d_body.append(MilOperation(IL_add) );
    delta(-2+1);
}

void MilEmitter::and_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_and) );
    delta(-2+1);
}

void MilEmitter::call_(const QByteArray& methodRef, int argCount, bool hasRet, bool isInstance)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_call,methodRef,isInstance) );
    delta(-argCount + (hasRet?1:0) );
}

void MilEmitter::calli_(const QByteArray& methodRef, int argCount, bool hasRet)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_calli,methodRef) );
    delta(-argCount + (hasRet?1:0) );
}

void MilEmitter::case_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_case) );
    delta(0);
}

void MilEmitter::castptr_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_castptr,typeRef) );
    delta(-1+1);
}

void MilEmitter::ceq_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ceq));
    delta(-2+1);
}

void MilEmitter::cgt_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(MilOperation(IL_cgt_un) );
    else
        d_body.append(MilOperation(IL_cgt) );
    delta(-2+1);
}

void MilEmitter::clt_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(MilOperation(IL_clt_un) );
    else
        d_body.append(MilOperation(IL_clt) );
    delta(-2+1);
}

void MilEmitter::conv_(MilEmitter::ToType t)
{
    // NOTE that the value on the stack is at least int32 or uint32
    Q_ASSERT( !d_method.isEmpty() );
    switch( t )
    {
    case ToI1:
            d_body.append(MilOperation(IL_conv_i1));
        break;
    case ToI2:
            d_body.append(MilOperation(IL_conv_i2));
        break;
    case ToI4:
            d_body.append(MilOperation(IL_conv_i4));
        break;
    case ToI8:
            d_body.append(MilOperation(IL_conv_i8));
        break;
    case ToR4:
        d_body.append(MilOperation(IL_conv_r4));
        break;
    case ToR8:
        d_body.append(MilOperation(IL_conv_r8));
        break;
    case ToU1:
            d_body.append(MilOperation(IL_conv_u1));
        break;
    case ToU2:
            d_body.append(MilOperation(IL_conv_u2));
        break;
    case ToU4:
            d_body.append(MilOperation(IL_conv_u4));
        break;
    case ToU8:
            d_body.append(MilOperation(IL_conv_u8));
        break;
    case ToIp:
            d_body.append(MilOperation(IL_conv_ip));
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    delta(-1+1);
}

void MilEmitter::div_()
{
    Q_ASSERT( !d_method.isEmpty() );
        d_body.append(MilOperation(IL_div ) );
        delta(-2+1);
}

void MilEmitter::disp_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_disp) );
    delta(-1);
}

void MilEmitter::do_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_do) );
    delta(0);
}

void MilEmitter::dup_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_dup ) );
    delta(+1);
}

void MilEmitter::else_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_else) );
    delta(0);
}

void MilEmitter::end_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_end) );
    delta(0);
}

void MilEmitter::exit_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_exit) );
    delta(0);
}

void MilEmitter::goto_(const QByteArray& label)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_goto, label) );
    delta(0);
}

void MilEmitter::if_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_if) );
    delta(0);
}

void MilEmitter::label_(const QByteArray& name)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_label,name) );
    delta(0);
}

void MilEmitter::ldarg_(quint16 arg)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(arg)
    {
    case 0:
        d_body.append(MilOperation(IL_ldarg_0) );
        break;
    case 1:
        d_body.append(MilOperation(IL_ldarg_1) );
        break;
    case 2:
        d_body.append(MilOperation(IL_ldarg_2) );
        break;
    case 3:
        d_body.append(MilOperation(IL_ldarg_3) );
        break;
    default:
        if( arg >= 4 && arg <= 255 )
            d_body.append(MilOperation(IL_ldarg_s,arg) );
        else
            d_body.append(MilOperation(IL_ldarg,arg) );
   }
    delta(+1);
}

void MilEmitter::ldarga_(quint16 arg)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( arg <= 255 )
        d_body.append(MilOperation(IL_ldarga_s,arg) );
    else
        d_body.append(MilOperation(IL_ldarga,arg) );
    delta(+1);
}

void MilEmitter::ldc_i4(qint32 v)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(v)
    {
    case 0:
        d_body.append(MilOperation(IL_ldc_i4_0));
        break;
    case 1:
        d_body.append(MilOperation(IL_ldc_i4_1));
        break;
    case 2:
        d_body.append(MilOperation(IL_ldc_i4_2));
        break;
    case 3:
        d_body.append(MilOperation(IL_ldc_i4_3));
        break;
    case 4:
        d_body.append(MilOperation(IL_ldc_i4_4));
        break;
    case 5:
        d_body.append(MilOperation(IL_ldc_i4_5));
        break;
    case 6:
        d_body.append(MilOperation(IL_ldc_i4_6));
        break;
    case 7:
        d_body.append(MilOperation(IL_ldc_i4_7));
        break;
    case 8:
        d_body.append(MilOperation(IL_ldc_i4_8));
        break;
    case -1:
        d_body.append(MilOperation(IL_ldc_i4_m1));
        break;
    default:
        if( v >= -128 && v <= 127 )
            d_body.append(MilOperation(IL_ldc_i4_s,v) );
        else
            d_body.append(MilOperation(IL_ldc_i4,QByteArray::number(v)) );
    }
    delta(+1);
}

void MilEmitter::ldc_i8(qint64 v)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldc_i8,QByteArray::number(v)) );
    delta(+1);
}

void MilEmitter::ldc_r4(double v)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldc_r4,QByteArray::number(v,'g',9)) );
    delta(+1);
}

void MilEmitter::ldc_r8(double v)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldc_r8,QByteArray::number(v,'g',17)) );
    delta(+1);
}

void MilEmitter::ldelem_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( typeRef == "int8" )
        d_body.append(MilOperation(IL_ldelem_i1));
    else if( typeRef == "int16" )
        d_body.append(MilOperation(IL_ldelem_i2));
    else if( typeRef == "int32" )
        d_body.append(MilOperation(IL_ldelem_i4));
    else if( typeRef == "int64" )
        d_body.append(MilOperation(IL_ldelem_i8));
    else if( typeRef == "float32" )
        d_body.append(MilOperation(IL_ldelem_r4));
    else if( typeRef == "float64" )
        d_body.append(MilOperation(IL_ldelem_r8));
    else if( typeRef == "uint8" )
        d_body.append(MilOperation(IL_ldelem_u1));
    else if( typeRef == "uint16" )
        d_body.append(MilOperation(IL_ldelem_u2));
    else if( typeRef == "uint32" )
        d_body.append(MilOperation(IL_ldelem_u4));
    else if( typeRef == "uint64" )
        d_body.append(MilOperation(IL_ldelem_u8));
    else
        d_body.append(MilOperation(IL_ldelem,typeRef));
    delta(-2+1);
}

void MilEmitter::ldelema_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldelema,typeRef));
    delta(-2+1);
}

void MilEmitter::ldfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldfld,fieldRef));
    delta(-1+1);
}

void MilEmitter::ldflda_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldflda,fieldRef));
    delta(-1+1);
}

void MilEmitter::ldproc_(const QByteArray& methodRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldftn,methodRef));
    delta(+1);
}

void MilEmitter::ldind_(MilEmitter::IndType t)
{
    Q_ASSERT( !d_method.isEmpty() );
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
    d_body.append(MilOperation(i));
    delta(-1+1);
}

void MilEmitter::ldloc_(quint16 loc)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(loc)
    {
    case 0:
        d_body.append(MilOperation(IL_ldloc_0) );
        break;
    case 1:
        d_body.append(MilOperation(IL_ldloc_1) );
        break;
    case 2:
        d_body.append(MilOperation(IL_ldloc_2) );
        break;
    case 3:
        d_body.append(MilOperation(IL_ldloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            d_body.append(MilOperation(IL_ldloc_s,loc) );
        else
            d_body.append(MilOperation(IL_ldloc,loc) );
   }
    delta(+1);
}

void MilEmitter::ldloca_(quint16 loc)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( loc <= 255 )
        d_body.append(MilOperation(IL_ldloca_s,loc));
    else
        d_body.append(MilOperation(IL_ldloca,loc));
    delta(+1);
}

void MilEmitter::ldnull_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldnull));
    delta(+1);
}

void MilEmitter::ldobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldobj,typeRef));
    delta(-1+1);
}

void MilEmitter::ldvar_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldvar, fieldRef));
    delta(+1);
}

void MilEmitter::ldvara_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ldvara,fieldRef));
    delta(+1);
}

void MilEmitter::ldstr_(const QByteArray& utf8)
{
    Q_ASSERT( !d_method.isEmpty() );
    Q_ASSERT( !utf8.isEmpty() && utf8.startsWith('"') && utf8.endsWith('"') );
    // expecting a string in "" and properly escaped
    d_body.append(MilOperation(IL_ldstr,utf8));
    delta(+1);
}

void MilEmitter::line_(quint32 l)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_line,l) );
    delta(0);
}

void MilEmitter::loop_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_loop) );
    delta(0);
}

void MilEmitter::mul_()
{
    Q_ASSERT( !d_method.isEmpty() );
        d_body.append(MilOperation(IL_mul ) );
    delta(-2+1);
}

void MilEmitter::neg_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_neg ) );
    delta(-1+1);
}

void MilEmitter::newarr_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_newarr,typeRef));
    delta(-1+1);
}

void MilEmitter::newvla_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_newvla,typeRef) );
    delta(0);
}

void MilEmitter::newobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_newobj,typeRef));
    delta(-0+1);
}

void MilEmitter::not_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_not));
    delta(-1+1);
}

void MilEmitter::or_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_or));
    delta(-2+1);
}

void MilEmitter::pop_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_pop));
    delta(-1);
}

void MilEmitter::rem_()
{
    Q_ASSERT( !d_method.isEmpty() );
        d_body.append(MilOperation(IL_rem ) );
        delta(-2+1);
}

void MilEmitter::repeat_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_repeat) );
    delta(0);
}

void MilEmitter::ret_(bool hasRet)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_ret));
    delta( hasRet ? -1: 0 );
}

void MilEmitter::shl_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_shl));
    delta(-2+1);
}

void MilEmitter::shr_(bool withUnsigned)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( withUnsigned )
        d_body.append(MilOperation(IL_shr_un ) );
    else
        d_body.append(MilOperation(IL_shr ) );
    delta(-2+1);
}

void MilEmitter::starg_(quint16 arg)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( arg <= 255 )
        d_body.append(MilOperation(IL_starg_s,arg ) );
    else
        d_body.append(MilOperation(IL_starg,arg ) );
    delta(-1);
}

void MilEmitter::stelem_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    if( typeRef == "int8" )
        d_body.append(MilOperation(IL_stelem_i1));
    else if( typeRef == "int16" )
        d_body.append(MilOperation(IL_stelem_i2));
    else if( typeRef == "int32" )
        d_body.append(MilOperation(IL_stelem_i4));
    else if( typeRef == "int64" )
        d_body.append(MilOperation(IL_stelem_i8));
    else if( typeRef == "float32" )
        d_body.append(MilOperation(IL_stelem_r4));
    else if( typeRef == "float64" )
        d_body.append(MilOperation(IL_stelem_r8));
    else
        d_body.append(MilOperation(IL_stelem,typeRef));
    delta(-3);
}

void MilEmitter::stfld_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_stfld,fieldRef ) );
    delta(-2);
}

void MilEmitter::stind_(MilEmitter::IndType t)
{
    Q_ASSERT( !d_method.isEmpty() );
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
    d_body.append(MilOperation(i));
    delta(-2);
}

void MilEmitter::stloc_(quint16 loc)
{
    Q_ASSERT( !d_method.isEmpty() );
    switch(loc)
    {
    case 0:
        d_body.append(MilOperation(IL_stloc_0) );
        break;
    case 1:
        d_body.append(MilOperation(IL_stloc_1) );
        break;
    case 2:
        d_body.append(MilOperation(IL_stloc_2) );
        break;
    case 3:
        d_body.append(MilOperation(IL_stloc_3) );
        break;
    default:
        if( loc >= 4 && loc <= 255 )
            d_body.append(MilOperation(IL_stloc_s,loc) );
        else
            d_body.append(MilOperation(IL_stloc,loc) );
   }
    delta(-1);
}

void MilEmitter::stobj_(const QByteArray& typeRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_stobj,typeRef) );
    delta(-2);
}

void MilEmitter::stvar_(const QByteArray& fieldRef)
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_stvar,fieldRef) );
    delta(-1);
}

void MilEmitter::sub_()
{
    Q_ASSERT( !d_method.isEmpty() );
        d_body.append(MilOperation(IL_sub ) );
    delta(-2+1);
}

void MilEmitter::switch_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_switch) );
    delta(0);
}

void MilEmitter::then_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_then) );
    delta(0);
}

void MilEmitter::until_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_until) );
    delta(0);
}

void MilEmitter::while_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_while) );
    delta(0);
}

void MilEmitter::xor_()
{
    Q_ASSERT( !d_method.isEmpty() );
    d_body.append(MilOperation(IL_xor) );
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

void IlAsmRenderer::beginModule(const QByteArray& moduleName, const QString& sourceFile)
{
    source = sourceFile;
    d_moduleName = moduleName;
    sourceRendered = false;
    out << "// Generated by " << qApp->applicationName() << " " << qApp->applicationVersion() << " on "
           << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;

    out << "module " << moduleName;
    out << endl << endl;

    state = Module;
    level++;
}

void IlAsmRenderer::endModule()
{
    if( d_begin.d_kind == MilProcedure::ModuleInit )
        render(d_begin);
    level--;
    out << ws() << "end " << d_moduleName << endl;
    state = Idle;
}

void IlAsmRenderer::addImport(const QByteArray& path, const QByteArray& name)
{

}

void IlAsmRenderer::addProcedure(const MilProcedure& m)
{
    if( m.d_kind == MilProcedure::Invalid )
        return;
    if( m.d_kind == MilProcedure::ModuleInit )
        d_begin = m;
    else
        render(m);

}

void IlAsmRenderer::beginStruct(const QByteArray& className, bool isPublic, quint8 classKind)
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

void IlAsmRenderer::endStruct()
{
    level--;
    out << ws() << "end" << endl;
    state = Module;
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

void IlAsmRenderer::render(const MilProcedure& m)
{
    out << ws() << "procedure " << m.d_name;
    State old = state;
    state = Proc;
    if( m.d_isPublic )
        out << "* ";

    if( m.d_retType.isEmpty() )
        out << "void";
    else

    out << "(";

    for( int i = 0; i < m.d_args.size(); i++ )
    {
        if( i != 0 )
            out << "; ";
        if( !m.d_args[i].second.isEmpty() )
            out << m.d_args[i].second;
        else
            out << i;
        out << ": "<< m.d_args[i].first;
    }
    if( m.d_isVararg )
        out << ", .. ";

    out << ") ";
    if( m.d_retType.isEmpty() )
        out << ": " << m.d_retType;

    out << endl;

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
        default:
            out << ws() << s_opName[op.d_ilop];
            if( !op.d_arg.isEmpty() )
                out << " " << op.d_arg; // TODO better indentation for control structures
            out << endl;
            break;
        }
    }
    level--;
    out << ws() << "end " << m.d_name << endl;

    state = old;
}
