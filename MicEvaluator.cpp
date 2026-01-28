/*
** Copyright (C) 2024 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the Micron language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include "MicEvaluator.h"
#include "MilEmitter.h"
#include "MicBuiltins.h"
#include "MicToken.h"
#include <bitset>
#include <QtDebug>
using namespace Mic;

bool Evaluator::evaluate(Expression* e, bool assureOnMilStack)
{
    err.clear();
    if( e == 0 )
        return true; // ok, error reported earlier
    if( !recursiveRun(e) )
        return false;
    if(assureOnMilStack)
        assureTopOnMilStack(false, e->pos);
    return err.isEmpty();
}

bool Evaluator::unaryOp(quint8 op, const RowCol& pos)
{
    err.clear();
    if( stack.isEmpty() )
        return error("nothing on the stack",pos);

    Value& v = stack.back();
    switch( op )
    {
    case Expression::Not:
        notOp(v, pos);
        break;
    case Expression::Plus:
        unaryPlusOp(v, pos);
        break;
    case Expression::Minus:
        unaryMinusOp(v, pos);
        break;
    default:
        Q_ASSERT(false);
        // NOTE: @ is directly handled in the parser
        break;
    }
    return err.isEmpty();
}

Type*Evaluator::smallestUIntType(const QVariant& v) const
{
    const quint64 u = v.toULongLong();
    if( u <= Type::getMax(Type::UINT8).toULongLong() )
        return mdl->getType(Type::UINT8);
    else if( u <= Type::getMax(Type::UINT16).toULongLong() )
        return mdl->getType(Type::UINT16);
    else if( u <= Type::getMax(Type::UINT32).toULongLong() )
        return mdl->getType(Type::UINT32);
    else
        return mdl->getType(Type::UINT64);
}

Type*Evaluator::smallestIntType(const QVariant& v) const
{
    const qint64 i = v.toLongLong();
    if( i >= Type::getMin(Type::INT8).toLongLong() && i <= Type::getMax(Type::INT8).toLongLong() )
        return mdl->getType(Type::INT8);
    else if( i >= Type::getMin(Type::INT16).toLongLong() && i <= Type::getMax(Type::INT16).toLongLong() )
        return mdl->getType(Type::INT16);
    else if( i >= Type::getMin(Type::INT32).toLongLong() && i <= Type::getMax(Type::INT32).toLongLong() )
        return mdl->getType(Type::INT32);
    else
        return mdl->getType(Type::INT64);
}

bool Evaluator::bindUniInt(Expression * e, bool isSigned) const
{
    if( e && e->hasConstValue() && e->getType()->kind == Type::UniInt )
    {
        e->setType(isSigned ? smallestIntType(e->val) : smallestUIntType(e->val));
        return true;
    }else
        return false;
}

bool Evaluator::binaryOp(quint8 op, const RowCol& pos)
{
    err.clear();
    if( stack.size() < 2 )
    {
        err = "expecting two values on the stack";
        return false;
    }
    Value rhs = stack.takeLast();
    Value lhs = stack.takeLast();

    switch( op )
    {
    // Arith
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::Add:
    case Expression::Sub:
        stack.push_back(arithOp(op,lhs,rhs, pos));
        break;
    // Logic
    case Expression::And:
    case Expression::Or:
        stack.push_back(logicOp(op,lhs,rhs, pos));
        break;
    // Relation
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
        stack.push_back(relationOp(op, lhs,rhs, pos));
        break;
    case Expression::In:
        stack.push_back(inOp(lhs,rhs, pos));
        break;
    case Expression::Is:
        stack.push_back(isOp(lhs,rhs, pos));
        break;
    default:
        Q_ASSERT(false);
    }
    return err.isEmpty();
}

bool Evaluator::prepareRhs(Type* lhs, bool assig, const RowCol& pos)
{
    err.clear();
    if( stack.size() < 1 )
    {
        return error("expecting a value on the stack",pos);
    }

    const Value& rhs = stack.back();

    if( rhs.mode == Value::TypeDecl )
    {
        return error("a type declaration cannot be used as a value",pos);
    }

    // make sure also a string literal is put on the stack by value
    if( !assig && lhs && lhs->kind == Type::Array && lhs->len && lhs->getType()->kind == Type::CHAR &&
            (rhs.type->kind == Type::StrLit ||
             rhs.ref && rhs.type->kind == Type::Array && rhs.type->len == 0 &&
                rhs.type->getType()->kind == Type::CHAR))
    {
        // Tv is a non-open array of CHAR, Te is a string literal, or an open array of CHAR;
        assureTopOnMilStack(false, pos);
        out->line_(pos);
        out->ldind_(toQuali(lhs));
    }else if( lhs && lhs->kind == Type::Array && lhs->len &&
              rhs.type->kind == Type::Array && rhs.type->len > 0 && rhs.ref )
    {
        // copy rhs non-open array ref by value to stack to copy to non-open lhs array
        Q_ASSERT( lhs->len == rhs.type->len );
        assureTopOnMilStack(false, pos);
        out->line_(pos);
        out->ldind_(toQuali(lhs));
    }else if( lhs && lhs->kind == Type::CHAR && rhs.type->kind == Type::StrLit )
    {
        out->line_(pos);
        out->ldc_i4(quint8(dequote(rhs.val.toByteArray())[0]));
    }else if( lhs && lhs->kind == Type::Proc && rhs.mode == Value::Procedure )
    {
        Declaration* proc = rhs.val.value<Declaration*>();
        Q_ASSERT(proc);
        if( proc->inline_ )
        {
            return error("cannot take address of INLINE procedure", pos);
        }
        out->line_(pos);
        out->ldproc_(toQuali(proc));
    }else if( lhs && lhs->kind == Type::Proc && (rhs.mode == Value::Method || rhs.mode == Value::IntfMeth) )
    {
        Declaration* proc = rhs.val.value<Declaration*>();
        Q_ASSERT(proc);
        if( proc->inline_ )
        {
            return error("cannot take address of INLINE procedure", pos);
        }
        if( !lhs->typebound )
        {
            return error( "cannot assign method to procedure type", pos);
        }
        const Mil::Trident trident = qMakePair(toQuali(proc->outer),proc->name);
        out->line_(pos);
        out->ldmeth_(trident);
    }else if( lhs && lhs->kind == Type::Interface && rhs.type && rhs.type->kind != Type::Interface )
    {
        if( rhs.type->kind != Type::Pointer || rhs.type->getType() == 0
                || !(rhs.type->getType()->kind == Type::Object || rhs.type->getType()->kind == Type::Record) )
        {
            return error("expecting pointer to object or record",pos);
        }
        out->line_(pos);
        out->ldiface_(toQuali(lhs));
    }else
    {
        Type* t = rhs.type;
        assureTopOnMilStack(false, pos); // modifies stack.back() i.e. rhs
        adjustNumType(t,lhs);
    }

    return true;
}

static inline Mil::Quali coreName(const QByteArray& proc)
{
    Mil::Quali res;
    res.first = Token::getSymbol("MIC$");
    res.second = Token::getSymbol(proc);
    return res;
}

bool Evaluator::assign(const RowCol& pos)
{
    err.clear();
    if( stack.size() < 2 )
    {
        err = "expecting two values on the stack";
        return false;
    }

    if( !prepareRhs( stack[stack.size()-2].type, true, pos ) )
        return false;

    Value rhs = stack.takeLast();
    Value lhs = stack.takeLast();

    if( !lhs.ref )
        return false; // error reported elsewhere

    out->line_(pos);

    if( lhs.type->kind == Type::Array )
    {
        if( lhs.type->getType()->kind == Type::CHAR && lhs.type->len != 0 )
        {
            // special case: lhs is non-open char array and rhs is either open char array or string literal
            if( rhs.type->kind == Type::Array && rhs.type->getType()->kind == Type::CHAR && rhs.type->len == 0 )
            {
                Q_ASSERT(rhs.ref);
                out->strcpy_();
                return err.isEmpty();
            }else if( rhs.type->kind == Type::StrLit )
            {
                out->strcpy_();
                return err.isEmpty();
            }
        } // else copy memory block TODO
    }

    switch(lhs.type->kind)
    {
    case Type::BOOL:
    case Type::CHAR:
    case Type::UINT8:
    case Type::INT8:
        out->stind_(Mil::EmiTypes::I1);
        break;
    case Type::UINT16:
    case Type::INT16:
        out->stind_(Mil::EmiTypes::I2);
        break;
    case Type::UINT32:
    case Type::INT32:
    case Type::SET:
    case Type::ConstEnum:
        out->stind_(Mil::EmiTypes::I4);
        break;
    case Type::UINT64:
    case Type::INT64:
        out->stind_(Mil::EmiTypes::I8);
        break;
    case Type::FLT32:
        out->stind_(Mil::EmiTypes::R4);
        break;
    case Type::FLT64:
        out->stind_(Mil::EmiTypes::R8);
        break;
    case Type::Nil:
    case Type::StrLit:
    case Type::Pointer:
        out->stind_(Mil::EmiTypes::IntPtr);
        break;
    case Type::Proc:
        if( lhs.type->typebound )
            out->stind_(Mil::EmiTypes::IPP);
        else
            out->stind_(Mil::EmiTypes::IntPtr);
        break;
    case Type::Record:
    case Type::Object:
    case Type::Array:
    case Type::Generic:
        out->stind_(toQuali(lhs.type));
        break;
    default:
        Q_ASSERT( false );
    }

    return err.isEmpty();
}

bool Evaluator::assign(Expression* lhs, Expression* rhs, const RowCol& pos)
{
    if( lhs->getType() && lhs->getType()->isCharArray() && lhs->getType()->len != 0  &&
            ( (rhs->getType() && rhs->getType()->isCharArray() && rhs->getType()->len == 0) ||
               rhs->getType()->kind == Type::StrLit ) )
        return stind(lhs, rhs, pos);

    switch( lhs->kind )
    {
    case Expression::Index:
        return stelem(lhs, rhs, pos);
    case Expression::FieldSelect:
        return stfld(lhs, rhs, pos);
    case Expression::LocalVar:
        if( !evaluate(rhs) ) // value
            return false;
        if( !prepareRhs( lhs->getType(), true, pos ) )
            return false;
        stack.takeLast();
        out->line_(pos);
        out->stloc_(lhs->val.toUInt());
        break;
    case Expression::Param:
        if( !evaluate(rhs) ) // value
            return false;
        if( !prepareRhs( lhs->getType(), true, pos ) )
            return false;
        stack.takeLast();
        out->line_(pos);
        out->starg_(lhs->val.toUInt());
        break;
    case Expression::ModuleVar:
        if( !evaluate(rhs) ) // value
            return false;
        if( !prepareRhs( lhs->getType(), true, pos ) )
            return false;
        stack.takeLast();
        out->line_(pos);
        out->stvar_(lhs->val.value<Qualident>());
        break;
    default:
        return stind(lhs, rhs, pos);
    }
    return true;
}

bool Evaluator::derefPointer(bool byVal, const RowCol& pos)
{
    err.clear();
    if( stack.isEmpty() )
    {
        return error("expecting a value on the stack",pos);
    }

    Value v = stack.takeLast();

    Q_ASSERT( v.type && v.type->kind == Type::Pointer && !v.ref );

    v.type = v.type->getType();
    v.ref = true;

    if( v.type->isCharArray() && v.type->len == 0 && byVal )
        byVal = false; // avoid ldind for an open array of char

    stack.push_back(v);

    if( byVal )
        derefValue(pos); // actually do it, because we need a value, not an lvalue

    return err.isEmpty();
}

bool Evaluator::derefValue(const RowCol& pos)
{
    err.clear();
    if( stack.isEmpty() )
    {
        return error("expecting a value on the stack", pos);
    }
    Value v = stack.takeLast();
    if( v.isConst() || !v.ref || v.type == 0 )
    {
        return error("expecting a non const reference on the stack",pos);
    }
    Q_ASSERT(!v.isConst());
    Q_ASSERT(v.ref && v.type);
    v.ref = false;
    switch(v.type->kind)
    {
    case Type::BOOL:
    case Type::CHAR:
    case Type::UINT8:
        out->ldind_(Mil::EmiTypes::U1);
        break;
    case Type::UINT16:
        out->ldind_(Mil::EmiTypes::U2);
        break;
    case Type::UINT32:
    case Type::SET:
        out->ldind_(Mil::EmiTypes::U4);
        break;
    case Type::UINT64:
        out->ldind_(Mil::EmiTypes::U8);
        break;
    case Type::INT8:
        out->ldind_(Mil::EmiTypes::I1);
        break;
    case Type::INT16:
        out->ldind_(Mil::EmiTypes::I2);
        break;
    case Type::INT32:
    case Type::ConstEnum:
        out->ldind_(Mil::EmiTypes::I4);
        break;
    case Type::INT64:
        out->ldind_(Mil::EmiTypes::I8);
        break;
    case Type::FLT32:
        out->ldind_(Mil::EmiTypes::R4);
        break;
    case Type::FLT64:
        out->ldind_(Mil::EmiTypes::R8);
        break;
    case Type::Pointer:
        out->ldind_(Mil::EmiTypes::IntPtr);
        break;
    case Type::Proc:
        if( v.type->typebound )
            out->ldind_(Mil::EmiTypes::IPP);
        else
            out->ldind_(Mil::EmiTypes::IntPtr);
        break;
    case Type::Record:
    case Type::Object:
    case Type::Array:
    case Type::Generic:
        out->ldind_(toQuali(v.type));
        break;
    case Type::Interface:
        out->ldind_(Mil::EmiTypes::IPP);
        break;
    default:
        return false;
    }

    stack.push_back(v);
    return err.isEmpty();
}

void Evaluator::assureTopIsValue(const RowCol& pos)
{
    if( stack.isEmpty() )
        return;
    Value& v = stack.back();
    if( v.ref )
        derefValue(pos);
}

bool Evaluator::desigField(Declaration* field, bool byVal, const RowCol& pos)
{
    err.clear();
    if( stack.size() < 1 )
    {
        return error("expecting a value on the stack",pos);
    }
    Value lhs = stack.takeLast(); // record reference

    // TODO: desig const record

    Q_ASSERT(lhs.ref && lhs.type && (lhs.type->kind == Type::Record || lhs.type->kind == Type::Object));

    Q_ASSERT(field);
    out->line_(pos);
    const Mil::Trident desig = qMakePair(toQuali(lhs.type),field->name);
    if( byVal )
        out->ldfld_(desig);
    else
        out->ldflda_(desig);

    Value res;
    res.ref = !byVal;
    res.type = field->getType();
    res.visi = 0;
    res.mode = Value::Val;
    stack.push_back(res);

    return err.isEmpty();
}

bool Evaluator::desigVar(bool byVal, const RowCol& pos)
{
    err.clear();
    if( stack.isEmpty() )
    {
        return error("expecting a value on the stack",pos);
    }
    Value v = stack.takeLast();

    out->line_(pos);
    if( byVal )
        switch( v.mode )
        {
        case Value::VarDecl:
            out->ldvar_(v.val.value<Qualident>());
            break;
        case Value::LocalDecl:
            out->ldloc_(v.val.toInt());
            break;
        case Value::ParamDecl:
            out->ldarg_(v.val.toInt());
            break;
        default:
            Q_ASSERT(false);
        }
    else
        switch( v.mode )
        {
        case Value::VarDecl:
            out->ldvara_(v.val.value<Qualident>());
            break;
        case Value::LocalDecl:
            out->ldloca_(v.val.toInt());
            break;
        case Value::ParamDecl:
            out->ldarga_(v.val.toInt());
            break;
        default:
            Q_ASSERT(false);
        }

    v.ref = !byVal;
    stack.push_back(v);

    return err.isEmpty();
}

bool Evaluator::desigIndex(bool byVal, const RowCol& pos)
{
    err.clear();

    Value rhs = stack.takeLast(); // index
    Value lhs = stack.takeLast(); // array reference

    if( !lhs.ref || lhs.type == 0 || lhs.type->kind != Type::Array)
        return false;
    Q_ASSERT(lhs.ref && lhs.type && lhs.type->kind == Type::Array);

    if( rhs.isConst() )
    {
        const qint64 idx = rhs.val.toLongLong();
        if( idx < 0  || lhs.type->len && lhs.type->len <= idx )
        {
            return error("index out of range",pos);
        }
        pushMilStack(rhs, pos);
    }

    const Qualident elemType = toQuali(lhs.type->getType());
    out->line_(pos);
    if( byVal )
        out->ldelem_(elemType);
    else
        out->ldelema_(elemType);

    Value res;
    res.ref = !byVal;
    res.type = lhs.type->getType();
    res.visi = 0;
    res.mode = Value::Val;
    stack.push_back(res);

    return err.isEmpty();
}

bool Evaluator::call(int nArgs, const RowCol& pos)
{
    err.clear();
    if( stack.size() < nArgs + 1 )
    {
        return error(QString("expecting %1 values on the stack").arg(nArgs+1), pos);
    }

    Value callee = stack.takeLast();
    if( callee.type == 0 )
        callee.type = mdl->getType(Type::NoType);

    Type* ret = 0;
    switch( callee.mode )
    {
    case Value::Builtin:
        {
            Builtins bi(this);
            bi.callBuiltin(callee.val.toInt(),nArgs, pos);
            return err.isEmpty();
        }

    case Value::Procedure:
        {
            Declaration* proc = callee.val.value<Declaration*>();
            Q_ASSERT(proc);
            ret = proc->getType();
            out->line_(pos);
            out->call_(toQuali(proc),nArgs, ret != 0);
        }
        break;
    case Value::Super:
        {
            Declaration* proc = callee.val.value<Declaration*>();
            Q_ASSERT(proc);
            ret = proc->getType();
            out->line_(pos);
            const Mil::Trident trident = qMakePair(toQuali(proc->outer),proc->name);
            out->callinst_(trident,nArgs, ret != 0);
        }
        break;
    case Value::Method:
        {
            Declaration* proc = callee.val.value<Declaration*>();
            Q_ASSERT(proc);
            ret = proc->getType();
            const Mil::Trident trident = qMakePair(toQuali(proc->outer),proc->name);
            out->line_(pos);
            if( proc->dynamic )
                out->callvirt_(trident,nArgs, ret != 0);
            else
                out->callinst_(trident,nArgs, ret != 0);
        }
        break;
    case Value::IntfMeth:
        {
            Declaration* proc = callee.val.value<Declaration*>();
            Q_ASSERT(proc);
            ret = proc->getType();
            const Mil::Trident trident = qMakePair(toQuali(proc->outer),proc->name);
            out->line_(pos);
            out->ldmeth_(trident);
            out->callmi_(trident, nArgs, ret != 0);
        }
        break;
    case Value::VarDecl:
    case Value::LocalDecl:
    case Value::ParamDecl:
    case Value::Val:
        ret = callee.type->getType();
        if( callee.type->kind == Type::Proc )
        {
            out->line_(pos);
            if( callee.type->typebound )
                out->callmi_(toQuali(callee.type), nArgs, ret != 0);
            else
                out->calli_(toQuali(callee.type), nArgs, ret != 0);
            break;
        }
        // else fall through
    default:
        return error("this expression cannot be called",pos);
    }

    for( int i = 0; i < nArgs; i++ )
        stack.pop_back();

    Value tmp;
    tmp.mode = Value::Val;
    if( ret )
        tmp.type = ret;
    else
        tmp.type = mdl->getType(Type::NoType);
    stack.push_back(tmp);

    return err.isEmpty();
}

bool Evaluator::castPtr(Type* to, const RowCol& pos)
{
    err.clear();
    if( stack.size() < 1 )
    {
        return error("expecting a value on the stack", pos);
    }
    Value lhs = stack.takeLast(); // object
    // TODO
    Q_ASSERT(to && to->kind == Type::Pointer);
    out->line_(pos);
    out->castptr_(toQuali(to->getType()));
    // TODO restrict to pointers, add to MIL
    lhs.type = to;
    stack.push_back(lhs);
    return err.isEmpty();
}

bool Evaluator::convNum(Type* to, const RowCol& pos)
{
    Q_ASSERT( to && to->isNumber() );
    err.clear();
    if( stack.size() < 1 )
    {
        return error("expecting a value on the stack", pos);
    }
    if( stack.back().isConst() )
    {
        stack.back().type = to;
    }else
    {
        Mil::EmiTypes::Basic tt;
        switch(to->kind)
        {
        case Type::BOOL:
        case Type::CHAR:
        case Type::UINT8:
            tt = Mil::EmiTypes::U1;
            break;
        case Type::UINT16:
            tt = Mil::EmiTypes::U2;
            break;
        case Type::UINT32:
        case Type::SET:
            tt = Mil::EmiTypes::U4;
            break;
        case Type::UINT64:
            tt = Mil::EmiTypes::U8;
            break;
        case Type::INT8:
            tt = Mil::EmiTypes::I1;
            break;
        case Type::INT16:
            tt = Mil::EmiTypes::I2;
            break;
        case Type::INT32:
            tt = Mil::EmiTypes::I4;
            break;
        case Type::INT64:
            tt = Mil::EmiTypes::I8;
            break;
        case Type::FLT32:
            tt = Mil::EmiTypes::R4;
            break;
        case Type::FLT64:
            tt = Mil::EmiTypes::R8;
            break;
        default:
            Q_ASSERT(false);
        }

        out->line_(pos);
        out->conv_(tt);
    }
    return err.isEmpty();
}

template <typename Out, typename In>
bool bitCast(QVariant &v) {
    Out result; In tmp;
    // Determine the internal storage format to avoid value-altering conversions
    if (v.type() == QVariant::Double) {
        tmp = v.toDouble();
    } else if (v.type() == QVariant::LongLong) {
        tmp = v.toLongLong();
    } else if (v.type() == QVariant::ULongLong) {
        tmp = v.toULongLong();
    } else {
        Q_ASSERT(false);
    }
    memcpy(&result, &tmp, sizeof(In));
    v = result;
}

static inline bool bitCast(QVariant &v, Type* out, Type* in)
{
    Q_ASSERT(out && in && out->getByteSize() == in->getByteSize());
    if( in->kind == out->kind )
        return true;
    switch(out->kind)
    {
    case Type::UINT8:
        return bitCast<quint8,qint8>(v);
    case Type::INT8:
        return bitCast<qint8,quint8>(v);
    case Type::UINT16:
        return bitCast<quint16,qint16>(v);
    case Type::INT16:
        return bitCast<qint16,quint16>(v);
    case Type::UINT32:
        if(in->kind == Type::INT32)
            return bitCast<quint32,qint32>(v);
        else if( in->kind == Type::FLT32 )
            return bitCast<quint32,float>(v);
        else
            Q_ASSERT(false);
    case Type::INT32:
        if(in->kind == Type::UINT32)
            return bitCast<qint32,quint32>(v);
        else if( in->kind == Type::FLT32 )
            return bitCast<qint32,float>(v);
        else
            Q_ASSERT(false);
    case Type::FLT32:
        if(in->kind == Type::UINT32)
            return bitCast<float,quint32>(v);
        else if( in->kind == Type::INT32 )
            return bitCast<float,qint32>(v);
        else
            Q_ASSERT(false);
    case Type::UINT64:
        if(in->kind == Type::INT64)
            return bitCast<quint64,qint64>(v);
        else if( in->kind == Type::FLT64 )
            return bitCast<quint64,double>(v);
        else
            Q_ASSERT(false);
    case Type::INT64:
        if(in->kind == Type::UINT64)
            return bitCast<qint64,quint64>(v);
        else if( in->kind == Type::FLT64 )
            return bitCast<qint64,double>(v);
        else
            Q_ASSERT(false);
    case Type::FLT64:
        if(in->kind == Type::UINT64)
            return bitCast<double,quint64>(v);
        else if( in->kind == Type::INT64 )
            return bitCast<double,qint64>(v);
        else
            Q_ASSERT(false);
    default:
        break;
    }
    return false;
}

bool Evaluator::castNum(Type* to, const RowCol& pos)
{
    err.clear();
    if( to == 0 || !to->isNumber() )
        return error("cannot cast to this type",pos);
    if( stack.size() < 1 )
        return error("expecting a value on the stack",pos);
    if( stack.back().isConst() )
    {
        bitCast(stack.back().val, to, stack.back().type);
        stack.back().type = to;
    }else
    {
        Mil::EmiTypes::Basic tt;
        switch(to->kind)
        {
        case Type::BOOL:
        case Type::CHAR:
        case Type::UINT8:
            tt = Mil::EmiTypes::U1;
            break;
        case Type::UINT16:
            tt = Mil::EmiTypes::U2;
            break;
        case Type::UINT32:
        case Type::SET:
            tt = Mil::EmiTypes::U4;
            break;
        case Type::UINT64:
            tt = Mil::EmiTypes::U8;
            break;
        case Type::INT8:
            tt = Mil::EmiTypes::I1;
            break;
        case Type::INT16:
            tt = Mil::EmiTypes::I2;
            break;
        case Type::INT32:
            tt = Mil::EmiTypes::I4;
            break;
        case Type::INT64:
            tt = Mil::EmiTypes::I8;
            break;
        case Type::FLT32:
            tt = Mil::EmiTypes::R4;
            break;
        case Type::FLT64:
            tt = Mil::EmiTypes::R8;
            break;
        default:
            Q_ASSERT(false);
        }

        out->line_(pos);
        out->cast_(tt);
    }
    return err.isEmpty();
}

Value Evaluator::pop()
{
    Value res;
    if( !stack.isEmpty() )
        res = stack.takeLast();
    return res;
}

Value Evaluator::top()
{
    Value res;
    if( !stack.isEmpty() )
        res = stack.last();
    return res;
}

void Evaluator::assureTopOnMilStack(bool pop, const RowCol& pos)
{
    if( top().isConst() )
    {
        Value v = stack.takeLast();
        pushMilStack(v, pos);
        v.mode = Value::Val;
        stack.push_back(v);
    }
    if(pop)
        this->pop();
}

void Evaluator::shortCircuitAnd(Expression* e)
{
#if 0
    recursiveRun(e->lhs);
    if( e->lhs->isConst() && !e->rhs->isConst() )
        assureTopOnMilStack();
    recursiveRun(e->rhs);
    if( !e->lhs->isConst() && e->rhs->isConst() )
        assureTopOnMilStack();
    binaryOp(e->kind);
#else
    if( e->lhs->isConst() && e->rhs->isConst() )
    {
        recursiveRun(e->lhs);
        recursiveRun(e->rhs);
        binaryOp(e->kind,e->pos);
    }else
    {
        // p and q : if p then q, else FALSE
        out->line_(e->pos);
        out->iif_();
        recursiveRun(e->lhs);
        assureTopOnMilStack(true, e->lhs->pos);
        out->line_(e->pos);
        out->then_();
        recursiveRun(e->rhs);
        assureTopOnMilStack(false, e->rhs->pos); // leave the bool result on the stack
        out->line_(e->pos);
        out->else_();
        out->ldc_i4(0); // push FALSE
        out->end_();
    }
#endif
}

void Evaluator::shortCircuitOr(Expression* e)
{
#if 0
    recursiveRun(e->lhs);
    if( e->lhs->isConst() && !e->rhs->isConst() )
        assureTopOnMilStack();
    recursiveRun(e->rhs);
    if( !e->lhs->isConst() && e->rhs->isConst() )
        assureTopOnMilStack();
    binaryOp(e->kind);
#else
    if( e->lhs->isConst() && e->rhs->isConst() )
    {
        recursiveRun(e->lhs);
        recursiveRun(e->rhs);
        binaryOp(e->kind, e->pos);
    }else
    {
        // p or q : if p then TRUE, else q
        out->line_(e->pos);
        out->iif_();
        recursiveRun(e->lhs);
        assureTopOnMilStack(true, e->lhs->pos);
        out->line_(e->pos);
        out->then_();
        out->ldc_i4(1); // push TRUE
        out->line_(e->pos);
        out->else_();
        recursiveRun(e->rhs);
        assureTopOnMilStack(false, e->rhs->pos); // leave the bool result on the stack
        out->end_();
    }
#endif
}

bool Evaluator::pushMilStack(const Value& v, const RowCol& pos)
{
    err.clear();
    switch( v.mode )
    {
    case Value::Const:
        if( v.type->isSimple() || v.type->kind == Type::ConstEnum )
        {
            out->line_(pos);
            switch( v.type->kind )
            {
            case Type::StrLit:
                out->ldstr_( v.val.toByteArray() );
                break;
            case Type::Nil:
                out->ldnull_();
                break;
            case Type::BOOL:
            case Type::CHAR:
            case Type::UINT8:
            case Type::UINT16:
            case Type::UINT32:
            case Type::SET:
                out->ldc_i4(v.val.toUInt());
                //v.type = mdl->getType(Type::INT32);
                break;
            case Type::UINT64:
                out->ldc_i8(v.val.toULongLong());
                break;
            case Type::INT8:
            case Type::INT16:
            case Type::INT32:
            case Type::ConstEnum:
                out->ldc_i4(v.val.toInt());
                //v.type = mdl->getType(Type::INT32);
                break;
            case Type::INT64:
                out->ldc_i8(v.val.toLongLong());
                break;
            case Type::FLT32:
                out->ldc_r4(v.val.toFloat());
                break;
            case Type::FLT64:
                out->ldc_r8(v.val.toDouble());
                break;
            case Type::UniInt: {
                    qWarning() << "pushMilStack cannot handle universal integer" << pos.d_row; // TODO
                    Value vv = v;
                    vv.type = smallestUIntType(vv.val);
                    pushMilStack(vv, pos);
                }
                break;
            case Type::ByteArrayLit: {
                qWarning() << "WARNING: load byte array literals not yet implemented";
                //out->ldstr_( v.val.toByteArray().toHex() );
                Mil::ConstrLiteral obj;
                obj.data = v.val;
                out->ldc_obj(obj);
            } break;

            default:
                Q_ASSERT(false);
            }
        }else
        {
            Mil::ConstrLiteral obj;
            obj.typeRef = toQuali(v.type);
            obj.data = v.val;
            out->line_(pos);
            out->ldc_obj(obj);
        }
        break;
    case Value::VarDecl:
    case Value::LocalDecl:
    case Value::ParamDecl:
        {
            qWarning() << "TODO: Evaluator::pushMilStack value not yet supported";
        }
        break;
    default:
        return error("not supported for this kind of declaration",pos);
    }
    return true;
}

void Evaluator::emitRelOp(quint8 op, bool unsig, const RowCol& pos)
{
    out->line_(pos);
    switch(op)
    {
    case Expression::Eq:
        out->ceq_();
        break;
    case Expression::Neq: // not eq
        out->ceq_();
        out->ldc_i4(0);
        out->ceq_();
        break;
    case Expression::Geq: // not lt
        out->clt_(unsig);
        out->ldc_i4(0);
        out->ceq_();
        break;
    case Expression::Gt:
        out->cgt_(unsig);
        break;
    case Expression::Leq: // not gt
        out->cgt_(unsig);
        out->ldc_i4(0);
        out->ceq_();
        break;
    case Expression::Lt:
        out->clt_(unsig);
        break;
    default:
        Q_ASSERT(false);
        break;
    }
}

void Evaluator::emitArithOp(quint8 op, bool unsig, bool i64, const RowCol &pos)
{
    out->line_(pos);
    switch(op)
    {
    case Expression::Mul:
        out->mul_();
        break;
    case Expression::Fdiv:
        out->div_();
        break;
    case Expression::Div:
        out->div_(unsig);
        break;
    case Expression::Mod:
        out->rem_(unsig);
        break;
    case Expression::Add:
        out->add_();
        break;
    case Expression::Sub:
        out->sub_();
        break;
    default:
        Q_ASSERT(false);
    }

}

void Evaluator::adjustNumType(Type* me, Type* other)
{
    if( me && me->isNumber() && other && other->isNumber() )
    {
        if( me->isInt() && other->isInt() &&
                other->kind == Type::INT64 && me->kind < Type::INT64 )
            out->conv_(Mil::EmiTypes::I8);
        else if( me->isUInt() && other->isUInt() &&
                 other->kind == Type::UINT64 && me->kind < Type::UINT64 )
            out->conv_(Mil::EmiTypes::U8);
        else if( me->isUInt() && other->isInt() )
        {
            // Tv is a signed integer and e is an unsigned integer constant, and Tv includes the
            // smallest integer type necessary to represent e.
            if( other->kind == Type::INT64 && me->kind < Type::UINT64 )
                out->conv_(Mil::EmiTypes::I8);
            else
                out->conv_(Mil::EmiTypes::I4);
        }
        else if( me->isReal() && other->isReal() &&
                 other->kind == Type::FLT64 && me->kind < Type::FLT64 )
            out->conv_(Mil::EmiTypes::R8);
    }
}

bool Evaluator::error(const QString &msg, const RowCol &pos)
{
    err = msg;
    errPos = pos;
    return false;
}

Qualident Evaluator::toQuali(Type * t)
{
    return toQuali(t, mdl->getTopModule());
}

Qualident Evaluator::toQuali(Declaration * d)
{
    return toQuali(d, mdl->getTopModule());
}

void Evaluator::notOp(Value& v, const RowCol& pos)
{
    Q_ASSERT( v.type->isBoolean() );
    if( v.isConst() )
        v.val = !v.val.toBool();
    else
    {
        // logic not
        out->line_(pos);
        out->ldc_i4(0);
        out->ceq_();
    }
}

Value Evaluator::logicOp(quint8 op, const Value& lhs, const Value& rhs, const RowCol& pos)
{
    Value res;
    if( lhs.type->isBoolean() && rhs.type->isBoolean() )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            res.mode = lhs.mode;
            res.type = lhs.type;
            if( op == Expression::And )
                res.val = lhs.val.toBool() && rhs.val.toBool();
            else
                res.val = lhs.val.toBool() || rhs.val.toBool();
        }else
        {
            Q_ASSERT(false); // handled elsewhere
        }
    }else
        Q_ASSERT(false);
    return res;
}

static inline Type* maxType(Type* lhs, Type* rhs)
{
    if( lhs->kind >= rhs->kind )
        return lhs;
    else
        return rhs;
}

Value Evaluator::arithOp(quint8 op, const Value& lhs, const Value& rhs, const RowCol& pos)
{
    Value res;
    res.mode = Value::Val;
    res.type = lhs.type;

    if( lhs.type == 0 || rhs.type == 0 )
        return Value();

    if( lhs.type->isNumber() && rhs.type->isNumber() )
    {
        res.type = maxType(lhs.type,rhs.type);
        if( lhs.type->isInt() && rhs.type->isInt() )
        {
            if( lhs.isConst() && rhs.isConst() )
            {
                res.mode = Value::Const;
                const qint64 a = lhs.val.toLongLong();
                const qint64 b = rhs.val.toLongLong();
                switch(op)
                {
                case Expression::Mul:
                    res.val = a * b;
                    break;
                case Expression::Div:
                    res.val = (qint64)(a < 0 ? (a - b + 1) / b : a / b);
                    break;
                case Expression::Mod:
                    res.val = a < 0 ? (b - 1) + ((a - b + 1)) % b : a % b;
                    break;
                case Expression::Add:
                    res.val = a + b;
                    break;
                case Expression::Sub:
                    res.val = a - b;
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }else
            {
                emitArithOp(op,false, res.type->kind == Type::INT64, pos );
            }
        }else if( lhs.type->isUInt() && rhs.type->isUInt() )
        {
            if( lhs.isConst() && rhs.isConst() )
            {
                res.mode = Value::Const;
                const quint64 a = lhs.val.toULongLong();
                const quint64 b = rhs.val.toULongLong();
                switch(op)
                {
                case Expression::Mul:
                    res.val = (quint64)(a * b);
                    break;
                case Expression::Div:
                    res.val = (quint64)(a / b);
                    break;
                case Expression::Mod:
                    res.val = (quint64)(a % b);
                    break;
                case Expression::Add:
                    res.val = (quint64)(a + b);
                    break;
                case Expression::Sub:
                    res.val = (quint64)(a - b);
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }else
            {
                emitArithOp(op,true, res.type->kind == Type::UINT64, pos);
            }
        }else if( lhs.type->isReal() && rhs.type->isReal() )
        {
            if( lhs.isConst() && rhs.isConst() )
            {
                res.mode = Value::Const;
                switch(op)
                {
                case Expression::Mul:
                    res.val = lhs.val.toDouble() * rhs.val.toDouble();
                    break;
                case Expression::Fdiv:
                    res.val = lhs.val.toDouble() / rhs.val.toDouble();
                    break;
                case Expression::Add:
                    res.val = lhs.val.toDouble() + rhs.val.toDouble();
                    break;
                case Expression::Sub:
                    res.val = lhs.val.toDouble() - rhs.val.toDouble();
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }else
            {
                emitArithOp(op, false, false, pos );
            }
        }else
            error(QString("operation '%1' not supported for these operands: %2, %3").arg(Expression::getName(op))
                    .arg(lhs.type->getName()).arg(rhs.type->getName()), pos);
    }else if( lhs.type->isSet() && rhs.type->isSet() )
    {
        // + - * /
        if( lhs.isConst() && rhs.isConst() )
        {
            res.mode = Value::Const;
            switch(op)
            {
            case Expression::Mul:
                res.val = lhs.val.toUInt() & rhs.val.toUInt();
                break;
            case Expression::Fdiv:
                res.val = ~(lhs.val.toUInt() & rhs.val.toUInt()) & (lhs.val.toUInt() | rhs.val.toUInt());
                break;
            case Expression::Add:
                res.val = lhs.val.toUInt() | rhs.val.toUInt();
                break;
            case Expression::Sub:
                res.val = lhs.val.toUInt() & ~rhs.val.toUInt();
                break;
            default:
                Q_ASSERT(false);
                break;
            }
        }else
        {
            out->line_(pos);
            switch(op)
            {
            case Expression::Mul:
                out->and_();
                break;
            case Expression::Fdiv:
                out->call_(coreName("SetDiv"),2,1);
                break;
            case Expression::Add:
                out->or_();
                break;
            case Expression::Sub:
                out->not_();
                out->and_();
                break;
            default:
                Q_ASSERT(false);
                break;
            }
        }
    }else if( (lhs.type->kind == Type::StrLit || lhs.type->kind == Type::CHAR) &&
              (rhs.type->kind == Type::StrLit || rhs.type->kind == Type::CHAR) )
    {
        // + only
        res.type = mdl->getType(Type::StrLit);
        Q_ASSERT( op == Expression::Add );
        Q_ASSERT( lhs.isConst() && rhs.isConst() );
        res.val = lhs.val.toByteArray() + rhs.val.toByteArray();
    }else
        error(QString("operation '%1' not supported for these operands: %2, %3").arg(Expression::getName(op))
                .arg(lhs.type->getName()).arg(rhs.type->getName()), pos);

    return res;
}

Value Evaluator::relationOp(quint8 op, const Value& lhs, const Value& rhs, const RowCol& pos)
{
    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(Type::BOOL);

    if( lhs.type == 0 || rhs.type == 0 )
        return res;

    static const char* msg = "INTERNAL ERROR: operation not supported for given operands"; // should not happen due to Parser::checkRelOp
    if( lhs.type->isNumber() && rhs.type->isNumber() )
    {
        if( lhs.type->isInt() && rhs.type->isInt() )
        {
            if( lhs.isConst() && rhs.isConst() )
            {
                res.mode = Value::Const;
                switch(op)
                {
                case Expression::Geq:
                    res.val = lhs.val.toLongLong() >= rhs.val.toLongLong();
                    break;
                case Expression::Gt:
                    res.val = lhs.val.toLongLong() > rhs.val.toLongLong();
                    break;
                case Expression::Eq:
                    res.val = lhs.val.toLongLong() == rhs.val.toLongLong();
                    break;
                case Expression::Leq:
                    res.val = lhs.val.toLongLong() <= rhs.val.toLongLong();
                    break;
                case Expression::Neq:
                    res.val = lhs.val.toLongLong() != rhs.val.toLongLong();
                    break;
                case Expression::Lt:
                    res.val = lhs.val.toLongLong() < rhs.val.toLongLong();
                    break;
                default:
                    error(msg, pos);
                    return res;
                }
            }else
            {
                emitRelOp(op,false, pos);
            }
        }else if( lhs.type->isUInt() && rhs.type->isUInt() )
        {
            if( lhs.isConst() && rhs.isConst() )
            {
                res.mode = Value::Const;
                switch(op)
                {
                case Expression::Geq:
                    res.val = lhs.val.toULongLong() >= rhs.val.toULongLong();
                    break;
                case Expression::Gt:
                    res.val = lhs.val.toULongLong() > rhs.val.toULongLong();
                    break;
                case Expression::Eq:
                    res.val = lhs.val.toULongLong() == rhs.val.toULongLong();
                    break;
                case Expression::Leq:
                    res.val = lhs.val.toULongLong() <= rhs.val.toULongLong();
                    break;
                case Expression::Neq:
                    res.val = lhs.val.toULongLong() != rhs.val.toULongLong();
                    break;
                case Expression::Lt:
                    res.val = lhs.val.toULongLong() < rhs.val.toULongLong();
                    break;
                default:
                    error(msg, pos);
                    return res;
                }
            }else
            {
                emitRelOp(op,false, pos);
            }
        }else if( lhs.type->isReal() && rhs.type->isReal() )
        {
            if( lhs.isConst() && rhs.isConst() )
            {
                res.mode = Value::Const;
                switch(op)
                {
                case Expression::Geq:
                    res.val = lhs.val.toDouble() >= rhs.val.toDouble();
                    break;
                case Expression::Gt:
                    res.val = lhs.val.toDouble() > rhs.val.toDouble();
                    break;
                case Expression::Eq:
                    res.val = lhs.val.toDouble() == rhs.val.toDouble();
                    break;
                case Expression::Leq:
                    res.val = lhs.val.toDouble() <= rhs.val.toDouble();
                    break;
                case Expression::Neq:
                    res.val = lhs.val.toDouble() != rhs.val.toDouble();
                    break;
                case Expression::Lt:
                    res.val = lhs.val.toDouble() < rhs.val.toDouble();
                    break;
                default:
                    error(msg, pos);
                    return res;
                }
            }else
            {
                emitRelOp(op,false, pos);
            }
        }else
        {
            error(QString("operands not compatible: %1 %3 %2").arg(Type::name[lhs.type->kind]).
                    arg(Type::name[rhs.type->kind]).arg(Expression::getName(op)),pos);
            return res;
        }
    }else if( lhs.type->isText() && rhs.type->isText() )
    {
        switch(op)
        {
        case Expression::Geq:
            out->line_(pos);
            out->ldc_i4(6);
            break;
        case Expression::Gt:
            out->line_(pos);
            out->ldc_i4(5);
            break;
        case Expression::Eq:
            out->line_(pos);
            out->ldc_i4(1);
            break;
        case Expression::Leq:
            out->line_(pos);
            out->ldc_i4(4);
            break;
        case Expression::Neq:
            out->line_(pos);
            out->ldc_i4(2);
            break;
        case Expression::Lt:
            out->line_(pos);
            out->ldc_i4(3);
            break;
        default:
            error(msg, pos);
            return res;
        }
        if( lhs.type->kind == Type::CHAR && rhs.type->kind == Type::CHAR )
            out->call_(coreName("relop4"),3,true);
        else if( lhs.type->kind == Type::CHAR )
            out->call_(coreName("relop3"),3,true);
        else if( rhs.type->kind == Type::CHAR )
            out->call_(coreName("relop2"),3,true);
        else
            out->call_(coreName("relop1"),3,true);
    }else if( lhs.type->kind == Type::Pointer && rhs.type->kind == Type::Pointer ||
              lhs.type->kind == Type::Pointer && rhs.type->kind == Type::Nil ||
              lhs.type->kind == Type::Nil && rhs.type->kind == Type::Pointer ||
              lhs.type->kind == Type::Nil && rhs.type->kind == Type::Nil )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            // Micron supports pointer literals!
            res.mode = Value::Const;
            switch(op)
            {
            case Expression::Geq:
                res.val = lhs.val.toULongLong() >= rhs.val.toULongLong();
                break;
            case Expression::Gt:
                res.val = lhs.val.toULongLong() > rhs.val.toULongLong();
                break;
            case Expression::Eq:
                res.val = lhs.val.toULongLong() == rhs.val.toULongLong();
                break;
            case Expression::Leq:
                res.val = lhs.val.toULongLong() <= rhs.val.toULongLong();
                break;
            case Expression::Neq:
                res.val = lhs.val.toULongLong() != rhs.val.toULongLong();
                break;
            case Expression::Lt:
                res.val = lhs.val.toULongLong() < rhs.val.toULongLong();
                break;
            default:
                error(msg, pos);
                return res;
            }
        }else
        {
            emitRelOp(op,true,pos);
        }
    }else if( lhs.type->kind == Type::ConstEnum  && rhs.type->kind == Type::ConstEnum )
    {
        // Q_ASSERT( lhs.type == rhs.type );
        if( lhs.isConst() && rhs.isConst() )
        {
            res.mode = Value::Const;
            switch(op)
            {
            case Expression::Geq:
                res.val = lhs.val.toLongLong() >= rhs.val.toLongLong();
                break;
            case Expression::Gt:
                res.val = lhs.val.toLongLong() > rhs.val.toLongLong();
                break;
            case Expression::Eq:
                res.val = lhs.val.toLongLong() == rhs.val.toLongLong();
                break;
            case Expression::Leq:
                res.val = lhs.val.toLongLong() <= rhs.val.toLongLong();
                break;
            case Expression::Neq:
                res.val = lhs.val.toLongLong() != rhs.val.toLongLong();
                break;
            case Expression::Lt:
                res.val = lhs.val.toLongLong() < rhs.val.toLongLong();
                break;
            default:
                error(msg,pos);
                return res;
            }
        }else
        {
            emitRelOp(op,false,pos);
        }
    }else if( ( lhs.type->isSet() && rhs.type->isSet() ) ||
              (lhs.type->isBoolean() && rhs.type->isBoolean()) )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            res.mode = Value::Const;
            switch(op)
            {
            case Expression::Eq:
                res.val = lhs.val.toUInt() == rhs.val.toUInt();
                break;
            case Expression::Neq:
                res.val = lhs.val.toUInt() != rhs.val.toUInt();
                break;
            default:
                error(msg, pos);
                return res;
            }
        }else
        {
            emitRelOp(op,true, pos);
        }
    }else if( lhs.type->kind == Type::Proc && rhs.type->kind == Type::Proc ||
              lhs.type->kind == Type::Proc && rhs.type->kind == Type::Nil ||
              lhs.type->kind == Type::Nil && rhs.type->kind == Type::Proc )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            // TODO: actual generic module parameters could be proc type literals
        }else
        {
            emitRelOp(op,true, pos);
        }
    }else
    {
        error(QString("operands not compatible: %1 %3 %2").arg(Type::name[lhs.type->kind]).
                arg(Type::name[rhs.type->kind]).arg(Expression::getName(op)),pos);
    }

    return res;
}

Value Evaluator::inOp(const Value& lhs, const Value& rhs, const RowCol& pos)
{
    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(Type::BOOL);

    if( lhs.type->isInteger() && rhs.type->isSet() )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            const int index = lhs.val.toInt();
            if( index < 0 || index > 31 )
                error("left side must be between 0 and 31", pos);
            else
            {
                const quint32 set = rhs.val.toUInt();
                res.val = ((1 << index) & set) != 0;
            }
            res.mode = Value::Const;
        }else
        {
            out->line_(pos);
            out->call_(coreName("SetIn"),2,1);
        }

    }else
        error("operation not compatible with given operands", pos);
    return res;
}

Value Evaluator::isOp(const Value& lhs, const Value& rhs, const RowCol& pos)
{
    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(Type::BOOL);


    if( lhs.type->kind == Type::Pointer && lhs.type->getType() && lhs.type->getType()->kind == Type::Object
            && ( rhs.type->kind == Type::Object ||
                (rhs.type->kind == Type::Pointer && rhs.type->getType() && rhs.type->getType()->kind == Type::Object) ) )
    {
        Type* t = rhs.type;
        if( t->kind == Type::Pointer )
            t = t->getType();
        const Qualident type = toQuali(t);
        out->line_(pos);
        out->isinst_(type);
    }else
        error("operation not compatible with given operands", pos);
    return res;
}

void Evaluator::unaryMinusOp(Value& v, const RowCol& pos)
{
    if( v.type->isNumber() )
    {
        if( v.type->isUInt() )
        {
            if( v.isConst() )
                v.val = -v.val.toLongLong();
            else
            {
                out->line_(pos);
                if( v.type->kind == Type::INT32 )
                    out->conv_(Mil::EmiTypes::I4);
                else
                    out->conv_(Mil::EmiTypes::I8);
                out->neg_();
            }
        }else if( v.type->isInteger() )
        {
            if( v.isConst() )
                v.val = -v.val.toLongLong();
            else
            {
                out->line_(pos);
                out->neg_();
            }
        }else if( v.type->isReal() )
        {
            if( v.isConst() )
                v.val = -v.val.toDouble();
            else
            {
                out->line_(pos);
                out->neg_();
            }
        }else
            Q_ASSERT(false);
    }else if( v.type->isSet() )
    {
        if( v.isConst() )
            v.val = ~v.val.toUInt();
        else
        {
            out->line_(pos);
            out->not_();
        }
    }else
        Q_ASSERT(false);
}

void Evaluator::unaryPlusOp(Value& v, const RowCol& pos)
{
    // NOP
}

Qualident Evaluator::toQuali(Declaration* d, Declaration *module)
{
    Q_ASSERT( d && d->kind != Declaration::Field ); // use toTriple for fields

    QByteArray desig;
    Declaration* last = 0;
    bool doSymbol = false;
    while( d && d->kind != Declaration::Module )
    {
        Q_ASSERT(d->kind != Declaration::Field);
        if( d->kind == Declaration::LocalDecl ||
                d->kind == Declaration::ParamDecl )
            return qMakePair(QByteArray(),d->name); // locals and params have no desig
        if( d->kind == Declaration::Procedure && d->typebound )
            return qMakePair(QByteArray(),d->name); // bound procs are in the namespace of the type
        if( d->kind == Declaration::TypeDecl && d->outer == 0 && last == 0 )
            return toQuali(d->getType(), module); // this is a built-in type
        if( !desig.isEmpty() )
        {
            desig = "$" + desig;
            doSymbol = true;
        }
        desig = d->name + desig;
        last = d;
        d = d->outer;
    }
    if( doSymbol )
        desig = Token::getSymbol(desig);

    Q_ASSERT( d && d->kind == Declaration::Module );
    if( d == module )
        return qMakePair(QByteArray(),desig); // local symbol
    // else imported symbol
    ModuleData md = d->data.value<ModuleData>();
    return qMakePair(md.fullName, desig);
}

Qualident Evaluator::toQuali(Type* t, Declaration *module)
{
    static QByteArray symbols[Type::MaxBasicType];

    if( t == 0 )
        return Qualident();

    if( t->isSimple() )
    {
        if( symbols[Type::Any].isEmpty() )
        {
            symbols[Type::Any] = Token::getSymbol("any");
            symbols[Type::Nil] = Token::getSymbol("nil");
            symbols[Type::BOOL] = Token::getSymbol("bool");
            symbols[Type::CHAR] = Token::getSymbol("char");
            symbols[Type::UINT8] = Token::getSymbol("uint8");
            symbols[Type::UINT16] = Token::getSymbol("uint16");
            symbols[Type::UINT32] = symbols[Type::SET] = Token::getSymbol("uint32");
            symbols[Type::UINT64] = Token::getSymbol("uint64");
            symbols[Type::INT8] = Token::getSymbol("int8");
            symbols[Type::INT16] = Token::getSymbol("int16");
            symbols[Type::INT32] = Token::getSymbol("int32");
            symbols[Type::INT64] = Token::getSymbol("int64");
            symbols[Type::FLT32] = Token::getSymbol("float32");
            symbols[Type::FLT64] = Token::getSymbol("float64");
        }
        return qMakePair(QByteArray(),symbols[t->kind]);
    }else if( t->decl)
    {
        Q_ASSERT( t && t->decl );
        return toQuali(t->decl, module);
    }
    return Qualident();
}

QByteArray Evaluator::dequote(const QByteArray& str)
{
    QByteArray res;
    if( str.startsWith('\'') && str.endsWith('\'') ||
            str.startsWith('"') && str.endsWith('"') )
        res = str.mid(1,str.size()-2);
    else
        res = str;
    res += '\0'; // make terminating zero explicit in value
    return res;
}

Type *Evaluator::enumFoundationalType(Type * t)
{
    Q_ASSERT( t && t->kind == Type::ConstEnum);
    QList<qint64> vals;
    foreach( Declaration* d, t->subs )
    {
        vals << d->data.toLongLong();
    }
    qSort(vals);
    if( vals.isEmpty() )
        return mdl->getType(Type::UINT8);
    else if( vals.size() == 1 )
        return vals.first() < 0 ? smallestIntType(vals.first()) : smallestUIntType(vals.first());
    // else
    if( vals.first() < 0 )
    {
        Type* low = smallestIntType(vals.first());
        Type* high = smallestIntType(vals.last());
        return maxType(low, high);
    }else
        return smallestUIntType(vals.last());
}

bool Evaluator::recursiveRun(Expression* e)
{
    if( e == 0 )
        return false;

    switch( e->kind )
    {
    case Expression::Plus:
    case Expression::Minus:
    case Expression::Not: // Unary
        if( !recursiveRun(e->lhs) )
            return false;
        unaryOp(e->kind, e->pos);
        break;
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::Is:
    case Expression::In: // Relation
    case Expression::Add:
    case Expression::Sub: // AddOp
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod: // MulOp
        if( !recursiveRun(e->lhs) )
            return false;
        if( e->lhs->isConst() && !e->rhs->isConst() )
            assureTopOnMilStack(false, e->lhs->pos);
        if( !recursiveRun(e->rhs) )
            return false;
        if( !e->lhs->isConst() && e->rhs->isConst() )
            assureTopOnMilStack(false, e->rhs->pos);
        if( !binaryOp(e->kind, e->pos) )
            return false;
        break;
    case Expression::Or:
        shortCircuitOr(e);
        break;
    case Expression::And:
        shortCircuitAnd(e);
        break;
    case Expression::FieldSelect:
        if( !recursiveRun(e->lhs) )
            return false;
        desigField(e->val.value<Declaration*>(), e->byVal, e->pos);
        break;
    case Expression::Index:
        if( !recursiveRun(e->lhs) )
            return false;
        if( !recursiveRun(e->rhs) )
            return false;
        desigIndex(e->byVal, e->pos);
        break;
    case Expression::Cast:
        if( !recursiveRun(e->lhs) )
            return false;
        castPtr(e->getType(), e->pos);
        break;
    case Expression::AutoConv:
        if( !recursiveRun(e->lhs) )
            return false;
        convNum(e->getType(), e->pos);
        break;
    case Expression::Deref:
        if( !recursiveRun(e->lhs) )
            return false;
        derefPointer(e->byVal, e->pos);
        break;
    case Expression::Addr:
        if( !recursiveRun(e->lhs) )
            return false;
        stack.back().type = e->getType(); // NOP otherwise
        break;
    case Expression::Literal:
        stack.push_back(Value(e->getType(),e->val,Value::Const));
        break;
    case Expression::LocalVar:
        stack.push_back(Value(e->getType(),e->val,Value::LocalDecl));
        desigVar(e->byVal, e->pos);
        break;
    case Expression::Param:
        stack.push_back(Value(e->getType(),e->val,Value::ParamDecl));
        desigVar(e->byVal, e->pos);
        break;
    case Expression::ModuleVar:
        stack.push_back(Value(e->getType(),e->val,Value::VarDecl));
        desigVar(e->byVal, e->pos);
        break;
    case Expression::ProcDecl:
        stack.push_back(Value(e->getType(),e->val,Value::Procedure));
        break;
    case Expression::MethSelect:
        if( e->lhs )
        {
            // e->lhs for a call is evaluated in the call op before the arguments;
            // here we evaluate in case of pointer to method assignment
            if( !recursiveRun(e->lhs) )
                return false;
            stack.pop_back();
            // we have to remove it here, otherwise prepareRhs sees the wrong stack element;
            // logically removing this element would be a concern of out->ldmeth_
        } // else already reported
        stack.push_back(Value(e->getType(),e->val,Value::Method));
        break;
    case Expression::IntfSelect:
        if( e->lhs )
        {
            Q_ASSERT( e->lhs->getType()->kind == Type::Interface );

            e->lhs->setByVal(); // we need an interface reference by value, not a pointer to it
            if( !recursiveRun(e->lhs) )
                return false;
            stack.pop_back();
            stack.push_back(Value(e->getType(),e->val,Value::IntfMeth));
        } // else already reported
        break;
    case Expression::Builtin:
        stack.push_back(Value(e->getType(),e->val,Value::Builtin));
        break;
    case Expression::ConstDecl:
        {
            Value tmp;
            tmp.mode = Value::Const;
            tmp.val = e->val.value<Declaration*>()->data;
            tmp.type = e->getType();
            stack.push_back(tmp);
        }
        break;
    case Expression::TypeDecl:
        stack.push_back(Value(e->getType(),e->val,Value::TypeDecl)); // for import meta actuals or cast(type,v)
        break;
    case Expression::Call:
        {
            if( e->lhs->lhs && (e->lhs->kind == Expression::MethSelect || e->lhs->kind == Expression::Super) &&
                    e->lhs->lhs->getType()->kind != Type::Interface )
            {
                // assure that in a virtual call via designator like a.b(), 'a' is evaluated before the args
                Expression* proc = e->lhs;
                if( proc->kind == Expression::Super )
                    proc = proc->lhs;
                if( !recursiveRun(proc->lhs) ) // fetch self
                    return false;
                proc->lhs = 0; // to avoid yet another evaluation
            }

            ExpList args = e->val.value<ExpList>();
            const DeclList formals = e->lhs->kind == Expression::Super && e->lhs->lhs
                    ? e->lhs->lhs->getFormals()
                    : e->lhs->getFormals(); // no receiver here because args doesn't include it
            for(int i = 0; i < args.size(); i++ )
            {
                bindUniInt(args[i], i < formals.size() ? formals[i]->getType()->isInt() : false);

                if( !recursiveRun(args[i]) )
                    return false;

                if( i == 0 && e->lhs->kind == Expression::Builtin && e->lhs->val.toInt() == Builtin::ORD && args[i]->getType()->kind == Type::StrLit )
                {
                    // this is a fix because we need the first char of the string literal here, not a string
                    stack.back().val = (char)stack.back().val.toByteArray()[0];
                    stack.back().type = mdl->getType(Type::CHAR);
                }
                if( i < formals.size() )
                    prepareRhs(formals[i]->getType(), false, args[i]->pos);
                else
                    assureTopOnMilStack(false, args[i]->pos); // effects builtin args and variable args
            }
            if( !recursiveRun(e->lhs) ) // here 'b' of "a.b()" is evaluated in case of proc type calls;
                return false;
            call(args.size(), e->pos);
        }
        break;

    case Expression::Range:
    case Expression::NameValue:
    case Expression::IndexValue:
        break;
    case Expression::Constructor:
        constructor(e);
        break;

    case Expression::Super: {
            Q_ASSERT(e->lhs && e->lhs->kind == Expression::MethSelect);

            Declaration* method = e->lhs->val.value<Declaration*>();

            if( curProcs.isEmpty() || curProcs.back() != method )
            {
                return error("invalid super call (wrong method)", e->pos);
            }
            method = method->data.value<Declaration*>(); // super
            if( method == 0 )
            {
                return error("there is no overridden method", e->pos);
            }
            stack.push_back(Value(e->getType(),QVariant::fromValue(method),Value::Super));
        } break;

    default:
    case Expression::Invalid:
        Q_ASSERT(false);
        break;
    }
    return err.isEmpty();
}

void Evaluator::constructor(Expression* e)
{
    // Expression::create(Expression::Literal, ttok.toRowCol());
    if( e->isConst() )
    {
        recurseConstConstructor(e);
        return;
    }
    // else TODO
    error("dynamic constructors not yet supported by code generator", e->pos);
}

bool Evaluator::recurseConstConstructor(Expression* e)
{
    switch( e->getType()->kind )
    {
    case Type::Record:
    case Type::Object: {
            Mil::RecordLiteral rec;
            Expression* c = e->rhs;
            while( c )
            {
                Q_ASSERT( c->kind == Expression::NameValue );
                if( !evaluate(c->rhs) )
                    return false;
                Value v = stack.takeLast();
                Q_ASSERT( v.isConst() );
                rec.append(qMakePair(c->val.value<Declaration*>()->name, v.val));
                c = c->next;
            }
            Value v;
            v.mode = Value::Const;
            v.val = QVariant::fromValue(rec); // v.val = Mil::RecordLiteral
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    case Type::Array: {
            Q_ASSERT( e->getType()->len > 0 );
            QVector<QVariant> arr(e->getType()->len);
            Expression* c = e->rhs;
            if( (e->getType()->isCharArray() && c->getType()->kind == Type::StrLit) ||
                    (e->getType()->isByteArray() && c->getType() && c->getType()->kind == Type::ByteArrayLit))
            {
                // special case: char array constructor initialized with string literal
                const QByteArray str = c->val.toByteArray();
                for( int i = 0; i < str.size(); i++ )
                    arr[i] = QVariant::fromValue((char)str[i]);
            }else
                while( c )
                {
                    Q_ASSERT( c->kind == Expression::IndexValue );
                    if( !evaluate(c->rhs) )
                        return false;
                    Value v = stack.takeLast();
                    Q_ASSERT( v.isConst() );
                    arr[c->val.toLongLong()] = v.val;
                    c = c->next;
                }
            Value v;
            v.mode = Value::Const;
            v.val = QVariant::fromValue(arr.toList()); // v.val = QVariantList
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    case Type::Pointer: {
            Q_ASSERT(e->rhs);
            Value v;
            v.mode = Value::Const;
            v.val = e->rhs->val; // v.val = quint64
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    case Type::SET: {
            std::bitset<32> set;
            Expression* c = e->rhs;
            while( c )
            {
                if( c->kind == Expression::Range )
                {
                    if( !evaluate(c->lhs) )
                        return false;
                    const qint64 lhs = stack.takeLast().val.toLongLong();
                    if( !evaluate(c->rhs) )
                        return false;
                    const qint64 rhs = stack.takeLast().val.toLongLong();
                    if( lhs < 0 || lhs >= set.size() )
                    {
                        return error(QString("element %1 out of range").arg(lhs), e->pos);
                    }else if( rhs < 0 || rhs >= set.size() )
                    {
                        return error(QString("element %1 out of range").arg(rhs), e->pos);
                    }else
                    {
                        if( lhs <= rhs )
                            for( int i = lhs; i <= rhs; i++ )
                            {
                                if( set.test(i) )
                                {
                                    return error(QString("element %1 already included").arg(i), e->pos);
                                }else
                                    set.set(i);
                            }
                        else
                            for( int i = rhs; i <= lhs; i++ )
                            {
                                if( set.test(i) )
                                {
                                    return error(QString("element %1 already included").arg(i), e->pos);
                                }else
                                    set.set(i);
                            }
                    }
                }else
                {
                    if( !evaluate(c) )
                        return false;
                    const qint64 i = stack.takeLast().val.toLongLong();
                    if( i < 0 || i >= set.size() )
                    {
                        return error(QString("element %1 out of range").arg(i), e->pos);
                    }else if(set.test(i))
                    {
                        return error(QString("element %1 already included").arg(i), e->pos);
                    }else
                        set.set(i);
                }
                c = c->next;
            }
            Value v;
            v.mode = Value::Const;
            v.val = (quint64)set.to_ulong(); // v.val = quint32
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    }
    return true;
}

bool Evaluator::stind(Expression* lhs, Expression* rhs, const RowCol& pos)
{
    if( !evaluate(lhs) )
        return false;
    if( rhs && !evaluate(rhs) )
        return false;         // value is pushed in assign
    if( rhs && !assign(pos) )
        return false;
    return true;
}

bool Evaluator::stelem(Expression* lhs, Expression* rhs, const RowCol& pos)
{
    err.clear();
    if( lhs == 0 || lhs->lhs == 0 || lhs->rhs == 0 || rhs == 0 )
        return false;

    if( !evaluate(lhs->lhs) ) // array
        return false;
    Value array = stack.takeLast();
    if( !array.ref || array.type == 0 || array.type->kind != Type::Array)
        return false;
    Q_ASSERT(array.ref && array.type && array.type->kind == Type::Array);

    if( !evaluate(lhs->rhs) ) // index
        return false;
    Value index = stack.takeLast();
    if( index.isConst() )
    {
        const qint64 idx = index.val.toLongLong();
        if( idx < 0  || array.type->len && array.type->len <= idx )
            return error("index out of range",pos);

        pushMilStack(index, pos);
    }

    if( !evaluate(rhs) ) // value
        return false;

    if( !prepareRhs( array.type->getType(), true, pos ) )
        return false;

    Value value = stack.takeLast();

    out->line_(pos);
    out->stelem_(toQuali(array.type->getType()));

    return err.isEmpty();
}

bool Evaluator::stfld(Expression* lhs, Expression* rhs, const RowCol &pos)
{
    err.clear();
    if( lhs == 0 || lhs->lhs == 0 || rhs == 0 )
        return false;

    if( !evaluate(lhs->lhs) ) // pointer to record
        return false;
    Value pointer = stack.takeLast(); // record reference
    Q_ASSERT(pointer.ref && pointer.type && (pointer.type->kind == Type::Record || pointer.type->kind == Type::Object));

    Declaration* field = lhs->val.value<Declaration*>();
    Q_ASSERT(field);
    const Mil::Trident desig = qMakePair(toQuali(pointer.type),field->name);

    if( !evaluate(rhs) ) // value
        return false;

    if( !prepareRhs( field->getType(), true, pos ) )
        return false;

    Value value = stack.takeLast();

    out->line_(pos);
    out->stfld_(desig);

    return err.isEmpty();
}
