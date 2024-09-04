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
#include "MicMilEmitter.h"
#include "MicBuiltins.h"
#include "MicToken.h"
#include <bitset>
using namespace Mic;

bool Evaluator::evaluate(Expression* e, bool assureOnMilStack)
{
    err.clear();
    if( e == 0 )
        return false;
    recursiveRun(e);
    if(assureOnMilStack)
        assureTopOnMilStack();
    return err.isEmpty();
}

bool Evaluator::unaryOp(quint8 op)
{
    err.clear();
    if( stack.isEmpty() )
    {
        err = "nothing on the stack";
        return false;
    }
    Value& v = stack.back();
    switch( op )
    {
    case Expression::Not:
        notOp(v);
        break;
    case Expression::Plus:
        unaryPlusOp(v);
        break;
    case Expression::Minus:
        unaryMinusOp(v);
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
    if( u <= BasicType::getMax(BasicType::UINT8).toULongLong() )
        return mdl->getType(BasicType::UINT8);
    else if( u <= BasicType::getMax(BasicType::UINT16).toULongLong() )
        return mdl->getType(BasicType::UINT16);
    else if( u <= BasicType::getMax(BasicType::UINT32).toULongLong() )
        return mdl->getType(BasicType::UINT32);
    else
        return mdl->getType(BasicType::UINT64);
}

Type*Evaluator::smallestIntType(const QVariant& v) const
{
    const qint64 i = v.toLongLong();
    if( i >= BasicType::getMin(BasicType::INT8).toLongLong() && i <= BasicType::getMax(BasicType::INT8).toLongLong() )
        return mdl->getType(BasicType::INT8);
    else if( i >= BasicType::getMin(BasicType::INT16).toLongLong() && i <= BasicType::getMax(BasicType::INT16).toLongLong() )
        return mdl->getType(BasicType::INT16);
    else if( i >= BasicType::getMin(BasicType::INT32).toLongLong() && i <= BasicType::getMax(BasicType::INT32).toLongLong() )
        return mdl->getType(BasicType::INT32);
    else
        return mdl->getType(BasicType::INT64);
}

bool Evaluator::binaryOp(quint8 op)
{
    err.clear();
    if( stack.size() < 2 )
    {
        err = "expecting two values on the stack";
        return false;
    }
    Value rhs = stack.takeLast();
    Value lhs = stack.takeLast();

    if( !(lhs.isConst() && rhs.isConst()) )
    {
        if( lhs.isConst() )
            pushMilStack(lhs);
        if( rhs.isConst() )
            pushMilStack(rhs);
    }

    switch( op )
    {
    // Arith
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::Add:
    case Expression::Sub:
        stack.push_back(arithOp(op,lhs,rhs));
        break;
    // Logic
    case Expression::And:
    case Expression::Or:
        stack.push_back(logicOp(op,lhs,rhs));
        break;
    // Relation
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
        stack.push_back(relationOp(op, lhs,rhs));
       break;
    case Expression::In:
        stack.push_back(inOp(lhs,rhs));
        break;
    }
    return err.isEmpty();
}

bool Evaluator::prepareRhs(Type* lhs)
{
    err.clear();
    if( stack.size() < 1 )
    {
        err = "expecting a value on the stack";
        return false;
    }

    const Value& rhs = stack.back();

    // make sure also a string literal is put on the stack by value
    if( lhs && lhs->form == Type::Array && lhs->base->form == BasicType::CHAR &&
            rhs.type->form == BasicType::String )
    {   // NOTE: already checked that lhs is large enough for rhs
        Q_ASSERT( lhs->len > quint32(dequote(rhs.val.toByteArray()).size()) );
        out->ldobj_(toDesig(lhs));
    }else if( lhs && lhs->form == BasicType::CHAR &&
              rhs.type->form == BasicType::String )
        out->ldc_i4(quint8(dequote(rhs.val.toByteArray())[0]));
    else if( lhs && lhs->form == Type::Proc &&
             rhs.mode == Value::Procedure )
        out->ldproc_(toDesig(rhs.val.value<Declaration*>()));
    else
        assureTopOnMilStack();

    return true;
}

bool Evaluator::assign()
{
    err.clear();
    if( stack.size() < 2 )
    {
        err = "expecting two values on the stack";
        return false;
    }

    prepareRhs( stack[stack.size()-2].type );

    Value rhs = stack.takeLast();
    Value lhs = stack.takeLast();

    if( !lhs.ref )
        return false; // error reported elsewhere

    adjustNumType(rhs.type,lhs.type);

    if( lhs.type->form == Type::Array )
    {
        if( lhs.type->base->form == BasicType::CHAR )
        {
            // special case both sides char arrays, just copy up to and including zero
            if( rhs.type->form == Type::Array && rhs.type->base->form == BasicType::CHAR )
            {
                Q_ASSERT(rhs.ref);
                out->call_("$MIC$strcopy",2);
                return err.isEmpty();
            }
        } // else copy memory block TODO
    }

    switch(lhs.type->form)
    {
    case BasicType::BOOLEAN:
    case BasicType::CHAR:
    case BasicType::UINT8:
    case BasicType::INT8:
        out->stind_(MilEmitter::I1);
        break;
    case BasicType::UINT16:
    case BasicType::INT16:
        out->stind_(MilEmitter::I2);
        break;
    case BasicType::UINT32:
    case BasicType::INT32:
    case BasicType::SET:
    case Type::ConstEnum:
        out->stind_(MilEmitter::I4);
        break;
    case BasicType::UINT64:
    case BasicType::INT64:
        out->stind_(MilEmitter::I8);
        break;
    case BasicType::REAL:
        out->stind_(MilEmitter::R4);
        break;
    case BasicType::LONGREAL:
        out->stind_(MilEmitter::R8);
        break;
    case BasicType::Nil:
    case BasicType::String:
    case Type::Pointer:
    case Type::Proc:
        out->stind_(MilEmitter::IntPtr);
        break;
    case Type::Record:
    case Type::Array:
        out->stobj_(toDesig(lhs.type));
        break;
    default:
        Q_ASSERT( false );
    }

    return err.isEmpty();
}

bool Evaluator::derefPointer()
{
    err.clear();
    if( stack.isEmpty() )
    {
        err = "expecting a value on the stack";
        return false;
    }
    assureTopIsValue();
    Value v = stack.takeLast();

    Q_ASSERT( v.type && v.type->form == Type::Pointer && !v.ref );

    v.type = v.type->base;
    v.ref = true;

    stack.push_back(v);

    return err.isEmpty();
}

bool Evaluator::derefValue()
{
    err.clear();
    if( stack.isEmpty() )
    {
        err = "expecting a value on the stack";
        return false;
    }
    Value v = stack.takeLast();
    if( v.isConst() || !v.ref || v.type == 0 )
        return false;
    Q_ASSERT(!v.isConst());
    Q_ASSERT(v.ref && v.type);
    v.ref = false;
    switch(v.type->form)
    {
    case BasicType::BOOLEAN:
    case BasicType::CHAR:
    case BasicType::UINT8:
        out->ldind_(MilEmitter::U1);
        break;
    case BasicType::UINT16:
        out->ldind_(MilEmitter::U2);
        break;
    case BasicType::UINT32:
    case BasicType::SET:
        out->ldind_(MilEmitter::U4);
        break;
    case BasicType::UINT64:
        out->ldind_(MilEmitter::U8);
        break;
    case BasicType::INT8:
        out->ldind_(MilEmitter::I1);
        break;
    case BasicType::INT16:
        out->ldind_(MilEmitter::I2);
        break;
    case BasicType::INT32:
    case Type::ConstEnum:
        out->ldind_(MilEmitter::I4);
        break;
    case BasicType::INT64:
        out->ldind_(MilEmitter::I8);
        break;
    case BasicType::REAL:
        out->ldind_(MilEmitter::R4);
        break;
    case BasicType::LONGREAL:
        out->ldind_(MilEmitter::R8);
        break;
    case Type::Pointer:
    case Type::Proc:
        out->ldind_(MilEmitter::IntPtr);
        break;
    case Type::Record:
        out->ldobj_(toDesig(v.type));
        break;
    case Type::Array:
        // error was already reported: Q_ASSERT( v.type->len );
        out->ldobj_(toDesig(v.type));
        break;
    default:
        return false;
    }

    stack.push_back(v);
    return err.isEmpty();
}

void Evaluator::assureTopIsValue()
{
    if( stack.isEmpty() )
        return;
    Value& v = stack.back();
    if( v.ref )
    {
        derefValue();
        v.ref = false;
    }
}

bool Evaluator::desigField(Declaration* field, bool byVal)
{
    err.clear();
    if( stack.size() < 1 )
    {
        err = "expecting a value on the stack";
        return false;
    }
    Value lhs = stack.takeLast(); // record reference

    // TODO: desig const record

    Q_ASSERT(lhs.ref && lhs.type && lhs.type->form == Type::Record);

    Q_ASSERT(field);
    const QByteArray desig = toDesig(lhs.type) + "." + field->name;
    if( byVal )
        out->ldfld_(desig);
    else
        out->ldflda_(desig);

    Value res;
    res.ref = !byVal;
    res.type = field->type;
    res.visi = 0;
    res.mode = Value::Val;
    stack.push_back(res);

    return err.isEmpty();
}

bool Evaluator::desigVar(bool byVal)
{
    err.clear();
    if( stack.isEmpty() )
    {
        err = "expecting a value on the stack";
        return false;
    }
    Value v = stack.takeLast();

    if( byVal )
        switch( v.mode )
        {
        case Value::VarDecl:
            out->ldvar_(v.val.toByteArray());
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
            out->ldvara_(v.val.toByteArray());
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

bool Evaluator::desigIndex(bool byVal)
{
    err.clear();

    Value rhs = stack.takeLast(); // index
    Value lhs = stack.takeLast(); // array reference

    Q_ASSERT(lhs.ref && lhs.type && lhs.type->form == Type::Array);

    if( rhs.isConst() )
    {
        const qint64 idx = rhs.val.toLongLong();
        if( idx < 0  || rhs.type->len && rhs.type->len <= idx )
        {
            err = "index out of range";
            return false;
        }
        pushMilStack(rhs);
    }

    const QByteArray elemType = toDesig(lhs.type->base);
    if( byVal )
        out->ldelem_(elemType);
    else
        out->ldelema_(elemType);

    Value res;
    res.ref = !byVal;
    res.type = lhs.type->base;
    res.visi = 0;
    res.mode = Value::Val;
    stack.push_back(res);

    return err.isEmpty();
}

bool Evaluator::call(int nArgs)
{
    err.clear();
    if( stack.size() < nArgs + 1 )
    {
        err = QString("expecting %1 values on the stack").arg(nArgs+1);
        return false;
    }

    const Value& callee = stack[stack.size()-nArgs-1];
    Type* ret = 0;
    switch( callee.mode )
    {
    case Value::Builtin:
        {
            Builtins bi(this);
            bi.callBuiltin(callee.val.toInt(),nArgs);
            return err.isEmpty();
        }

    case Value::Procedure:
        {
            Declaration* proc = callee.val.value<Declaration*>();
            Q_ASSERT(proc);
            ret = proc->type;
            out->call_(toDesig(proc),nArgs, ret != 0); // TODO: desig in imported module
        }
        break;
    case Value::VarDecl:
    case Value::LocalDecl:
    case Value::ParamDecl:
    case Value::Val:
        ret = callee.type->base;
        if( callee.type->form == Type::Proc )
        {
            out->calli_(toDesig(callee.type), nArgs, ret != 0);
            break;
        }
        // else fall through
    default:
        err = "this expression cannot be called";
        break;
    }

    for( int i = 0; i < nArgs; i++ )
        stack.pop_back();
    stack.pop_back(); // callee

    Value tmp;
    tmp.mode = Value::Val;
    if( ret )
        tmp.type = ret;
    else
        tmp.type = mdl->getType(BasicType::NoType);
    stack.push_back(tmp);

    return err.isEmpty();
}

bool Evaluator::castPtr(Type* to)
{
    err.clear();
    if( stack.size() < 1 )
    {
        err = "expecting a value on the stack";
        return false;
    }
    Value lhs = stack.takeLast(); // object
    // TODO
    Q_ASSERT(to && to->form == Type::Pointer);
    out->castptr_(toDesig(to->base));
    // TODO restrict to pointers, add to MIL
    lhs.type = to;
    stack.push_back(lhs);
    return err.isEmpty();
}

bool Evaluator::castNum(Type* to)
{
    Q_ASSERT( to && to->isNumber() );
    err.clear();
    if( stack.size() < 1 )
    {
        err = "expecting a value on the stack";
        return false;
    }
    if( stack.back().isConst() )
        stack.back().type = to;
    else
    {
        MilEmitter::ToType tt;
        switch(to->form)
        {
        case BasicType::BOOLEAN:
        case BasicType::CHAR:
        case BasicType::UINT8:
            tt = MilEmitter::ToU1;
            break;
        case BasicType::UINT16:
            tt = MilEmitter::ToU2;
            break;
        case BasicType::UINT32:
        case BasicType::SET:
            tt = MilEmitter::ToU4;
            break;
        case BasicType::UINT64:
            tt = MilEmitter::ToU8;
            break;
        case BasicType::INT8:
            tt = MilEmitter::ToI1;
            break;
        case BasicType::INT16:
            tt = MilEmitter::ToI2;
            break;
        case BasicType::INT32:
            tt = MilEmitter::ToI4;
            break;
        case BasicType::INT64:
            tt = MilEmitter::ToI8;
            break;
        case BasicType::REAL:
            tt = MilEmitter::ToR4;
            break;
        case BasicType::LONGREAL:
            tt = MilEmitter::ToR8;
            break;
        default:
            Q_ASSERT(false);
        }

        out->conv_(tt);
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

void Evaluator::assureTopOnMilStack(bool pop)
{
    if( top().isConst() )
        pushMilStack(top());
    if(pop)
        this->pop();
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

bool Evaluator::pushMilStack(const Value& v)
{
    err.clear();
    switch( v.mode )
    {
    case Value::Const:
        if( v.type->isSimple() || v.type->form == Type::ConstEnum )
        {
            switch( v.type->form )
            {
            case BasicType::String:
                {
                    const QByteArray bytes = v.val.toByteArray();
                    const QString str = QString::fromLatin1(bytes);
                    if(isAllPrintable(str))
                    {
                        if( str.contains('\"') )
                            out->ldstr_( "'" + bytes + "'" );
                        else
                            out->ldstr_( "\"" + bytes + "\"" );
                    }else
                    {
                        out->ldstr_( "#" + bytes.toHex() + "00#" );
                    }
                }
                break;
            case BasicType::Nil:
                out->ldnull_();
                break;
            case BasicType::BOOLEAN:
            case BasicType::CHAR:
            case BasicType::UINT8:
            case BasicType::UINT16:
            case BasicType::UINT32:
            case BasicType::SET:
                out->ldc_i4(v.val.toUInt());
                break;
            case BasicType::UINT64:
                out->ldc_i8(v.val.toULongLong());
                break;
            case BasicType::INT8:
            case BasicType::INT16:
            case BasicType::INT32:
            case Type::ConstEnum:
                out->ldc_i4(v.val.toInt());
                break;
            case BasicType::INT64:
                out->ldc_i8(v.val.toLongLong());
                break;
            case BasicType::REAL:
                out->ldc_r4(v.val.toFloat());
                break;
            case BasicType::LONGREAL:
                out->ldc_r8(v.val.toDouble());
                break;
            default:
                Q_ASSERT(false);
            }
        }else
        {
            MilObject obj;
            obj.typeRef = toDesig(v.type);
            obj.data = v.val;
            out->ldc_obj(obj);
        }
        break;
    case Value::VarDecl:
    case Value::LocalDecl:
        {
            // TODO
        }
        break;
    case Value::ParamDecl:
        {
            // TODO
        }
        break;
    default:
        err = "not supported for this kind of declaration";
        return false;
    }
    return true;
}

void Evaluator::emitRelOp(quint8 op, bool unsig)
{
    switch(op)
    {
    case Expression::Eq:
        out->ceq_();
        break;
    case Expression::Neq:
        out->ceq_();
        out->not_();
        break;
    case Expression::Geq: // not lt
        out->clt_(unsig);
        out->not_();
        break;
    case Expression::Gt:
        out->cgt_(unsig);
        break;
    case Expression::Leq: // not gt
        out->cgt_(unsig);
        out->not_();
        break;
    case Expression::Lt:
        out->clt_(unsig);
        break;
    default:
        Q_ASSERT(false);
        break;
    }
}

void Evaluator::emitArithOp(quint8 op, bool unsig, bool i64)
{
    switch(op)
    {
    case Expression::Mul:
        out->mul_();
        break;
    case Expression::Fdiv:
        out->div_();
        break;
    case Expression::Div:
        if( unsig )
            out->div_(unsig);
        else
        {
            if( i64 )
                out->call_("$MIC$Div64",2,1);
            else
                out->call_("$MIC$Div32",2,1);
        }
        break;
    case Expression::Mod:
        if( unsig )
            out->rem_(unsig);
        else
        {
            if( i64 )
                out->call_("$MIC$Mod64",2,1);
            else
                out->call_("$MIC$Mod32",2,1);
        }
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
    if( me->isNumber() && other->isNumber() )
    {
        if( me->isInt() && other->isInt() &&
                other->form == BasicType::INT64 && me->form < BasicType::INT64 )
            out->conv_(MilEmitter::ToI8);
        else if( me->isUInt() && other->isUInt() &&
                 other->form == BasicType::UINT64 && me->form < BasicType::UINT64 )
            out->conv_(MilEmitter::ToU8);
        else if( me->isReal() && other->isReal() &&
                 other->form == BasicType::LONGREAL && me->form < BasicType::LONGREAL )
            out->conv_(MilEmitter::ToR8);
    }
}

void Evaluator::notOp(Value& v)
{
    Q_ASSERT( v.type->isBoolean() );
    if( v.isConst() )
        v.val = !v.val.toBool();
    else
        out->not_();
}

Value Evaluator::logicOp(quint8 op, const Value& lhs, const Value& rhs)
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
            res.mode = Value::Val;
            res.type = lhs.type;
            // order is irrelevant for logic ops
            if( op == Expression::And)
                out->and_();
            else
                out->or_();
        }
    }else
        Q_ASSERT(false);
    return res;
}

static inline Type* maxType(Type* lhs, Type* rhs)
{
    if( lhs->form >= rhs->form )
        return lhs;
    else
        return rhs;
}

Value Evaluator::arithOp(quint8 op, const Value& lhs, const Value& rhs)
{
    Value res;
    res.mode = Value::Val;
    res.type = lhs.type;

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
                emitArithOp(op,false, res.type->form == BasicType::INT64 );
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
                emitArithOp(op,true, res.type->form == BasicType::UINT64);
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
                emitArithOp(op);
            }
        }else
            Q_ASSERT(false);
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
            switch(op)
            {
            case Expression::Mul:
                out->and_();
                break;
            case Expression::Fdiv:
                out->call_("$MIC$SetDiv",2,1);
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
    }else if( (lhs.type->form == BasicType::String || lhs.type->form == BasicType::CHAR) &&
              (rhs.type->form == BasicType::String || rhs.type->form == BasicType::CHAR) )
    {
        // + only
        res.type = mdl->getType(BasicType::String);
        Q_ASSERT( op == Expression::Add );
        Q_ASSERT( lhs.isConst() && rhs.isConst() );
        res.val = lhs.val.toByteArray() + rhs.val.toByteArray();
    }else
        Q_ASSERT(false);

    return res;
}

Value Evaluator::relationOp(quint8 op, const Value& lhs, const Value& rhs)
{
    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(BasicType::BOOLEAN);

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
                    Q_ASSERT(false);
                    break;
                }
            }else
            {
                emitRelOp(op,false);
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
                    Q_ASSERT(false);
                    break;
                }
            }else
            {
                emitRelOp(op,false);
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
                    Q_ASSERT(false);
                    break;
                }
            }else
            {
                emitRelOp(op,false);
            }
        }else
            Q_ASSERT(false);
    }else if( lhs.type->isText() && rhs.type->isText() )
    {
        switch(op)
        {
        case Expression::Geq:
            out->ldc_i4(6);
            break;
        case Expression::Gt:
            out->ldc_i4(5);
            break;
        case Expression::Eq:
            out->ldc_i4(1);
            break;
        case Expression::Leq:
            out->ldc_i4(4);
            break;
        case Expression::Neq:
            out->ldc_i4(2);
            break;
        case Expression::Lt:
            out->ldc_i4(3);
            break;
        default:
            Q_ASSERT(false);
            break;
        }
        if( lhs.type->form == BasicType::CHAR && rhs.type->form == BasicType::CHAR )
            out->call_("$MIC$relop4",3,true);
        else if( lhs.type->form == BasicType::CHAR )
            out->call_("$MIC$relop3",3,true);
        else if( rhs.type->form == BasicType::CHAR )
            out->call_("$MIC$relop2",3,true);
        else
            out->call_("$MIC$relop1",3,true);
    }else if( lhs.type->form == Type::Pointer && rhs.type->form == Type::Pointer ||
              lhs.type->form == Type::Pointer && rhs.type->form == BasicType::Nil ||
              lhs.type->form == BasicType::Nil && rhs.type->form == Type::Pointer ||
              lhs.type->form == BasicType::Nil && rhs.type->form == BasicType::Nil )
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
                Q_ASSERT(false);
                break;
            }
        }else
        {
            emitRelOp(op,true);
        }
    }else if( lhs.type->form == Type::ConstEnum  && rhs.type->form == Type::ConstEnum )
    {
        Q_ASSERT( lhs.type == rhs.type );
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
                Q_ASSERT(false);
                break;
            }
        }else
        {
            emitRelOp(op,false);
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
                Q_ASSERT(false);
            }
        }else
        {
            emitRelOp(op,true);
        }
    }else if( lhs.type->form == Type::Proc && rhs.type->form == Type::Proc ||
              lhs.type->form == Type::Proc && rhs.type->form == BasicType::Nil ||
              lhs.type->form == BasicType::Nil && rhs.type->form == Type::Proc )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            // TODO: actual generic module parameters could be proc type literals
        }else
        {
            emitRelOp(op,true);
        }
    }else
        Q_ASSERT(false);

    return res;
}

Value Evaluator::inOp(const Value& lhs, const Value& rhs)
{
    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(BasicType::BOOLEAN);

    if( lhs.type->isInteger() && rhs.type->isSet() )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            const int index = lhs.val.toInt();
            if( index < 0 || index > 31 )
                err = "left side must be between 0 and 31";
            else
            {
                const quint32 set = rhs.val.toUInt();
                res.val = ((1 << index) & set) != 0;
            }
            res.mode = Value::Const;
        }else
        {
            out->calli_("$MIC$SetIn",2,1);
        }

    }else
        err = "operation not compatible with given operands";
    return res;
}

void Evaluator::unaryMinusOp(Value& v)
{
    if( v.type->isNumber() )
    {
        if( v.type->isUInt() )
        {
            if( v.isConst() )
                v.val = -v.val.toLongLong();
            else
            {
                if( v.type->form == BasicType::INT32 )
                    out->conv_(MilEmitter::ToI4);
                else
                    out->conv_(MilEmitter::ToI8);
                out->neg_();
            }
        }else if( v.type->isInteger() )
        {
            if( v.isConst() )
                v.val = -v.val.toLongLong();
            else
                out->neg_();
        }else if( v.type->isReal() )
        {
            if( v.isConst() )
                v.val = -v.val.toDouble();
            else
                out->neg_();
        }else
            Q_ASSERT(false);
    }else if( v.type->isSet() )
    {
        if( v.isConst() )
            v.val = ~v.val.toUInt();
        else
            out->not_();
    }else
        Q_ASSERT(false);
}

void Evaluator::unaryPlusOp(Value& v)
{
    // NOP
}

QByteArray Evaluator::toDesig(Declaration* d)
{
    QByteArrayList path;
    bool isImported = false;
    while( d && d->mode != Declaration::Module )
    {
        path.push_front(d->name);
        if( d->mode == Declaration::VarDecl ||
                d->mode == Declaration::LocalDecl ||
                d->mode == Declaration::ParamDecl )
            break;
        if( d->mode == Declaration::Import )
            isImported = true;
        d = d->outer;
    }
    if( isImported )
        return path.join('.'); // here the imported names cannot include '$' because only top-level declarations
    else
        return path.join('$');
}

QByteArray Evaluator::toDesig(Type* t)
{
    if( t == 0 )
        return QByteArray();

    if( t->isSimple() )
    {
        switch( t->form )
        {
        case BasicType::BOOLEAN:
            return "bool";
        case BasicType::CHAR:
            return "char";
        case BasicType::UINT8:
            return "uint8";
        case BasicType::UINT16:
            return "uint16";
        case BasicType::UINT32:
        case BasicType::SET:
            return "uint32";
        case BasicType::UINT64:
            return "uint64";
        case BasicType::INT8:
            return "int8";
        case BasicType::INT16:
            return "int16";
        case BasicType::INT32:
            return "int32";
        case BasicType::INT64:
            return "int64";
        case BasicType::REAL:
            return "float32";
        case BasicType::LONGREAL:
            return "float64";
        default:
            return "<invalid type>";
        }
    }else if( t->decl)
    {
        Q_ASSERT( t && t->decl );
        return toDesig(t->decl);
    }else if( t->form == Type::NameRef )
        return toDesig(t->subs.first());
    return QByteArray();
}

QByteArray Evaluator::dequote(const QByteArray& str)
{
    if( str.startsWith('\'') && str.endsWith('\'') ||
            str.startsWith('"') && str.endsWith('"') )
        return str.mid(1,str.size()-2);
    else
        return str;
}

void Evaluator::recursiveRun(Expression* e)
{
    switch( e->kind )
    {
    case Expression::Plus:
    case Expression::Minus:
    case Expression::Not: // Unary
        recursiveRun(e->lhs);
        unaryOp(e->kind);
        break;
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::In: // Relation
    case Expression::Add:
    case Expression::Sub:
    case Expression::Or: // AddOp
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::And: // MulOp
        recursiveRun(e->lhs);
        recursiveRun(e->rhs);
        binaryOp(e->kind);
        break;
    case Expression::Select:
        recursiveRun(e->lhs);
        desigField(e->val.value<Declaration*>(), e->byVal);
        break;
    case Expression::Index:
        recursiveRun(e->lhs);
        recursiveRun(e->rhs);
        desigIndex(e->byVal);
        break;
    case Expression::Cast:
        recursiveRun(e->lhs);
        castPtr(e->type);
        break;
    case Expression::AutoCast:
        recursiveRun(e->lhs);
        castNum(e->type);
        break;
    case Expression::Deref:
        recursiveRun(e->lhs);
        derefPointer();
        // TODO: derefValue?
        break;
    case Expression::Addr:
        recursiveRun(e->lhs);
        stack.back().type = e->type; // NOP otherwise
        break;
    case Expression::Literal:
        stack.push_back(Value(e->type,e->val,Value::Const));
        break;
    case Expression::LocalVar:
        stack.push_back(Value(e->type,e->val,Value::LocalDecl));
        desigVar(e->byVal);
        break;
    case Expression::Param:
        stack.push_back(Value(e->type,e->val,Value::ParamDecl));
        desigVar(e->byVal);
        break;
    case Expression::ModuleVar:
        stack.push_back(Value(e->type,e->val,Value::VarDecl));
        desigVar(e->byVal);
        break;
    case Expression::ProcDecl:
        stack.push_back(Value(e->type,e->val,Value::Procedure));
        break;
    case Expression::Builtin:
        stack.push_back(Value(e->type,e->val,Value::Builtin));
        break;
    case Expression::ConstDecl:
        {
            Value tmp;
            tmp.mode = Value::Const;
            tmp.val = e->val.value<Declaration*>()->data;
            tmp.type = e->type;
            stack.push_back(tmp);
        }
        break;
    case Expression::Call:
        {
            recursiveRun(e->lhs);
            ExpList args = e->val.value<ExpList>();
            const DeclList formals = e->lhs->getFormals();
            for(int i = 0; i < args.size(); i++ )
            {
                recursiveRun(args[i]);
                if( i < formals.size() )
                    prepareRhs(formals[i]->type);
                else
                    assureTopOnMilStack();
            }
            call(args.size());
        }
        break;

    case Expression::Range:
        // TODO
        break;
    case Expression::Set:
        // TODO
        break;

    case Expression::Invalid:
    case Expression::TypeDecl:
        Q_ASSERT(false);
        break;
    }
}
