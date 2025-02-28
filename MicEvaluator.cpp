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
#include <QtDebug>
using namespace Mic;

bool Evaluator::evaluate(Expression* e, bool assureOnMilStack)
{
    err.clear();
    if( e == 0 )
        return false;
    if( !recursiveRun(e) )
        return false;
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

    if( rhs.mode == Value::TypeDecl )
    {
        err = "a type declaration cannot be used as a value";
        return false;
    }

    // make sure also a string literal is put on the stack by value
    if( lhs && lhs->kind == Type::Array && lhs->getType()->kind == BasicType::CHAR &&
            rhs.type->kind == BasicType::String )
    {   // NOTE: already checked that lhs is large enough for rhs
        Q_ASSERT( lhs->len >= quint32(dequote(rhs.val.toByteArray()).size()) );
        assureTopOnMilStack();
        out->ldobj_(MilQuali());
    }else if( lhs && lhs->kind == BasicType::CHAR &&
              rhs.type->kind == BasicType::String )
        out->ldc_i4(quint8(dequote(rhs.val.toByteArray())[0]));
    else if( lhs && lhs->kind == Type::Proc &&
             rhs.mode == Value::Procedure )
        out->ldproc_(toQuali(rhs.val.value<Declaration*>()));
    else
        assureTopOnMilStack();

    return true;
}

static inline MilQuali coreName(const QByteArray& proc)
{
    MilQuali res;
    res.first = Token::getSymbol("$MIC");
    res.second = Token::getSymbol(proc);
    return res;
}

bool Evaluator::assign()
{
    err.clear();
    if( stack.size() < 2 )
    {
        err = "expecting two values on the stack";
        return false;
    }

    if( !prepareRhs( stack[stack.size()-2].type ) )
        return false;

    Value rhs = stack.takeLast();
    Value lhs = stack.takeLast();

    if( !lhs.ref )
        return false; // error reported elsewhere

    adjustNumType(rhs.type,lhs.type);

    if( lhs.type->kind == Type::Array )
    {
        if( lhs.type->getType()->kind == BasicType::CHAR )
        {
            // special case both sides char arrays, just copy up to and including zero
            if( rhs.type->kind == Type::Array && rhs.type->getType()->kind == BasicType::CHAR )
            {
                Q_ASSERT(rhs.ref);
                out->call_(coreName("strcopy"),2);
                return err.isEmpty();
            }
        } // else copy memory block TODO
    }

    switch(lhs.type->kind)
    {
    case BasicType::BOOL:
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
    case BasicType::FLT32:
        out->stind_(MilEmitter::R4);
        break;
    case BasicType::FLT64:
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
    case Type::Generic:
        out->stobj_(toQuali(lhs.type));
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

    Q_ASSERT( v.type && v.type->kind == Type::Pointer && !v.ref );

    v.type = v.type->getType();
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
    switch(v.type->kind)
    {
    case BasicType::BOOL:
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
    case BasicType::FLT32:
        out->ldind_(MilEmitter::R4);
        break;
    case BasicType::FLT64:
        out->ldind_(MilEmitter::R8);
        break;
    case Type::Pointer:
    case Type::Proc:
        out->ldind_(MilEmitter::IntPtr);
        break;
    case Type::Record:
    case Type::Array:
    case Type::Generic:
        out->ldobj_(toQuali(v.type));
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
        derefValue();
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

    Q_ASSERT(lhs.ref && lhs.type && lhs.type->kind == Type::Record);

    Q_ASSERT(field);
    const MilTrident desig = qMakePair(toQuali(lhs.type),field->name);
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

bool Evaluator::desigIndex(bool byVal)
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
        if( idx < 0  || rhs.type->len && rhs.type->len <= idx )
        {
            err = "index out of range";
            return false;
        }
        pushMilStack(rhs);
    }

    const Qualident elemType = toQuali(lhs.type->getType());
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

bool Evaluator::call(int nArgs)
{
    err.clear();
    if( stack.size() < nArgs + 1 )
    {
        err = QString("expecting %1 values on the stack").arg(nArgs+1);
        return false;
    }

    Value callee = stack.takeLast();
    if( callee.type == 0 )
        callee.type = mdl->getType(BasicType::NoType);

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
            ret = proc->getType();
            out->call_(toQuali(proc),nArgs, ret != 0); // TODO: desig in imported module
        }
        break;
    case Value::VarDecl:
    case Value::LocalDecl:
    case Value::ParamDecl:
    case Value::Val:
        ret = callee.type->getType();
        if( callee.type->kind == Type::Proc )
        {
            out->calli_(toQuali(callee.type), nArgs, ret != 0);
            break;
        }
        // else fall through
    default:
        err = "this expression cannot be called";
        break;
    }

    for( int i = 0; i < nArgs; i++ )
        stack.pop_back();

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
    Q_ASSERT(to && to->kind == Type::Pointer);
    out->castptr_(toQuali(to->getType()));
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
        MilEmitter::Type tt;
        switch(to->kind)
        {
        case BasicType::BOOL:
        case BasicType::CHAR:
        case BasicType::UINT8:
            tt = MilEmitter::U1;
            break;
        case BasicType::UINT16:
            tt = MilEmitter::U2;
            break;
        case BasicType::UINT32:
        case BasicType::SET:
            tt = MilEmitter::U4;
            break;
        case BasicType::UINT64:
            tt = MilEmitter::U8;
            break;
        case BasicType::INT8:
            tt = MilEmitter::I1;
            break;
        case BasicType::INT16:
            tt = MilEmitter::I2;
            break;
        case BasicType::INT32:
            tt = MilEmitter::I4;
            break;
        case BasicType::INT64:
            tt = MilEmitter::I8;
            break;
        case BasicType::FLT32:
            tt = MilEmitter::R4;
            break;
        case BasicType::FLT64:
            tt = MilEmitter::R8;
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
    {
        Value v = stack.takeLast();
        pushMilStack(v);
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
        binaryOp(e->kind);
    }else
    {
        // p and q : if p then q, else FALSE
        out->iif_();
        recursiveRun(e->lhs);
        assureTopOnMilStack(true);
        out->then_();
        recursiveRun(e->rhs);
        assureTopOnMilStack(); // leave the bool result on the stack
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
        binaryOp(e->kind);
    }else
    {
        // p or q : if p then TRUE, else q
        out->iif_();
        recursiveRun(e->lhs);
        assureTopOnMilStack(true);
        out->then_();
        out->ldc_i4(1); // push TRUE
        out->else_();
        recursiveRun(e->rhs);
        assureTopOnMilStack(); // leave the bool result on the stack
        out->end_();
    }
#endif
}

bool Evaluator::pushMilStack(const Value& v)
{
    err.clear();
    switch( v.mode )
    {
    case Value::Const:
        if( v.type->isSimple() || v.type->kind == Type::ConstEnum )
        {
            switch( v.type->kind )
            {
            case BasicType::String:
                out->ldstr_( v.val.toByteArray() );
                break;
            case BasicType::Nil:
                out->ldnull_();
                break;
            case BasicType::BOOL:
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
            case BasicType::FLT32:
                out->ldc_r4(v.val.toFloat());
                break;
            case BasicType::FLT64:
                out->ldc_r8(v.val.toDouble());
                break;
            default:
                Q_ASSERT(false);
            }
        }else
        {
            MilObject obj;
            obj.typeRef = toQuali(v.type);
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
                other->kind == BasicType::INT64 && me->kind < BasicType::INT64 )
            out->conv_(MilEmitter::I8);
        else if( me->isUInt() && other->isUInt() &&
                 other->kind == BasicType::UINT64 && me->kind < BasicType::UINT64 )
            out->conv_(MilEmitter::U8);
        else if( me->isReal() && other->isReal() &&
                 other->kind == BasicType::FLT64 && me->kind < BasicType::FLT64 )
            out->conv_(MilEmitter::R8);
    }
}

void Evaluator::notOp(Value& v)
{
    Q_ASSERT( v.type->isBoolean() );
    if( v.isConst() )
        v.val = !v.val.toBool();
    else
    {
        // logic not
        out->ldc_i4(0);
        out->ceq_();
    }
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
    if( lhs->kind >= rhs->kind )
        return lhs;
    else
        return rhs;
}

Value Evaluator::arithOp(quint8 op, const Value& lhs, const Value& rhs)
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
                emitArithOp(op,false, res.type->kind == BasicType::INT64 );
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
                emitArithOp(op,true, res.type->kind == BasicType::UINT64);
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
            err = "operation not supported";
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
    }else if( (lhs.type->kind == BasicType::String || lhs.type->kind == BasicType::CHAR) &&
              (rhs.type->kind == BasicType::String || rhs.type->kind == BasicType::CHAR) )
    {
        // + only
        res.type = mdl->getType(BasicType::String);
        Q_ASSERT( op == Expression::Add );
        Q_ASSERT( lhs.isConst() && rhs.isConst() );
        res.val = lhs.val.toByteArray() + rhs.val.toByteArray();
    }else
        err = "operation not supported";

    return res;
}

Value Evaluator::relationOp(quint8 op, const Value& lhs, const Value& rhs)
{
    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(BasicType::BOOL);

    if( lhs.type == 0 || rhs.type == 0 )
        return res;

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
                    return res; // Q_ASSERT(false);
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
                    return res; // Q_ASSERT(false);
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
                    return res; // Q_ASSERT(false);
                    break;
                }
            }else
            {
                emitRelOp(op,false);
            }
        }else
            return res; // Q_ASSERT(false);
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
            return res; // Q_ASSERT(false);
            break;
        }
        if( lhs.type->kind == BasicType::CHAR && rhs.type->kind == BasicType::CHAR )
            out->call_(coreName("relop4"),3,true);
        else if( lhs.type->kind == BasicType::CHAR )
            out->call_(coreName("relop3"),3,true);
        else if( rhs.type->kind == BasicType::CHAR )
            out->call_(coreName("relop2"),3,true);
        else
            out->call_(coreName("relop1"),3,true);
    }else if( lhs.type->kind == Type::Pointer && rhs.type->kind == Type::Pointer ||
              lhs.type->kind == Type::Pointer && rhs.type->kind == BasicType::Nil ||
              lhs.type->kind == BasicType::Nil && rhs.type->kind == Type::Pointer ||
              lhs.type->kind == BasicType::Nil && rhs.type->kind == BasicType::Nil )
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
                return res; // Q_ASSERT(false);
                break;
            }
        }else
        {
            emitRelOp(op,true);
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
                return res; // Q_ASSERT(false);
            }
        }else
        {
            emitRelOp(op,true);
        }
    }else if( lhs.type->kind == Type::Proc && rhs.type->kind == Type::Proc ||
              lhs.type->kind == Type::Proc && rhs.type->kind == BasicType::Nil ||
              lhs.type->kind == BasicType::Nil && rhs.type->kind == Type::Proc )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            // TODO: actual generic module parameters could be proc type literals
        }else
        {
            emitRelOp(op,true);
        }
    }else
        return res;
        //Q_ASSERT(false);

    return res;
}

Value Evaluator::inOp(const Value& lhs, const Value& rhs)
{
    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(BasicType::BOOL);

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
            out->calli_(coreName("SetIn"),2,1);
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
                if( v.type->kind == BasicType::INT32 )
                    out->conv_(MilEmitter::I4);
                else
                    out->conv_(MilEmitter::I8);
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

Qualident Evaluator::toQuali(Declaration* d)
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
        if( d->outer == 0 && last == 0 )
            return toQuali(d->getType()); // this is a built-in type
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
    if( d == mdl->getTopModule() )
        return qMakePair(QByteArray(),desig); // local symbol
    // else imported symbol
    ModuleData md = d->data.value<ModuleData>();
    return qMakePair(md.fullName, desig);
}

Qualident Evaluator::toQuali(Type* t)
{
    static QByteArray symbols[BasicType::Max];

    if( t == 0 )
        return Qualident();

    if( t->isSimple() )
    {
        if( symbols[BasicType::Any].isEmpty() )
        {
            symbols[BasicType::Any] = Token::getSymbol("any");
            symbols[BasicType::Nil] = Token::getSymbol("nil");
            symbols[BasicType::BOOL] = Token::getSymbol("bool");
            symbols[BasicType::CHAR] = Token::getSymbol("char");
            symbols[BasicType::UINT8] = Token::getSymbol("uint8");
            symbols[BasicType::UINT16] = Token::getSymbol("uint16");
            symbols[BasicType::UINT32] = symbols[BasicType::SET] = Token::getSymbol("uint32");
            symbols[BasicType::UINT64] = Token::getSymbol("uint64");
            symbols[BasicType::INT8] = Token::getSymbol("int8");
            symbols[BasicType::INT16] = Token::getSymbol("int16");
            symbols[BasicType::INT32] = Token::getSymbol("int32");
            symbols[BasicType::INT64] = Token::getSymbol("int64");
            symbols[BasicType::FLT32] = Token::getSymbol("float32");
            symbols[BasicType::FLT64] = Token::getSymbol("float64");
        }
        return qMakePair(QByteArray(),symbols[t->kind]);
    }else if( t->decl)
    {
        Q_ASSERT( t && t->decl );
        return toQuali(t->decl);
    }else if( t->kind == Type::NameRef )
        return toQuali(t->subs.first());
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
    case Expression::Sub: // AddOp
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod: // MulOp
        if( !recursiveRun(e->lhs) )
            return false;
        if( e->lhs->isConst() && !e->rhs->isConst() )
            assureTopOnMilStack();
        if( !recursiveRun(e->rhs) )
            return false;
        if( !e->lhs->isConst() && e->rhs->isConst() )
            assureTopOnMilStack();
        binaryOp(e->kind);
        break;
    case Expression::Or:
        shortCircuitOr(e);
        break;
    case Expression::And:
        shortCircuitAnd(e);
        break;
    case Expression::Select:
        if( !recursiveRun(e->lhs) )
            return false;
        desigField(e->val.value<Declaration*>(), e->byVal);
        break;
    case Expression::Index:
        if( !recursiveRun(e->lhs) )
            return false;
        if( !recursiveRun(e->rhs) )
            return false;
        desigIndex(e->byVal);
        break;
    case Expression::Cast:
        if( !recursiveRun(e->lhs) )
            return false;
        castPtr(e->getType());
        break;
    case Expression::AutoCast:
        if( !recursiveRun(e->lhs) )
            return false;
        castNum(e->getType());
        break;
    case Expression::Deref:
        if( !recursiveRun(e->lhs) )
            return false;
        derefPointer();
        // TODO: derefValue?
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
        desigVar(e->byVal);
        break;
    case Expression::Param:
        stack.push_back(Value(e->getType(),e->val,Value::ParamDecl));
        desigVar(e->byVal);
        break;
    case Expression::ModuleVar:
        stack.push_back(Value(e->getType(),e->val,Value::VarDecl));
        desigVar(e->byVal);
        break;
    case Expression::ProcDecl:
        stack.push_back(Value(e->getType(),e->val,Value::Procedure));
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
            ExpList args = e->val.value<ExpList>();
            const DeclList formals = e->lhs->getFormals();
            for(int i = 0; i < args.size(); i++ )
            {
                if( !recursiveRun(args[i]) )
                    return false;
                if( i < formals.size() )
                    prepareRhs(formals[i]->getType());
                else
                    assureTopOnMilStack(); // effects builtin args and variable args
            }
            if( !recursiveRun(e->lhs) )
                return false;
            call(args.size());
        }
        break;

    case Expression::Range:
    case Expression::NameValue:
    case Expression::IndexValue:
        break;
    case Expression::Constructor:
        constructor(e);
        break;

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
    // else
    err = "dynamic constructors not yet supported by code generator";
}

void Evaluator::recurseConstConstructor(Expression* e)
{
    switch( e->getType()->kind )
    {
    case Type::Record: {
            MilRecordLiteral rec;
            Expression* c = e->rhs;
            while( c )
            {
                Q_ASSERT( c->kind == Expression::NameValue );
                if( !evaluate(c->rhs) )
                    return;
                Value v = stack.takeLast();
                Q_ASSERT( v.isConst() );
                rec.append(qMakePair(c->val.value<Declaration*>()->name, v.val));
                c = c->next;
            }
            Value v;
            v.mode = Value::Const;
            v.val = QVariant::fromValue(rec);
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    case Type::Array: {
            Q_ASSERT( e->getType()->len > 0 );
            QVector<QVariant> arr(e->getType()->len);
            Expression* c = e->rhs;
            while( c )
            {
                Q_ASSERT( c->kind == Expression::IndexValue );
                if( !evaluate(c->rhs) )
                    return;
                Value v = stack.takeLast();
                Q_ASSERT( v.isConst() );
                arr[c->val.toLongLong()] = v.val;
                c = c->next;
            }
            Value v;
            v.mode = Value::Const;
            v.val = QVariant::fromValue(arr.toList());
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    case Type::Pointer: {
            Q_ASSERT(e->rhs);
            Value v;
            v.mode = Value::Const;
            v.val = e->rhs->val;
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    case BasicType::SET: {
            std::bitset<32> set;
            Expression* c = e->rhs;
            while( c )
            {
                if( c->kind == Expression::Range )
                {
                    if( !evaluate(c->lhs) )
                        return;
                    const qint64 lhs = stack.takeLast().val.toLongLong();
                    if( !evaluate(c->rhs) )
                        return;
                    const qint64 rhs = stack.takeLast().val.toLongLong();
                    if( lhs < 0 || lhs >= set.size() )
                    {
                        err = QString("element %1 out of range").arg(lhs);
                        return;
                    }else if( rhs < 0 || rhs >= set.size() )
                    {
                        err = QString("element %1 out of range").arg(rhs);
                        return;
                    }else
                    {
                        if( lhs <= rhs )
                            for( int i = lhs; i <= rhs; i++ )
                            {
                                if( set.test(i) )
                                {
                                    err = QString("element %1 already included").arg(i);
                                    return;
                                }else
                                    set.set(i);
                            }
                        else
                            for( int i = rhs; i <= lhs; i++ )
                            {
                                if( set.test(i) )
                                {
                                    err = QString("element %1 already included").arg(i);
                                    return;
                                }else
                                    set.set(i);
                            }
                    }
                }else
                {
                    if( !evaluate(c) )
                        return;
                    const qint64 i = stack.takeLast().val.toLongLong();
                    if( i < 0 || i >= set.size() )
                    {
                        err = QString("element %1 out of range").arg(i);
                        return;
                    }else if(set.test(i))
                    {
                        err = QString("element %1 already included").arg(i);
                        return;
                    }else
                        set.set(i);
                }
                c = c->next;
            }
            Value v;
            v.mode = Value::Const;
            v.val = set.to_ullong();
            v.type = e->getType();
            stack.push_back(v);
            break;
        }
    }
}
