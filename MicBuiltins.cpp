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

#include "MicBuiltins.h"
#include "MicEvaluator.h"
#include "MicMilEmitter.h"
#include "MicToken.h"
using namespace Mic;

static inline MilQuali coreName(const QByteArray& proc)
{
    MilQuali res;
    res.first = Token::getSymbol("$MIC");
    res.second = Token::getSymbol(proc);
    return res;
}

static inline void expectingNArgs(const ExpList& args,int n)
{
    if( args.size() != n )
        throw QString("expecting %1 arguments").arg(n);
}

static inline void expectingNMArgs(const ExpList& args,int n, int m)
{
    if( args.size() < n || args.size() > m)
        throw QString("expecting %1 to %2 arguments").arg(n).arg(m);
}

static inline Expression* createAutoCast(Expression* e, Type* t)
{
    Expression* tmp = Expression::create(Expression::AutoCast,e->pos);
    tmp->setType(t);
    tmp->lhs = e;
    return tmp;
}

static void checkBitArith(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl)
{
    expectingNArgs(args,2);
    if( !args[0]->getType()->isUInt() )
        throw QString("expecing unsigned first argument");
    if( !args[1]->getType()->isUInt() )
        throw QString("expecing unsigned second argument");
    Type* lhs = args[0]->getType();
    Type* rhs = args[1]->getType();
    if( lhs->kind < Type::UINT32 )
        lhs = mdl->getType(Type::UINT32);
    if( rhs->kind < Type::UINT32 )
        rhs = mdl->getType(Type::UINT32);
    if( lhs->kind < rhs->kind )
        lhs = rhs;
    else if( lhs->kind > rhs->kind )
        rhs = lhs;
    if( lhs != args[0]->getType() )
        args[0] = createAutoCast(args[0], lhs);
    if( rhs != args[1]->getType() )
        args[1] = createAutoCast(args[1], rhs);
    *ret = args[0]->getType();
}

static void checkBitShift(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl)
{
    expectingNArgs(args,2);
    if( builtin == Builtin::BITASR )
    {
        if( !args[0]->getType()->isInt() )
            throw QString("expecing unsigned first argument");
    }else
    {
        if( !args[0]->getType()->isUInt() )
            throw QString("expecing unsigned first argument");
    }
    if( !args[1]->getType()->isUInt() )
        throw QString("expecing unsigned second argument");

    if( builtin == Builtin::BITASR )
    {
        if( args[0]->getType()->kind < Type::INT32 )
            args[0] = createAutoCast(args[0], mdl->getType(Type::INT32) );
    }else
    {
        if( args[0]->getType()->kind < Type::UINT32 )
            args[0] = createAutoCast(args[0], mdl->getType(Type::UINT32) );
    }
    if( args[1]->getType()->kind < Type::UINT32 )
        args[1] = createAutoCast(args[1], mdl->getType(Type::UINT32) );

    *ret = args[0]->getType();
}

QString Builtins::checkArgs(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl)
{
    Q_ASSERT(ret);

    *ret = mdl->getType(Type::NoType);

    // TODO:
    try
    {
    switch(builtin)
    {
    // functions:
    case Builtin::ABS:
        expectingNArgs(args,1);
        if( !args.first()->getType()->isNumber() )
            throw "expecting numeric argument";
        *ret = args.first()->getType();
        break;
    case Builtin::CAP:
        expectingNArgs(args,1);
        break;
    case Builtin::BITAND:
        checkBitArith(builtin, args, ret, mdl);
        break;
    case Builtin::BITASR:
        checkBitShift(builtin, args, ret, mdl);
        break;
    case Builtin::BITNOT:
        expectingNArgs(args,1);
        if( !args.first()->getType()->isUInt() )
            throw "expecting unsigned integer";
        if( args.first()->getType()->kind < Type::UINT32 )
            args[0] = createAutoCast(args[0], mdl->getType(Type::UINT32) );
        *ret = args[0]->getType();
        break;
    case Builtin::BITOR:
        checkBitArith(builtin, args, ret, mdl);
        break;
    case Builtin::BITS:
        expectingNArgs(args,1);
        break;
    case Builtin::BITSHL:
        checkBitShift(builtin, args, ret, mdl);
        break;
    case Builtin::BITSHR:
        checkBitShift(builtin, args, ret, mdl);
        break;
    case Builtin::BITXOR:
        checkBitArith(builtin, args, ret, mdl);
        break;
    case Builtin::CAST:
        expectingNArgs(args,2);
        break;
    case Builtin::CHR:
        expectingNArgs(args,1);
        break;
    case Builtin::DEFAULT:
        expectingNArgs(args,1);
       break;
    case Builtin::FLOOR:
        expectingNArgs(args,1);
        break;
    case Builtin::FLT:
        expectingNArgs(args,1);
        if( !args.first()->getType()->isInt() )
            throw "expecting signed integer argument";
        if( args.first()->getType()->kind == Type::INT64 )
            *ret = ev->mdl->getType(Type::FLT64);
        else
            *ret = ev->mdl->getType(Type::FLT32);
        break;
    case Builtin::GETENV:
        expectingNArgs(args,2);
        break;
    case Builtin::LEN:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::UINT32);
        break;
    case Builtin::LONG:
        expectingNArgs(args,1);
        break;
    case Builtin::MAX:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::MIN:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::ODD:
        expectingNArgs(args,1);
        break;
    case Builtin::ORD:
        expectingNArgs(args,1);
        break;
    case Builtin::SHORT:
        expectingNArgs(args,1);
        break;
    case Builtin::SIGNED:
        expectingNArgs(args,1);
        switch(args.first()->getType()->kind)
        {
        case Type::UINT8:
            *ret = ev->mdl->getType(Type::INT8);
            break;
        case Type::UINT16:
            *ret = ev->mdl->getType(Type::INT16);
            break;
        case Type::UINT32:
            *ret = ev->mdl->getType(Type::INT32);
            break;
        case Type::UINT64:
            *ret = ev->mdl->getType(Type::INT64);
            break;
        default:
            throw "expecting unsigned integer";
        }
        break;
    case Builtin::SIZE:
        expectingNArgs(args,1);
        break;
    case Builtin::STRLEN:
        expectingNArgs(args,1);
        break;
    case Builtin::UNSIGNED:
        expectingNArgs(args,1);
        switch(args.first()->getType()->kind)
        {
        case Type::INT8:
            *ret = ev->mdl->getType(Type::UINT8);
            break;
        case Type::INT16:
            *ret = ev->mdl->getType(Type::UINT16);
            break;
        case Type::INT32:
            *ret = ev->mdl->getType(Type::UINT32);
            break;
        case Type::INT64:
            *ret = ev->mdl->getType(Type::UINT64);
            break;
        default:
            throw "expecting signed integer";
        }
        break;
    case Builtin::VARARG:
        expectingNMArgs(args,2,3);
        break;
    case Builtin::VARARGS:
        expectingNArgs(args,0);
        break;

    // procedures:
    case Builtin::ASSERT:
        expectingNArgs(args,1);
        break;
    case Builtin::DEC:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::DISPOSE:
        expectingNArgs(args,1);
        break;
    case Builtin::EXCL:
        expectingNArgs(args,2);
        break;
    case Builtin::HALT:
        expectingNArgs(args,1);
        break;
    case Builtin::INC:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::INCL:
        expectingNArgs(args,2);
        break;
    case Builtin::NEW:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::PCALL:
        break;
    case Builtin::PRINT:
        expectingNArgs(args,1);
        break;
    case Builtin::PRINTLN:
        expectingNArgs(args,1);
       break;
    case Builtin::RAISE:
        expectingNArgs(args,1);
        break;
    case Builtin::SETENV:
        expectingNArgs(args,2);
        break;
    }
    }catch( const QString& err )
    {
        return err;
    }catch( const char* str)
    {
        return str;
    }

    return QString();
}

bool Builtins::requiresLvalue(quint8 builtin, quint8 arg)
{
    switch( builtin )
    {
    case Builtin::NEW:
    case Builtin::INC:
    case Builtin::DEC:
    case Builtin::EXCL:
    case Builtin::INCL:
    case Builtin::PCALL:
        if( arg == 0 )
            return true;
        break;
    }
    return false;
}

Builtins::Builtins(Evaluator* ev):ev(ev)
{
    Q_ASSERT(ev);
}

void Builtins::bitarith(int op)
{
    Value rhs = ev->stack.takeLast();
    Value lhs = ev->stack.takeLast();

    //Q_ASSERT( lhs.type == rhs.type );

    Value res = lhs;
    if( lhs.isConst() && rhs.isConst() )
    {
        switch(op)
        {
        case Builtin::BITAND:
            res.val = lhs.val.toULongLong() & rhs.val.toULongLong();
            break;
        case Builtin::BITOR:
            res.val = lhs.val.toULongLong() | rhs.val.toULongLong();
            break;
        case Builtin::BITXOR:
            if( lhs.type->kind = Type::UINT32 )
                res.val = lhs.val.toUInt() ^ rhs.val.toUInt();
            else
                res.val = lhs.val.toULongLong() ^ rhs.val.toULongLong();
            break;
        default:
            Q_ASSERT(false);
        }
    }else
    {
#if 0
        // no, args were already pushed in Evaluator::recursiveRun Expression::Call
        if( lhs.isConst() )
            ev->pushMilStack(lhs);
        if( rhs.isConst() )
            ev->pushMilStack(rhs);
#endif

        switch(op)
        {
        case Builtin::BITAND:
            ev->out->and_();
            break;
        case Builtin::BITOR:
            ev->out->or_();
            break;
        case Builtin::BITXOR:
            ev->out->xor_();
            break;
        default:
            Q_ASSERT(false);
        }
        res.mode = Value::Val;
    }
    ev->stack.push_back(res);
}

void Builtins::bitnot()
{
    Value v = ev->stack.takeLast();

    if( v.isConst() )
    {
        if( v.type->kind == Type::UINT32 )
            v.val = ~v.val.toUInt();
        else
            v.val = ~v.val.toULongLong();
    }else
    {
        ev->out->not_();
        v.mode = Value::Val;
    }
    ev->stack.push_back(v);
}

void Builtins::doSigned()
{
    Value v = ev->stack.takeLast();
    Type* t;
    MilEmitter::Type tt;
    switch(v.type->kind)
    {
    case Type::UINT8:
        t = ev->mdl->getType(Type::INT8);
        tt = MilEmitter::I1;
        break;
    case Type::UINT16:
        t = ev->mdl->getType(Type::INT16);
        tt = MilEmitter::I2;
        break;
    case Type::UINT32:
    default:
        t = ev->mdl->getType(Type::INT32);
        tt = MilEmitter::I4;
        break;
    case Type::UINT64:
        t = ev->mdl->getType(Type::INT64);
        tt = MilEmitter::I8;
        break;
    }
    v.type = t;
    if( !v.isConst() )
    {
        ev->out->conv_(tt);
    }
    ev->stack.push_back(v);
}

void Builtins::doUnsigned()
{
    Value v = ev->stack.takeLast();
    Type* t;
    MilEmitter::Type tt;
    switch(v.type->kind)
    {
    case Type::INT8:
        t = ev->mdl->getType(Type::UINT8);
        tt = MilEmitter::U1;
        break;
    case Type::INT16:
        t = ev->mdl->getType(Type::UINT16);
        tt = MilEmitter::U2;
        break;
    case Type::INT32:
    default:
        t = ev->mdl->getType(Type::UINT32);
        tt = MilEmitter::U4;
        break;
    case Type::INT64:
        t = ev->mdl->getType(Type::UINT64);
        tt = MilEmitter::U8;
        break;
    }
    v.type = t;
    if( !v.isConst() )
    {
        ev->out->conv_(tt);
    }
    ev->stack.push_back(v);
}

void Builtins::doAbs()
{
    static int count = 0;
    Value v = ev->stack.takeLast();
    if( v.isConst() )
    {
        if( v.type->isInteger() )
            v.val = qAbs(v.val.toLongLong() );
        else
            v.val = qAbs(v.val.toDouble() );
    }else if( v.type->isInt() || v.type->isReal() )
    {
        ev->out->dup_();
        if( v.type->kind == Type::INT64 )
            ev->out->ldc_i8(0);
        else if( v.type->kind == Type::FLT64 )
            ev->out->ldc_r8(0);
        else if( v.type->isInt() )
            ev->out->ldc_i4(0);
        else
            ev->out->ldc_r4(0);
        ev->out->cgt_(); // push v > 0, i.e. jump if v < 0
        const QByteArray label = "$ABS" + QByteArray::number(count++);
        ev->out->ifgoto_(label);
        ev->out->neg_();
        ev->out->label_(label);
    }
    ev->stack.push_back(v);
}

void Builtins::doFlt()
{
    Value v = ev->stack.takeLast();
    Q_ASSERT(v.type->isInt());
    if( v.type->kind == Type::INT64 )
    {
        v.type = ev->mdl->getType(Type::FLT64);
        if( v.isConst() )
            v.val = (double)v.val.toLongLong();
        else
            ev->out->conv_(MilEmitter::R8);
    }else
    {
        v.type = ev->mdl->getType(Type::FLT32);
        if( v.isConst() )
            v.val = (double)v.val.toLongLong();
        else
            ev->out->conv_(MilEmitter::R4);
    }
    ev->stack.push_back(v);
}

void Builtins::doShiftRight()
{
    Value rhs = ev->stack.takeLast();
    Value lhs = ev->stack.takeLast();
    if( lhs.type->isInt() )
    {
        if( lhs.isConst() )
            lhs.val = lhs.val.toLongLong() >> rhs.val.toUInt();
        else
            ev->out->shr_();
    }else
    {
        if( lhs.isConst() )
            lhs.val = lhs.val.toULongLong() >> rhs.val.toUInt();
        else
            ev->out->shr_(true);
    }
    ev->stack.push_back(lhs);
}

void Builtins::doShiftLeft()
{
    Value rhs = ev->stack.takeLast();
    Value lhs = ev->stack.takeLast();
    if( lhs.isConst() )
        lhs.val = lhs.val.toULongLong() << rhs.val.toUInt();
    else
        ev->out->shl_();
    ev->stack.push_back(lhs);
}

void Builtins::checkNumOfActuals(int nArgs, int min, int max)
{
    Q_ASSERT( max == 0 || min <= max );
    if( min == max || max == 0 )
    {
        if( nArgs != min )
            throw QString("expecting exactly %1 arguments").arg(min);
    }else
    {
        if( nArgs < min || nArgs > max )
            throw QString("expecting %1 to %2 arguments").arg(min).arg(max);
    }
}

void Builtins::ASSERT(int nArgs)
{
    Value file = ev->stack.takeLast();
    Value line = ev->stack.takeLast();
    Value cond = ev->stack.takeLast();

#if 0
    // no, args were already pushed in Evaluator::recursiveRun Expression::Call
    if( cond.isConst() )
        ev->pushMilStack(cond);
    if( line.isConst() )
        ev->pushMilStack(line);
    if( file.isConst() )
        ev->pushMilStack(file);
#endif

    if( cond.type->kind != Type::BOOL )
    {
        ev->err = "expecting boolean first argument";
        return;
    }
    if( !line.type->isInteger() )
    {
        ev->err = "expecting integer second argument";
        return;
    }
    if( !file.type->isText() )
    {
        ev->err = "expecting string third argument";
        return;
    }

    ev->out->call_(coreName("assert"),3);

    Value res;
    res.mode = Value::Val;
    res.type = ev->mdl->getType(Type::NoType);
    ev->stack.push_back(res);
}

void Builtins::incdec(int nArgs, bool inc)
{
    Value step;
    int tmp = -1;
    if( nArgs == 2 )
    {
        step = ev->stack.takeLast();
        if( !step.isConst() )
        {
            tmp = addIncDecTmp();
            ev->out->stloc_(tmp); // store second argument to temporary, remove it from ev->stack
        }
    }
    Value what = ev->stack.takeLast();

    if( !what.isLvalue() && !what.ref )
    {
        ev->err = "cannot write to first argument";
        return;
    }

    if( what.type->isInteger() )
    {
        if( what.type->kind == Type::UINT64 || what.type->kind == Type::INT64 )
        {
            ev->out->dup_();
            ev->out->ldind_(what.type->kind == Type::UINT64 ? MilEmitter::U8 : MilEmitter::I8);
            if( nArgs == 2 )
            {
                if( step.isConst() )
                    ev->out->ldc_i8(step.val.toInt());
                else
                {
                    ev->out->ldloc_(tmp);
                    ev->out->conv_(MilEmitter::I8);
                }
            }else
                ev->out->ldc_i8(1);
            if( inc )
                ev->out->add_();
            else
                ev->out->sub_();
            ev->out->stind_(what.type->kind == Type::UINT64 ? MilEmitter::U8 : MilEmitter::I8);
        }else
        {
            ev->out->dup_();
            ev->out->ldind_(what.type->isUInt() ? MilEmitter::U4 : MilEmitter::I4);
            if( nArgs == 2 )
            {
                if( step.isConst() )
                    ev->out->ldc_i4(step.val.toInt());
                else
                    ev->out->ldloc_(tmp);
            }else
                ev->out->ldc_i4(1);
            if( inc )
                ev->out->add_();
            else
                ev->out->sub_();
            ev->out->stind_(what.type->isUInt() ? MilEmitter::U4 : MilEmitter::I4);
        }
    }else if( what.type->kind == Type::ConstEnum )
    {
        if( nArgs == 2 )
        {
            ev->err = "second argument not supported for const enumerations";
            return;
        }
        ev->out->dup_();
        ev->out->ldind_(MilEmitter::I4);
        ev->out->ldc_i4(1);
        if( inc )
            ev->out->add_();
        else
            ev->out->sub_();
        ev->out->stind_(MilEmitter::I4);
        // TODO: check overflow and halt?
        // TODO: do we expect more enums than fit in I4?
    }else if( what.type->kind == Type::Pointer )
    {
        ev->out->dup_();
        ev->out->ldind_(MilEmitter::IntPtr);
        if( nArgs == 2 )
        {
            if( step.isConst() )
                ev->out->ldc_i4(step.val.toInt());
            else
                ev->out->ldloc_(tmp);
        }
        ev->out->ptroff_(ev->toQuali(what.type->getType()));
        ev->out->stind_(MilEmitter::IntPtr);
    }else
        ev->err = "invalid argument types";
}

void Builtins::INC(int nArgs)
{
    incdec(nArgs,true);
}

void Builtins::DEC(int nArgs)
{
    incdec(nArgs,false);
}

void Builtins::LEN(int nArgs)
{
    Value what = ev->stack.takeLast();
    if( !what.isConst() )
        ev->out->pop_();
    Type* arr = what.type;
    if( arr->kind == Type::Pointer )
        arr = arr->getType();
    if( arr->kind != Type::Array || arr->len == 0 )
    {
        ev->err = "function only applicable to non-open arrays";
        return;
    }
    //ev->out->ldc_i4(arr->len);
    Value res;
    res.mode = Value::Const;
    res.type = ev->mdl->getType(Type::UINT32);
    res.val = arr->len;
    ev->stack.push_back(res);
}

void Builtins::PRINT(int nArgs, bool ln)
{
    if( nArgs < 1 || ev->stack.back().type == 0 ||
            !(ev->stack.back().type->isSimple() || ev->stack.back().type->isText() ))
    {
        ev->err = "expecting one argument of basic or char array type";
        return;
    }
    if( ev->stack.back().type->kind == Type::ConstEnum )
    {
        ev->out->conv_(MilEmitter::I8);
        ev->out->call_(coreName("printI8"),1,false);
    }else if( ev->stack.back().type->isInt() )
    {
        if( ev->stack.back().type->kind != Type::INT64 )
            ev->out->conv_(MilEmitter::I8);
        ev->out->call_(coreName("printI8"),1,false);
    }else if( ev->stack.back().type->isUInt() )
    {
        if( ev->stack.back().type->kind != Type::UINT64 )
            ev->out->conv_(MilEmitter::U8);
        ev->out->call_(coreName("printU8"),1,false);
    }else if( ev->stack.back().type->isReal() )
    {
        if( ev->stack.back().type->kind != Type::FLT64 )
            ev->out->conv_(MilEmitter::R8);
        ev->out->call_(coreName("printF8"),1,false);
    }else if( ev->stack.back().type->isText() )
    {
        // TODO: do we really accept array of string by value?
        if( ev->stack.back().type->kind != Type::CHAR )
            ev->out->call_(coreName("printStr"),1,false);
        else
            ev->out->call_(coreName("printCh"),1,false);
    }else if( ev->stack.back().type->isBoolean() )
        ev->out->call_(coreName("printBool"),1,false);
    else if( ev->stack.back().type->isSet() )
        ev->out->call_(coreName("printSet"),1,false);
    else
        ev->err = "given type not supported with PRINT or PRINTLN";
    if( ln )
    {
        ev->out->ldc_i4(0xa); // LF
        ev->out->call_(coreName("printCh"),1,false);
    }
}

void Builtins::NEW(int nArgs)
{
    Value len;
    if( nArgs == 2 )
        len = ev->stack.takeLast();
    Value what = ev->stack.takeLast();

    if( what.type == 0 || what.type->getType() == 0 )
        return; // already reported

    if( what.type->kind != Type::Pointer &&
            !(what.type->getType()->kind == Type::Record || what.type->getType()->kind == Type::Object ||
              what.type->getType()->kind == Type::Array) )
    {
        ev->err = "first argument must be a pointer to record or array";
        return;
    }
    if( !what.ref )
    {
        ev->err = "cannot write to first argument";
        return;
    }
    if( what.type->getType()->kind == Type::Record || what.type->getType()->kind == Type::Object )
    {
        ev->out->newobj_(ev->toQuali(what.type->getType()));
        ev->out->stind_(MilEmitter::IntPtr);
    }else if( what.type->getType()->len > 0 ) // fixed size array
    {
        if( nArgs > 1 )
        {
            ev->err = "cannot dynamically set array length for non-open array";
            return;
        }
        ev->out->ldc_i4(what.type->getType()->len);
        ev->out->newarr_(ev->toQuali(what.type->getType()->getType()));
        ev->out->stind_(MilEmitter::IntPtr);
    }else // open array
    {
        if( nArgs != 2 )
        {
            ev->err = "expecting two arguments, the second as the explicit length";
            return;
        }
        ev->out->newarr_(ev->toQuali(what.type->getType()->getType()));
        ev->out->stind_(MilEmitter::IntPtr);
    }
}

void Builtins::DISPOSE(int nArgs)
{
    Value what = ev->stack.takeLast();
    if( what.type->kind != Type::Pointer &&
            !(what.type->getType()->kind == Type::Record || what.type->getType()->kind == Type::Object ||
              what.type->getType()->kind == Type::Array) )
    {
        ev->err = "argument must be a pointer to record or array";
        return;
    }

    ev->out->free_();

}

void Builtins::callBuiltin(quint8 builtin, int nArgs)
{
    Value ret;
    ret.mode = Value::Val;
    ret.type = ev->mdl->getType(Type::NoType);
    bool handleStack = true;
    try
    {
    switch( builtin )
    {
    case Builtin::PRINT:
    case Builtin::PRINTLN:
        checkNumOfActuals(nArgs, 1);
        PRINT(nArgs,builtin == Builtin::PRINTLN);
        break;
    case Builtin::NEW:
        checkNumOfActuals(nArgs, 1, 2);
        NEW(nArgs);
        handleStack = false;
        break;
    case Builtin::DISPOSE:
        checkNumOfActuals(nArgs, 1);
        DISPOSE(nArgs);
        handleStack = false;
        break;
    case Builtin::INC:
        checkNumOfActuals(nArgs, 1, 2);
        INC(nArgs);
        handleStack = false;
        break;
    case Builtin::DEC:
        checkNumOfActuals(nArgs, 1, 2);
        DEC(nArgs);
        handleStack = false;
        break;
    case Builtin::LEN:
        checkNumOfActuals(nArgs, 1);
        LEN(nArgs);
        handleStack = false;
        break;
    case Builtin::ASSERT:
        checkNumOfActuals(nArgs, 3);
        ASSERT(nArgs);
        handleStack = false;
        break;
    case Builtin::BITAND:
    case Builtin::BITOR:
    case Builtin::BITXOR:
        checkNumOfActuals(nArgs, 2);
        bitarith(builtin);
        handleStack = false;
        break;
    case Builtin::BITNOT:
        checkNumOfActuals(nArgs, 1);
        bitnot();
        handleStack = false;
        break;
    case Builtin::BITASR:
    case Builtin::BITSHR:
        doShiftRight();
        handleStack = false;
        break;
    case Builtin::BITSHL:
        doShiftLeft();
        handleStack = false;
        break;
    case Builtin::SIGNED:
        checkNumOfActuals(nArgs, 1);
        doSigned();
        handleStack = false;
        break;
    case Builtin::UNSIGNED:
        checkNumOfActuals(nArgs, 1);
        doUnsigned();
        handleStack = false;
        break;
    case Builtin::ABS:
        checkNumOfActuals(nArgs, 1);
        doAbs();
        handleStack = false;
        break;
    case Builtin::FLT:
        checkNumOfActuals(nArgs, 1);
        doFlt();
        handleStack = false;
        break;
    default:
        throw QString("built-in not yet implemented");
        break;
    }
    }catch(const QString& str)
    {
        ev->err = str;
    }

    if( handleStack )
    {
        for( int i = 0; i < nArgs; i++ )
            ev->stack.pop_back();
        ev->stack.push_back(ret);
    }
}

int Builtins::addIncDecTmp()
{
    bool doublette;
    Declaration* decl = ev->mdl->addDecl(Token::getSymbol("$incdec"),&doublette);
    if( !doublette )
    {
        decl->kind = Declaration::LocalDecl;
        decl->setType(ev->mdl->getType(Type::INT32));
        decl->outer = ev->mdl->getTopScope();
        decl->id = ev->out->addLocal(ev->toQuali(decl->getType()),decl->name);
    }
    return decl->id;
}

