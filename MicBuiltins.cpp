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
#include "MilEmitter.h"
#include "MicToken.h"
using namespace Mic;

static inline Mil::Quali coreName(const QByteArray& proc)
{
    Mil::Quali res;
    res.first = Token::getSymbol("MIC$");
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

static void checkBitArith(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl, Evaluator* ev)
{
    expectingNArgs(args,2);

    Type* lhs = args[0]->getType();
    Type* rhs = args[1]->getType();

    if( lhs == 0 || rhs == 0 )
        return;

    bool signed_ = false;
    if( lhs->kind == Type::UniInt && rhs->kind == Type::UniInt )
        ;
    else if( lhs->kind == Type::UniInt )
        signed_ = rhs->isInt();
    else if( rhs->kind == Type::UniInt )
        signed_ = lhs->isInt();

    ev->bindUniInt(args[0], signed_);
    ev->bindUniInt(args[1], signed_);

    lhs = args[0]->getType();
    rhs = args[1]->getType();

    if( lhs->isInt() && rhs->isInt() )
        *ret = lhs->is64() || rhs->is64() ? mdl->getType(Type::INT64) : mdl->getType(Type::INT32);
    else
        *ret = lhs->is64() || rhs->is64() ? mdl->getType(Type::UINT64) : mdl->getType(Type::UINT32);

    if( lhs->kind >= Type::UINT8 && lhs->kind < Type::UINT32 )
        lhs = mdl->getType(Type::UINT32);
    if( lhs->kind >= Type::INT8 && lhs->kind < Type::INT32 )
        lhs = mdl->getType(Type::INT32);

    if( rhs->kind >= Type::UINT8 && rhs->kind < Type::UINT32 )
        rhs = mdl->getType(Type::UINT32);
    if( rhs->kind >= Type::INT8 && rhs->kind < Type::INT32 )
        rhs = mdl->getType(Type::INT32);

    if( *ret != args[0]->getType() )
        args[0] = Evaluator::createAutoConv(args[0], *ret);
    if( *ret != args[1]->getType() )
        args[1] = Evaluator::createAutoConv(args[1], *ret);
}

static void checkBitShift(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl, Evaluator* ev)
{
    expectingNArgs(args,2);
    if( builtin == Builtin::ASR )
    {
        ev->bindUniInt(args[0], true);
        if( !args[0]->getType()->isInt() )
            throw QString("expecing unsigned first argument");
    }else
    {
        ev->bindUniInt(args[0], false);
        // accept both signed and unsigned
    }
    ev->bindUniInt(args[1], false);
    if( !args[1]->getType()->isUInt() )
        throw QString("expecing unsigned second argument");

    if( builtin == Builtin::ASR )
    {
        if( args[0]->getType()->kind < Type::INT32 )
            args[0] = Evaluator::createAutoConv(args[0], mdl->getType(Type::INT32) );
    }else
    {
        if( args[0]->getType()->kind < Type::UINT32 )
            args[0] = Evaluator::createAutoConv(args[0], mdl->getType(Type::UINT32) );
    }
    if( args[1]->getType()->kind < Type::UINT32 )
        args[1] = Evaluator::createAutoConv(args[1], mdl->getType(Type::UINT32) );

    *ret = args[0]->getType();
}

QString Builtins::checkArgs(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl)
{
    Q_ASSERT(ret);

    *ret = mdl->getType(Type::NoType);

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
    case Builtin::BAND:
        checkBitArith(builtin, args, ret, mdl, ev);
        break;
    case Builtin::ASR:
        checkBitShift(builtin, args, ret, mdl, ev);
        break;
    case Builtin::BNOT: {
        expectingNArgs(args,1);
        ev->bindUniInt(args.first(), false);
        Type* t = args.first()->getType();
        if( t->kind >= Type::UINT8 && t->kind < Type::UINT32 )
            *ret = mdl->getType(Type::UINT32);
        else if( t->kind >= Type::INT8 && t->kind < Type::INT32 )
            *ret = mdl->getType(Type::INT32);
        else
            *ret = t;
        if( t != *ret )
            args[0] = Evaluator::createAutoConv(args[0], *ret );
        } break;
    case Builtin::BOR:
        checkBitArith(builtin, args, ret, mdl, ev);
        break;
    case Builtin::BSET:
        expectingNArgs(args,1);
        ev->bindUniInt(args.first(), false);
        if( !args.first()->getType()->isUInt() )
            throw "expecting unsigned integer";
        if( args.first()->getType()->kind < Type::UINT32 )
            args[0] = Evaluator::createAutoConv(args[0], mdl->getType(Type::UINT32) );
        *ret =  mdl->getType(Type::SET);
        break;
    case Builtin::SHL:
        checkBitShift(builtin, args, ret, mdl, ev);
        break;
    case Builtin::SHR:
        checkBitShift(builtin, args, ret, mdl, ev);
        break;
    case Builtin::BXOR:
        checkBitArith(builtin, args, ret, mdl, ev);
        break;
    case Builtin::CAST:
        expectingNArgs(args,2);
        if( args.first()->kind != Expression::TypeDecl ||
                !(args.first()->getType()->isNumber() || args.first()->getType()->kind == Type::Pointer) )
            throw QString("expecting declaration of number or pointer type as first argument");
        if( !args.last()->getType()->isNumber() && !args.last()->getType()->kind == Type::Pointer )
            throw QString("expecting a number or pointer type as second argument");
        if( (args.first()->getType()->isNumber() && !args.last()->getType()->isNumber()) ||
            (args.first()->getType()->kind == Type::Pointer && args.last()->getType()->kind != Type::Pointer) )
            throw QString("both arguments must be either number or pointer type");
        if( args.first()->getType()->getByteSize() != args.last()->getType()->getByteSize())
            throw QString("can only cast between types of same byte width");
        *ret = args.first()->getType();
        break;
    case Builtin::VAL:
        expectingNArgs(args,2);
        if( args.first()->kind != Expression::TypeDecl || !(args.first()->getType()->isNumber() || args.first()->getType()->kind == Type::ConstEnum) )
            throw QString("expecting declaration of number or enumeration type as first argument");
        if( !args.last()->getType()->isNumber() )
            throw QString("expecting a number type as second argument");
        if( args.first()->getType()->kind == Type::ConstEnum && !args.last()->getType()->isInteger())
            throw QString("expecting an integer type as second argument");
        *ret = args.first()->getType();
        break;
    case Builtin::CHR:
        expectingNArgs(args,1);
        ev->bindUniInt(args.first(), false);
        if( args.first()->getType() == 0 || !args.first()->getType()->isInteger() )
            throw "expecting integer";
        if( args.first()->getType()->kind != Type::UINT8 )
            args[0] = Evaluator::createAutoConv(args[0], mdl->getType(Type::UINT8) );
        *ret =  mdl->getType(Type::CHAR);
        break;
    case Builtin::DEFAULT:
        expectingNArgs(args,1);
        *ret = args.first()->getType();
       break;
    case Builtin::FLOOR:
        expectingNArgs(args,1);
        if( !args.first()->getType()->isReal() )
            throw "expecting floating point type";
        if( args.first()->getType()->kind != Type::UINT8 )
            args[0] = Evaluator::createAutoConv(args[0], mdl->getType(Type::UINT8) );
        if( args[0]->getType()->kind == Type::FLT64 )
            *ret = mdl->getType(Type::INT64);
        else
            *ret =  mdl->getType(Type::INT32);
        break;
    case Builtin::FLT:
        expectingNArgs(args,1);
        ev->bindUniInt(args.first(), true);
        if( !args.first()->getType()->isInt() )
            throw "expecting signed integer argument";
        if( args.first()->getType()->kind == Type::INT64 )
            *ret = ev->mdl->getType(Type::FLT64);
        else
            *ret = ev->mdl->getType(Type::FLT32);
        break;
    case Builtin::GETENV:
        expectingNArgs(args,2);
        // TODO
        break;
    case Builtin::LEN:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::UINT32);
        break;
    case Builtin::MAX:
        expectingNMArgs(args,1,2);
        ev->bindUniInt(args.first(), false);
        // TODO: check valid types
        if( args.size() == 2 )
        {
            ev->bindUniInt(args.last(), false);
            *ret = Evaluator::maxType(args.first()->getType(),args.last()->getType());
            if( *ret != args.first()->getType() )
                args[0] = Evaluator::createAutoConv(args.first(),*ret);
            if( *ret != args.last()->getType() )
                args[1] = Evaluator::createAutoConv(args.last(),*ret);
        }else
            *ret = args.first()->getType();
        break;
    case Builtin::MIN:
        expectingNMArgs(args,1,2);
        ev->bindUniInt(args.first(), false);
        // TODO: check valid types
        if( args.size() == 2 )
        {
            ev->bindUniInt(args.last(), false);
            *ret = Evaluator::maxType(args.first()->getType(),args.last()->getType());
            if( *ret != args.first()->getType() )
                args[0] = Evaluator::createAutoConv(args.first(),*ret);
            if( *ret != args.last()->getType() )
                args[1] = Evaluator::createAutoConv(args.last(),*ret);
        }else
            *ret = args.first()->getType();
        break;
    case Builtin::ODD:
        expectingNArgs(args,1);
        ev->bindUniInt(args.first(), false);
        if( !args.first()->getType()->isInteger() )
            throw QString("expecting an integer type");
        *ret = ev->mdl->getType(Type::BOOL);
        break;
    case Builtin::ORD:
        expectingNArgs(args,1);
        switch(args.first()->getType()->kind)
        {
        case Type::CHAR:
        case Type::BOOL:
            *ret = ev->mdl->getType(Type::UINT8);
            break;
        case Type::StrLit:
            if( args.first()->strLitLen() == 1 )
                *ret = ev->mdl->getType(Type::UINT8);
            else
                throw "this string literal cannot be interpreted as CHAR";
            break;
        case Type::SET:
            *ret = ev->mdl->getType(Type::UINT32);
            break;
        case Type::ConstEnum:
            *ret = ev->enumFoundationalType(args.first()->getType());
            break;
        case Type::Pointer:
        case Type::Proc:
            *ret = mdl->getType(Type::UINT64); // TODO: adjust to target?
            break;
        default:
            throw "expecting CHAR, BOOLEAN, SET, enumeration or pointer/procedure type";
        }
        break;
    case Builtin::SIG:
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
        *ret = ev->mdl->getType(Type::UINT32);
        break;
    case Builtin::STRLEN: {
            expectingNArgs(args,1);
            Type* t = args.first()->getType();
            bool wasPtr = false;
            if( t->kind == Type::Pointer )
            {
                t = t->getType();
                wasPtr = true;
            }
            if( !t->isCharArray() && t->kind != Type::StrLit && wasPtr && t->kind != Type::CHAR )
                throw "expecting array of char or string literal";
            *ret = mdl->getType(Type::UINT32);
        } break;
    case Builtin::USIG:
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

    // procedures:
        // TODO: complete
    case Builtin::ASSERT:
        expectingNArgs(args,1);
        break;
    case Builtin::DEC:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::DISPOSE:
        expectingNArgs(args,1);
        if( args[0]->getType()->kind != Type::Pointer )
            throw "expecting a pointer argument";
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
        if( args[0]->getType()->kind != Type::Pointer )
            throw "expecting a pointer as the first argument";
        break;
    case Builtin::PCALL:
        break;
    case Builtin::PRINT:
    case Builtin::PRINTLN: {
            expectingNArgs(args,1);
            Q_ASSERT(args.size() == 1);
            Type* t = args.first()->getType();
            if( t && t->kind != Type::Array && t->kind != Type::Record && t->kind != Type::Object )
                args.first()->setByVal();
        } break;
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

    case Builtin::PRINT: // because we need a pointer to array of chars
    case Builtin::PRINTLN:
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
        case Builtin::BAND:
            res.val = lhs.val.toULongLong() & rhs.val.toULongLong();
            break;
        case Builtin::BOR:
            res.val = lhs.val.toULongLong() | rhs.val.toULongLong();
            break;
        case Builtin::BXOR:
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
        // both operands are already on MIL stack, even if one was const
        switch(op)
        {
        case Builtin::BAND:
            ev->out->and_();
            break;
        case Builtin::BOR:
            ev->out->or_();
            break;
        case Builtin::BXOR:
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

void Builtins::doDefault()
{
    Value v = ev->stack.takeLast();

    v.mode = Value::Const;

    if( v.type->isInteger() || v.type->kind == Type::BOOL || v.type->kind == Type::CHAR ||
            v.type->kind == Type::SET || v.type->kind == Type::ConstEnum )
        v.val = 0;
    else if( v.type->isReal() )
        v.val = 0.0;
    else if( v.type->kind == Type::Nil || v.type->kind == Type::Pointer || (v.type->kind == Type::Proc && !v.type->typebound) )
        v.val = 0;
    else if( v.type->kind == Type::Interface || (v.type->kind == Type::Proc && v.type->typebound) )
    {
        Mil::RecordLiteral rec;
        v.val = QVariant::fromValue(rec); // TODO
    }else if( v.type->kind == Type::Record || v.type->kind == Type::Object )
    {
        Mil::RecordLiteral rec;
        v.val = QVariant::fromValue(rec); // TODO
    }else if( v.type->kind == Type::Array )
    {
        QVector<QVariant> arr(1);
        v.val = QVariant::fromValue(arr.toList()); // TODO
    }else
        Q_ASSERT(false);

    ev->stack.push_back(v);
}

void Builtins::doCast(const RowCol &pos)
{
    Value value = ev->stack.takeLast();
    Value type = ev->stack.takeLast();
    Q_ASSERT(type.mode == Value::TypeDecl);
    ev->stack.push_back(value);
    if( type.type->isNumber() )
        ev->castNum(type.type, pos);
    else
        ev->out->castptr_(ev->toQuali(type.type));
    ev->stack.back().type = type.type;
}

void Builtins::doVal(const RowCol &pos)
{
    Value value = ev->stack.takeLast();
    Value type = ev->stack.takeLast();
    Q_ASSERT(type.mode == Value::TypeDecl);
    ev->stack.push_back(value);
    ev->convNum(type.type, pos); // handles const
    ev->stack.back().type = type.type;
}

void Builtins::doAbs()
{
    Value v = ev->stack.takeLast();
    if( v.isConst() )
    {
        if( v.type->isInteger() )
            v.val = qAbs(v.val.toLongLong() );
        else
            v.val = qAbs(v.val.toDouble() );
    }else if( v.type->isInt() || v.type->isReal() )
    {
        ev->out->abs_();
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
            ev->out->conv_(Mil::EmiTypes::R8);
    }else
    {
        v.type = ev->mdl->getType(Type::FLT32);
        if( v.isConst() )
            v.val = (double)v.val.toLongLong();
        else
            ev->out->conv_(Mil::EmiTypes::R4);
    }
    ev->stack.push_back(v);
}

void Builtins::doShiftRight(const RowCol& pos)
{
    Value rhs = ev->stack.takeLast();
    Value lhs = ev->stack.takeLast();
    if( lhs.type->isInt() )
    {
        if( lhs.isConst() && rhs.isConst() )
            lhs.val = lhs.val.toLongLong() >> rhs.val.toUInt();
        else
        {
            lhs.mode = Value::Val;
            ev->out->shr_();
        }
    }else
    {
        if( lhs.isConst() && rhs.isConst() )
            lhs.val = lhs.val.toULongLong() >> rhs.val.toUInt();
        else
        {
            lhs.mode = Value::Val;
            ev->out->shr_(true);
        }
    }
    ev->stack.push_back(lhs);
}

void Builtins::doShiftLeft(const RowCol& pos)
{
    Value rhs = ev->stack.takeLast();
    Value lhs = ev->stack.takeLast();
    if( lhs.isConst() && rhs.isConst() )
        lhs.val = lhs.val.toULongLong() << rhs.val.toUInt();
    else
    {
        // both are already on MIL stack, regardless whether one might be const
        lhs.mode = Value::Val;
        ev->out->shl_();
    }
    ev->stack.push_back(lhs);
}

void Builtins::doOrd(const RowCol& pos)
{
    Value v = ev->stack.takeLast();
    if( v.isConst() )
    {
        switch( v.type->kind )
        {
        case Type::StrLit:
            v.val = (quint8)v.val.toByteArray()[0];
            v.type = ev->mdl->getType(Type::UINT8);
            break;
        case Type::CHAR:
            v.val = (quint8)v.val.toUInt();
            v.type = ev->mdl->getType(Type::UINT8);
            break;
        case Type::ConstEnum:
            v.type = ev->enumFoundationalType(v.type);
            break;
        case Type::BOOL:
            v.val = quint8(v.val.toBool());
            v.type = ev->mdl->getType(Type::UINT8);
            break;
        case Type::SET:
            v.type = ev->mdl->getType(Type::UINT32);
            break;
        case Type::Pointer:
        case Type::Proc:
            v.type = ev->mdl->getType(Type::UINT64); // TODO: target byte width
            break;
        default:
            Q_ASSERT(false);
        }
    }else
    {
        // value is already on MIL stack
        switch( v.type->kind )
        {
        case Type::StrLit:
            v.type = ev->mdl->getType(Type::UINT8);
            break;
        case Type::CHAR:
            v.type = ev->mdl->getType(Type::UINT8);
            break;
        case Type::ConstEnum:
            v.type = ev->enumFoundationalType(v.type);
            break;
        case Type::BOOL:
            v.type = ev->mdl->getType(Type::UINT8);
            break;
        case Type::SET:
            break;
        case Type::Pointer:
        case Type::Proc:
            v.type = ev->mdl->getType(Type::UINT64); // TODO: target byte width
            break;
        default:
            Q_ASSERT(false);
        }
    }
    ev->stack.push_back(v);

}

void Builtins::doSize(const RowCol& pos)
{
    Value v = ev->stack.takeLast();
    if( (v.type->kind == Type::Array && v.type->len == 0) || v.type->kind == Type::StrLit )
    {
        ev->error("function not applicable to open arrays or string literals",pos);
        return;
    }

    if( v.isConst() || v.type->kind < Type::MaxBasicType )
    {
        v.val = v.type->getByteSize();
        v.mode = Value::Const;
    }else
    {
        ev->out->sizeof_(ev->toQuali(v.type));
        v.mode = Value::Val;
    }
    v.type = ev->mdl->getType(Type::UINT32);
    ev->stack.push_back(v);
}

void Builtins::doStrlen(const RowCol &pos)
{
    Value v = ev->stack.takeLast();

    if( v.isConst() )
    {
        Q_ASSERT( v.type->kind == Type::StrLit );
        v.val = (quint32)strlen(v.val.toByteArray().constData() );
    }else
    {
        ev->out->call_(coreName("strlen"),1,true);
    }

    v.type = ev->mdl->getType(Type::UINT32);
    v.mode = Value::Val;
    ev->stack.push_back(v);
}

void Builtins::doSig(const RowCol &pos)
{
    Value value = ev->stack.takeLast();
    Type* to;
    switch(value.type->kind)
    {
    case Type::UINT8:
        to = ev->mdl->getType(Type::INT8);
        break;
    case Type::UINT16:
        to = ev->mdl->getType(Type::INT16);
        break;
    case Type::UINT32:
        to = ev->mdl->getType(Type::INT32);
        break;
    case Type::UINT64:
        to = ev->mdl->getType(Type::INT64);
        break;
    default:
        //Q_ASSERT(false);
        break;
    }
    ev->stack.push_back(value);
    ev->castNum(to, pos);
    ev->stack.back().type = to;
}

void Builtins::doUsig(const RowCol &pos)
{
    Value value = ev->stack.takeLast();
    Type* to;
    switch(value.type->kind)
    {
    case Type::INT8:
        to = ev->mdl->getType(Type::UINT8);
        break;
    case Type::INT16:
        to = ev->mdl->getType(Type::UINT16);
        break;
    case Type::INT32:
        to = ev->mdl->getType(Type::UINT32);
        break;
    case Type::INT64:
        to = ev->mdl->getType(Type::UINT64);
        break;
    default:
        Q_ASSERT(false);
    }
    ev->stack.push_back(value);
    ev->castNum(to, pos);
    ev->stack.back().type = to;
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


void Builtins::pushActualsToMilStack(int nArgs, const RowCol &pos)
{
    Q_ASSERT( ev->stack.size() >= nArgs );
    if( !allArgsConst(nArgs) )
        return;
    for( int i = nArgs; i > 0; i-- )
    {
        Value& v = ev->stack[ev->stack.size() - i];
        ev->pushMilStack(v, pos);
        if( v.isConst() )
            v.mode = Value::Val;
    }
}

bool Builtins::allArgsConst(int nArgs)
{
    Q_ASSERT( ev->stack.size() >= nArgs );
    for( int i = nArgs; i > 0; i-- )
    {
        const Value& v = ev->stack[ev->stack.size() - i];
        if( !v.isConst() )
            return false;
    }
    return true;
}

void Builtins::ASSERT(int nArgs,const RowCol& pos)
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
        ev->error("expecting boolean first argument",pos);
        return;
    }
    if( !line.type->isInteger() )
    {
        ev->error("expecting integer second argument",pos);
        return;
    }
    if( !file.type->isText() )
    {
        ev->error("expecting string third argument",pos);
        return;
    }

    ev->out->call_(coreName("assert"),3);

    Value res;
    res.mode = Value::Val;
    res.type = ev->mdl->getType(Type::NoType);
    ev->stack.push_back(res);
}

void Builtins::incdec(int nArgs, bool inc,const RowCol& pos)
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
        ev->error("cannot write to first argument",pos);
        return;
    }

    if( what.type->isInteger() )
    {
        if( what.type->kind == Type::UINT64 || what.type->kind == Type::INT64 )
        {
            ev->out->dup_();
            ev->out->ldind_(what.type->kind == Type::UINT64 ? Mil::EmiTypes::U8 : Mil::EmiTypes::I8);
            if( nArgs == 2 )
            {
                if( step.isConst() )
                    ev->out->ldc_i8(step.val.toInt());
                else
                {
                    ev->out->ldloc_(tmp);
                    ev->out->conv_(Mil::EmiTypes::I8);
                }
            }else
                ev->out->ldc_i8(1);
            if( inc )
                ev->out->add_();
            else
                ev->out->sub_();
            ev->out->stind_(what.type->kind == Type::UINT64 ? Mil::EmiTypes::U8 : Mil::EmiTypes::I8);
        }else
        {
            ev->out->dup_();
            ev->out->ldind_(what.type->isUInt() ? Mil::EmiTypes::U4 : Mil::EmiTypes::I4);
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
            ev->out->stind_(what.type->isUInt() ? Mil::EmiTypes::U4 : Mil::EmiTypes::I4);
        }
    }else if( what.type->kind == Type::ConstEnum )
    {
        if( nArgs == 2 )
        {
            ev->error("second argument not supported for const enumerations",pos);
            return;
        }
        ev->out->dup_();
        ev->out->ldind_(Mil::EmiTypes::I4);
        ev->out->ldc_i4(1);
        if( inc )
            ev->out->add_();
        else
            ev->out->sub_();
        ev->out->stind_(Mil::EmiTypes::I4);
        // TODO: check overflow and halt?
        // TODO: do we expect more enums than fit in I4?
    }else if( what.type->kind == Type::Pointer )
    {
        ev->out->dup_();
        ev->out->ldind_(Mil::EmiTypes::IntPtr);
        if( nArgs == 2 )
        {
            if( step.isConst() )
                ev->out->ldc_i4(step.val.toInt());
            else
                ev->out->ldloc_(tmp);
        }else
            ev->out->ldc_i4(1);
        ev->out->ptroff_(ev->toQuali(what.type->getType()));
        ev->out->stind_(Mil::EmiTypes::IntPtr);
    }else
        ev->error("invalid argument types",pos);
}

void Builtins::INC(int nArgs,const RowCol& pos)
{
    incdec(nArgs,true,pos);
}

void Builtins::DEC(int nArgs,const RowCol& pos)
{
    incdec(nArgs,false,pos);
}

void Builtins::LEN(int nArgs,const RowCol& pos)
{
    Value what = ev->stack.takeLast();

    if( !what.isConst() )
        ev->out->pop_();
    Type* arr = what.type;
    if( arr->kind == Type::Pointer )
        arr = arr->getType();
    if( arr->kind != Type::Array || arr->len == 0 )
    {
        ev->error("function only applicable to non-open arrays",pos);
        return;
    }

    Value res;
    res.mode = Value::Const;
    res.type = ev->mdl->getType(Type::UINT32);
    res.val = arr->len;
    ev->stack.push_back(res);
}

void Builtins::PRINT(int nArgs, bool ln, const RowCol& pos)
{
    if( nArgs < 1 || ev->stack.back().type == 0 ||
            !(ev->stack.back().type->isSimple() ||
              ev->stack.back().type->isText() ||
              ev->stack.back().type->kind == Type::ConstEnum ||
              ev->stack.back().type->kind == Type::Generic ))
    {
        ev->error("expecting one argument of basic, enum or char array type",pos);
        return;
    }
    if( ev->stack.back().type->kind == Type::ConstEnum )
    {
        ev->out->conv_(Mil::EmiTypes::I8);
        ev->out->call_(coreName("printI8"),1,false);
    }else if( ev->stack.back().type->isInt() )
    {
        if( ev->stack.back().type->kind != Type::INT64 )
            ev->out->conv_(Mil::EmiTypes::I8);
        ev->out->call_(coreName("printI8"),1,false);
    }else if( ev->stack.back().type->isUInt() )
    {
        if( ev->stack.back().type->kind != Type::UINT64 )
            ev->out->conv_(Mil::EmiTypes::U8);
        ev->out->call_(coreName("printU8"),1,false);
    }else if( ev->stack.back().type->isReal() )
    {
        if( ev->stack.back().type->kind != Type::FLT64 )
            ev->out->conv_(Mil::EmiTypes::R8);
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
    else if( ev->stack.back().type->kind == Type::Generic )
        ev->out->pop_(); // we don't have a generic print operation, just remove the argument from stack
    else
        ev->error("given type not supported with PRINT or PRINTLN", pos);
    if( ln )
    {
        ev->out->ldc_i4(0xa); // LF
        ev->out->call_(coreName("printCh"),1,false);
    }
}

void Builtins::NEW(int nArgs,const RowCol& pos)
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
        ev->error("first argument must be a pointer to record or array",pos);
        return;
    }
    if( !what.ref )
    {
        ev->error("cannot write to first argument",pos);
        return;
    }
    if( what.type->getType()->kind == Type::Record || what.type->getType()->kind == Type::Object )
    {
        ev->out->newobj_(ev->toQuali(what.type->getType()));
        ev->out->stind_(Mil::EmiTypes::IntPtr);
    }else if( what.type->getType()->len > 0 ) // fixed size array
    {
        if( nArgs > 1 )
        {
            ev->error("cannot dynamically set array length for non-open array",pos);
            return;
        }
        ev->out->newobj_(ev->toQuali(what.type->getType())); // don't use newarr for fixed size arrays
        ev->out->stind_(Mil::EmiTypes::IntPtr);
    }else // open array
    {
        if( nArgs != 2 )
        {
            ev->error("expecting two arguments, the second as the explicit length",pos);
            return;
        }
        ev->out->newarr_(ev->toQuali(what.type->getType()->getType()));
        ev->out->stind_(Mil::EmiTypes::IntPtr);
    }
}

void Builtins::DISPOSE(int nArgs,const RowCol& pos)
{
    Value what = ev->stack.takeLast();
    if( what.type->kind != Type::Pointer &&
            !(what.type->getType()->kind == Type::Record || what.type->getType()->kind == Type::Object ||
              what.type->getType()->kind == Type::Array) )
    {
        ev->error("argument must be a pointer to record or array",pos);
        return;
    }

    ev->out->free_();

}

void Builtins::callBuiltin(quint8 builtin, int nArgs, const RowCol &pos)
{
    Value ret;
    ret.mode = Value::Val;
    ret.type = ev->mdl->getType(Type::NoType);

    // NOTE: when we come here, all args are on ev->stack and not yet pushed to MIL stack (new since 2026-01-30
    bool handleStack = true;
    try
    {
    switch( builtin )
    {
    case Builtin::PRINT:
    case Builtin::PRINTLN:
        checkNumOfActuals(nArgs, 1);
        pushActualsToMilStack(nArgs,pos);
        PRINT(nArgs,builtin == Builtin::PRINTLN,pos);
        break;
    case Builtin::NEW:
        checkNumOfActuals(nArgs, 1, 2);
        pushActualsToMilStack(nArgs,pos);
        NEW(nArgs,pos);
        handleStack = false;
        break;
    case Builtin::DISPOSE:
        checkNumOfActuals(nArgs, 1);
        pushActualsToMilStack(nArgs,pos);
        DISPOSE(nArgs,pos);
        handleStack = false;
        break;
    case Builtin::INC:
        checkNumOfActuals(nArgs, 1, 2);
        pushActualsToMilStack(nArgs,pos);
        INC(nArgs,pos);
        handleStack = false;
        break;
    case Builtin::DEC:
        checkNumOfActuals(nArgs, 1, 2);
        pushActualsToMilStack(nArgs,pos);
        DEC(nArgs,pos);
        handleStack = false;
        break;
    case Builtin::LEN:
        checkNumOfActuals(nArgs, 1);
        LEN(nArgs,pos);
        handleStack = false;
        break;
    case Builtin::ASSERT:
        checkNumOfActuals(nArgs, 3);
        pushActualsToMilStack(nArgs,pos);
        ASSERT(nArgs,pos);
        handleStack = false;
        break;
    case Builtin::BAND:
    case Builtin::BOR:
    case Builtin::BXOR:
        checkNumOfActuals(nArgs, 2);
        bitarith(builtin);
        handleStack = false;
        break;
    case Builtin::BNOT:
        checkNumOfActuals(nArgs, 1);
        bitnot();
        handleStack = false;
        break;
    case Builtin::ASR:
    case Builtin::SHR:
        checkNumOfActuals(nArgs, 2);
        doShiftRight(pos);
        handleStack = false;
        break;
    case Builtin::SHL:
        checkNumOfActuals(nArgs, 2);
        doShiftLeft(pos);
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
    case Builtin::ORD:
        checkNumOfActuals(nArgs, 1);
        doOrd(pos);
        handleStack = false;
        break;
    case Builtin::DEFAULT:
        checkNumOfActuals(nArgs, 1);
        doDefault();
        handleStack = false;
        break;
    case Builtin::VAL:
        checkNumOfActuals(nArgs, 2);
        doVal(pos);
        handleStack = false;
        break;
    case Builtin::CAST:
        checkNumOfActuals(nArgs, 2);
        doCast(pos);
        handleStack = false;
        break;
    case Builtin::HALT:
        checkNumOfActuals(nArgs, 1);
        pushActualsToMilStack(nArgs,pos);
        ev->out->call_(coreName("exit"),1,false);
        break;
    case Builtin::SIZE:
        checkNumOfActuals(nArgs, 1);
        doSize(pos);
        handleStack = false;
        break;
    case Builtin::CHR:
        checkNumOfActuals(nArgs, 1);
        ev->stack.back().type = ev->mdl->getType(Type::CHAR);
        break;
    case Builtin::STRLEN:
        checkNumOfActuals(nArgs, 1);
        doStrlen(pos);
        handleStack = false;
        break;
    case Builtin::SIG:
        checkNumOfActuals(nArgs, 1);
        doSig(pos);
        handleStack = false;
        break;
    case Builtin::USIG:
        checkNumOfActuals(nArgs, 1);
        doUsig(pos);
        handleStack = false;
        break;


    default:
        throw QString("built-in not yet implemented");
        break;
    }
    }catch(const QString& str)
    {
        ev->error(str,pos);
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
    Declaration* decl = ev->mdl->addDecl(Token::getSymbol("_$incdec"),&doublette);
    if( !doublette )
    {
        decl->kind = Declaration::LocalDecl;
        decl->setType(ev->mdl->getType(Type::INT32));
        decl->outer = ev->mdl->getTopScope();
        decl->id = ev->out->addLocal(ev->toQuali(decl->getType()),decl->name);
    }
    return decl->id;
}

