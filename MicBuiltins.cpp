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
    tmp->type = t;
    tmp->lhs = e;
    return tmp;
}

static void checkBitArith(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl)
{
    expectingNArgs(args,2);
    if( !args[0]->type->isUInt() )
        throw QString("expecing unsigned first argument");
    if( !args[1]->type->isUInt() )
        throw QString("expecing unsigned second argument");
    Type* lhs = args[0]->type;
    Type* rhs = args[1]->type;
    if( lhs->form < BasicType::UINT32 )
        lhs = mdl->getType(BasicType::UINT32);
    if( rhs->form < BasicType::UINT32 )
        rhs = mdl->getType(BasicType::UINT32);
    if( lhs->form < rhs->form )
        lhs = rhs;
    else if( lhs->form > rhs->form )
        rhs = lhs;
    if( lhs != args[0]->type )
        args[0] = createAutoCast(args[0], lhs);
    if( rhs != args[1]->type )
        args[1] = createAutoCast(args[1], rhs);
    *ret = args[0]->type;
}

static void checkBitShift(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl)
{
    expectingNArgs(args,2);
    if( !args[0]->type->isUInt() )
        throw QString("expecing unsigned first argument");
    if( !args[1]->type->isUInt() )
        throw QString("expecing unsigned second argument");

    if( args[0]->type->form < BasicType::UINT32 )
        args[0] = createAutoCast(args[0], mdl->getType(BasicType::UINT32) );
    if( args[1]->type->form < BasicType::UINT32 )
        args[1] = createAutoCast(args[1], mdl->getType(BasicType::UINT32) );

    *ret = args[0]->type;
}

QString Builtins::checkArgs(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl)
{
    Q_ASSERT(ret);

    *ret = mdl->getType(BasicType::NoType);

    // TODO:
    try
    {
    switch(builtin)
    {
    // functions:
    case Builtin::ABS:
        expectingNArgs(args,1);
        if( !args.first()->type->isNumber() )
            throw QString("expecting numeric argument");
        *ret = args.first()->type;
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
        if( !args.first()->type->isUInt() )
            throw QString("expecting unsigned integer");
        if( args.first()->type->form < BasicType::UINT32 )
            args[0] = createAutoCast(args[0], mdl->getType(BasicType::UINT32) );
        *ret = args[0]->type;
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
        break;
    case Builtin::GETENV:
        expectingNArgs(args,2);
        break;
    case Builtin::LEN:
        expectingNArgs(args,1);
        *ret = mdl->getType(BasicType::UINT32);
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
        break;
    case Builtin::SIZE:
        expectingNArgs(args,1);
        break;
    case Builtin::STRLEN:
        expectingNArgs(args,1);
        break;
    case Builtin::UNSIGNED:
        expectingNArgs(args,1);
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

void Builtins::BITARITH(int op, int nArgs)
{
    Value rhs = ev->stack.takeLast();
    Value lhs = ev->stack.takeLast();
    ev->stack.pop_back(); // callee

    if( !lhs.type->isUInt() || !rhs.type->isUInt() )
    {
        ev->err = "both operands must be unsigned integers";
        return;
    }
    if( lhs.isConst() )
    {
        ev->pushMilStack(lhs);
        if( rhs.type->form == BasicType::UINT64 )
            ev->adjustNumType(lhs.type, rhs.type);
    }
    if( rhs.isConst() )
    {
        ev->pushMilStack(rhs);
    }

}

void Builtins::ASSERT(int nArgs)
{
    Value file = ev->stack.takeLast();
    Value line = ev->stack.takeLast();
    Value cond = ev->stack.takeLast();
    ev->stack.pop_back(); // callee

    if( cond.isConst() )
        ev->pushMilStack(cond);
    if( line.isConst() )
        ev->pushMilStack(line);
    if( file.isConst() )
        ev->pushMilStack(file);

    if( cond.type->form != BasicType::BOOLEAN )
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
        ev->err = "expecting string thirs argument";
        return;
    }

    ev->out->call_("$MIC$assert",3);

    Value res;
    res.mode = Value::Val;
    res.type = ev->mdl->getType(BasicType::NoType);
    ev->stack.push_back(res);
}

void Builtins::incdec(int nArgs, bool inc)
{
    if( nArgs == 0 || nArgs > 2 )
    {
        ev->err = "expecting one or two arguments";
        return;
    }
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
    ev->stack.pop_back(); // callee

    if( !what.isLvalue() || !what.ref )
    {
        ev->err = "cannot write to first argument";
        return;
    }

    if( what.type->isInteger() )
    {
        if( what.type->form == BasicType::UINT64 || what.type->form == BasicType::INT64 )
        {
            ev->out->dup_();
            ev->out->ldind_(what.type->form == BasicType::UINT64 ? MilEmitter::U8 : MilEmitter::I8);
            if( nArgs == 2 )
            {
                if( step.isConst() )
                    ev->out->ldc_i8(step.val.toInt());
                else
                {
                    ev->out->ldloc_(tmp);
                    ev->out->conv_(MilEmitter::ToI8);
                }
            }else
                ev->out->ldc_i8(1);
            if( inc )
                ev->out->add_();
            else
                ev->out->sub_();
            ev->out->stind_(what.type->form == BasicType::UINT64 ? MilEmitter::U8 : MilEmitter::I8);
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
    }else if( what.type->form == Type::ConstEnum )
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
    }else if( what.type->form == Type::Pointer )
    {
        ev->out->dup_();
        ev->out->ldind_(MilEmitter::IntPtr);
        ev->out->sizeof_(ev->toDesig(what.type->base));
        if( nArgs == 2 )
        {
            if( step.isConst() )
                ev->out->ldc_i4(step.val.toInt());
            else
                ev->out->ldloc_(tmp);
            ev->out->mul_();
        }
        if( inc )
            ev->out->add_();
        else
            ev->out->sub_();
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
    ev->stack.pop_back(); // callee
    if( !what.isConst() )
        ev->out->pop_();
    Type* arr = what.type;
    if( arr->form == Type::Pointer )
        arr = arr->base;
    if( arr->form != Type::Array || arr->len == 0 )
    {
        ev->err = "function only applicable to non-open arrays";
        return;
    }
    //ev->out->ldc_i4(arr->len);
    Value res;
    res.mode = Value::Const;
    res.type = ev->mdl->getType(BasicType::UINT32);
    res.val = arr->len;
    ev->stack.push_back(res);
}

void Builtins::PRINT(int nArgs, bool ln)
{
    ev->assureTopOnMilStack();
    if( nArgs != 1 && !(ev->stack.back().type->isSimple() || ev->stack.back().type->isText() ))
        ev->err = "expecting one argument of basic or char array type";
    else if( ev->stack.back().type->form == Type::ConstEnum )
    {
        ev->out->conv_(MilEmitter::ToI8);
        ev->out->call_("$MIC$printI8",1,false);
    }else if( ev->stack.back().type->isInt() )
    {
        if( ev->stack.back().type->form != BasicType::INT64 )
            ev->out->conv_(MilEmitter::ToI8);
        ev->out->call_("$MIC$printI8",1,false);
    }else if( ev->stack.back().type->isUInt() )
    {
        if( ev->stack.back().type->form != BasicType::UINT64 )
            ev->out->conv_(MilEmitter::ToU8);
        ev->out->call_("$MIC$printU8",1,false);
    }else if( ev->stack.back().type->isReal() )
    {
        if( ev->stack.back().type->form != BasicType::LONGREAL )
            ev->out->conv_(MilEmitter::ToR8);
        ev->out->call_("$MIC$printF8",1,false);
    }else if( ev->stack.back().type->isText() )
    {
        // TODO: do we really accept array of string by value?
        if( ev->stack.back().type->form != BasicType::CHAR )
            ev->out->call_("$MIC$printStr",1,false);
        else
            ev->out->call_("$MIC$printCh",1,false);
    }else if( ev->stack.back().type->isBoolean() )
        ev->out->call_("$MIC$printBool",1,false);
    else if( ev->stack.back().type->isSet() )
        ev->out->call_("$MIC$printSet",1,false);
    else
        ev->err = "given type not supported with PRINT or PRINTLN";
    if( ln )
    {
        ev->out->ldc_i4(0xa); // LF
        ev->out->call_("$MIC$printCh",1,false);
    }
}

void Builtins::NEW(int nArgs)
{
    if( nArgs == 0 || nArgs > 2 )
    {
        ev->err = "expecting one or two arguments";
        return;
    }
    Value len;
    if( nArgs == 2 )
        len = ev->stack.takeLast();
    Value what = ev->stack.takeLast();
    ev->stack.pop_back(); // callee
    if( what.type->form != Type::Pointer &&
            !(what.type->base->form == Type::Record || what.type->base->form == Type::Array) )
    {
        ev->err = "first argument must be a pointer to record or array";
        return;
    }
    if( !what.ref )
    {
        ev->err = "cannot write to first argument";
        return;
    }
    if( what.type->base->form == Type::Record )
    {
        ev->out->sizeof_(ev->toDesig(what.type->base));
        ev->out->call_("$MIC$alloc32",1,1);
        ev->out->castptr_(ev->toDesig(what.type->base));
        ev->out->stind_(MilEmitter::IntPtr);
    }else if( what.type->base->len > 0 ) // fixed size array
    {
        if( nArgs > 1 )
        {
            ev->err = "cannot dynamically set array length for non-open array";
            return;
        }
        ev->out->sizeof_(ev->toDesig(what.type->base->base));
        ev->out->ldc_i4(what.type->base->len);
        ev->out->mul_();
        ev->out->call_("$MIC$alloc32",1,1);
        ev->out->castptr_(ev->toDesig(what.type->base));
        ev->out->stind_(MilEmitter::IntPtr);
    }else // open array
    {
        if( nArgs != 2 )
        {
            ev->err = "expecting two arguments, the second as the explicit length";
            return;
        }
        ev->out->sizeof_(ev->toDesig(what.type->base->base));
        ev->out->mul_();
        ev->out->call_("$MIC$alloc32",1,1);
        ev->out->castptr_(ev->toDesig(what.type->base));
        ev->out->stind_(MilEmitter::IntPtr);
    }
}

void Builtins::DISPOSE(int nArgs)
{
    Value what = ev->stack.takeLast();
    ev->stack.pop_back(); // callee
    if( what.type->form != Type::Pointer &&
            !(what.type->base->form == Type::Record || what.type->base->form == Type::Array) )
    {
        ev->err = "argument must be a pointer to record or array";
        return;
    }

    ev->out->call_("$MIC$free",1,1);

}

void Builtins::callBuiltin(quint8 builtin, int nArgs)
{
    Value ret;
    ret.mode = Value::Val;
    ret.type = ev->mdl->getType(BasicType::NoType);
    bool handleStack = true;
    switch( builtin )
    {
    case Builtin::PRINT:
    case Builtin::PRINTLN:
        PRINT(nArgs,builtin == Builtin::PRINTLN);
        break;
    case Builtin::NEW:
        NEW(nArgs);
        handleStack = false;
        break;
    case Builtin::DISPOSE:
        DISPOSE(nArgs);
        handleStack = false;
        break;
    case Builtin::INC:
        INC(nArgs);
        handleStack = false;
        break;
    case Builtin::DEC:
        DEC(nArgs);
        handleStack = false;
        break;
    case Builtin::LEN:
        LEN(nArgs);
        handleStack = false;
        break;
    case Builtin::ASSERT:
        ASSERT(nArgs);
        handleStack = false;
        break;
    case Builtin::BITAND:
    case Builtin::BITNOT:
    case Builtin::BITOR:
    case Builtin::BITXOR:
        BITARITH(builtin,nArgs);
        handleStack = false;
        break;
        /* TODO
    case Builtin::BITASR:
    case Builtin::BITSHL:
    case Builtin::BITSHR:
        BITSHIFT(builtin,nArgs);
        break;
        */
    default:
        ev->err = "built-in not yet implemented";
        break;
    }

    if( handleStack )
    {
        for( int i = 0; i < nArgs; i++ )
            ev->stack.pop_back();
        ev->stack.pop_back(); // callee
        ev->stack.push_back(ret);
    }
}

int Builtins::addIncDecTmp()
{
    bool doublette;
    Declaration* decl = ev->mdl->addDecl(Token::getSymbol("$incdec"),&doublette);
    if( !doublette )
    {
        decl->mode = Declaration::LocalDecl;
        decl->type = ev->mdl->getType(BasicType::INT32);
        decl->outer = ev->mdl->getTopScope();
        decl->id = ev->out->addLocal(ev->toDesig(decl->type),decl->name);
    }
    return decl->id;
}

