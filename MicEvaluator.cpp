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
#include "MicToken.h"
#include <bitset>
using namespace Mic;

bool MicEvaluator::unaryOp(quint8 op)
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
    case Tok_Tilde:
    case Tok_NOT:
        notOp(v);
        break;
    case Tok_Plus:
        unaryPlusOp(v);
        break;
    case Tok_Minus:
        unaryMinusOp(v);
        break;
    default:
        Q_ASSERT(false);
        // NOTE: Tok_At is directly handled in the parser
        break;
    }
    return err.isEmpty();
}

Type*MicEvaluator::smallestUIntType(const QVariant& v) const
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

Type*MicEvaluator::smallestIntType(const QVariant& v) const
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

bool MicEvaluator::binaryOp(quint8 op)
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
        // make sure the lhs const is immediately pushed
        // and converted to the right value
        bool lhsWasUint = false;
        if( lhs.isConst() )
        {
            if( lhs.type->isUInt() && rhs.type->isInt() )
            {
                // If one of the operands is an unsigned integer constant and the other operand is of type integer,
                // the unsigned integer constant is converted to the smallest integer type which includes the constant value.
                lhs.type = smallestIntType(lhs.val);
                lhsWasUint = true;
            }
            pushMilStack(lhs);
        }
        adjustNumType(lhs.type, rhs.type); // TODO only possible if rhs is not yet on stack
        if( rhs.isConst() )
        {

            if( rhs.type->isUInt() && lhs.type->isInt() && !lhsWasUint )
                rhs.type = smallestIntType(rhs.val);
            pushMilStack(rhs);
        }
        adjustNumType(rhs.type, lhs.type);
    }

    switch( op )
    {
    // Arith
    case Tok_Star:
    case Tok_Slash:
    case Tok_DIV:
    case Tok_MOD:
    case Tok_Plus:
    case Tok_Minus:
        stack.push_back(arithOp(op,lhs,rhs));
        break;
    // Logic
    case Tok_Amp:
    case Tok_AND:
    case Tok_OR:
        stack.push_back(logicOp(op,lhs,rhs));
        break;
    // Relation
    case Tok_Eq:
    case Tok_Hash:
    case Tok_Lt:
    case Tok_Leq:
    case Tok_Gt:
    case Tok_Geq:
        stack.push_back(relationOp(op, lhs,rhs));
       break;
    case Tok_IN:
        stack.push_back(inOp(lhs,rhs));
        break;
    }
    return err.isEmpty();
}

bool MicEvaluator::prepareRhs(Type* lhs)
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
             rhs.mode == Declaration::Procedure )
        out->ldproc_(toDesig(rhs.val.value<Declaration*>()));
    else
        assureTopOnMilStack();

    return true;
}

bool MicEvaluator::assign()
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

bool MicEvaluator::derefPointer()
{
    err.clear();
    if( stack.isEmpty() )
    {
        err = "expecting a value on the stack";
        return false;
    }
    Value v = stack.takeLast();

    Q_ASSERT( v.type && v.type->form == Type::Pointer && !v.ref );

    v.type = v.type->base;
    v.ref = true;
    //v.mode = Value::Val;

    stack.push_back(v);

    return err.isEmpty();
}

bool MicEvaluator::derefValue()
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

bool MicEvaluator::desigField()
{
    err.clear();
    if( stack.size() < 2 )
    {
        err = "expecting two values on the stack";
        return false;
    }
    Value rhs = stack.takeLast(); // field
    Value lhs = stack.takeLast(); // record reference

    // TODO: desig const record

    Q_ASSERT(lhs.ref && lhs.type && lhs.type->form == Type::Record);
    Q_ASSERT(rhs.mode == Declaration::Field);

    Declaration* field = rhs.val.value<Declaration*>();
    Q_ASSERT(field);
    const QByteArray desig = toDesig(lhs.type) + "." + field->name;
    out->ldflda_(desig);

    Value res;
    res.ref = true;
    res.type = field->type;
    res.visi = 0;
    res.mode = Value::Val;
    stack.push_back(res);

    return err.isEmpty();
}

bool MicEvaluator::desigVar()
{
    err.clear();
    if( stack.isEmpty() )
    {
        err = "expecting a value on the stack";
        return false;
    }
    Value v = stack.takeLast();
    Q_ASSERT( !v.ref );

    switch( v.mode )
    {
    case Declaration::VarDecl:
        out->ldvara_(v.val.toByteArray());
        break;
    case Declaration::LocalDecl:
        out->ldloca_(v.val.toInt());
        break;
    case Declaration::ParamDecl:
        out->ldarga_(v.val.toInt());
        break;
    default:
        Q_ASSERT(false);
    }

    v.ref = true;
    stack.push_back(v);

    return err.isEmpty();
}

bool MicEvaluator::desigIndex()
{
    err.clear();

    Value rhs = stack.takeLast(); // index
    Value lhs = stack.takeLast(); // array reference

    Q_ASSERT(lhs.ref && lhs.type && lhs.type->form == Type::Array);

    if( rhs.isConst() )
        pushMilStack(rhs);

    const QByteArray elemType = toDesig(lhs.type->base);
    out->ldelema_(elemType);

    Value res;
    res.ref = true;
    res.type = lhs.type->base;
    res.visi = 0;
    res.mode = Value::Val;
    stack.push_back(res);

    return err.isEmpty();
}

bool MicEvaluator::call(int nArgs)
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
    case Declaration::Builtin:
        callBuiltin(callee.val.toInt(),nArgs);
        return err.isEmpty();

    case Declaration::Procedure:
        {
            Declaration* proc = callee.val.value<Declaration*>();
            Q_ASSERT(proc);
            ret = proc->type;
            out->call_(toDesig(proc),nArgs, ret != 0); // TODO: desig in imported module
        }
        break;
    case Declaration::Field:
    case Declaration::Variant:
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
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

bool MicEvaluator::cast()
{
    err.clear();
    if( stack.size() < 2 )
    {
        err = "expecting two values on the stack";
        return false;
    }
    Value rhs = stack.takeLast(); // new type
    Value lhs = stack.takeLast(); // object
    // TODO
    out->castptr_(toDesig(rhs.type));
    // TODO restrict to pointers, add to MIL
    lhs.type = rhs.type;
    stack.push_back(lhs);
    return err.isEmpty();
}

bool MicEvaluator::push(const Value& v)
{
    stack.push_back(v);
}

bool MicEvaluator::dup()
{
    err.clear();
    if( stack.isEmpty() )
    {
        err = "expecting a value on the stack";
        return false;
    }
    Value v = stack.back();
    stack.push_back(v);
    out->dup_();
    return err.isEmpty();
}

Value MicEvaluator::pop()
{
    Value res;
    if( !stack.isEmpty() )
        res = stack.takeLast();
    return res;
}

Value MicEvaluator::top()
{
    Value res;
    if( !stack.isEmpty() )
        res = stack.last();
    return res;
}

void MicEvaluator::assureTopOnMilStack(bool pop)
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

bool MicEvaluator::pushMilStack(const Value& v)
{
    err.clear();
    switch( v.mode )
    {
    case Declaration::ConstDecl:
        {
            Value tmp = v;
            tmp.mode = Value::Const;
            tmp.val = v.val.value<Declaration*>()->data;
            return pushMilStack(tmp);
        }
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
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
        {
            // TODO
        }
        break;
    case Declaration::ParamDecl:
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

void MicEvaluator::emitRelOp(quint8 op, bool unsig)
{
    switch(op)
    {
    case Tok_Eq:
        out->ceq_();
        break;
    case Tok_Hash:
        out->ceq_();
        out->not_();
        break;
    case Tok_Geq: // not lt
        out->clt_(unsig);
        out->not_();
        break;
    case Tok_Gt:
        out->cgt_(unsig);
        break;
    case Tok_Leq: // not gt
        out->cgt_(unsig);
        out->not_();
        break;
    case Tok_Lt:
        out->clt_(unsig);
        break;
    default:
        Q_ASSERT(false);
        break;
    }
}

void MicEvaluator::emitArithOp(quint8 op, bool unsig, bool i64)
{
    switch(op)
    {
    case Tok_Star:
        out->mul_();
        break;
    case Tok_Slash:
        out->div_();
        break;
    case Tok_DIV:
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
    case Tok_MOD:
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
    case Tok_Plus:
        out->add_();
        break;
    case Tok_Minus:
        out->sub_();
        break;
    default:
        err = "operator not supported for integer operands";
        break;
    }

}

void MicEvaluator::adjustNumType(Type* me, Type* other)
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

void MicEvaluator::callBuiltin(int builtin, int nArgs)
{
    Value ret;
    ret.mode = Value::Val;
    ret.type = mdl->getType(BasicType::NoType);
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
        err = "built-in not yet implemented";
        break;
    }

    if( handleStack )
    {
        for( int i = 0; i < nArgs; i++ )
            stack.pop_back();
        stack.pop_back(); // callee
        stack.push_back(ret);
    }
}

int MicEvaluator::addIncDecTmp()
{
    bool doublette;
    Declaration* decl = mdl->addDecl(Token::getSymbol("$incdec"),&doublette);
    if( !doublette )
    {
        decl->mode = Declaration::LocalDecl;
        decl->type = mdl->getType(BasicType::INT32);
        decl->outer = mdl->getTopScope();
        decl->level = out->addLocal(toDesig(decl->type),decl->name);
    }
    return decl->level;
}

void MicEvaluator::PRINT(int nArgs, bool ln)
{
    assureTopOnMilStack();
    if( nArgs != 1 && !(stack.back().type->isSimple() || stack.back().type->isText() ))
        err = "expecting one argument of basic or char array type";
    else if( stack.back().type->form == Type::ConstEnum )
    {
        out->conv_(MilEmitter::ToI8);
        out->call_("$MIC$printI8",1,false);
    }else if( stack.back().type->isInt() )
    {
        if( stack.back().type->form != BasicType::INT64 )
            out->conv_(MilEmitter::ToI8);
        out->call_("$MIC$printI8",1,false);
    }else if( stack.back().type->isUInt() )
    {
        if( stack.back().type->form != BasicType::UINT64 )
            out->conv_(MilEmitter::ToU8);
        out->call_("$MIC$printU8",1,false);
    }else if( stack.back().type->isReal() )
    {
        if( stack.back().type->form != BasicType::LONGREAL )
            out->conv_(MilEmitter::ToR8);
        out->call_("$MIC$printF8",1,false);
    }else if( stack.back().type->isText() )
    {
        if( stack.back().type->form != BasicType::CHAR )
            out->call_("$MIC$printStr",1,false);
        else
            out->call_("$MIC$printCh",1,false);
    }else if( stack.back().type->isBoolean() )
        out->call_("$MIC$printBool",1,false);
    else if( stack.back().type->isSet() )
        out->call_("$MIC$printSet",1,false);
    else
        err = "given type not supported with PRINT or PRINTLN";
    if( ln )
    {
        out->ldc_i4(0xa); // LF
        out->call_("$MIC$printCh",1,false);
    }
}

void MicEvaluator::NEW(int nArgs)
{
    if( nArgs == 0 || nArgs > 2 )
    {
        err = "expecting one or two arguments";
        return;
    }
    Value len;
    if( nArgs == 2 )
        len = stack.takeLast();
    Value what = stack.takeLast();
    stack.pop_back(); // callee
    if( what.type->form != Type::Pointer &&
            !(what.type->base->form == Type::Record || what.type->base->form == Type::Array) )
    {
        err = "first argument must be a pointer to record or array";
        return;
    }
    if( !what.ref )
    {
        err = "cannot write to first argument";
        return;
    }
    if( what.type->base->form == Type::Record )
    {
        out->sizeof_(toDesig(what.type->base));
        out->call_("$MIC$alloc32",1,1);
        out->castptr_(toDesig(what.type->base));
        out->stind_(MilEmitter::IntPtr);
    }else if( what.type->base->len > 0 ) // fixed size array
    {
        if( nArgs > 1 )
        {
            err = "cannot dynamically set array length for non-open array";
            return;
        }
        out->sizeof_(toDesig(what.type->base->base));
        out->ldc_i4(what.type->base->len);
        out->mul_();
        out->call_("$MIC$alloc32",1,1);
        out->castptr_(toDesig(what.type->base));
        out->stind_(MilEmitter::IntPtr);
    }else // open array
    {
        if( nArgs != 2 )
        {
            err = "expecting two arguments, the second as the explicit length";
            return;
        }
        out->sizeof_(toDesig(what.type->base->base));
        out->mul_();
        out->call_("$MIC$alloc32",1,1);
        out->castptr_(toDesig(what.type->base));
        out->stind_(MilEmitter::IntPtr);
    }
}

void MicEvaluator::DISPOSE(int nArgs)
{
    if( nArgs != 1 )
    {
        err = "expecting one argument";
        return;
    }
    Value what = stack.takeLast();
    stack.pop_back(); // callee
    if( what.type->form != Type::Pointer &&
            !(what.type->base->form == Type::Record || what.type->base->form == Type::Array) )
    {
        err = "argument must be a pointer to record or array";
        return;
    }

    out->call_("$MIC$free",1,1);

}

void MicEvaluator::INC(int nArgs)
{
    incdec(nArgs,true);
}

void MicEvaluator::DEC(int nArgs)
{
    incdec(nArgs,false);
}

void MicEvaluator::LEN(int nArgs)
{
    if( nArgs != 1 )
    {
        err = "expecting one argument";
        return;
    }
    Value what = stack.takeLast();
    stack.pop_back(); // callee
    if( !what.isConst() )
        out->pop_();
    Type* arr = what.type;
    if( arr->form == Type::Pointer )
        arr = arr->base;
    if( arr->form != Type::Array || arr->len == 0 )
    {
        err = "function only applicable to non-open arrays";
        return;
    }
    //out->ldc_i4(arr->len);
    Value res;
    res.mode = Value::Const;
    res.type = mdl->getType(BasicType::UINT32);
    res.val = arr->len;
    stack.push_back(res);
}

void MicEvaluator::incdec(int nArgs, bool inc)
{
    if( nArgs == 0 || nArgs > 2 )
    {
        err = "expecting one or two arguments";
        return;
    }
    Value step;
    int tmp = -1;
    if( nArgs == 2 )
    {
        step = stack.takeLast();
        if( !step.isConst() )
        {
            tmp = addIncDecTmp();
            out->stloc_(tmp); // store second argument to temporary, remove it from stack
        }
    }
    Value what = stack.takeLast();
    stack.pop_back(); // callee

    if( !what.isLvalue() || !what.ref )
    {
        err = "cannot write to first argument";
        return;
    }

    if( what.type->isInteger() )
    {
        if( what.type->form == BasicType::UINT64 || what.type->form == BasicType::INT64 )
        {
            out->dup_();
            out->ldind_(what.type->form == BasicType::UINT64 ? MilEmitter::U8 : MilEmitter::I8);
            if( nArgs == 2 )
            {
                if( step.isConst() )
                    out->ldc_i8(step.val.toInt());
                else
                {
                    out->ldloc_(tmp);
                    out->conv_(MilEmitter::ToI8);
                }
            }else
                out->ldc_i8(1);
            if( inc )
                out->add_();
            else
                out->sub_();
            out->stind_(what.type->form == BasicType::UINT64 ? MilEmitter::U8 : MilEmitter::I8);
        }else
        {
            out->dup_();
            out->ldind_(what.type->isUInt() ? MilEmitter::U4 : MilEmitter::I4);
            if( nArgs == 2 )
            {
                if( step.isConst() )
                    out->ldc_i4(step.val.toInt());
                else
                    out->ldloc_(tmp);
            }else
                out->ldc_i4(1);
            if( inc )
                out->add_();
            else
                out->sub_();
            out->stind_(what.type->isUInt() ? MilEmitter::U4 : MilEmitter::I4);
        }
    }else if( what.type->form == Type::ConstEnum )
    {
        if( nArgs == 2 )
        {
            err = "second argument not supported for const enumerations";
            return;
        }
        out->dup_();
        out->ldind_(MilEmitter::I4);
        out->ldc_i4(1);
        if( inc )
            out->add_();
        else
            out->sub_();
        out->stind_(MilEmitter::I4);
        // TODO: check overflow and halt?
        // TODO: do we expect more enums than fit in I4?
    }else if( what.type->form == Type::Pointer )
    {
        out->dup_();
        out->ldind_(MilEmitter::IntPtr);
        out->sizeof_(toDesig(what.type->base));
        if( nArgs == 2 )
        {
            if( step.isConst() )
                out->ldc_i4(step.val.toInt());
            else
                out->ldloc_(tmp);
            out->mul_();
        }
        if( inc )
            out->add_();
        else
            out->sub_();
        out->stind_(MilEmitter::IntPtr);
    }else
        err = "invalid argument types";
}

void MicEvaluator::ASSERT(int nArgs)
{
    if( nArgs != 3 )
    {
        err = "expecting three arguments";
        return;
    }
    Value file = stack.takeLast();
    Value line = stack.takeLast();
    Value cond = stack.takeLast();
    stack.pop_back(); // callee

    if( cond.isConst() )
        pushMilStack(cond);
    if( line.isConst() )
        pushMilStack(line);
    if( file.isConst() )
        pushMilStack(file);

    if( cond.type->form != BasicType::BOOLEAN )
    {
        err = "expecting boolean first argument";
        return;
    }
    if( !line.type->isInteger() )
    {
        err = "expecting integer second argument";
        return;
    }
    if( !file.type->isText() )
    {
        err = "expecting string thirs argument";
        return;
    }

    out->call_("$MIC$assert",3);

    Value res;
    res.mode = Value::Val;
    res.type = mdl->getType(BasicType::NoType);
    stack.push_back(res);
}

void MicEvaluator::BITARITH(int op, int nArgs)
{
    if( nArgs != 2 )
    {
        err = "expecting two arguments";
        return;
    }
    Value rhs = stack.takeLast();
    Value lhs = stack.takeLast();
    stack.pop_back(); // callee

    if( !lhs.type->isUInt() || !rhs.type->isUInt() )
    {
        err = "both operands must be unsigned integers";
        return;
    }
    if( lhs.isConst() )
    {
        pushMilStack(lhs);
        if( rhs.type->form == BasicType::UINT64 )
            adjustNumType(lhs.type, rhs.type);
    }
    if( rhs.isConst() )
    {
        pushMilStack(rhs);
    }

}

void MicEvaluator::notOp(Value& v)
{
    if( v.type->isBoolean() )
    {
        if( v.isConst() )
            v.val = !v.val.toBool();
        else
            out->not_();
    }else
        err = "unary '~' or 'NOT' not applicable to this type";
}

Value MicEvaluator::logicOp(quint8 op, const Value& lhs, const Value& rhs)
{
    Value res;
    if( lhs.type->isBoolean() && rhs.type->isBoolean() )
    {
        if( lhs.isConst() && rhs.isConst() )
        {
            res.mode = lhs.mode;
            res.type = lhs.type;
            if( op == Tok_AND || op == Tok_Amp)
                res.val = lhs.val.toBool() && rhs.val.toBool();
            else
                res.val = lhs.val.toBool() || rhs.val.toBool();
        }else
        {
            res.mode = Value::Val;
            res.type = lhs.type;
            // order is irrelevant for logic ops
            if( op == Tok_AND || op == Tok_Amp)
                out->and_();
            else
                out->or_();
        }
    }else
        err = "operation expects boolean operands";
    return res;
}

static inline Type* maxType(Type* lhs, Type* rhs)
{
    if( lhs->form >= rhs->form )
        return lhs;
    else
        return rhs;
}

Value MicEvaluator::arithOp(quint8 op, const Value& lhs, const Value& rhs)
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
                case Tok_Star:
                    res.val = a * b;
                    break;
                case Tok_DIV:
                    res.val = (qint64)(a < 0 ? (a - b + 1) / b : a / b);
                    break;
                case Tok_MOD:
                    res.val = a < 0 ? (b - 1) + ((a - b + 1)) % b : a % b;
                    break;
                case Tok_Plus:
                    res.val = a + b;
                    break;
                case Tok_Minus:
                    res.val = a - b;
                    break;
                default:
                    err = "operator not supported for integer operands";
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
                case Tok_Star:
                    res.val = (quint64)(a * b);
                    break;
                case Tok_DIV:
                    res.val = (quint64)(a / b);
                    break;
                case Tok_MOD:
                    res.val = (quint64)(a % b);
                    break;
                case Tok_Plus:
                    res.val = (quint64)(a + b);
                    break;
                case Tok_Minus:
                    res.val = (quint64)(a - b);
                    break;
                default:
                    err = "operator not supported for integer operands";
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
                case Tok_Star:
                    res.val = lhs.val.toDouble() * rhs.val.toDouble();
                    break;
                case Tok_Slash:
                    res.val = lhs.val.toDouble() / rhs.val.toDouble();
                    break;
                case Tok_Plus:
                    res.val = lhs.val.toDouble() + rhs.val.toDouble();
                    break;
                case Tok_Minus:
                    res.val = lhs.val.toDouble() - rhs.val.toDouble();
                    break;
                default:
                    err = "operator not supported for real operands";
                    break;
                }
            }else
            {
                emitArithOp(op);
            }
        }else
            err = "operands are not of the same type";
    }else if( lhs.type->isSet() && rhs.type->isSet() )
    {
        // + - * /
        if( lhs.isConst() && rhs.isConst() )
        {
            res.mode = Value::Const;
            switch(op)
            {
            case Tok_Star:
                res.val = lhs.val.toUInt() & rhs.val.toUInt();
                break;
            case Tok_Slash:
                res.val = ~(lhs.val.toUInt() & rhs.val.toUInt()) & (lhs.val.toUInt() | rhs.val.toUInt());
                break;
            case Tok_Plus:
                res.val = lhs.val.toUInt() | rhs.val.toUInt();
                break;
            case Tok_Minus:
                res.val = lhs.val.toUInt() & ~rhs.val.toUInt();
                break;
            default:
                err = "operator not supported for set operands";
                break;
            }
        }else
        {
            switch(op)
            {
            case Tok_Star:
                out->and_();
                break;
            case Tok_Slash:
                out->call_("$MIC$SetDiv",2,1);
                break;
            case Tok_Plus:
                out->or_();
                break;
            case Tok_Minus:
                out->not_();
                out->and_();
                break;
            default:
                err = "operator not supported for set operands";
                break;
            }
        }
    }else if( (lhs.type->form == BasicType::String || lhs.type->form == BasicType::CHAR) &&
              (rhs.type->form == BasicType::String || rhs.type->form == BasicType::CHAR) )
    {
        // + only
        res.type = mdl->getType(BasicType::String);
        if( op != Tok_Plus )
            err = "only the '+' operator can be applied to string and char literals";
        else if( lhs.isConst() && rhs.isConst() )
        {
            res.val = lhs.val.toByteArray() + rhs.val.toByteArray();
        }else
            err = "operation is only available for string and char literals";
    }else
        err = "operands not compatible with operator";

    return res;
}

Value MicEvaluator::relationOp(quint8 op, const Value& lhs, const Value& rhs)
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
                case Tok_Geq:
                    res.val = lhs.val.toLongLong() >= rhs.val.toLongLong();
                    break;
                case Tok_Gt:
                    res.val = lhs.val.toLongLong() > rhs.val.toLongLong();
                    break;
                case Tok_Eq:
                    res.val = lhs.val.toLongLong() == rhs.val.toLongLong();
                    break;
                case Tok_Leq:
                    res.val = lhs.val.toLongLong() <= rhs.val.toLongLong();
                    break;
                case Tok_Hash:
                    res.val = lhs.val.toLongLong() != rhs.val.toLongLong();
                    break;
                case Tok_Lt:
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
                case Tok_Geq:
                    res.val = lhs.val.toULongLong() >= rhs.val.toULongLong();
                    break;
                case Tok_Gt:
                    res.val = lhs.val.toULongLong() > rhs.val.toULongLong();
                    break;
                case Tok_Eq:
                    res.val = lhs.val.toULongLong() == rhs.val.toULongLong();
                    break;
                case Tok_Leq:
                    res.val = lhs.val.toULongLong() <= rhs.val.toULongLong();
                    break;
                case Tok_Hash:
                    res.val = lhs.val.toULongLong() != rhs.val.toULongLong();
                    break;
                case Tok_Lt:
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
                case Tok_Geq:
                    res.val = lhs.val.toDouble() >= rhs.val.toDouble();
                    break;
                case Tok_Gt:
                    res.val = lhs.val.toDouble() > rhs.val.toDouble();
                    break;
                case Tok_Eq:
                    res.val = lhs.val.toDouble() == rhs.val.toDouble();
                    break;
                case Tok_Leq:
                    res.val = lhs.val.toDouble() <= rhs.val.toDouble();
                    break;
                case Tok_Hash:
                    res.val = lhs.val.toDouble() != rhs.val.toDouble();
                    break;
                case Tok_Lt:
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
            err = "operands are not of the same type";
    }else if( lhs.type->isText() && rhs.type->isText() )
    {
        switch(op)
        {
        case Tok_Geq:
            out->ldc_i4(6);
            break;
        case Tok_Gt:
            out->ldc_i4(5);
            break;
        case Tok_Eq:
            out->ldc_i4(1);
            break;
        case Tok_Leq:
            out->ldc_i4(4);
            break;
        case Tok_Hash:
            out->ldc_i4(2);
            break;
        case Tok_Lt:
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
            case Tok_Geq:
                res.val = lhs.val.toULongLong() >= rhs.val.toULongLong();
                break;
            case Tok_Gt:
                res.val = lhs.val.toULongLong() > rhs.val.toULongLong();
                break;
            case Tok_Eq:
                res.val = lhs.val.toULongLong() == rhs.val.toULongLong();
                break;
            case Tok_Leq:
                res.val = lhs.val.toULongLong() <= rhs.val.toULongLong();
                break;
            case Tok_Hash:
                res.val = lhs.val.toULongLong() != rhs.val.toULongLong();
                break;
            case Tok_Lt:
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
        if( lhs.type != rhs.type )
            err = "cannot compare the elements of different enumeration types";
        if( lhs.isConst() && rhs.isConst() )
        {
            res.mode = Value::Const;
            switch(op)
            {
            case Tok_Geq:
                res.val = lhs.val.toLongLong() >= rhs.val.toLongLong();
                break;
            case Tok_Gt:
                res.val = lhs.val.toLongLong() > rhs.val.toLongLong();
                break;
            case Tok_Eq:
                res.val = lhs.val.toLongLong() == rhs.val.toLongLong();
                break;
            case Tok_Leq:
                res.val = lhs.val.toLongLong() <= rhs.val.toLongLong();
                break;
            case Tok_Hash:
                res.val = lhs.val.toLongLong() != rhs.val.toLongLong();
                break;
            case Tok_Lt:
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
            case Tok_Eq:
                res.val = lhs.val.toUInt() == rhs.val.toUInt();
                break;
            case Tok_Hash:
                res.val = lhs.val.toUInt() != rhs.val.toUInt();
                break;
            default:
                err = "operation not supported for given operands";
                break;
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
        err = "operands not compatible with operator";

    return res;
}

Value MicEvaluator::inOp(const Value& lhs, const Value& rhs)
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

void MicEvaluator::unaryMinusOp(Value& v)
{
    if( v.type->isNumber() )
    {
        if( v.type->isUInt() )
        {
            switch(v.type->form)
            {
            case BasicType::UINT8:
                v.type = mdl->getType(BasicType::INT16);
                break;
            case BasicType::UINT16:
                v.type = mdl->getType(BasicType::INT32);
                break;
            case BasicType::UINT32:
                v.type = mdl->getType(BasicType::INT64);
                break;
            case BasicType::UINT64:
                err = "unary operator is not applicable to operands of UINT64 type";
                return;
            }

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
        err = "unary '-' not applicable to this type";
}

void MicEvaluator::unaryPlusOp(Value& v)
{
    if( !v.type->isNumber() )
        err = "unary '+' not applicable to this type";
    if( v.type->isUInt() )
    {
        switch(v.type->form)
        {
        case BasicType::UINT8:
            v.type = mdl->getType(BasicType::INT16);
            break;
        case BasicType::UINT16:
            v.type = mdl->getType(BasicType::INT32);
            break;
        case BasicType::UINT32:
            v.type = mdl->getType(BasicType::INT64);
            break;
        case BasicType::UINT64:
            err = "unary operator is not applicable to operands of UINT64 type";
            break;
        }
    }
    // no code is generated for this
}

QByteArray MicEvaluator::toDesig(Declaration* d)
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

QByteArray MicEvaluator::toDesig(Type* t)
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

QByteArray MicEvaluator::dequote(const QByteArray& str)
{
    if( str.startsWith('\'') && str.endsWith('\'') ||
            str.startsWith('"') && str.endsWith('"') )
        return str.mid(1,str.size()-2);
    else
        return str;
}
