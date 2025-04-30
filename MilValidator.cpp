/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MilValidator.h"
#include <QtDebug>
#include <limits>
using namespace Mil;

Validator::Validator(AstModel* m):mdl(m),curMod(0),curProc(0),needsPointerInit(true)
{

}

bool Validator::validate(Declaration* module)
{
    curMod = module;

    Declaration* sub = curMod->subs;
    while( sub )
    {
        Type* t = deref(sub->getType());
        switch(sub->kind)
        {
        case Declaration::TypeDecl:
            if( t->kind == Type::Object )
            {
                Type* base = deref(t->getType());
                int id = base->numOfNonFwdNonOverrideProcs();
                foreach( Declaration* d, t->subs )
                {
                    if( d->kind == Declaration::Procedure && !d->forward )
                    {
                        Declaration* p = base->findSubByName(d->name, false);
                        if( p )
                        {
                            d->off = p->off;
                            d->override_ = true;
                            // TODO: check param equality with super
                        }else
                            d->off = id++;
                        visitProcedure(d);
                    }
                }
            }else if( t->kind == Type::Union )
                foreach( Declaration* d, t->subs )
                {
                    if( d->kind == Declaration::Field )
                        if( deref(d->getType())->kind == Type::Object )
                            error(d->pos, "union fields cannot be of object type");
                }
            t->objectInit = checkIfObjectInit(t);
            t->pointerInit = checkIfPointerInit(t);
            break;
        case Declaration::ConstDecl:
            sub->setType(toType(sub->c));
            break;
        case Declaration::Procedure:
            visitProcedure(sub);
            break;
        }

        sub = sub->next;
    }

    foreach( Expression* e, newExprs )
    {
        if( module->toDelete == 0 )
            module->toDelete = new ToDelete(e);
        else
            module->toDelete->append(new ToDelete(e));
    }
    newExprs.clear();

    module->validated = errors.isEmpty();

    return errors.isEmpty();
}

Type*Validator::deref(Type* t)
{
    if( t && t->kind == Type::NameRef )
        return deref(t->getType());
    else if( t )
        return t;
    else
        return mdl->getBasicType(Type::Undefined);
}

void Validator::visitProcedure(Declaration* proc)
{
    if( proc->forward || proc->extern_ )
    {
        return; // TODO: check param compat
    }
    pc = 0;
    curProc = proc;
    stack.clear();
    visitStatSeq(proc->body);
    if( !stack.isEmpty() )
        error(proc->pos, "stack not empty at the end of the procedure");
    curProc = 0;
}

void Validator::visitStatSeq(Statement* stat)
{
    while(stat)
    {
        // TODO: check types
        switch(stat->kind)
        {
        case Statement::ExprStat:
            {
                expectN(0, stat);
                Expression* e = visitExpr(stat->e);
                if( stack.isEmpty() )
                {
                    Q_ASSERT( stat->args == 0 );
                    stat->args = e;
                    if( e->next )
                    {
                        // split ExprStat
                        Statement* s = new Statement();
                        s->kind = (TokenType)Statement::ExprStat;
                        s->pos = e->next->pos;
                        s->e = e->next;
                        e->next = 0;
                        s->next = stat->next;
                        stat->next = s;
                    }
                }else if( e != 0 && e->next != 0 )
                {
                    // visitExpr returned after non-function call and we would expect the stack to be empty
                    error(e->pos,QString("stack is not empty (%1 elements on stack)").arg(stack.size()));
                }
            }
            break;
        case Tok_EXIT:
        case Tok_GOTO:
        case Tok_LABEL:
        case Tok_LINE:
            expectN(0, stat);
            break;
        case Tok_FREE:
        case Tok_POP:
        case Tok_STARG:
        case Tok_STLOC:
        case Tok_STLOC_S:
        case Tok_STLOC_0:
        case Tok_STLOC_1:
        case Tok_STLOC_2:
        case Tok_STLOC_3:
        case Tok_STVAR:
            expectN(1, stat);
            break;
        case Tok_IF:
            expectN(0, stat);
            stat = visitIfThenElse(stat);
            break;
        case Tok_LOOP:
            expectN(0, stat);
            visitLoop(stat);
            break;
        case Tok_REPEAT:
            expectN(0, stat);
            visitRepeat(stat);
            break;
        case Tok_RET:
            if( curProc && curProc->getType() )
                expectN(1, stat);
            else
                expectN(0, stat);
            break;
        case Tok_STELEM:
        case Tok_STELEM_I1:
        case Tok_STELEM_I2:
        case Tok_STELEM_I4:
        case Tok_STELEM_I8:
        case Tok_STELEM_R4:
        case Tok_STELEM_R8:
        case Tok_STELEM_IP:
            // TODO: IPP
            expectN(3, stat);
            break;
        case Tok_STFLD:
        case Tok_STIND:
        case Tok_STIND_I1:
        case Tok_STIND_I2:
        case Tok_STIND_I4:
        case Tok_STIND_I8:
        case Tok_STIND_R4:
        case Tok_STIND_R8:
        case Tok_STIND_IP:
        case Tok_STIND_IPP:
        case Tok_STRCPY:
            expectN(2, stat);
            break;
        case Tok_SWITCH:
            expectN(0, stat);
            stat = visitSwitch(stat);
            break;
        case Tok_WHILE:
            expectN(0, stat);
            visitWhile(stat);
            break;
        default:
            error(stat->pos, QString("unexpected statement operator '%1'").arg(tokenTypeString(stat->kind)));
            break;
        }
        stat = nextStat(stat);
    }
}

Statement* Validator::visitIfThenElse(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == Tok_IF );
    visitExpr(stat->e);
    pc++; // THEN
    expectN(1, stat); // IF args points to the boolean expression, TODO: check boolean
    visitStatSeq(stat->body);
    if( stat->next && stat->next->kind == Tok_ELSE )
    {
        stat = nextStat(stat);
        visitStatSeq(stat->body);
    }
    pc++; // END
    return stat;
}

void Validator::visitLoop(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == Tok_LOOP );
    visitStatSeq(stat->body);
    pc++; // END
}

void Validator::visitRepeat(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == Tok_REPEAT );
    visitStatSeq(stat->body);
    pc++; // UNTIL
    visitExpr(stat->e);
    expectN(1, stat); // TODO check boolean
    pc++; // END
}

Statement*Validator::visitSwitch(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == Tok_SWITCH );
    visitExpr(stat->e);
    expectN(1, stat); // TODO check boolean
    while( stat->next && stat->next->kind == Tok_CASE )
    {
        stat = nextStat(stat);
        // stat->e is a list of integers, each as an Expression
        pc++; // THEN
        visitStatSeq(stat->body);
    }
    if( stat->next && stat->next->kind == Tok_ELSE )
    {
        stat = nextStat(stat);
        visitStatSeq(stat->body);
    }
    pc++; // END
    return stat;
}

void Validator::visitWhile(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == Tok_WHILE );
    visitExpr(stat->e);
    expectN(1, stat); // TODO: check boolean
    pc++; // DO
    visitStatSeq(stat->body);
    pc++; // END
}

Statement*Validator::nextStat(Statement* stat)
{
    if( stat->kind != Statement::ExprStat )
        pc++;
    return stat->next;
}

static bool isInt32(Type* t)
{
    if( 0 )
        return false;
    else
        return t->isInt32OnStack();
}

static bool isPointer(Type* t)
{
    if( t == 0 )
        return false;
    else
        return t->isPointer();
}

static bool isFunc(Type* t)
{
    if( t == 0 )
        return false;
    else
        return t->isFuncOnStack();
}

static bool isMeth(Type* t)
{
    if( t == 0 )
        return false;
    else
        return t->isMethOnStack();
}

Expression* Validator::visitExpr(Expression* e)
{
    while(e)
    {
        switch( e->kind )
        {
        case Tok_ADD:
        case Tok_DIV_UN:
        case Tok_DIV:
        case Tok_MUL:
        case Tok_SUB:
            if( expectN(2,e) )
            {
                e->lhs = stackAt(-2);
                e->rhs = stackAt(-1);
                Type* lhs = deref(e->lhs->getType());
                Type* rhs = deref(e->rhs->getType());
                Type* t = 0;
                if( lhs->isInt64() && rhs->isInt64() )
                    t = mdl->getBasicType(Type::INT64);
                else if( isInt32(lhs) && isInt32(rhs) )
                    t = mdl->getBasicType(Type::INT32);
                else if( lhs->kind == Type::FLOAT32 && rhs->kind == Type::FLOAT32 )
                    t = mdl->getBasicType(Type::FLOAT32);
                else if( lhs->kind == Type::FLOAT64 && rhs->kind == Type::FLOAT64 )
                    t = mdl->getBasicType(Type::FLOAT64);
                else if( lhs->isFloat() && rhs->isFloat() )
                    t = mdl->getBasicType(Type::FLOAT64);
                else
                    error(e->pos, "the values on the stack are not compatible with the operation");
                e->setType(t);
                stack.pop_back();
                stack.last() = e;
            }
            break;
        case Tok_AND:
        case Tok_OR:
        case Tok_XOR:
        case Tok_REM:
        case Tok_REM_UN:
            if( expectN(2,e) )
            {
                e->lhs = stackAt(-2);
                e->rhs = stackAt(-1);
                Type* lhs = deref(e->lhs->getType());
                Type* rhs = deref(e->rhs->getType());
                Type* t = 0;
                if( lhs->isInt64() && rhs->isInt64() )
                    t = mdl->getBasicType(Type::INT64);
                else if( isInt32(lhs) && isInt32(rhs) )
                    t = mdl->getBasicType(Type::INT32);
                else
                    error(e->pos, "the values on the stack are not compatible with the operation");
                e->setType(t);
                stack.pop_back();
                stack.last() = e;
            }
            break;
        case Tok_SHL:
        case Tok_SHR_UN:
        case Tok_SHR:
            if( expectN(2,e) )
            {
                e->lhs = stackAt(-2); // value
                e->rhs = stackAt(-1); // shift amount
                Type* lhs = deref(e->lhs->getType());
                Type* rhs = deref(e->rhs->getType());
                Type* t = 0;
                if( lhs->isInt64() && rhs->isInt64() )
                    t = mdl->getBasicType(Type::INT64);
                else if( isInt32(lhs) && isInt32(rhs) )
                    t = mdl->getBasicType(Type::INT32);
                else
                    error(e->pos, "the values on the stack are not compatible with the operation");
                e->setType(t);
                stack.pop_back();
                stack.last() = e;
            }
            break;
        case Tok_NOT:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1);
                Type* lhs = deref(e->lhs->getType());
                Type* t = 0;
                if( lhs->isInt64() )
                    t = mdl->getBasicType(Type::INT64);
                else if( isInt32(lhs) )
                    t = mdl->getBasicType(Type::INT32);
                else
                    error(e->pos, "the values on the stack are not compatible with the operation");
                e->setType(t);
                stack.last() = e;
            }
            break;
        case Tok_NEG:
        case Tok_ABS:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1);
                Type* lhs = deref(e->lhs->getType());
                Type* t = 0;
                if( lhs->isInt64() )
                    t = mdl->getBasicType(Type::INT64);
                else if( isInt32(lhs) )
                    t = mdl->getBasicType(Type::INT32);
                else if( lhs->kind == Type::FLOAT32 )
                    t = mdl->getBasicType(Type::FLOAT32);
                else if( lhs->kind == Type::FLOAT64 )
                    t = mdl->getBasicType(Type::FLOAT64);
                else
                    error(e->pos, "the values on the stack are not compatible with the operation");
                e->setType(t);
                stack.last() = e;
            }
            break;
        case Tok_LDC_I4_0:
        case Tok_LDC_I4_1:
        case Tok_LDC_I4_2:
        case Tok_LDC_I4_3:
        case Tok_LDC_I4_4:
        case Tok_LDC_I4_5:
        case Tok_LDC_I4_6:
        case Tok_LDC_I4_7:
        case Tok_LDC_I4_8:
        case Tok_LDC_I4_M1:
        case Tok_LDC_I4_S:
        case Tok_LDC_I4:
            e->setType(mdl->getBasicType(Type::INT32));
            stack.push_back(e);
            break;
        case Tok_LDC_I8:
            e->setType(mdl->getBasicType(Type::INT64));
            stack.push_back(e);
            break;
        case Tok_LDC_R4:
            e->setType(mdl->getBasicType(Type::FLOAT32));
            stack.push_back(e);
            break;
        case Tok_LDC_R8:
            e->setType(mdl->getBasicType(Type::FLOAT64));
            stack.push_back(e);
            break;
        // TODO: LDC_IP
        case Tok_LDNULL:
            e->setType(mdl->getBasicType(Type::NIL));
            stack.push_back(e);
            break;
        case Tok_LDSTR:
            e->setType(mdl->getBasicType(Type::StringLit));
            stack.push_back(e);
            break;
        case Tok_LDOBJ:
            e->setType(toType(e->c));
            stack.push_back(e);
            break;
        case Tok_LDPROC:
            stack.push_back(e);
            break;
        case Tok_LDMETH:
            {
                // expect the object instance pointer on the stack
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                Type* ot1 = deref(lhsT->getType());
                if( lhsT->kind != Type::Pointer || ot1->kind != Type::Object)
                {
                    error(e->pos, "expecting a pointer to object on the stack");
                    break;
                }
                // TODO: check whether object on stack is compat with e->d
                stack.back() = e; // replace the stack top with methref
            }
            break;
        case Tok_SIZEOF:
            e->setType(mdl->getBasicType(Type::INT32));
            stack.push_back(e);
            break;
        case Tok_CONV_I1:
        case Tok_CONV_I2:
        case Tok_CONV_I4:
        case Tok_CONV_U1:
        case Tok_CONV_U2:
        case Tok_CONV_U4:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::INT32));
            stack.back() = e;
            break;
        case Tok_CONV_I8:
        case Tok_CONV_U8:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::INT64));
            stack.back() = e;
            break;
        case Tok_CONV_R4:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::FLOAT32));
            stack.back() = e;
            break;
        case Tok_CONV_R8:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::FLOAT64));
            stack.back() = e;
            break;
        case Tok_CASTPTR:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                if( lhsT->kind != Type::Pointer )
                {
                    error(e->pos, "expecting a pointer on the stack");
                    break;
                }
                e->setType(deref(e->d->getType())); // TODO: check if e->d->getType is indeed a pointer type
                stack.back() = e;
            }
            break;
        case Tok_INITOBJ:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                Type* t1 = deref(lhsT->getType());
                if( lhsT->kind != Type::Pointer ||
                        !(t1->kind == Type::Struct || t1->kind == Type::Union ||
                            t1->kind != Type::Object || t1->kind != Type::Array) ||
                         (t1->kind == Type::Array && t1->len == 0) )
                {
                    error(e->pos, "expecting a pointer to struct, union, object or fixed size array on the stack");
                    break;
                }
                Type* t2 = deref(e->d->getType());
                if( !equal(t2, t1) )
                {
                    error(e->pos, "initobj type not compatible with type on the stack");
                    break;
                }
                stack.pop_back();
            }
            break;
        case Tok_CEQ:
        case Tok_CGT_UN:
        case Tok_CGT:
        case Tok_CLT_UN:
        case Tok_CLT:
            if( expectN(2,e) )
            {
                e->lhs = stackAt(-2);
                e->rhs = stackAt(-1);
                Type* lhs = deref(e->lhs->getType());
                Type* rhs = deref(e->rhs->getType());
                Type* t = 0;
                if( (lhs->isInt64() && rhs->isInt64()) ||
                        (isInt32(lhs) && isInt32(rhs)) ||
                        (lhs->kind == Type::FLOAT32 && rhs->kind == Type::FLOAT32) ||
                        (lhs->kind == Type::FLOAT64 && rhs->kind == Type::FLOAT64) ||
                        (isInt32(lhs) && isPointer(rhs)) ||
                        (isPointer(lhs) && isInt32(rhs)) ||
                        (isPointer(lhs) && isPointer(rhs)) ||
                        (isFunc(lhs) && isFunc(rhs)) ||
                        (isMeth(lhs) && isMeth(rhs)))
                    t = mdl->getBasicType(Type::INT32);
                else
                    error(e->pos, "the values on the stack are not compatible with the operation");
                e->setType(t);
                stack.pop_back();
                stack.last() = e;
            }
            break;
        case Tok_LDARG_0:
        case Tok_LDARG_1:
        case Tok_LDARG_2:
        case Tok_LDARG_3:
        case Tok_LDARG_S:
        case Tok_LDARG:
        case Tok_LDARGA_S:
        case Tok_LDARGA:
            {
                DeclList params = curProc->getParams();
                if( e->id >= params.size() )
                {
                    error(e->pos, "the referenced parameter does not exist");
                    break;
                }
                if( e->kind == Tok_LDARGA_S || e->kind == Tok_LDARGA )
                {
                    Type* ptr = new Type();
                    ptr->kind = Type::Pointer;
                    ptr->setType(params[e->id]->getType());
                    e->setType(ptr);
                }else
                    e->setType(params[e->id]->getType());
                stack.push_back(e);
            }
            break;
        case Tok_LDLOC_0:
        case Tok_LDLOC_1:
        case Tok_LDLOC_2:
        case Tok_LDLOC_3:
        case Tok_LDLOC_S:
        case Tok_LDLOC:
        case Tok_LDLOCA_S:
        case Tok_LDLOCA:
            {
                DeclList locals = curProc->getLocals();
                if( e->id >= locals.size() )
                {
                    error(e->pos, "the referenced local variable does not exist");
                    break;
                }
                if( e->kind == Tok_LDLOCA_S || e->kind == Tok_LDLOCA )
                {
                    Type* ptr = new Type();
                    ptr->kind = Type::Pointer;
                    ptr->setType(locals[e->id]->getType());
                    e->setType(ptr);
                }else
                    e->setType(locals[e->id]->getType());
                stack.push_back(e);
            }
            break;
        case Tok_LDELEM_I1:
        case Tok_LDELEM_I2:
        case Tok_LDELEM_I4:
        case Tok_LDELEM_I8:
        case Tok_LDELEM_IP:
            // TODO IPP
        case Tok_LDELEM_R4:
        case Tok_LDELEM_R8:
        case Tok_LDELEM_U1:
        case Tok_LDELEM_U2:
        case Tok_LDELEM_U4:
        case Tok_LDELEM_U8:
        case Tok_LDELEM:
        case Tok_LDELEMA:
            if( expectN(2,e) )
            {
                e->lhs = stackAt(-2); // pointer to array
                e->rhs = stackAt(-1); // index
                Type* lhsT = deref(e->lhs->getType());
                Type* rhsT = deref(e->rhs->getType());
                Type* array = deref(lhsT->getType());
                if( lhsT->kind != Type::Pointer || array->kind != Type::Array || !rhsT->isInteger() )
                {
                    error(e->pos, "expecting a pointer to array and an integer index on the stack");
                    break;
                }
                Type* et1 = tokToBasicType(mdl, e->kind);
                if( e->kind == Tok_LDELEM )
                    et1 = deref(e->d->getType());
                Type* et2 = deref(array->getType());
                if( et1 && !equal(et1,et2) )
                {
                    error(e->pos, "ldelem type not compatible with array element type on the stack");
                    break;
                }
                if( e->kind == Tok_LDELEMA )
                {
                    Type* ptr = new Type();
                    ptr->kind = Type::Pointer;
                    ptr->setType(et2);
                    e->setType(ptr);
                }else
                    e->setType(et2);
                stack.pop_back();
                stack.back() = e;
            }
            break;
        case Tok_LDFLD:
        case Tok_LDFLDA:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                Type* ot1 = deref(lhsT->getType());
                if( lhsT->kind != Type::Pointer ||
                        !(ot1->kind == Type::Struct || ot1->kind == Type::Union || ot1->kind == Type::Object) )
                {
                    error(e->pos, "expecting a pointer to struct, union or object on the stack");
                    break;
                }
                Type* ft = deref(e->d->getType());
                Q_ASSERT(e->d->outer);
                Type* ot2 = deref(e->d->outer->getType());
                if( !equal(ot2, ot1) )
                {
                    error(e->pos, "ldfld type not compatible with type on the stack");
                    break;
                }

                if( e->kind == Tok_LDFLDA )
                {
                    Type* ptr = new Type();
                    ptr->kind = Type::Pointer;
                    ptr->setType(ft);
                    e->setType(ptr);
                }else
                    e->setType(ft);
                stack.back() = e;
            }
            break;
        case Tok_LDVAR:
        case Tok_LDVARA:
            {
                Type* t = deref(e->d->getType());
                if( e->kind == Tok_LDVARA )
                {
                    Type* ptr = new Type();
                    ptr->kind = Type::Pointer;
                    ptr->setType(t);
                    e->setType(ptr);
                }else
                    e->setType(t);
                stack.push_back(e);
            }
            break;
        case Tok_PTROFF:
            if( expectN(2,e) )
            {
                e->lhs = stackAt(-2); // pointer
                e->rhs = stackAt(-1); // offset
                Type* lhsT = deref(e->lhs->getType());
                Type* rhsT = deref(e->rhs->getType());
                if( lhsT->kind != Type::Pointer || !(rhsT->isInteger() || isInt32(rhsT) || rhsT->kind == Type::INTPTR) )
                {
                    error(e->pos, "expecting a pointer and an integer offset on the stack");
                    break;
                }
                e->setType(lhsT);
                stack.pop_back();
                stack.back() = e;
            }
            break;
        case Tok_LDIND_I1:
        case Tok_LDIND_I2:
        case Tok_LDIND_I4:
        case Tok_LDIND_I8:
        case Tok_LDIND_IP:
        case Tok_LDIND_IPP:
        case Tok_LDIND_R4:
        case Tok_LDIND_R8:
        case Tok_LDIND_U1:
        case Tok_LDIND_U2:
        case Tok_LDIND_U4:
        case Tok_LDIND_U8:
        case Tok_LDIND:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                if( lhsT->kind != Type::Pointer && lhsT->kind != Type::StringLit ) {
                    error(e->pos, "expecting a pointer on the stack"); break;
                }
                Type* bt1 = deref(lhsT->getType());
                Type* bt2 = tokToBasicType(mdl, e->kind);
                if( e->kind == Tok_LDIND )
                    bt2 = deref(e->d->getType());
                if( e->kind == Tok_LDIND &&
                        (bt1->kind == Type::Array && bt1->len == 0 && deref(bt1->getType())->kind == Type::CHAR ||
                         lhsT->kind == Type::StringLit ) &&
                        bt2->kind == Type::Array && bt2->len != 0 && deref(bt2->getType())->kind == Type::CHAR )
                {
                    ; // ok to store an open char array or strlit in a non-open char array on stack.
                }else if( !equal(bt2, bt1) )
                {
                    error(e->pos, "ldind type not compatible with type on the stack");
                    break;
                }
                if( e->kind == Tok_LDIND )
                    e->setType(bt2);
                else
                    e->setType(bt1);
                stack.back() = e;
            }
            break;
        case Tok_NEWOBJ:
            {
                Type* t = deref(e->d->getType());
                Type* ptr = new Type();
                ptr->kind = Type::Pointer;
                ptr->setType(t);
                ptr->objectInit = t->objectInit;
                ptr->pointerInit = t->pointerInit;
                e->setType(ptr);
                stack.push_back(e);
            }
            break;
        case Tok_NEWARR:
        case Tok_NEWVLA:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // numElems
                Type* lhsT = deref(e->lhs->getType());
                if( !isInt32(lhsT) && lhsT->kind == Type::INTPTR )
                    error(e->pos, "expecing an int32 or intptr on stack");
                Type* t = deref(e->d->getType());
                Type* array = new Type();
                array->kind = Type::Array;
                array->objectInit = t->objectInit;
                array->pointerInit = t->pointerInit;
                array->setType(t);
                Type* ptr = new Type();
                ptr->kind = Type::Pointer;
                ptr->setType(array);
                ptr->objectInit = t->objectInit;
                ptr->pointerInit = t->pointerInit;
                e->setType(ptr);
                stack.back() = e;
            }
            break;
        case Tok_ISINST:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                Type* t1 = deref(lhsT->getType());
                if( lhsT->kind != Type::Pointer || t1->kind != Type::Object )
                {
                    error(e->pos, "expecting a pointer to object on the stack");
                    break;
                }
                e->setType(mdl->getBasicType(Type::INT32));
                stack.back() = e;
            }
            break;
        case Tok_DUP:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1);
                e->setType(e->lhs->getType());
                stack.push_back(e);
            }
            break;
        case Tok_NOP:
            break;
        case Tok_CALLI:
        case Tok_CALLVI:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // func/methref
                stack.pop_back();
                Type* proc = deref(e->lhs->getType());
                if( e->kind == Tok_CALLVI && !proc->typebound )
                {
                    error(e->pos, "expecting a methref on the stack");
                    break;
                }else if(e->kind == Tok_CALLI && proc->typebound)
                {
                    error(e->pos, "expecting a function pointer on the stack");
                    break;
                }
                const int numOfParams = proc->subs.size();
                if( !expectN(numOfParams, e) )
                    break;

                e->rhs = eatStack(numOfParams);
                // TODO: check param compat?

                if(proc->getType())
                {
                    e->setType(proc->getType());
                    stack.push_back(e);
                }else // we're in a ExprStat which needs to be split
                    return e;
            }
            break;
        case Tok_CALL:
            {
                Declaration* proc = e->d;
                DeclList params = proc->getParams();
                const int numOfParams = params.size();
                if( !expectN(numOfParams, e) )
                    break;
                e->rhs = eatStack(numOfParams);
                // TODO: check param compat?
                // TODO: varargs?

                if(proc->getType())
                {
                    e->setType(proc->getType());
                    stack.push_back(e);
                }else // we're in a ExprStat which needs to be split
                    return e;
            }
            break;
        case Tok_CALLVIRT:
            {
                Declaration* proc = e->d;
                DeclList params = proc->getParams();
                const int numOfParams = params.size();
                if( !expectN(numOfParams, e) )
                    break;
                e->rhs = eatStack(numOfParams-1); // all true params without self
                // TODO: check param compat?

                e->lhs = stack.takeLast(); // self
                // TODO: is this necessary, or just the first param?

                if(proc->getType())
                {
                    e->setType(proc->getType());
                    stack.push_back(e);
                }else // we're in a ExprStat which needs to be split
                    return e;
            }
            break;
        case Tok_IIF:
            {
                Q_ASSERT(e->next->kind == Tok_THEN && e->next->next->kind == Tok_ELSE);
                Expression* iif_ = e;
                Expression* then_ = e->next;
                Expression* else_ = e->next->next;

                visitExpr(iif_->e);
                if( !expectN(1, iif_) )
                    break;
                iif_->lhs = eatStack(1); // TODO: check boolean
                iif_->rhs = then_;
                visitExpr(then_->e);
                if( !expectN(1, then_) )
                    break;
                then_->lhs = eatStack(1);
                visitExpr(else_->e);
                if( !expectN(1, else_) )
                    break;
                then_->rhs = eatStack(1);
                e->setType(then_->lhs->getType());
                stack.push_back(e);
                pc += 2;
                e = else_;
            }
            break;
        default:
            error(e->pos, QString("unexpected expression operator '%1'").arg(tokenTypeString(e->kind)));
            break;
        }

        pc++;
        if( e->next == 0 )
            return e;
        e = e->next;
    }
}

void Validator::error(const Mic::RowCol& pos, const QString& str)
{
    Error e;
    e.msg = str;
    e.pos = pos;
    e.pc = pc;
    e.where = curProc->toPath();
    errors << e;
}

bool Validator::expectN(quint32 n, Expression* e)
{
    if( stack.size() < n )
    {
        error(e->pos,QString("expecting %1 values on expression stack").arg(n));
        return false;
    }else
        return true;
}

bool Validator::expectN(quint32 n, Statement* stat)
{
    if( stack.size() < n )
    {
        error(stat->pos,QString("expecting %1 values on expression stack").arg(n));
        return false;
    }else
    {
        stat->args = eatStack(n);
        return true;
    }
}

Expression*Validator::stackAt(int i) const
{
    if( i < 0 )
    {
        return stack[ stack.size() + i ];
    }else
        return stack[i];
}

Expression* Validator::eatStack(quint32 n)
{
    if( n == 1 )
    {
        // direct expression
        if( !stack.isEmpty() )
            return stack.takeLast();
        else
            return 0;
    }

    Expression* res = 0;

    // using one Argument expression for one or two arguments
    // stack: ..., arg1, arg2, arg3 (top)
    // statement->args->exp1{rhs=arg3, lhs=arg2}->exp2{rhs=arg1}
    while( !stack.isEmpty() && n > 0 )
    {
        Expression* a = new Expression();
        a->kind = (TokenType)Expression::Argument;
        a->rhs = stack.takeLast();
        n--;
        if( n && !stack.isEmpty() )
        {
           a->lhs = stack.takeLast();
           n--;
        }
        if( res == 0 )
        {
            res = a;
            newExprs.append(a);
        }else
            res->append(a);
    }

    return res;
}

Type*Validator::tokToBasicType(AstModel* mdl, int t)
{
    switch(t)
    {
    case Tok_LDELEM_I1:
    case Tok_LDIND_I1:
    case Tok_CONV_I1:
        return mdl->getBasicType(Type::INT8);
    case Tok_LDELEM_I2:
    case Tok_LDIND_I2:
    case Tok_CONV_I2:
        return mdl->getBasicType(Type::INT16);
    case Tok_LDELEM_I4:
    case Tok_LDIND_I4:
    case Tok_CONV_I4:
    case Tok_LDC_I4_0:
    case Tok_LDC_I4_1:
    case Tok_LDC_I4_2:
    case Tok_LDC_I4_3:
    case Tok_LDC_I4_4:
    case Tok_LDC_I4_5:
    case Tok_LDC_I4_6:
    case Tok_LDC_I4_7:
    case Tok_LDC_I4_8:
    case Tok_LDC_I4_M1:
    case Tok_LDC_I4_S:
    case Tok_LDC_I4:
        return mdl->getBasicType(Type::INT32);
    case Tok_LDELEM_I8:
    case Tok_LDIND_I8:
    case Tok_CONV_I8:
    case Tok_LDC_I8:
        return mdl->getBasicType(Type::INT64);
    case Tok_LDELEM_IP:
    case Tok_LDIND_IP:
        return mdl->getBasicType(Type::INTPTR);
    case Tok_LDIND_IPP:
        return mdl->getBasicType(Type::DBLINTPTR);
    case Tok_LDELEM_R4:
    case Tok_LDIND_R4:
    case Tok_CONV_R4:
    case Tok_LDC_R4:
        return mdl->getBasicType(Type::FLOAT32);
    case Tok_LDELEM_R8:
    case Tok_LDIND_R8:
    case Tok_CONV_R8:
    case Tok_LDC_R8:
        return mdl->getBasicType(Type::FLOAT64);
    case Tok_LDELEM_U1:
    case Tok_LDIND_U1:
    case Tok_CONV_U1:
        return mdl->getBasicType(Type::UINT8);
    case Tok_LDELEM_U2:
    case Tok_LDIND_U2:
    case Tok_CONV_U2:
        return mdl->getBasicType(Type::UINT16);
    case Tok_LDELEM_U4:
    case Tok_LDIND_U4:
    case Tok_CONV_U4:
        return mdl->getBasicType(Type::UINT32);
    case Tok_LDELEM_U8:
    case Tok_LDIND_U8:
    case Tok_CONV_U8:
        return mdl->getBasicType(Type::UINT64);
    case Tok_LDNULL:
        return mdl->getBasicType(Type::NIL);
    case Tok_LDSTR:
        return mdl->getBasicType(Type::StringLit);
    default:
        return 0;
    }
}

Type*Validator::toType(Constant* c)
{
    if( c == 0 )
        return 0;
    switch( c->kind )
    {
    case Constant::D:
        return mdl->getBasicType(Type::FLOAT64);
    case Constant::I:
        if( c->i <= std::numeric_limits<qint32>::max() && c->i >= std::numeric_limits<qint32>::min())
            return mdl->getBasicType(Type::INT32);
        else
            return mdl->getBasicType(Type::INT64);
    case Constant::S:
        return mdl->getBasicType(Type::StringLit);
    case Constant::B:
        return mdl->getBasicType(Type::ByteArrayLit);
    case Constant::R:
        return deref(c->r->getType());
    case Constant::C:
        return deref(c->c->type);
    default:
        return 0;
   }
}

bool Validator::equal(Type* lhs, Type* rhs)
{
    if( lhs == 0 || rhs == 0 )
        return false;
    if( lhs == rhs )
        return true;
    if( lhs->kind == Type::INTPTR && rhs->kind == Type::INTPTR ||
            lhs->kind == Type::Pointer && rhs->kind == Type::INTPTR ||
            lhs->kind == Type::INTPTR && rhs->kind == Type::Pointer )
        return true; // happens for ldind_ip, stind_ip, which we cannot check more precise
    if( lhs->kind == Type::Pointer && rhs->kind == Type::Pointer )
        return equal(deref(lhs->getType()), deref(rhs->getType()));
    return false;
}

bool Validator::checkIfObjectInit(Type* t)
{
    switch(t->kind)
    {
    case Type::Array:
        if( deref(t->getType())->kind == Type::Object )
            return true;
        break;
    case Type::Struct:
        foreach( Declaration* d, t->subs )
        {
            if( d->kind == Declaration::Field && deref(d->getType())->kind == Type::Object )
                return true;
        }
        return false;
    case Type::Union:
        return false;
    case Type::Object:
        return true;
    case Type::NameRef:
        return checkIfObjectInit(deref(t));
    default:
        return false;
    }
}

bool Validator::checkIfPointerInit(Type* t)
{
    if( !needsPointerInit )
        return false;
    switch(t->kind)
    {
    case Type::Array:
        if( deref(t->getType())->kind == Type::Pointer )
            return true;
        break;
    case Type::Struct:
    case Type::Union:
    case Type::Object:
        foreach( Declaration* d, t->subs )
        {
            if( d->kind == Declaration::Field && deref(d->getType())->kind == Type::Pointer )
                return true;
        }
        return false;
    case Type::NameRef:
        return checkIfObjectInit(deref(t));
    default:
        return false;
    }
}

