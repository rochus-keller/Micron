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
#include <float.h>
#include <math.h>
using namespace Mil;

// NOTE: removed all INTPTR from the checks. There should never be an actual INTPTR type on stack since there is no conv_ip

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
                        Declaration* p = base->findSubByName(d->name, true);
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
    if( proc->typebound )
    {
        DeclList params = proc->getParams();
        if( proc->outer && (deref(params.first()->getType())->kind != Type::Pointer ||
                 deref(deref(params.first()->getType())->getType()) != proc->outer->getType() ) )
            error(params.first()->pos, "the SELF parameter must be a pointer to the bound object type");
    }
    gotos.clear(); labels.clear();
    visitStatSeq(proc->body);
    if( !stack.isEmpty() )
        error(proc->pos, "stack not empty at the end of the procedure");

    foreach( const NamePos& np, gotos )
    {
        const int label = findLabel(np.name->name);
        if( label == -1 )
            error(np.name->pos, QString("cannot find label: '%1'").arg(np.name->name));
        else if( !np.pos.contains( labels[label].pos.last() ) )
            error(np.name->pos, QString("cannot cannot jump to this label: '%1'").arg(np.name->name));
    }

    curProc = 0;
}

static inline bool isInt32(Type* t)
{
    if( 0 )
        return false;
    else
        return t->isInt32OnStack();
}

void Validator::visitStatSeq(Statement* stat)
{
    blockStack.push_back(stat);

    while(stat)
    {
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
                        s->kind = (IL_op)Statement::ExprStat;
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
        case IL_goto:
            gotos << NamePos(stat, blockStack);
            break;
        case IL_label:
            labels << NamePos(stat, blockStack);
            break;
        case IL_line:
            expectN(0, stat); // TODO
            break;
        case IL_exit:
            if( loopStack.isEmpty() )
                error(stat->pos, "exit requires enclosing loop");
            break;
        case IL_pop:
            expectN(1, stat); // we have no reference type information to check here
            break;
        case IL_free:
            if( expectN(1, stat) )
            {
                Type* t = deref(stat->args->getType());
                if( t->kind != Type::Pointer )
                {
                    error(stat->pos, "expecting a pointer argument");
                    break;
                }
            }
            break;
        case IL_starg:
            if( expectN(1, stat) )
            {
                DeclList params = curProc->getParams();
                if( stat->id >= params.size() )
                {
                    error(stat->pos, "the referenced parameter does not exist");
                    break;
                }
                Type* lhsT = deref(params[stat->id]->getType());
                if( !assigCompat(lhsT,stat->args) )
                    error(stat->pos, "argument type not compatible with value on stack");
            }
            break;
        case IL_stloc:
        case IL_stloc_s:
        case IL_stloc_0:
        case IL_stloc_1:
        case IL_stloc_2:
        case IL_stloc_3:
            if( expectN(1, stat) )
            {
                DeclList locals = curProc->getLocals();
                if( stat->id >= locals.size() )
                {
                    error(stat->pos, "the referenced local does not exist");
                    break;
                }
                Type* lhsT = deref(locals[stat->id]->getType());
                if( !assigCompat(lhsT,stat->args) )
                    error(stat->pos, "local variable type not compatible with value on stack");
            }
            break;
        case IL_stvar:
            if( expectN(1, stat) )
            {
                Type* lhsT = deref(stat->d->getType());
                if( !assigCompat(lhsT,stat->args) )
                    error(stat->pos, "module variable type not compatible with value on stack");
            }
            break;
        case IL_if:
            expectN(0, stat);
            stat = visitIfThenElse(stat);
            break;
        case IL_loop:
            expectN(0, stat);
            visitLoop(stat);
            break;
        case IL_repeat:
            expectN(0, stat);
            visitRepeat(stat);
            break;
        case IL_ret:
            if( curProc && curProc->getType() )
            {
                if( expectN(1, stat) && !assigCompat(deref(curProc->getType()), stat->args) )
                    error(stat->pos,"return type incompatible with function type");
            }else
            {
                expectN(0, stat);
                if( curProc && curProc->getType() )
                    error(stat->pos,"return requires a value");
            }
            break;
        case IL_stelem:
        case IL_stelem_i1:
        case IL_stelem_i2:
        case IL_stelem_i4:
        case IL_stelem_i8:
        case IL_stelem_r4:
        case IL_stelem_r8:
        case IL_stelem_ip:
        case IL_stelem_ipp:
            if( expectN(3, stat) )
            {
                Type* aptr = deref(stat->args->next->rhs->getType());
                if( aptr->kind != Type::Pointer )
                    error(stat->pos,"first argument must be a pointer");
                Type* aos = deref(aptr->getType()); // array actually on stack
                if( aos->kind != Type::Array  )
                    error(stat->pos,"first argument must be a pointer to array");

                Type* index = deref(stat->args->lhs->getType());
                if( !isInt32(index) )
                    error(stat->pos,"second argument must be a 32 bit integer");

                Type* etOs = deref(aos->getType()); // element type on stack
                Type* refT = tokToBasicType(mdl, stat->kind);
                if( stat->kind == IL_stelem )
                    refT = deref(stat->d->getType());

                if( etOs && !equal(etOs,refT) )
                    error(stat->pos,"the element type on stack is not compatible with the required type");

                if( etOs == 0 )
                    etOs = refT;
                if( !assigCompat(etOs, stat->args->rhs) )
                    error(stat->pos,"value on stack is not compatible with the element type");
            }
            break;
        case IL_stfld:
            if( expectN(2, stat) )
            {
                Type* objptr = deref(stat->args->lhs->getType());
                if( objptr->kind != Type::Pointer )
                    error(stat->pos,"first argument must be a pointer");

                Type* objOs = deref(objptr->getType()); // object actually on stack
                if( objOs->kind != Type::Struct && objOs->kind != Type::Union && objOs->kind != Type::Object )
                    error(stat->pos,"first argument must be a pointer to struct, union or object");

                Type* refObj = deref(stat->d->outer->getType());
                if( objOs && !equal(objOs, refObj) )
                    error(stat->pos,"the pointer base type on stack is not compatible with the required type");

                Type* refFld = deref(stat->d->getType());
                if( !assigCompat(refFld, stat->args->rhs) )
                    error(stat->pos,"value on stack is not compatible with the pointer base type");
            }
            break;
        case IL_strcpy:
            if( expectN(2, stat) )
            {
                Type* lhsT = deref(stat->args->lhs->getType());
                if( lhsT->kind != Type::Pointer )
                    error(stat->pos,"first argument must be a pointer");
                Type* a = deref(lhsT->getType());
                if( a->kind != Type::Array || a->len != 0 || deref(a->getType())->kind != Type::CHAR )
                    error(stat->pos,"first argument must be a pointer to an open char array");
                Type* rhsT = deref(stat->args->rhs->getType());
                if( rhsT->kind == Type::Pointer )
                {
                    a = deref(rhsT->getType());
                    if( a->kind != Type::Array || a->len != 0 || deref(a->getType())->kind != Type::CHAR )
                        error(stat->pos,"second argument must be a pointer to an open char array or string literal");
                }else if( rhsT->kind != Type::StringLit )
                    error(stat->pos,"second argument must be a pointer to an open char array or string literal");
            }
            break;
        case IL_stind:
        case IL_stind_i1:
        case IL_stind_i2:
        case IL_stind_i4:
        case IL_stind_i8:
        case IL_stind_r4:
        case IL_stind_r8:
        case IL_stind_ip:
        case IL_stind_ipp:
            if( expectN(2, stat) )
            {
                Type* ptrT = deref(stat->args->lhs->getType());
                if( ptrT->kind != Type::Pointer )
                    error(stat->pos,"first argument must be a pointer");

                Type* baseOs = deref(ptrT->getType()); // pointer basetype on stack

                Type* refT = tokToBasicType(mdl, stat->kind);
                if( stat->kind == IL_stind )
                    refT = deref(stat->d->getType());

                if( baseOs && !equal(baseOs,refT) )
                    error(stat->pos,"the pointer base type on stack is not compatible with the required type");
                if( baseOs == 0 )
                    baseOs = refT;
                if( !assigCompat(baseOs, stat->args->rhs) )
                {
                    assigCompat(baseOs, stat->args->rhs); // TEST
                    error(stat->pos,"value on stack is not compatible with the pointer base type");
                }
            }
            break;
        case IL_switch:
            expectN(0, stat);
            stat = visitSwitch(stat);
            break;
        case IL_while:
            expectN(0, stat);
            visitWhile(stat);
            break;
        default:
            error(stat->pos, QString("unexpected statement operator '%1'").arg(s_opName[stat->kind]));
            break;
        }
        if( stat->kind != Statement::ExprStat && stack.size() != 0 )
            error(stat->pos,QString("evaluation stack not empty after statement"));

        stat = nextStat(stat);
    }

    blockStack.pop_back();
}

Statement* Validator::visitIfThenElse(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == IL_if );
    visitExpr(stat->e);
    pc++; // THEN
    expectN(1, stat); // IF args points to the boolean expression,
    if( !isInt32(deref(stat->args->getType())) )
        error(stat->pos,"expecting a 32 bit result of boolean expression");
    visitStatSeq(stat->body);
    if( stat->next && stat->next->kind == IL_else )
    {
        stat = nextStat(stat);
        visitStatSeq(stat->body);
    }
    pc++; // END
    return stat;
}

void Validator::visitLoop(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == IL_loop );
    loopStack.push_back(stat);
    visitStatSeq(stat->body);
    loopStack.pop_back();
    pc++; // END
}

void Validator::visitRepeat(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == IL_repeat );
    visitStatSeq(stat->body);
    pc++; // UNTIL
    visitExpr(stat->e);
    expectN(1, stat);
    if( !isInt32(deref(stat->args->getType())) )
        error(stat->pos,"expecting a 32 bit result of boolean expression");
    pc++; // END
}

Statement*Validator::visitSwitch(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == IL_switch );
    visitExpr(stat->e);
    expectN(1, stat);
    Type* t = deref(stat->args->getType());
    if( !isInt32(t) && !t->isInt64() )
        error(stat->pos,"expecting a 32 or 64 bit integer expression");
    while( stat->next && stat->next->kind == IL_case )
    {
        stat = nextStat(stat);
        Expression* e = stat->e;
        while( e )
        {
            // stat->e is a list of integers, each as an Expression
            Type* tt = deref(e->getType());
            if( !isInt32(tt) && !tt->isInt64() )
                error(stat->pos,"expecting a 32 or 64 bit integer expression");
            e = e->next;
        }
        pc++; // THEN
        visitStatSeq(stat->body);
    }
    if( stat->next && stat->next->kind == IL_else )
    {
        stat = nextStat(stat);
        visitStatSeq(stat->body);
    }
    pc++; // END
    return stat;
}

void Validator::visitWhile(Statement* stat)
{
    Q_ASSERT( stat && stat->kind == IL_while );
    visitExpr(stat->e);
    expectN(1, stat);
    if( !isInt32(deref(stat->args->getType())) )
        error(stat->pos,"expecting a 32 bit result of boolean expression");
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
        case IL_add:
        case IL_div_un:
        case IL_div:
        case IL_mul:
        case IL_sub:
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
        case IL_and:
        case IL_or:
        case IL_xor:
        case IL_rem:
        case IL_rem_un:
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
        case IL_shl:
        case IL_shr_un:
        case IL_shr:
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
        case IL_not:
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
        case IL_neg:
        case IL_abs:
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
        case IL_ldc_i4_0:
        case IL_ldc_i4_1:
        case IL_ldc_i4_2:
        case IL_ldc_i4_3:
        case IL_ldc_i4_4:
        case IL_ldc_i4_5:
        case IL_ldc_i4_6:
        case IL_ldc_i4_7:
        case IL_ldc_i4_8:
        case IL_ldc_i4_m1:
        case IL_ldc_i4_s:
        case IL_ldc_i4:
            e->setType(mdl->getBasicType(Type::INT32));
            stack.push_back(e);
            break;
        case IL_ldc_i8:
            e->setType(mdl->getBasicType(Type::INT64));
            stack.push_back(e);
            break;
        case IL_ldc_r4:
            e->setType(mdl->getBasicType(Type::FLOAT32));
            stack.push_back(e);
            break;
        case IL_ldc_r8:
            e->setType(mdl->getBasicType(Type::FLOAT64));
            stack.push_back(e);
            break;
        // TODO: LDC_ip
        case IL_ldnull:
            e->setType(mdl->getBasicType(Type::NIL));
            stack.push_back(e);
            break;
        case IL_ldstr:
            e->setType(mdl->getBasicType(Type::StringLit));
            stack.push_back(e);
            break;
        case IL_ldobj:
            e->setType(toType(e->c));
            stack.push_back(e);
            break;
        case IL_ldproc:
            if( e->d && (e->d->kind != Declaration::Procedure || e->d->typebound) )
                error(e->pos, "expecting an unbound procedure");
            if( e->d && e->d->inline_ )
                error(e->pos, "cannot take address of inline procedure");
            stack.push_back(e);
            break;
        case IL_ldmeth:
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
                if( e->d && (e->d->kind != Declaration::Procedure || !e->d->typebound) )
                    error(e->pos, "expecting a bound procedure");
                // TODO: check whether object on stack is compat with e->d
                stack.back() = e; // replace the stack top with methref
            }
            break;
        case IL_sizeof:
            e->setType(mdl->getBasicType(Type::INT32));
            stack.push_back(e);
            break;
        case IL_conv_i1:
        case IL_conv_i2:
        case IL_conv_i4:
        case IL_conv_u1:
        case IL_conv_u2:
        case IL_conv_u4:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::INT32));
            stack.back() = e;
            break;
        case IL_conv_i8:
        case IL_conv_u8:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::INT64));
            stack.back() = e;
            break;
        case IL_conv_r4:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::FLOAT32));
            stack.back() = e;
            break;
        case IL_conv_r8:
            if( !expectN(1,e) )
                break;
            e->lhs = stackAt(-1);
            e->setType(mdl->getBasicType(Type::FLOAT64));
            stack.back() = e;
            break;
        case IL_castptr:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                if( lhsT->kind != Type::Pointer )
                {
                    error(e->pos, "expecting a pointer on the stack");
                    break;
                }
                Type* ptr = new Type();
                ptr->kind = Type::Pointer;
                ptr->setType(e->d->getType());
                e->setType(ptr);
                stack.back() = e;
            }
            break;
        case IL_initobj:
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
        case IL_ceq:
        case IL_cgt_un:
        case IL_cgt:
        case IL_clt_un:
        case IL_clt:
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
        case IL_ldarg_0:
        case IL_ldarg_1:
        case IL_ldarg_2:
        case IL_ldarg_3:
        case IL_ldarg_s:
        case IL_ldarg:
        case IL_ldarga_s:
        case IL_ldarga:
            {
                DeclList params = curProc->getParams();
                if( e->id >= params.size() )
                {
                    error(e->pos, "the referenced parameter does not exist");
                    break;
                }
                if( e->kind == IL_ldarga_s || e->kind == IL_ldarga )
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
        case IL_ldloc_0:
        case IL_ldloc_1:
        case IL_ldloc_2:
        case IL_ldloc_3:
        case IL_ldloc_s:
        case IL_ldloc:
        case IL_ldloca_s:
        case IL_ldloca:
            {
                DeclList locals = curProc->getLocals();
                if( e->id >= locals.size() )
                {
                    error(e->pos, "the referenced local variable does not exist");
                    break;
                }
                if( e->kind == IL_ldloca_s || e->kind == IL_ldloca )
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
        case IL_ldelem_i1:
        case IL_ldelem_i2:
        case IL_ldelem_i4:
        case IL_ldelem_i8:
        case IL_ldelem_ip:
        case IL_ldelem_ipp:
        case IL_ldelem_r4:
        case IL_ldelem_r8:
        case IL_ldelem_u1:
        case IL_ldelem_u2:
        case IL_ldelem_u4:
        case IL_ldelem_u8:
        case IL_ldelem:
        case IL_ldelema:
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
                if( e->kind == IL_ldelem )
                    et1 = deref(e->d->getType());
                Type* et2 = deref(array->getType());
                if( et1 && !equal(et1,et2) )
                {
                    error(e->pos, "ldelem type not compatible with array element type on the stack");
                    break;
                }
                if( e->kind == IL_ldelema )
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
        case IL_ldfld:
        case IL_ldflda:
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

                if( e->kind == IL_ldflda )
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
        case IL_ldvar:
        case IL_ldvara:
            {
                Type* t = deref(e->d->getType());
                if( e->kind == IL_ldvara )
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
        case IL_ptroff:
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
        case IL_ldind_i1:
        case IL_ldind_i2:
        case IL_ldind_i4:
        case IL_ldind_i8:
        case IL_ldind_ip:
        case IL_ldind_ipp:
        case IL_ldind_r4:
        case IL_ldind_r8:
        case IL_ldind_u1:
        case IL_ldind_u2:
        case IL_ldind_u4:
        case IL_ldind_u8:
        case IL_ldind:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // ptr
                Type* lhsT = deref(e->lhs->getType());
                if( lhsT->kind != Type::Pointer && lhsT->kind != Type::StringLit ) {
                    error(e->pos, "expecting a pointer on the stack"); break;
                }
                Type* bt1 = deref(lhsT->getType());
                Type* bt2 = tokToBasicType(mdl, e->kind);
                if( e->kind == IL_ldind )
                    bt2 = deref(e->d->getType());
                if( e->kind == IL_ldind &&
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
                if( e->kind == IL_ldind )
                    e->setType(bt2);
                else
                    e->setType(bt1);
                stack.back() = e;
            }
            break;
        case IL_newobj:
            {
                Type* t = deref(e->d->getType());
                Type* ptr = new Type();
                ptr->kind = Type::Pointer;
                ptr->setType(t);
                if( t->kind != Type::Struct && t->kind != Type::Union && t->kind != Type::Object &&
                        t->kind == Type::Array && t->len == 0)
                    error(e->pos, "expecting a structured type of fixed size");
                ptr->objectInit = t->objectInit;
                ptr->pointerInit = t->pointerInit;
                e->setType(ptr);
                stack.push_back(e);
            }
            break;
        case IL_newarr:
        case IL_newvla:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // numElems
                Type* lhsT = deref(e->lhs->getType());
                if( !isInt32(lhsT) )
                    error(e->pos, "expecing an 32 bit integer on stack");
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
        case IL_isinst:
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
        case IL_dup:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1);
                e->setType(e->lhs->getType());
                stack.push_back(e);
            }
            break;
        case IL_nop:
            break;
        case IL_calli:
        case IL_callvi:
            if( expectN(1,e) )
            {
                e->lhs = stackAt(-1); // func/methref
                stack.pop_back();
                Type* proc = deref(e->lhs->getType());
                if( e->kind == IL_callvi && !proc->typebound )
                {
                    error(e->pos, "expecting a methref on the stack");
                    break;
                }else if(e->kind == IL_calli && proc->typebound)
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
        case IL_call:
        case IL_callvirt:
            {
                Declaration* proc = e->d;
                DeclList params = proc->getParams();
                const int numOfParams = params.size();
                if( !expectN(numOfParams, e) )
                    break;
                // TODO: check param compat?
                // TODO: varargs?

                if(e->kind == IL_callvirt )
                {

                    e->rhs = eatStack(numOfParams-1); // all true params without self
                    e->lhs = stack.takeLast(); // self
                }else
                    e->rhs = eatStack(numOfParams);


                if(proc->getType())
                {
                    e->setType(proc->getType());
                    stack.push_back(e);
                }else // we're in a ExprStat which needs to be split
                    return e;
            }
            break;
        case IL_iif:
            {
                Q_ASSERT(e->next->kind == IL_then && e->next->next->kind == IL_else);
                Expression* iif_ = e;
                Expression* then_ = e->next;
                Expression* else_ = e->next->next;

                visitExpr(iif_->e);
                if( !expectN(1, iif_) )
                    break;
                iif_->lhs = eatStack(1);
                if( !isInt32(deref(iif_->lhs->getType())) )
                    error(iif_->lhs->pos,"expecting a 32 bit result of boolean expression");

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
            error(e->pos, QString("unexpected expression operator '%1'").arg(s_opName[e->kind]));
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
    if( stack.size() != n )
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
        a->kind = (IL_op)Expression::Argument;
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
    case IL_ldelem_i1:
    case IL_stelem_i1:
    case IL_ldind_i1:
    case IL_stind_i1:
    case IL_conv_i1:
        return mdl->getBasicType(Type::INT8);
    case IL_ldelem_i2:
    case IL_stelem_i2:
    case IL_ldind_i2:
    case IL_conv_i2:
        return mdl->getBasicType(Type::INT16);
    case IL_ldelem_i4:
    case IL_stelem_i4:
    case IL_ldind_i4:
    case IL_stind_i4:
    case IL_conv_i4:
    case IL_ldc_i4_0:
    case IL_ldc_i4_1:
    case IL_ldc_i4_2:
    case IL_ldc_i4_3:
    case IL_ldc_i4_4:
    case IL_ldc_i4_5:
    case IL_ldc_i4_6:
    case IL_ldc_i4_7:
    case IL_ldc_i4_8:
    case IL_ldc_i4_m1:
    case IL_ldc_i4_s:
    case IL_ldc_i4:
        return mdl->getBasicType(Type::INT32);
    case IL_ldelem_i8:
    case IL_stelem_i8:
    case IL_ldind_i8:
    case IL_stind_i8:
    case IL_conv_i8:
    case IL_ldc_i8:
        return mdl->getBasicType(Type::INT64);
    case IL_ldelem_ip:
    case IL_stelem_ip:
    case IL_ldind_ip:
    case IL_stind_ip:
        return mdl->getBasicType(Type::INTPTR);
    case IL_ldind_ipp:
    case IL_stind_ipp:
        return mdl->getBasicType(Type::DBLINTPTR);
    case IL_ldelem_r4:
    case IL_stelem_r4:
    case IL_ldind_r4:
    case IL_stind_r4:
    case IL_conv_r4:
    case IL_ldc_r4:
        return mdl->getBasicType(Type::FLOAT32);
    case IL_ldelem_r8:
    case IL_stelem_r8:
    case IL_ldind_r8:
    case IL_stind_r8:
    case IL_conv_r8:
    case IL_ldc_r8:
        return mdl->getBasicType(Type::FLOAT64);
    case IL_ldelem_u1:
    case IL_ldind_u1:
    case IL_conv_u1:
        return mdl->getBasicType(Type::UINT8);
    case IL_ldelem_u2:
    case IL_ldind_u2:
    case IL_conv_u2:
        return mdl->getBasicType(Type::UINT16);
    case IL_ldelem_u4:
    case IL_ldind_u4:
    case IL_conv_u4:
        return mdl->getBasicType(Type::UINT32);
    case IL_ldelem_u8:
    case IL_ldind_u8:
    case IL_conv_u8:
        return mdl->getBasicType(Type::UINT64);
    case IL_ldnull:
        return mdl->getBasicType(Type::NIL);
    case IL_ldstr:
        return mdl->getBasicType(Type::StringLit);
    default:
        return 0;
    }
}

static int is_float_range(double d) {
    return (fabs(d) <= FLT_MAX) && (fabs(d) >= FLT_MIN);
}

static int is_representable_as_float(double d) {
    if (!is_float_range(d))
        return 0;
    return (d == (double)(float)d);
}

Type*Validator::toType(Constant* c)
{
    if( c == 0 )
        return 0;
    switch( c->kind )
    {
    case Constant::D:
        if( is_representable_as_float(c->d) )
            return mdl->getBasicType(Type::FLOAT32);
        else
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
    if( isInt32(lhs) && isInt32(rhs) )
        return true;
    if( lhs->isInt64() && rhs->isInt64() )
        return true;
    if( lhs->kind == Type::FLOAT32 && rhs->kind == Type::FLOAT32 )
        return true;
    if( (lhs->kind == Type::INTPTR && rhs->kind == Type::INTPTR) ||
            (lhs->kind == Type::Pointer && rhs->kind == Type::INTPTR) ||
            (lhs->kind == Type::INTPTR && rhs->kind == Type::Pointer) ||
            (lhs->kind == Type::Proc && !lhs->typebound && rhs->kind == Type::INTPTR) ||
            (lhs->kind == Type::INTPTR && rhs->kind == Type::Proc && !rhs->typebound) )
        return true; // happens for ldind_ip, stind_ip, which we cannot check more precisely
    if( lhs->kind == Type::Pointer && rhs->kind == Type::Pointer )
        return equal(deref(lhs->getType()), deref(rhs->getType()));
    if( lhs->kind == Type::Array && rhs->kind == Type::Array && lhs->len == rhs->len )
        return equal(deref(lhs->getType()), deref(rhs->getType()));
    if( (lhs->kind == Type::DBLINTPTR && rhs->kind == Type::Proc && rhs->typebound) ||
            (lhs->kind == Type::Proc && lhs->typebound && rhs->kind == Type::DBLINTPTR) )
        return true;
    return false;
}

bool Validator::assigCompat(Type* lhs, Type* rhs)
{
    if( lhs == 0 || rhs == 0 )
        return false;
    if( equal(lhs,rhs) )
        return true;
    if( (lhs->kind == Type::Pointer || lhs->kind == Type::INTPTR || lhs->kind == Type::Proc) &&
            rhs->kind == Type::NIL )
        return true;

    if( lhs->kind == Type::Pointer && deref(lhs->getType())->kind == Type::Struct &&
            rhs->kind == Type::Pointer && deref(rhs->getType())->kind == Type::Struct )
    {
        Type* lstruct = deref(lhs->getType());
        Type* rstruct = deref(rhs->getType());
        while( rstruct && !rstruct->subs.isEmpty() && deref(rstruct->subs.first()->getType())->kind == Type::Struct )
        {
            Type* sub = deref(rstruct->subs.first()->getType());
            if( equal(lstruct, sub) )
                return true;
            rstruct = sub;
        }
    }

    if( lhs->kind == Type::Proc && rhs->kind == Type::Proc )
    {
        if( lhs->typebound != rhs->typebound )
            return false;
        if( lhs->subs.size() != rhs->subs.size() )
            return false;
        for( int i = 0; i < lhs->subs.size(); i++ )
            if( !equal(deref(lhs->subs[i]->getType()), deref(rhs->subs[i]->getType()) ) )
                return false;
        if( !equal(deref(lhs->getType()),deref(rhs->getType() ) ) )
            return false;
        return true;
    }

    if( lhs->kind == Type::Object && rhs->kind == Type::Object && Type::isA(rhs, lhs) )
        return true;

    if( lhs->kind == Type::Pointer && rhs->kind == Type::Pointer )
        return assigCompat(deref(lhs->getType()), deref(rhs->getType()) );

    return false;
}

bool Validator::assigCompat(Type* lhs, Expression* rhs)
{
    switch( rhs->kind )
    {
    case IL_ldproc:
    case IL_ldmeth:
        return assigCompat(lhs, rhs->d);
    case IL_castptr:
    case IL_call:
    case IL_calli:
    case IL_callvirt:
    case IL_callvi:
    case IL_initobj:
    case IL_isinst:
    case IL_ldelem:
    case IL_ldelema:
    case IL_ldfld:
    case IL_ldflda:
    case IL_ldind:
    case IL_ldvar:
    case IL_ldvara:
    case IL_newarr:
    case IL_newvla:
    case IL_newobj:
    case IL_sizeof:
    case IL_ptroff:
    default:
        return assigCompat(lhs, deref(rhs->getType()));
    }
}

bool Validator::assigCompat(Type* lhs, Declaration* rhs)
{
    if( lhs->kind == Type::Proc && rhs->kind == Declaration::Procedure )
    {
        if( lhs->typebound != rhs->typebound )
            return false;
        DeclList params = rhs->getParams(false);
        if( lhs->subs.size() != params.size() )
            return false;
        for( int i = 0; i < params.size(); i++ )
            if( !equal(deref(lhs->subs[i]->getType()), deref(params[i]->getType()) ) )
                return false;
        if( !equal(deref(lhs->getType()),deref(rhs->getType() ) ) )
            return false;
        return true;
    }
    return assigCompat(lhs, deref(rhs->getType() ) );
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

int Validator::findLabel(const char* name) const
{
    for( int i = 0; i < labels.size(); i++ )
    {
        if( labels[i].name->name == name )
            return i;
    }
    return -1;
}

