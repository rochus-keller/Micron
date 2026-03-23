/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MilVmCode2.h"
#include "MilValidator.h"
#include <QDateTime>
#include <QCoreApplication>
#include <QtDebug>
using namespace Mil;
using namespace Vm;

Code2::Code2(AstModel* mdl, quint8 pointerWidth, quint8 stackAlignment):Code(mdl, pointerWidth, stackAlignment),
    reverseArguments(false)
{
}

bool Code2::translateStatSeq(Procedure &proc, Statement *s)
{
    while(s)
    {
        switch(s->kind)
        {
        case Statement::ExprStat:
            expression(proc, s->args);
            break;
        case IL_starg:
        case IL_starg_s:
        case IL_stloc:
        case IL_stloc_s:
        case IL_stloc_0:
        case IL_stloc_1:
        case IL_stloc_2:
        case IL_stloc_3:
        case IL_stvar:
        case IL_ret:
        case IL_free:
            expression(proc, s->args);
            if( !translateStat(proc, s) )
                return false;
            break;
        case IL_pop:
#if 1
            expression(proc, s->args);
            if( !translateStat(proc, s) )
                return false;
#endif
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
        case IL_stfld:
        case IL_strcpy:
            expression(proc, s->args->lhs);
            expression(proc, s->args->rhs);
            if( !translateStat(proc, s) )
                return false;
            break;
        case IL_stelem_ipp:
        case IL_stelem:
        case IL_stelem_i1:
        case IL_stelem_i2:
        case IL_stelem_i4:
        case IL_stelem_i8:
        case IL_stelem_r4:
        case IL_stelem_r8:
        case IL_stelem_ip:
            expression(proc, s->args->next->rhs);
            expression(proc, s->args->lhs);
            expression(proc, s->args->rhs);
            if( !translateStat(proc, s) )
                return false;
            break;
        default:
            if( !translateStat(proc, s) )
                return false;
            break;
        }
        s = s->next;
    }
    return true;
}

bool Code2::translateStatExpr(Procedure &proc, Statement *s)
{
    Expression* e = s->args;
    return expression(proc, e);
}

static void collectArgs(Expression* e, QList<Expression*>& args)
{
    if( e && e->kind == Expression::Argument )
    {
        if( e->next )
            collectArgs(e->next, args);
        if( e->lhs )
            args << e->lhs;
        args << e->rhs;
    }else if(e)
        args << e;
}

#define BINOP1(op) \
    expression(proc, e->lhs); \
    expression(proc, e->rhs); \
    if( t->isInt32OnStack() ) \
        emitOp(proc, LL_ ## op ## _i4); \
    else if( t->isInt64()) \
        emitOp(proc, LL_ ## op ## _i8); \
    else if(t->kind == Type::FLOAT32) \
        emitOp(proc, LL_ ## op ## _r4); \
    else if(t->kind == Type::FLOAT64) \
        emitOp(proc, LL_ ## op ## _r8); \
    else \
        Q_ASSERT(false);

#define BINOP2(op) \
    expression(proc, e->lhs); \
    expression(proc, e->rhs); \
    if( t->isInt32OnStack() ) \
        emitOp(proc, LL_ ## op ## _i4); \
    else if( t->isInt64()) \
        emitOp(proc, LL_ ## op ## _i8); \
    else \
        Q_ASSERT(false);

#define UNOP1(op) \
    expression(proc, e->lhs); \
    if( t->isInt32OnStack() ) \
        emitOp(proc, LL_ ## op ## _i4); \
    else if( t->isInt64()) \
        emitOp(proc, LL_ ## op ## _i8); \
    else if(t->kind == Type::FLOAT32) \
        emitOp(proc, LL_ ## op ## _r4); \
    else if(t->kind == Type::FLOAT64) \
        emitOp(proc, LL_ ## op ## _r8); \
    else \
        Q_ASSERT(false);

#define UNOP2(op) \
    expression(proc, e->lhs); \
    if( t->isInt32OnStack() ) \
        emitOp(proc, LL_ ## op ## _i4); \
    else if( t->isInt64()) \
        emitOp(proc, LL_ ## op ## _i8); \
    else \
        Q_ASSERT(false);



bool Code2::expression(Procedure &proc, Expression* e)
{
    if( e == 0 )
        return true;
    Type* t = deref(e->getType());
    switch(e->kind)
    {
    case IL_add:
        BINOP1(add);
        break;
    case IL_div_un:
        BINOP2(div_un);
        break;
    case IL_div:
        BINOP1(div);
        break;
    case IL_mul:
        BINOP1(mul);
        break;
    case IL_rem:
        BINOP2(rem);
        break;
   case IL_rem_un:
        BINOP2(rem_un);
        break;
    case IL_sub:
        BINOP1(sub);
        break;

    case IL_and:
        BINOP2(and);
        break;
    case IL_or:
        BINOP2(or);
        break;
    case IL_xor:
        BINOP2(xor);
        break;

    case IL_shl:
        BINOP2(shl);
        break;
    case IL_shr_un:
        BINOP2(shr_un);
        break;
    case IL_shr:
        BINOP2(shr_un);
        break;

    case IL_neg:
        UNOP1(neg);
        break;

    case IL_abs:
        UNOP1(abs);
        break;

    case IL_not:
        UNOP2(not);
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
    case IL_ldc_i8:
    case IL_ldc_r4:
    case IL_ldc_r8:
    case IL_ldnull:
    case IL_ldstr:
    case IL_ldc_obj:
        translateExpr(proc, e);
        break;

    case IL_conv_i1:
    case IL_conv_i2:
    case IL_conv_i4:
    case IL_conv_i8:
    case IL_conv_r4:
    case IL_conv_r8:
    case IL_conv_u1:
    case IL_conv_u2:
    case IL_conv_u4:
    case IL_conv_u8:
        expression(proc, e->lhs);
        translateExpr(proc, e);
        break;

    case IL_cast_i4:
    case IL_cast_i8:
    case IL_cast_r4:
    case IL_cast_r8:
        // this is a nop here; it is unnecessary to convert the stack slot
        expression(proc, e->lhs);
        break;


    case IL_ceq:
    case IL_cgt_un:
    case IL_cgt:
    case IL_clt_un:
    case IL_clt:
        expression(proc, e->lhs);
        expression(proc, e->rhs);
        translateExpr(proc, e);
        break;


    case IL_ldvar:
    case IL_ldvara:
        translateExpr(proc, e);
        break;

    case IL_ldarg_0:
    case IL_ldarg_1:
    case IL_ldarg_2:
    case IL_ldarg_3:
    case IL_ldarg_s:
    case IL_ldarg:
        translateExpr(proc, e);
        break;

    case IL_ldarga_s:
    case IL_ldarga:
        translateExpr(proc, e);
        break;

    case IL_ldloc_0:
    case IL_ldloc_1:
    case IL_ldloc_2:
    case IL_ldloc_3:
    case IL_ldloc_s:
    case IL_ldloc:
        translateExpr(proc, e);
        break;

    case IL_ldloca_s:
    case IL_ldloca:
        translateExpr(proc, e);
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
        expression(proc, e->lhs);
        translateExpr(proc, e);
        break;

    case IL_ldelem_i1:
    case IL_ldelem_i2:
    case IL_ldelem_i4:
    case IL_ldelem_i8:
    case IL_ldelem_ip:
    case IL_ldelem_r4:
    case IL_ldelem_r8:
    case IL_ldelem_u1:
    case IL_ldelem_u2:
    case IL_ldelem_u4:
    case IL_ldelem_u8:
    case IL_ldelem:
        expression(proc, e->lhs);
        expression(proc, e->rhs);
        translateExpr(proc, e);
        break;

    case IL_ldelema:
        expression(proc, e->lhs);
        expression(proc, e->rhs);
        translateExpr(proc, e);
        break;

    case IL_ldfld:
        expression(proc, e->lhs );
        translateExpr(proc, e);
        break;

    case IL_ldflda:
        expression(proc, e->lhs );
        translateExpr(proc, e);
        break;

    case IL_ldproc:
        translateExpr(proc, e);
        break;

    case IL_ldiface:
    case IL_ldmeth:
    case IL_castptr:
        expression(proc, e->lhs);
        translateExpr(proc, e);
        break;


    case IL_call:
    case IL_callinst:
    case IL_callvirt:
        {
            QList<Expression*> args;
            if( e->kind == IL_callinst || e->kind == IL_callvirt )
                args << e->lhs;
            collectArgs(e->rhs, args);
            pushArgs(proc, args);
            translateExpr(proc, e);
        }
        break;

    case IL_calli:
        {
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            pushArgs(proc, args);
            expression(proc, e->lhs );
            translateExpr(proc, e);
        }
        break;

    case IL_callmi:
        {
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            pushArgs(proc, args);
            expression(proc, e->lhs );
            translateExpr(proc, e);
        }
        break;


    case IL_newobj:
        translateExpr(proc, e);
        break;

    case IL_newarr:
        expression(proc, e->lhs);
        translateExpr(proc, e);
        break;


    case IL_initobj:
        expression(proc, e->lhs );
        translateExpr(proc, e);
        break;

    case IL_dup:
        // No expression(proc, e->lhs);
        translateExpr(proc, e);
        break;

    case IL_nop:
        break;

    case IL_ptroff:
        expression(proc, e->lhs);
        expression(proc, e->rhs);
        translateExpr(proc, e);
        break;

    case IL_iif: {
            Expression* if_ = e->e;
            Q_ASSERT(if_ && if_->kind == IL_if && if_->next->kind == IL_then && if_->next->next->kind == IL_else &&
                     if_->next->next->next == 0); // no IL_end
            Expression* then_ = if_->next;
            Expression* else_ = if_->next->next;
            expression(proc, if_->lhs);
            const int iif = emitOp(proc,LL_brfalse_i4);
            expression(proc, then_->lhs);
            const int end_of_then = emitOp(proc, LL_br);
            branch_here(proc,iif);
            expression(proc, else_->lhs);
            branch_here(proc,end_of_then);

        } break;

    case IL_isinst:
        expression(proc, e->lhs );
        translateExpr(proc, e);
        break;

    case IL_sizeof:
    case IL_newvla:
        qWarning() << "TODO: Code2" << s_opName[e->kind] << "not implemented";
        break;

    default:
        Q_ASSERT(false);
    }
    return true;
}

void Code2::pushArgs(Procedure &proc, const QList<Expression *> &args)
{
    if( reverseArguments )
        for( int i = args.size()-1; i >= 0; i-- )
            expression(proc, args[i]);
    else
        for( int i = 0; i < args.size(); i++ )
            expression(proc, args[i]);

}

