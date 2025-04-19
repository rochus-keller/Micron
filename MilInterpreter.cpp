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

#include "MilInterpreter.h"
#include <QVector>
using namespace Mil;

enum IL_op
{
    IL_invalid,

    IL_add_i4, IL_add_i8, IL_add_r4, IL_add_r8,
    IL_sub_i4, IL_sub_i8, IL_sub_r4, IL_sub_r8,
    IL_mul_i4, IL_mul_i8, IL_mul_r4, IL_mul_r8,
    IL_div_i4, IL_div_i8, IL_div_r4, IL_div_r8, IL_div_un_i4, IL_div_un_i8,
    IL_rem_i4, IL_rem_i8, IL_rem_r4, IL_rem_r8, IL_rem_un_i4, IL_rem_un_i8,
    IL_abs_i4, IL_abs_i8, IL_abs_r4, IL_abs_r8,
    IL_neg_i4, IL_neg_i8, IL_neg_r4, IL_neg_r8,

    IL_and_i4, IL_and_i8, IL_or_i4, IL_or_i8,
    IL_xor_i4, IL_xor_i8, IL_not_i4, IL_not_i8,
    IL_shl_i4, IL_shl_i8, IL_shr_i4, IL_shr_i8, IL_shr_un_i4, IL_shr_un_i8,

    IL_ceq_i4, IL_ceq_i8, IL_ceq_r4, IL_ceq_r8,
    IL_cgt_i4, IL_cgt_i8, IL_cgt_r4, IL_cgt_r8,
    IL_cgt_u4, IL_cgt_u8,
    IL_clt_i4, IL_clt_i8, IL_clt_r4, IL_clt_r8,
    IL_clt_u4, IL_clt_u8,

    IL_conv_i1_i4, IL_conv_i1_i8, IL_conv_i1_r4, IL_conv_i1_r8, // to_from
    IL_conv_i2_i4, IL_conv_i2_i8, IL_conv_i2_r4, IL_conv_i2_r8,
    IL_conv_i4_i8, IL_conv_i4_r4, IL_conv_i4_r8,
    IL_conv_i8_i4, IL_conv_i8_r4, IL_conv_i8_r8,
    IL_conv_r4_i4, IL_conv_r4_i8, IL_conv_r4_r8,
    IL_conv_r8_i4, IL_conv_r8_i8, IL_conv_r8_r4,

    IL_conv_u1_i4, IL_conv_u1_i8, IL_conv_u1_r4, IL_conv_u1_r8,
    IL_conv_u2_i4, IL_conv_u2_i8, IL_conv_u2_r4, IL_conv_u2_r8,
    IL_conv_u4_i8, IL_conv_u4_r4, IL_conv_u4_r8,
    IL_conv_u8_i4, IL_conv_u8_r4, IL_conv_u8_r8,

    IL_ldarg_i1, IL_ldarg_i2, IL_ldarg_i4, IL_ldarg_i8,
    IL_ldarg_u1, IL_ldarg_u2, IL_ldarg_u4, IL_ldarg_u8,
    IL_ldarg_r4, IL_ldarg_r8, IL_ldarg_p, IL_ldarg_pp,
    IL_ldarg_vt, IL_ldarga,

    IL_starg_i1, IL_starg_i2, IL_starg_i4, IL_starg_i8,
    IL_starg_u1, IL_starg_u2, IL_starg_u4, IL_starg_u8,
    IL_starg_r4, IL_starg_r8, IL_starg_p, IL_starg_pp,
    IL_starg_vt, IL_starga,

    IL_ldelem_i1, IL_ldelem_i2, IL_ldelem_i4, IL_ldelem_i8,
    IL_ldelem_u1, IL_ldelem_u2, IL_ldelem_u4, IL_ldelem_u8,
    IL_ldelem_r4, IL_ldelem_r8, IL_ldelem_p, IL_ldelem_pp,
    IL_ldelema, IL_ldelem_vt,

    IL_stelem_i1, IL_stelem_i2, IL_stelem_i4, IL_stelem_i8,
    IL_stelem_u1, IL_stelem_u2, IL_stelem_u4, IL_stelem_u8,
    IL_stelem_r4, IL_stelem_r8, IL_stelem_p, IL_stelem_pp,
    IL_stelema, IL_stelem_vt,

    IL_ldfld_i1, IL_ldfld_i2, IL_ldfld_i4, IL_ldfld_i8,
    IL_ldfld_u1, IL_ldfld_u2, IL_ldfld_u4, IL_ldfld_u8,
    IL_ldfld_r4, IL_ldfld_r8, IL_ldfld_p, IL_ldfld_pp,
    IL_ldflda, IL_ldfld_vt,

    IL_stfld_i1, IL_stfld_i2, IL_stfld_i4, IL_stfld_i8,
    IL_stfld_u1, IL_stfld_u2, IL_stfld_u4, IL_stfld_u8,
    IL_stfld_r4, IL_stfld_r8, IL_stfld_p, IL_stfld_pp,
    IL_stflda, IL_stfld_vt,

    IL_ldind_i1, IL_ldind_i2, IL_ldind_i4, IL_ldind_i8,
    IL_ldind_u1, IL_ldind_u2, IL_ldind_u4, IL_ldind_u8,
    IL_ldind_r4, IL_ldind_r8, IL_ldind_p, IL_ldind_pp,
    IL_ldinda, IL_ldind_vt,

    IL_stind_i1, IL_stind_i2, IL_stind_i4, IL_stind_i8,
    IL_stind_u1, IL_stind_u2, IL_stind_u4, IL_stind_u8,
    IL_stind_r4, IL_stind_r8, IL_stind_p, IL_stind_pp,
    IL_stinda, IL_stind_vt,

    IL_ldloc_i1, IL_ldloc_i2, IL_ldloc_i4, IL_ldloc_i8,
    IL_ldloc_u1, IL_ldloc_u2, IL_ldloc_u4, IL_ldloc_u8,
    IL_ldloc_r4, IL_ldloc_r8, IL_ldloc_p, IL_ldloc_pp,
    IL_ldloca, IL_ldloc_vt,

    IL_stloc_i1, IL_stloc_i2, IL_stloc_i4, IL_stloc_i8,
    IL_stloc_u1, IL_stloc_u2, IL_stloc_u4, IL_stloc_u8,
    IL_stloc_r4, IL_stloc_r8, IL_stloc_p, IL_stloc_pp,
    IL_stloca, IL_stloc_vt,

    IL_ldvar_i1, IL_ldvar_i2, IL_ldvar_i4, IL_ldvar_i8,
    IL_ldvar_u1, IL_ldvar_u2, IL_ldvar_u4, IL_ldvar_u8,
    IL_ldvar_r4, IL_ldvar_r8, IL_ldvar_p, IL_ldvar_pp,
    IL_ldvara, IL_ldvar_vt,

    IL_stvar_i1, IL_stvar_i2, IL_stvar_i4, IL_stvar_i8,
    IL_stvar_u1, IL_stvar_u2, IL_stvar_u4, IL_stvar_u8,
    IL_stvar_r4, IL_stvar_r8, IL_stvar_p, IL_stvar_pp,
    IL_stvara, IL_stvar_vt,

    IL_ldc_i4, IL_ldc_i8, IL_ldc_r4, IL_ldc_r8,
    IL_ldc_i4_m1, IL_ldc_i4_0, IL_ldc_i4_1, IL_ldc_i4_2, IL_ldc_i4_3,
    IL_ldc_i4_4, IL_ldc_i4_5,IL_ldc_i4_6, IL_ldc_i4_7, IL_ldc_i4_8,
    IL_ldnull, IL_ldstr, IL_ldobj,

    IL_br,
    IL_brtrue_i4, IL_brtrue_i8, IL_brtrue_r4, IL_brtrue_r8,
    IL_brfalse_i4, IL_brfalse_i8, IL_brfalse_r4, IL_brfalse_r8,

    IL_ldproc, IL_ldmeth,
    IL_sizeof, IL_ptroff,
    IL_nop, IL_pop, IL_dup,
    IL_ret, IL_ret_void,
    IL_call, IL_calli,
    IL_newobj, IL_newarr, IL_free,

   // TODO
    IL_newvla,
    IL_callvi, IL_callvirt,
    IL_castptr, IL_conv_ip,
    IL_line,
    IL_initobj, IL_isinst,

    // special implementation ops
    IL_already_called, // puts int32 1 or 0 on stack

    IL_NUM_OF_OPS
};

struct Operation
{
    uint val : 22;
    uint minus : 1;
    uint op : 9;
    Operation(IL_op op = IL_invalid, quint32 val = 0, bool minus = false):val(val),minus(minus),op(op){}
};

struct Procedure
{
    QList<Operation> ops;
    Declaration* decl;
    quint32 called; // number of calls
};

struct Frame
{
    Procedure* proc;
    Frame* outer;
    quint8* args; // pointer to outer frame stack where args start, aligns to quint64
    QVector<quint8> stack; // locals, intermediate, calls, aligns to quint64
};

struct Interpreter::Imp
{
    AstModel* mdl;
    QList<QByteArray> strings;
    QList<double> doubles;
    QList<qint64> ints;
    QList<Procedure> procs;

    Imp(AstModel* mdl):mdl(mdl)
    {

    }

    bool translateModule(Declaration* m)
    {
        Q_ASSERT(m && m->kind == Declaration::Module);

        if( m->init )
            return true; // the module was already translated

        // look for the init procedure or synthesize one
        Declaration* init = m->subs;
        while(init)
        {
            if( init->init )
                break;
            init = init->next;
        }
        if(init == 0)
        {
            // no init proc was found, so we synthesize a minimal one
            procs.append(Procedure());
            Procedure& cur = procs.back();
            cur.decl = m;
            if( !translateInit(cur) )
                return false;
        }else if( !translateProc(init) )
            return false;
        return true;
    }

    bool translateProc(Declaration* proc)
    {
        if( proc->validated )
            return true; // the proc was already translated

        procs.append(Procedure());
        Procedure& cur = procs.back();

        if( proc->init )
        {
            // add a prefix which calls imports if not already called
            cur.decl = proc->getModule();
            if( !translateInit(cur) )
                return false;
        }else
            cur.decl = proc;

        return translateProc(cur);
    }

    int findProc(Declaration* proc) const
    {
        for( int i = 0; i < procs.size(); i++ )
        {
            if( procs[i].decl == proc )
                return i;
        }
        return -1;
    }

    bool run(Declaration* proc)
    {
        if( proc->init )
            proc = proc->getModule();
        const int i = findProc(proc);
        if( i >= 0 )
            return run(i);
        // proc not found
        return false;
    }

    quint32 addInt(qint64 i)
    {
        int id = ints.indexOf(i);
        if( id == -1 )
        {
            id = ints.size();
            ints.append(i);
        }
        return id;
    }

    quint32 addFloat(double f)
    {
        int id = doubles.indexOf(f);
        if( id == -1 )
        {
            id = doubles.size();
            doubles.append(f);
        }
        return id;
    }

    quint32 addString(const QByteArray& str)
    {
        int id = strings.indexOf(str);
        if( id == -1 )
        {
            id = strings.size();
            strings.append(str);
        }
        return id;
    }

    void emitOp(Procedure& proc, IL_op op, quint32 v = 0, bool minus = false )
    {
        proc.ops.append(Operation(op, v, minus));
    }

    bool translateInit(Procedure& proc);
    bool translateProc(Procedure& proc);
    bool translateStatSeq(Procedure& proc, Statement* s);
    bool translateExprSeq(Procedure& proc, Expression* e);

    bool run(quint32) { return false; } // TODO
};

Interpreter::Interpreter(AstModel* mdl)
{
    imp = new Imp(mdl);
}

Interpreter::~Interpreter()
{

}

bool Interpreter::run(Declaration* proc)
{
    Q_ASSERT(proc && proc->kind == Declaration::Procedure);
    Declaration* module = proc->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    // TODO: if we run for the second time, we might reset the interpreter

    if( !imp->translateModule(module) )
        return false;

    if( !proc->init )
    {
        if( !imp->run(module) )
            return false;
    }

    return imp->run(proc);
}

bool Interpreter::Imp::translateInit(Procedure& proc)
{
    Q_ASSERT( proc.decl && proc.decl->kind == Declaration::Module );

    // first check if already called
    emitOp(proc,IL_already_called);
    emitOp(proc,IL_brfalse_i4,1);
    emitOp(proc,IL_ret_void);

    Declaration* d = proc.decl->subs;
    while(d)
    {
        if( d->kind == Declaration::Import )
        {
            const int p = findProc(d->imported);
            if( p < 0 )
                return false; // procedure not found
            emitOp(proc,IL_call, p);
        }
        // TODO: initialize structs, arrays and objects for value objects vtables
        d = d->next;
    }
    return true;
}

bool Interpreter::Imp::translateProc(Procedure& proc)
{
    Q_ASSERT( proc.decl && proc.decl->kind == Declaration::Procedure );
    Statement* s = proc.decl->body;
    return translateStatSeq(proc, s);
}

bool Interpreter::Imp::translateStatSeq(Procedure& proc, Statement* s)
{
    while(s)
    {
        switch(s->kind)
        {
        case Statement::ExprStat:
            break;
        case Tok_IF:
        case Tok_LOOP:
        case Tok_REPEAT:
        case Tok_SWITCH:
        case Tok_WHILE:
        case Tok_EXIT:
        case Tok_STLOC:
        case Tok_STLOC_S:
        case Tok_STLOC_0:
        case Tok_STLOC_1:
        case Tok_STLOC_2:
        case Tok_STLOC_3:
        case Tok_STARG:
        case Tok_STIND:
        case Tok_STIND_I1:
        case Tok_STIND_I4:
        case Tok_STIND_I8:
        case Tok_STIND_R4:
        case Tok_STIND_R8:
        case Tok_STIND_IP:
        case Tok_STIND_IPP:
        case Tok_STELEM:
        case Tok_STELEM_I1:
        case Tok_STELEM_I2:
        case Tok_STELEM_I4:
        case Tok_STELEM_I8:
        case Tok_STELEM_R4:
        case Tok_STELEM_R8:
        case Tok_STELEM_IP:
        case Tok_STFLD:
        case Tok_STVAR:
        case Tok_RET:
        case Tok_POP:
        case Tok_FREE:
        case Tok_LABEL:
        case Tok_GOTO:
            break;
        default:
            Q_ASSERT(false);
        }

        s = s->next;
    }
    return true;
}

bool Interpreter::Imp::translateExprSeq(Procedure& proc, Expression* e)
{
    while(e)
    {
        Type* t = e->getType();
        if( t ) t = t->deref();
        switch(e->kind)
        {
        case Tok_ADD:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_add_i4);
            else if( t->isInt64())
                emitOp(proc, IL_add_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_add_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_add_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SUB:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_sub_i4);
            else if( t->isInt64())
                emitOp(proc, IL_sub_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_sub_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_sub_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_DIV:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_div_i4);
            else if( t->isInt64())
                emitOp(proc, IL_div_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_div_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_div_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_DIV_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_div_un_i4);
            else if( t->isInt64())
                emitOp(proc, IL_div_un_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_MUL:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_mul_i4);
            else if( t->isInt64())
                emitOp(proc, IL_mul_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_mul_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_mul_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_REM:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_rem_i4);
            else if( t->isInt64())
                emitOp(proc, IL_rem_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_rem_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_rem_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_REM_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_rem_un_i4);
            else if( t->isInt64())
                emitOp(proc, IL_rem_un_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_ABS:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_abs_i4);
            else if( t->isInt64())
                emitOp(proc, IL_abs_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_abs_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_abs_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_NEG:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_neg_i4);
            else if( t->isInt64())
                emitOp(proc, IL_neg_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_neg_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_neg_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_AND:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_and_i4);
            else if( t->isInt64())
                emitOp(proc, IL_and_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_OR:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_or_i4);
            else if( t->isInt64())
                emitOp(proc, IL_or_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_XOR:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_xor_i4);
            else if( t->isInt64())
                emitOp(proc, IL_xor_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SHL:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_shl_i4);
            else if( t->isInt64())
                emitOp(proc, IL_shl_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SHR_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_shr_un_i4);
            else if( t->isInt64())
                emitOp(proc, IL_shr_un_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SHR:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_shr_i4);
            else if( t->isInt64())
                emitOp(proc, IL_shr_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_NOT:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_not_i4);
            else if( t->isInt64())
                emitOp(proc, IL_not_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_LDC_I4_0:
            emitOp(proc, IL_ldc_i4_0);
            break;
        case Tok_LDC_I4_1:
            emitOp(proc, IL_ldc_i4_1);
            break;
        case Tok_LDC_I4_2:
            emitOp(proc, IL_ldc_i4_2);
            break;
        case Tok_LDC_I4_3:
            emitOp(proc, IL_ldc_i4_3);
            break;
        case Tok_LDC_I4_4:
            emitOp(proc, IL_ldc_i4_4);
            break;
        case Tok_LDC_I4_5:
            emitOp(proc, IL_ldc_i4_5);
            break;
        case Tok_LDC_I4_6:
            emitOp(proc, IL_ldc_i4_6);
            break;
        case Tok_LDC_I4_7:
            emitOp(proc, IL_ldc_i4_7);
            break;
        case Tok_LDC_I4_8:
            emitOp(proc, IL_ldc_i4_8);
            break;
        case Tok_LDC_I4_M1:
            emitOp(proc, IL_ldc_i4_m1);
            break;
        case Tok_LDC_I4_S:
        case Tok_LDC_I4:
            emitOp(proc, IL_ldc_i4, addInt(e->i));
            break;
        case Tok_LDC_I8:
            emitOp(proc, IL_ldc_i8, addInt(e->i));
            break;
        case Tok_LDC_R4:
            emitOp(proc, IL_ldc_r4, addFloat(e->f) );
            break;
        case Tok_LDC_R8:
            emitOp(proc, IL_ldc_r8, addFloat(e->f) );
            break;
        case Tok_LDNULL:
            emitOp(proc, IL_ldnull);
            break;
        case Tok_LDSTR:
            emitOp(proc, IL_ldstr, addString(e->c->s) );
            break;
        //case Tok_LDOBJ:
            // emitOp(proc, IL_ldobj, addString(e->c->s) ); // TODO
            // break;
        case Tok_LDPROC:
            translateProc(e->d);
            emitOp(proc, IL_ldproc, findProc(e->d));
            break;
        case Tok_LDMETH:
            translateProc(e->d);
            emitOp(proc, IL_ldmeth, findProc(e->d));
            break;
        case Tok_CONV_I1:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_i1_i4);
            else if( t->isInt64())
                emitOp(proc, IL_conv_i1_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i1_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i1_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_I2:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_i2_i4);
            else if( t->isInt64())
                emitOp(proc, IL_conv_i2_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i2_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i2_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_I4:
            if( t->isInt64())
                emitOp(proc, IL_conv_i4_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i4_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i4_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_I8:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_i8_i4);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i8_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i8_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_R4:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_r4_i4);
            else if(t->isInt64() )
                emitOp(proc, IL_conv_r4_i8);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_r4_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_R8:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_r8_i4);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_r8_r4);
            else if( t->isInt64() )
                emitOp(proc, IL_conv_r8_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_U1:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_u1_i4);
            else if( t->isInt64())
                emitOp(proc, IL_conv_u1_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u1_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u1_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_U2:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_u2_i4);
            else if( t->isInt64())
                emitOp(proc, IL_conv_u2_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u2_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u2_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_U4:
            if( t->isInt64() )
                emitOp(proc, IL_conv_u4_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u4_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u4_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_U8:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_u8_i4);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u8_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u8_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_IP:
            break; // TODO
        case Tok_CEQ:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_ceq_i4);
            else if( t->isInt64())
                emitOp(proc, IL_ceq_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_ceq_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_ceq_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CGT:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_cgt_i4);
            else if( t->isInt64())
                emitOp(proc, IL_cgt_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_cgt_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_cgt_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CGT_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_cgt_u4);
            else if( t->isInt64())
                emitOp(proc, IL_cgt_u8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CLT:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_clt_i4);
            else if( t->isInt64())
                emitOp(proc, IL_clt_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_clt_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_clt_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CLT_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_clt_u4);
            else if( t->isInt64())
                emitOp(proc, IL_clt_u8);
            else
                Q_ASSERT(false);
            break;
        case Tok_LDARG_0:
        case Tok_LDARG_1:
        case Tok_LDARG_2:
        case Tok_LDARG_3:
        case Tok_LDARG_S:
        case Tok_LDARG:
        case Tok_LDARGA_S:
        case Tok_LDARGA:
        case Tok_LDLOC_0:
        case Tok_LDLOC_1:
        case Tok_LDLOC_2:
        case Tok_LDLOC_3:
        case Tok_LDLOC_S:
        case Tok_LDLOC:
        case Tok_LDLOCA_S:
        case Tok_LDLOCA:
        case Tok_LDELEM_I1:
        case Tok_LDELEM_I2:
        case Tok_LDELEM_I4:
        case Tok_LDELEM_I8:
        case Tok_LDELEM_IP:
        case Tok_LDELEM_R4:
        case Tok_LDELEM_R8:
        case Tok_LDELEM_U1:
        case Tok_LDELEM_U2:
        case Tok_LDELEM_U4:
        case Tok_LDELEM_U8:
        case Tok_LDELEM:
        case Tok_LDELEMA:
        case Tok_LDFLD:
        case Tok_LDFLDA:
        case Tok_LDVAR:
        case Tok_LDVARA:
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
        case Tok_CASTPTR:
        case Tok_SIZEOF:
        case Tok_INITOBJ:
        case Tok_PTROFF:
        case Tok_NEWOBJ:
        case Tok_NEWARR:
        case Tok_NEWVLA:
        case Tok_ISINST:
        case Tok_DUP:
        case Tok_NOP:
        case Tok_CALLI:
        case Tok_CALLVI:
        case Tok_CALL:
        case Tok_CALLVIRT:
        case Tok_IIF:
            break;
        default:
            Q_ASSERT(false);
        }
        e = e->next;
    }
    return true;
}
