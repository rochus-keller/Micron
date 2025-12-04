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

#include "MilVmCode.h"
#include <QtDebug>
using namespace Mil;
using namespace Vm;

const char* Code::op_names[] = {
    "<invalid>",
    #define OPDEF(op, x) #op
    #include "MilVmOps.h"
    #undef OPDEF
};

static const int op_args[] = {
    0,
    #define OPDEF(op, n) n
    #include "MilVmOps.h"
    #undef OPDEF
};

Code::Code(AstModel* mdl, quint8 pointerWidth, quint8 stackAlignment):
    mdl(mdl),pointerWidth(pointerWidth), stackAlignment(stackAlignment)
{

}

Code::~Code()
{
    for( int i = 0; i < procs.size(); i++ )
        delete procs[i];
    for( int i = 0; i < vtables.size(); i++ )
        delete vtables[i];
}

void Code::addExternal(const char* module, const char* name, quint32 id)
{
    externals[module].insert(name, id);
}

bool Code::compile(Declaration* procOrModule)
{
    Q_ASSERT(procOrModule &&
             (procOrModule->kind == Declaration::Procedure ||
              procOrModule->kind == Declaration::Module));

    Declaration* module = procOrModule->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    const bool res = translateModule(module);
    if(res)
    {
        foreach(Vtable* vt, vtables )
            downcopy(vt);
    }
    return res;

}

bool Code::translateModule(Declaration* m)
{
    Q_ASSERT(m && m->kind == Declaration::Module);

    if( m->translated )
        return true; // the module was already translated

    m->translated = true; // re/misuse this flag to indicate that we already translated

    Declaration* sub = m->subs;
    while(sub)
    {
        if( sub->kind == Declaration::TypeDecl && sub->getType() && (sub->getType()->kind == Type::Object && sub->getType()->kind == Type::Struct) )
        {
            Vtable* vt = new Vtable();
            vtables.push_back(vt);
            vt->type = sub->getType();
            if( sub->getType()->getType() )
            {
                const int i = findVtable(deref(sub->getType()->getType()));
                if( i != -1 )
                    vt->parent = vtables[i];
            }
            vt->methods.resize(vt->type->getMethodTable().size());
        }
        sub = sub->next;
    }

    // look for the init procedure or synthesize one
    Declaration* init = m->findInitProc();
    if(init == 0)
    {
        // no init proc was found, so we synthesize a minimal one
        Procedure* cur = new Procedure();
        procs.push_back(cur);
        cur->decl = m;
        if( !translateInit(*cur, procs.size()-1) )
            return false;
    }else if( !translateProc(init) )
        return false;
    return true;
}

bool Code::translateProc(Declaration* proc)
{
    if( proc->validated )
        return true; // the proc was already translated
    proc->validated = true;
    if( proc->forward )
        return translateProc(proc->forwardTo);

    Q_ASSERT( proc->kind == Declaration::Procedure );

    Procedure* cur = new Procedure();
    procs.push_back(cur);
    cur->decl = proc;

    if( proc->entryPoint && !translateInit(*cur, procs.size()-1) )
        // add a prefix which calls imports if not already called
        return false;
    if( proc->typebound )
    {
        const int off = findVtable(proc->outer->getType());
        Q_ASSERT(off != -1);
        vtables[off]->methods[proc->getPd()->slot] = cur;
        if(proc->override_)
        {
            // go up the inheritance chain and assure all super methods are translated
            Type* super = deref(proc->outer->getType()->getType());
            Q_ASSERT(super && super->kind == Type::Object);
            Declaration* baseproc = super->findSubByName(proc->name, true);
            if( baseproc )
            {
                Q_ASSERT(proc->getPd()->slot == baseproc->getPd()->slot );
                translateProc(baseproc);
            }
        }
    }
    return translateProc(*cur);
}

bool Code::translateProc(Procedure& proc)
{
    Q_ASSERT( proc.decl && proc.decl->kind == Declaration::Procedure );
    const DeclList locals = proc.decl->getLocals();
    if( !locals.isEmpty() )
        proc.localsSize = locals.last()->off + locals.last()->getType()->getByteSize(sizeof(void*));
    const DeclList params = proc.decl->getParams();
    if( !params.isEmpty() )
        // this always includes SELF
        proc.fixArgSize = stackAligned(params.last()->off + params.last()->getType()->getByteSize(sizeof(void*)));
    if( proc.decl->getType() )
        proc.returnSize = stackAligned(proc.decl->getType()->getByteSize(sizeof(void*)));
    if( proc.decl->extern_ || proc.decl->foreign_ )
    {
        if(proc.decl->typebound)
        {
            qCritical() << "typebound external implementations not yet supported" << proc.decl->toPath();
            return false;
        }
        Declaration* module = proc.decl->getModule();

        const int id = externals.value(module->name.constData()).value(proc.decl->name.constData(), -1);
        if( id < 0 )
        {
            qCritical() << "cannot find external implementation for" << proc.decl->toPath();
            return false;
        }else
        {
            proc.external = true;
            proc.id = id;
        }
        return true;
    }else
    {
        Statement* s = proc.decl->body;
        ctxStack.push_back(Context());
        Context& ctx = ctxStack.back();
        ctx.curProc = &proc;
        const bool res = translateStatSeq(proc, s);

        foreach( const Where& pos, ctx.gotos )
        {
            const int label = ctx.findLabel(pos.name);
            if( label == -1 )
            {
                qCritical() << "cannot find label" << pos.name;
                return false;
            }
            if( ctx.labels[label].pc < pos.pc)
            {
                proc.ops[pos.pc].minus = true;
                proc.ops[pos.pc].val = pos.pc + 1 - ctx.labels[label].pc;
            }else
                proc.ops[pos.pc].val = ctx.labels[label].pc - pos.pc - 1;
        }
        ctxStack.pop_back();
        return res;
    }
}

bool Code::translateStatSeq(Procedure& proc, Statement* s)
{
    while(s)
    {
        switch(s->kind)
        {
        case IL_starg:
            {
                Q_ASSERT(ctxStack.back().curProc);
                DeclList params = ctxStack.back().curProc->decl->getParams();
                Q_ASSERT(s->id < params.size());
                Type* t = deref(params[s->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, LL_starg_i1,params[s->id]->off);
                    break;
                case Type::INT16:
                case Type::UINT16:
                    emitOp(proc, LL_starg_i2,params[s->id]->off);
                    break;
                case Type::INT32:
                case Type::UINT32:
                    emitOp(proc, LL_starg_i4,params[s->id]->off);
                    break;
                case Type::INT64:
                case Type::UINT64:
                    emitOp(proc, LL_starg_i8,params[s->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, LL_starg_r4,params[s->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, LL_starg_r8,params[s->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, LL_starg_pp,params[s->id]->off);
                    else
                        emitOp(proc, LL_starg_p,params[s->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, LL_starg_vt,params[s->id]->off);
                    emitOp(proc, LL_vt_size,t->getByteSize(sizeof(void*)));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case IL_stloc:
        case IL_stloc_s:
        case IL_stloc_0:
        case IL_stloc_1:
        case IL_stloc_2:
        case IL_stloc_3:
            {
                Q_ASSERT(ctxStack.back().curProc);
                DeclList locals = ctxStack.back().curProc->decl->getLocals();
                Q_ASSERT(s->id < locals.size());
                Type* t = deref(locals[s->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, LL_stloc_i1,locals[s->id]->off);
                    break;
                case Type::INT16:
                case Type::UINT16:
                    emitOp(proc, LL_stloc_i2,locals[s->id]->off);
                    break;
                case Type::INT32:
                case Type::UINT32:
                    emitOp(proc, LL_stloc_i4,locals[s->id]->off);
                    break;
                case Type::UINT64:
                case Type::INT64:
                    emitOp(proc, LL_stloc_i8,locals[s->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, LL_stloc_r4,locals[s->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, LL_stloc_r8,locals[s->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, LL_stloc_pp,locals[s->id]->off);
                    else
                        emitOp(proc, LL_stloc_p,locals[s->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, LL_stloc_vt,locals[s->id]->off);
                    emitOp(proc, LL_vt_size, t->getByteSize(sizeof(void*)));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case IL_stind:
            emitOp(proc, LL_stind_vt,deref(s->d->getType())->getByteSize(sizeof(void*)));
            break;
        case IL_stind_i1:
            emitOp(proc, LL_stind_i1);
            break;
        case IL_stind_i2:
            emitOp(proc, LL_stind_i2);
            break;
        case IL_stind_i4:
            emitOp(proc, LL_stind_i4);
            break;
        case IL_stind_i8:
            emitOp(proc, LL_stind_i8);
            break;
        case IL_stind_r4:
            emitOp(proc, LL_stind_r4);
            break;
        case IL_stind_r8:
            emitOp(proc, LL_stind_r8);
            break;
        case IL_stind_ip:
            emitOp(proc, LL_stind_p);
            break;
        case IL_stind_ipp:
            emitOp(proc, LL_stind_vt,sizeof(MethRef));
            break;
        case IL_stelem_ipp:
            emitOp(proc, LL_stelem_vt, sizeof(MethRef));
            break;
        case IL_stelem:
            emitOp(proc, LL_stelem_vt, deref(s->d->getType())->getByteSize(sizeof(void*)));
            break;
        case IL_stelem_i1:
            emitOp(proc, LL_stelem_i1);
            break;
        case IL_stelem_i2:
            emitOp(proc, LL_stelem_i2);
            break;
        case IL_stelem_i4:
            emitOp(proc, LL_stelem_i4);
            break;
        case IL_stelem_i8:
            emitOp(proc, LL_stelem_i8);
            break;
        case IL_stelem_r4:
            emitOp(proc, LL_stelem_r4);
            break;
        case IL_stelem_r8:
            emitOp(proc, LL_stelem_r8);
            break;
        case IL_stelem_ip:
            emitOp(proc, LL_stelem_p);
            break;
        case Statement::ExprStat:
            if( !translateExprSeq(proc, s->e) )
                return false;
            break;
        case IL_stfld:
            switch(deref(s->d->getType())->kind)
            {
            case Type::INT8:
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, LL_stfld_i1,s->d->f.off);
                break;
            case Type::INT16:
            case Type::UINT16:
                emitOp(proc, LL_stfld_i2,s->d->f.off);
                break;
            case Type::INT32:
            case Type::UINT32:
                emitOp(proc, LL_stfld_i4,s->d->f.off);
                break;
            case Type::UINT64:
            case Type::INT64:
                emitOp(proc, LL_stfld_i8,s->d->f.off);
                break;
            case Type::FLOAT32:
                emitOp(proc, LL_stfld_r4,s->d->f.off);
                break;
            case Type::FLOAT64:
                emitOp(proc, LL_stfld_r8,s->d->f.off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(deref(s->d->getType())->typebound)
                    emitOp(proc, LL_stfld_pp,s->d->f.off);
                else
                    emitOp(proc, LL_stfld_p,s->d->f.off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, LL_stfld_vt,s->d->f.off);
                emitOp(proc, LL_vt_size, deref(s->d->getType())->getByteSize(sizeof(void*)));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case IL_stvar:
            switch(deref(s->d->getType())->kind)
            {
            case Type::INT8:
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, LL_stvar_i1,s->d->off);
                break;
            case Type::INT16:
            case Type::UINT16:
                emitOp(proc, LL_stvar_i2,s->d->off);
                break;
            case Type::INT32:
            case Type::UINT32:
                emitOp(proc, LL_stvar_i4,s->d->off);
                break;
            case Type::UINT64:
            case Type::INT64:
                emitOp(proc, LL_stvar_i8,s->d->off);
                break;
            case Type::FLOAT32:
                emitOp(proc, LL_stvar_r4,s->d->off);
                break;
            case Type::FLOAT64:
                emitOp(proc, LL_stvar_r8,s->d->off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(deref(s->d->getType())->typebound)
                    emitOp(proc, LL_stvar_pp,s->d->off);
                else
                    emitOp(proc, LL_stvar_p,s->d->off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, LL_stvar_vt,s->d->off);
                emitOp(proc, LL_vt_size, deref(s->d->getType())->getByteSize(sizeof(void*)));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case IL_if:
            {
                if( !translateExprSeq(proc, s->e) )
                    return false;
                const int ifnot = emitOp(proc, LL_brfalse_i4);
                if( !translateStatSeq(proc, s->body) )
                    return false;
                const int after_if = emitOp(proc, LL_br);
                branch_here(proc, ifnot);
                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    if( !translateStatSeq(proc, s->body) )
                        return false;
                }
                branch_here(proc,after_if);
            }
            break;
        case IL_loop:
            {
                ctxStack.back().loopStack.push_back(QList<int>());
                if( !translateStatSeq(proc, s->body) )
                    return false;
                foreach( int pc, ctxStack.back().loopStack.back() )
                    branch_here(proc,pc);
                ctxStack.back().loopStack.pop_back();
            }
            break;
        case IL_exit:
            ctxStack.back().loopStack.back() << emitOp(proc,LL_br);
            break;
        case IL_repeat:
            {
                const int start = proc.ops.size();
                if( !translateStatSeq(proc, s->body) )
                    return false;
                if( !translateExprSeq(proc, s->e) )
                    return false;
                emitOp(proc, LL_brfalse_i4, proc.ops.size()-start+1, true );
            }
            break;
        case IL_while:
            {
                const int start = proc.ops.size();
                if( !translateExprSeq(proc, s->e) )
                    return false;
                const int while_ = emitOp(proc, LL_brfalse_i4 );
                if( !translateStatSeq(proc, s->body) )
                    return false;
                emitOp(proc, LL_br, proc.ops.size()-start+1, true);
                branch_here(proc, while_);
            }
            break;
        case IL_pop:
            emitOp(proc, LL_pop, s->args->getType()->getByteSize(sizeof(void*)));
            break;
        case IL_strcpy:
            emitOp(proc, LL_strcpy, deref(s->args->lhs->getType()->getType())->len);
            break;
        case IL_ret:
            emitOp(proc, LL_ret, s->args ? s->args->getType()->getByteSize(sizeof(void*)) : 0 );
            break;
        case IL_free:
            emitOp(proc, LL_free );
            break;
        case IL_label:
            ctxStack.back().labels << Where(s->name, proc.ops.size() );
            break;
        case IL_goto: {
                const int pc = emitOp(proc, LL_br );
                ctxStack.back().gotos << Where(s->name,pc);
            } break;
        case IL_switch: {
            // TODO: true jump table instead of ifs

                if( !translateExprSeq(proc, s->e) )
                    return false;

                bool is64 = s->e->getType()->isInt64();
                QList<int> after;
                while( s->next && s->next->kind == IL_case )
                {
                    s = s->next;

                    // stat->e is a list of integers, each as an Expression
                    Expression* e = s->e;
                    QList<int> toBody, afterBody;
                    while( e )
                    {
                        emitOp(proc, LL_dup, is64 ? 8 : 4); // duplicate the case expression value on stack
                        emitOp(proc, is64 ? LL_ldc_i8 : LL_ldc_i4, addInt(e->i));
                        emitOp(proc, is64 ? LL_ceq_i8 : LL_ceq_i4);
                        const int nextCond = emitOp(proc, LL_brfalse_i4);
                        emitOp(proc, LL_pop, is64 ? 8 : 4); // remove case expression
                        toBody << emitOp(proc, LL_br);
                        e = e->next;
                        if( e )
                            branch_here(proc,nextCond);
                        else
                            afterBody << nextCond;
                    }

                    foreach( int pc, toBody )
                        branch_here(proc,pc);

                    if( !translateStatSeq(proc, s->body) )
                        return false;
                    after << emitOp(proc, LL_br);

                    foreach( int pc, afterBody )
                        branch_here(proc,pc);
                }

                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    emitOp(proc, LL_pop, is64 ? 8 : 4); // remove case expression
                    if( !translateStatSeq(proc, s->body) )
                        return false;
                }

                foreach( int pc, after )
                    branch_here(proc,pc);

            } break;
        case IL_line:
            break; // NOP
        default:
            Q_ASSERT(false);
        }

        s = s->next;
    }
    return true;
}

bool Code::translateExprSeq(Procedure& proc, Expression* e)
{
    static const int pointerWidth = sizeof(void*);
    while(e)
    {
        Type* t = deref(e->getType());
        Type* lhsT = deref(e->lhs ? e->lhs->getType() : 0);
        Type* rhsT = deref(e->rhs ? e->rhs->getType() : 0);
        switch(e->kind)
        {
        case IL_add:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_add_i4);
            else if( t->isInt64())
                emitOp(proc, LL_add_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_add_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_add_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_sub:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_sub_i4);
            else if( t->isInt64())
                emitOp(proc, LL_sub_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_sub_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_sub_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_div:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_div_i4);
            else if( t->isInt64())
                emitOp(proc, LL_div_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_div_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_div_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_div_un:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_div_un_i4);
            else if( t->isInt64())
                emitOp(proc, LL_div_un_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_mul:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_mul_i4);
            else if( t->isInt64())
                emitOp(proc, LL_mul_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_mul_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_mul_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_rem:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_rem_i4);
            else if( t->isInt64())
                emitOp(proc, LL_rem_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_rem_un:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_rem_un_i4);
            else if( t->isInt64())
                emitOp(proc, LL_rem_un_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_abs:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_abs_i4);
            else if( t->isInt64())
                emitOp(proc, LL_abs_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_abs_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_abs_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_neg:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_neg_i4);
            else if( t->isInt64())
                emitOp(proc, LL_neg_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_neg_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_neg_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_and:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_and_i4);
            else if( t->isInt64())
                emitOp(proc, LL_and_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_or:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_or_i4);
            else if( t->isInt64())
                emitOp(proc, LL_or_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_xor:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_xor_i4);
            else if( t->isInt64())
                emitOp(proc, LL_xor_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_shl:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_shl_i4);
            else if( t->isInt64())
                emitOp(proc, LL_shl_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_shr_un:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_shr_un_i4);
            else if( t->isInt64())
                emitOp(proc, LL_shr_un_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_shr:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_shr_i4);
            else if( t->isInt64())
                emitOp(proc, LL_shr_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_not:
            if( t->isInt32OnStack() )
                emitOp(proc, LL_not_i4);
            else if( t->isInt64())
                emitOp(proc, LL_not_i8);
            else
                Q_ASSERT(false);
            break;
        case IL_ldc_i4_0:
            emitOp(proc, LL_ldc_i4_0);
            break;
        case IL_ldc_i4_1:
            emitOp(proc, LL_ldc_i4_1);
            break;
        case IL_ldc_i4_2:
            emitOp(proc, LL_ldc_i4_2);
            break;
        case IL_ldc_i4_3:
            emitOp(proc, LL_ldc_i4_3);
            break;
        case IL_ldc_i4_4:
            emitOp(proc, LL_ldc_i4_4);
            break;
        case IL_ldc_i4_5:
            emitOp(proc, LL_ldc_i4_5);
            break;
        case IL_ldc_i4_6:
            emitOp(proc, LL_ldc_i4_6);
            break;
        case IL_ldc_i4_7:
            emitOp(proc, LL_ldc_i4_7);
            break;
        case IL_ldc_i4_8:
            emitOp(proc, LL_ldc_i4_8);
            break;
        case IL_ldc_i4_m1:
            emitOp(proc, LL_ldc_i4_m1);
            break;
        case IL_ldc_i4_s:
        case IL_ldc_i4:
            emitOp(proc, LL_ldc_i4, addInt(e->i));
            break;
        case IL_ldc_i8:
            emitOp(proc, LL_ldc_i8, addInt(e->i));
            break;
        case IL_ldc_r4:
            emitOp(proc, LL_ldc_r4, addFloat(e->f) );
            break;
        case IL_ldc_r8:
            emitOp(proc, LL_ldc_r8, addFloat(e->f) );
            break;
        case IL_ldnull:
            emitOp(proc, LL_ldnull);
            break;
        case IL_ldstr:
            emitOp(proc, LL_ldstr, addString(e->c->s) );
            break;
        case IL_ldc_obj:
            emitOp(proc, LL_ldobj, addObject(e->c) );
            break;
        case IL_ldproc:
            if( !translateProc(e->d) )
                return false;
            emitOp(proc, LL_ldproc, findProc(e->d));
            break;
        case IL_ldmeth:
            if( !translateProc(e->d) )
                return false;
            emitOp(proc, LL_ldmeth, e->d->getPd()->slot);
            break;
        case IL_conv_i1:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_i1_i4);
            else if( lhsT->isInt64())
                emitOp(proc, LL_conv_i1_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_i1_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_i1_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_conv_i2:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_i2_i4);
            else if( lhsT->isInt64())
                emitOp(proc, LL_conv_i2_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_i2_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_i2_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_conv_i4:
            if( lhsT->isInt64())
                emitOp(proc, LL_conv_i4_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_i4_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_i4_r8);
            else if( !lhsT->isInt32OnStack() )
                Q_ASSERT(false);
            break;
        case IL_conv_i8:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_i8_i4);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_i8_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_i8_r8);
            else if( !lhsT->isInt64() )
                Q_ASSERT(false);
            break;
        case IL_conv_r4:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_r4_i4);
            else if(lhsT->isInt64() )
                emitOp(proc, LL_conv_r4_i8);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_r4_r8);
            else if( !lhsT->isFloat() )
                Q_ASSERT(false);
            break;
        case IL_conv_r8:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_r8_i4);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_r8_r4);
            else if( lhsT->isInt64() )
                emitOp(proc, LL_conv_r8_i8);
            else if( !lhsT->isFloat() )
                Q_ASSERT(false);
            break;
        case IL_conv_u1:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_u1_i4);
            else if( lhsT->isInt64())
                emitOp(proc, LL_conv_u1_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_u1_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_u1_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_conv_u2:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_u2_i4);
            else if( lhsT->isInt64())
                emitOp(proc, LL_conv_u2_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_u2_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_u2_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_conv_u4:
            if( lhsT->isInt64() )
                emitOp(proc, LL_conv_u4_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_u4_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_u4_r8);
            else if( !lhsT->isInt32OnStack() )
                Q_ASSERT(false);
            break;
        case IL_conv_u8:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, LL_conv_u8_i4);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, LL_conv_u8_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, LL_conv_u8_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_ceq:
            Q_ASSERT(lhsT);
            Q_ASSERT(rhsT);
            t = lhsT;
            if( t->isInt32OnStack() )
                emitOp(proc, LL_ceq_i4);
            else if( t->isInt64())
                emitOp(proc, LL_ceq_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_ceq_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_ceq_r8);
            else if(lhsT->kind == Type::Pointer || (lhsT->kind == Type::Proc && !lhsT->typebound) ||
                    rhsT->kind == Type::Pointer || (rhsT->kind == Type::Proc && !rhsT->typebound) )
                emitOp(proc, LL_ceq_p);
            else if( (lhsT->kind == Type::Proc && lhsT->typebound) || (rhsT->kind == Type::Proc && rhsT->typebound) )
                emitOp(proc, LL_ceq_pp);
            else
                Q_ASSERT(false);
            break;
        case IL_cgt:
            Q_ASSERT(lhsT);
            t = lhsT;
            if( t->isInt32OnStack() )
                emitOp(proc, LL_cgt_i4);
            else if( t->isInt64())
                emitOp(proc, LL_cgt_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_cgt_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_cgt_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_cgt_un:
            Q_ASSERT(lhsT);
            t = lhsT;
            if( t->isInt32OnStack() )
                emitOp(proc, LL_cgt_u4);
            else if( t->isInt64())
                emitOp(proc, LL_cgt_u8);
            else if(t->kind == Type::Pointer || t->kind == Type::NIL)
                emitOp(proc, LL_cgt_p);
            else
                Q_ASSERT(false);
            break;
        case IL_clt:
            Q_ASSERT(lhsT);
            t = lhsT;
            if( t->isInt32OnStack() )
                emitOp(proc, LL_clt_i4);
            else if( t->isInt64())
                emitOp(proc, LL_clt_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, LL_clt_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, LL_clt_r8);
            else
                Q_ASSERT(false);
            break;
        case IL_clt_un:
            Q_ASSERT(lhsT);
            t = lhsT;
            if( t->isInt32OnStack() )
                emitOp(proc, LL_clt_u4);
            else if( t->isInt64())
                emitOp(proc, LL_clt_u8);
            else if(t->kind == Type::Pointer || t->kind == Type::NIL)
                emitOp(proc, LL_clt_p);
            else
                Q_ASSERT(false);
            break;
        case IL_ldarg_0:
        case IL_ldarg_1:
        case IL_ldarg_2:
        case IL_ldarg_3:
        case IL_ldarg_s:
        case IL_ldarg:
            {
                DeclList params = proc.decl->getParams();
                Q_ASSERT(e->id < params.size());
                Type* t = deref(params[e->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                    emitOp(proc, LL_ldarg_i1,params[e->id]->off);
                    break;
                case Type::INT16:
                    emitOp(proc, LL_ldarg_i2,params[e->id]->off);
                    break;
                case Type::INT32:
                    emitOp(proc, LL_ldarg_i4,params[e->id]->off);
                    break;
                case Type::INT64:
                    emitOp(proc, LL_ldarg_i8,params[e->id]->off);
                    break;
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, LL_ldarg_u1,params[e->id]->off);
                    break;
                case Type::UINT16:
                    emitOp(proc, LL_ldarg_u2,params[e->id]->off);
                    break;
                case Type::UINT32:
                    emitOp(proc, LL_ldarg_u4,params[e->id]->off);
                    break;
                case Type::UINT64:
                    emitOp(proc, LL_ldarg_u8,params[e->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, LL_ldarg_r4,params[e->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, LL_ldarg_r8,params[e->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, LL_ldarg_pp,params[e->id]->off);
                    else
                        emitOp(proc, LL_ldarg_p,params[e->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, LL_ldarg_vt,params[e->id]->off);
                    emitOp(proc, LL_vt_size,t->getByteSize(pointerWidth));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case IL_ldarga_s:
        case IL_ldarga:
            {
                Q_ASSERT(ctxStack.back().curProc);
                DeclList params = ctxStack.back().curProc->decl->getParams();
                Q_ASSERT(e->id < params.size());
                emitOp(proc, LL_ldarga,params[e->id]->off);
            }
            break;
        case IL_ldloc_0:
        case IL_ldloc_1:
        case IL_ldloc_2:
        case IL_ldloc_3:
        case IL_ldloc_s:
        case IL_ldloc:
            {
                DeclList params = proc.decl->getLocals();
                Q_ASSERT(e->id < params.size());
                Type* t = deref(params[e->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                    emitOp(proc, LL_ldloc_i1,params[e->id]->off);
                    break;
                case Type::INT16:
                    emitOp(proc, LL_ldloc_i2,params[e->id]->off);
                    break;
                case Type::INT32:
                    emitOp(proc, LL_ldloc_i4,params[e->id]->off);
                    break;
                case Type::INT64:
                    emitOp(proc, LL_ldloc_i8,params[e->id]->off);
                    break;
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, LL_ldloc_u1,params[e->id]->off);
                    break;
                case Type::UINT16:
                    emitOp(proc, LL_ldloc_u2,params[e->id]->off);
                    break;
                case Type::UINT32:
                    emitOp(proc, LL_ldloc_u4,params[e->id]->off);
                    break;
                case Type::UINT64:
                    emitOp(proc, LL_ldloc_u8,params[e->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, LL_ldloc_r4,params[e->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, LL_ldloc_r8,params[e->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, LL_ldloc_pp,params[e->id]->off);
                    else
                        emitOp(proc, LL_ldloc_p,params[e->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, LL_ldloc_vt,params[e->id]->off);
                    emitOp(proc, LL_vt_size,t->getByteSize(pointerWidth));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case IL_ldloca_s:
        case IL_ldloca:
            {
                DeclList locals = proc.decl->getLocals();
                Q_ASSERT(e->id < locals.size());
                emitOp(proc, LL_ldloca,locals[e->id]->off);
            }
            break;
        case IL_ldind_i1:
            emitOp(proc, LL_ldind_i1);
            break;
        case IL_ldind_i2:
            emitOp(proc, LL_ldind_i2);
            break;
        case IL_ldind_i4:
            emitOp(proc, LL_ldind_i4);
            break;
        case IL_ldind_i8:
            emitOp(proc, LL_ldind_i8);
            break;
        case IL_ldind_ip:
            emitOp(proc, LL_ldind_p);
            break;
        case IL_ldind_ipp:
            emitOp(proc, LL_ldind_vt,sizeof(MethRef));
            break;
        case IL_ldind_r4:
            emitOp(proc, LL_ldind_r4);
            break;
        case IL_ldind_r8:
            emitOp(proc, LL_ldind_r8);
            break;
        case IL_ldind_u1:
            emitOp(proc, LL_ldind_u1);
            break;
        case IL_ldind_u2:
            emitOp(proc, LL_ldind_u2);
            break;
        case IL_ldind_u4:
            emitOp(proc, LL_ldind_u2);
            break;
        case IL_ldind_u8:
            emitOp(proc, LL_ldind_u8);
            break;
        case IL_ldind:
            if( lhsT && (lhsT->kind == Type::StringLit || lhsT->isPtrToOpenCharArray()) )
                emitOp(proc, LL_ldind_str,e->getType()->len);
            else
                emitOp(proc, LL_ldind_vt,t->getByteSize(pointerWidth));
            break;
        case IL_ldelem_i1:
            emitOp(proc, LL_ldelem_i1);
            break;
        case IL_ldelem_i2:
            emitOp(proc, LL_ldelem_i2);
            break;
        case IL_ldelem_i4:
            emitOp(proc, LL_ldelem_i4);
            break;
        case IL_ldelem_i8:
            emitOp(proc, LL_ldelem_i8);
            break;
        case IL_ldelem_ip:
            emitOp(proc, LL_ldelem_p);
            break;
        case IL_ldelem_r4:
            emitOp(proc, LL_ldelem_r4);
            break;
        case IL_ldelem_r8:
            emitOp(proc, LL_ldelem_r8);
            break;
        case IL_ldelem_u1:
            emitOp(proc, LL_ldelem_u1);
            break;
        case IL_ldelem_u2:
            emitOp(proc, LL_ldelem_u2);
            break;
        case IL_ldelem_u4:
            emitOp(proc, LL_ldelem_u4);
            break;
        case IL_ldelem_u8:
            emitOp(proc, LL_ldelem_u8);
            break;
        case IL_ldelem_ipp:
            emitOp(proc, LL_ldelem_vt,sizeof(MethRef));
            break;
        case IL_ldelem:
            emitOp(proc, LL_ldelem_vt,t->getByteSize(pointerWidth));
            break;
        case IL_ldelema:
            emitOp(proc, LL_ldelema,t->getType()->getByteSize(pointerWidth)); // deref pointer for et
            break;
        case IL_ldfld:
            switch(t->kind)
            {
            case Type::INT8:
                emitOp(proc, LL_ldfld_i1,e->d->f.off);
                break;
            case Type::INT16:
                emitOp(proc, LL_ldfld_i2,e->d->f.off);
                break;
            case Type::INT32:
                emitOp(proc, LL_ldfld_i4,e->d->f.off);
                break;
            case Type::INT64:
                emitOp(proc, LL_ldfld_i8,e->d->f.off);
                break;
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, LL_ldfld_u1,e->d->f.off);
                break;
            case Type::UINT16:
                emitOp(proc, LL_ldfld_u2,e->d->f.off);
                break;
            case Type::UINT32:
                emitOp(proc, LL_ldfld_u4,e->d->f.off);
                break;
            case Type::UINT64:
                emitOp(proc, LL_ldfld_u8,e->d->f.off);
                break;
            case Type::FLOAT32:
                emitOp(proc, LL_ldfld_r4,e->d->f.off);
                break;
            case Type::FLOAT64:
                emitOp(proc, LL_ldfld_r8,e->d->f.off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(t->typebound)
                    emitOp(proc, LL_ldfld_pp,e->d->f.off);
                else
                    emitOp(proc, LL_ldfld_p,e->d->f.off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, LL_ldfld_vt,e->d->f.off);
                emitOp(proc, LL_vt_size,t->getByteSize(pointerWidth));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case IL_ldflda:
            emitOp(proc, LL_ldflda, e->d->f.off);
            break;
        case IL_ldvar:
            switch(t->kind)
            {
            case Type::INT8:
                emitOp(proc, LL_ldvar_i1,e->d->off);
                break;
            case Type::INT16:
                emitOp(proc, LL_ldvar_i2,e->d->off);
                break;
            case Type::INT32:
                emitOp(proc, LL_ldvar_i4,e->d->off);
                break;
            case Type::INT64:
                emitOp(proc, LL_ldvar_i8,e->d->off);
                break;
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, LL_ldvar_u1,e->d->off);
                break;
            case Type::UINT16:
                emitOp(proc, LL_ldvar_u2,e->d->off);
                break;
            case Type::UINT32:
                emitOp(proc, LL_ldvar_u4,e->d->off);
                break;
            case Type::UINT64:
                emitOp(proc, LL_ldvar_u8,e->d->off);
                break;
            case Type::FLOAT32:
                emitOp(proc, LL_ldvar_r4,e->d->off);
                break;
            case Type::FLOAT64:
                emitOp(proc, LL_ldvar_r8,e->d->off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(t->typebound)
                    emitOp(proc, LL_ldvar_pp,e->d->off);
                else
                    emitOp(proc, LL_ldvar_p,e->d->off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, LL_ldvar_vt,e->d->off);
                emitOp(proc, LL_vt_size,t->getByteSize(pointerWidth));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case IL_ldvara:
            emitOp(proc, LL_ldvara, e->d->off);
            break;
        case IL_newobj:
        case IL_newarr:
        case IL_initobj: {
                Type* tt = deref(e->d->getType());
                const int len = tt->getByteSize(pointerWidth);
                const LL_op op = e->kind == IL_newobj ? LL_alloc1 :
                                 e->kind == IL_newarr ?
                                                       LL_allocN : // N is on stack
                                                             LL_initobj;
                if( tt->objectInit || tt->pointerInit )
                {
                    int id = findTemplate(tt);
                    if( id == -1 )
                    {
                        id = templates.size();
                        templates.push_back(Template());
                        Template& temp = templates.back();
                        temp.type = tt;
                        temp.mem.resize(len);
                        initMemory(temp.mem.data(), tt, true);
                    }
                    emitOp(proc,op, id, true); // minus -> template id
                }else
                    emitOp(proc,op, len);
            }
            break;
        case IL_nop:
        case IL_castptr:
            break; // NOP
        case IL_dup:
            emitOp(proc,LL_dup, e->getType()->getByteSize(pointerWidth));
            break;
        case IL_call:
        case IL_callvirt:
        case IL_callinst:
            {
                if( !translateProc(e->d) )
                    return false;
                const int id = findProc(e->d);
                if( id < 0 )
                {
                    qCritical() << "cannot find implementation of" << e->d->toPath();
                    return false;
                }
                if( e->kind == IL_call )
                    emitOp(proc, LL_call, id);
                else if( e->kind == IL_callinst )
                    emitOp(proc, LL_callinst, id);
                else
                    emitOp(proc, LL_callvirt, id);
            }
            break;
        case IL_calli:
            emitOp(proc, LL_calli);
            break;
        case IL_callmi:
            emitOp(proc, LL_callmi);
            break;
        case IL_iif:
            {
                Expression* if_ = e->e;
                Q_ASSERT(if_ && if_->kind == IL_if && if_->next->kind == IL_then && if_->next->next->kind == IL_else &&
                         if_->next->next->next == 0); // no IL_end
                Expression* then_ = if_->next;
                Expression* else_ = if_->next->next;
               if( !translateExprSeq(proc, if_->e) )
                    return false;
                const int iif = emitOp(proc,LL_brfalse_i4);
                if( !translateExprSeq(proc, then_->e) )
                    return false;
                const int end_of_then = emitOp(proc, LL_br);
                branch_here(proc,iif);
                if( !translateExprSeq(proc, else_->e) )
                    return false;
                branch_here(proc,end_of_then);
            }
            break;
        case IL_isinst: {
                const int id = findVtable(deref(e->d->getType()));
                if( id < 0 )
                {
                    qCritical() << "cannot find vtable of" << t->decl->toPath();
                    return false;
                }
                emitOp(proc, LL_isinst, id);
            }
            break;
        case IL_sizeof:
        case IL_ptroff:
        case IL_newvla:
        case IL_ldiface:
            qCritical() << "ERROR: not yet implemented in interpreter:" << s_opName[e->kind];
            return false;
        default:
            Q_ASSERT(false);
        }
        e = e->next;
    }
    return true;
}

bool Code::translateInit(Procedure& proc, quint32 id)
{
    Q_ASSERT( proc.decl && (proc.decl->kind == Declaration::Module || proc.decl->kind == Declaration::Procedure) );

    Declaration* d = proc.decl->subs;
    while(d)
    {
        if( d->kind == Declaration::Import )
        {
            translateModule(d->imported);
            const int p = findProc(d->imported);
            if( p < 0 )
            {
                qCritical() << "module initializer not found" << d->imported->toPath();
                return false; // procedure not found
            }
            emitOp(proc,LL_call, p);
        }
        // TODO: initialize structs, arrays and objects for value objects vtables
        d = d->next;
    }
    return true;
}

void Code::render(char* data, quint32 off, Type* t, Constant* c)
{
    switch(c->kind)
    {
    case Constant::D:
        if( t->kind == Type::FLOAT32 )
        {
            float tmp = c->d;
            memcpy(data+off, &tmp, sizeof(float));
        }else
        {
            Q_ASSERT(t->kind == Type::FLOAT64);
            memcpy(data+off, &c->d, sizeof(double));
        }
        break;
    case Constant::I:
        switch(t->kind)
        {
        case Type::BOOL:
        case Type::CHAR:
        case Type::UINT8:
        case Type::INT8: {
                qint8 tmp = c->i;
                *(data+off) = tmp;
                break;
            }
        case Type::UINT16:
        case Type::INT16: {
                qint16 tmp = c->i;
                memcpy(data+off, &tmp, sizeof(qint16));
                break;
            }
        case Type::UINT32:
        case Type::INT32: {
                qint32 tmp = c->i;
                memcpy(data+off, &tmp, sizeof(qint32));
                break;
            }
        case Type::UINT64:
        case Type::INT64:
            memcpy(data+off, &c->i, sizeof(qint64));
            break;
        default:
            Q_ASSERT(false);
        }
        break;
    case Constant::S:
        Q_ASSERT(t->kind == Type::Array && deref(t->getType())->kind == Type::CHAR && t->len);
        strncpy(data+off, c->s, t->len-1);
        *(data+off+t->len-1) = 0;
        break;
    case Constant::B:
        Q_ASSERT(t->kind == Type::Array && deref(t->getType())->kind == Type::UINT8 &&
                 t->len == c->b->len);
        memcpy(data+off, c->b, t->len);
        break;
    case Constant::R:
        Q_ASSERT(c->r->kind == Declaration::ConstDecl);
        render(data, off, t, c->r->c);
        break;
    case Constant::C:
        if( c->c->type == 0 )
            c->c->type = t;
        render(data, off, c->c);
        break;
    default:
        Q_ASSERT(false);
    }
}

void Code::render(char* data, quint32 start, ComponentList* cl)
{
    Type* t = deref(cl->type);
    if( t->kind == Type::Array )
    {
        Type* et = deref(t->getType());
        int off = start;
        for( int i = 0; i < cl->c.size(); i++ )
        {
            render(data, off, et, cl->c[i].c);
            off += et->getByteSize(sizeof(void*));
        }

    }else
    {
        qWarning() << "TODO record literals not yet implemented";
    }
}

void Code::downcopy(Vtable* vt)
{
    // make sure that each vtable is filled with inherited methods as far as used
    // TODO avoid multiple scans of same vt
    if( vt->parent )
    {
        downcopy(vt->parent);
        for( int i = 0; i < vt->parent->methods.size(); i++ )
        {
            if( vt->parent->methods[i] && vt->methods[i] == 0 )
                vt->methods[i] = vt->parent->methods[i];
        }
    }
}

bool Code::dumpProc(QTextStream& out, Declaration* proc)
{
    if( proc->forward )
        return false;
    const int i = findProc(proc);
    if( i == -1 )
        return false; // there is no implementation for this proc, not an error
    Procedure* p = getProc(i);
    out << "proc " << p->decl->toPath() << endl;
    for( int pc = 0; pc < p->ops.size(); pc++ )
    {
        out << "    " << QString("%1: ").arg(pc,2) << Code::op_names[p->ops[pc].op];
        switch(op_args[p->ops[pc].op])
        {
        case NoOpArgs:
            break;
        case OffArg:
            out << " " << p->ops[pc].val;
            break;
        case SizeArg:
            if( p->ops[pc].minus )
                out << " template";
            out << " " << p->ops[pc].val;
            break;
        case IntArg:
            out << " " << ints[p->ops[pc].val];
            break;
        case FloatArg:
            out << " " << doubles[p->ops[pc].val];
            break;
        case StrArg:
            out << " \"" << strings[p->ops[pc].val].c_str() << "\"";
            break;
        case ByteArrayArg: {
                const std::vector<char>& tmp = objects[p->ops[pc].val];
                QByteArray buf = QByteArray::fromRawData(tmp.data(),tmp.size());
                out << " $" << buf.toHex().left(40) << " (" << buf.size() << ")";
            } break;
        case ProcArg:
            if( p->ops[pc].val < procs.size() )
                out << " " << procs[p->ops[pc].val]->decl->toPath();
            else
                out << " invalid proc " << p->ops[pc].val;
            break;
        case JumpArg:
            out << " " << (p->ops[pc].minus ? "-" : "") << p->ops[pc].val << " -> "
                << QString("%1").arg(pc + 1 + (p->ops[pc].minus?-1:1) * p->ops[pc].val);
            break;
        case OffSizeArgs:
            Q_ASSERT(pc+1 < p->ops.size());
            out << " " << p->ops[pc].val;
            out << " " << p->ops[pc+1].val;
            pc++;
           break;
        case VtableArg:
            if( p->ops[pc].val < vtables.size() )
                out << " " << vtables[p->ops[pc].val]->type->decl->toPath();
            else
                out << " invalid vtable " << p->ops[pc].val;
            break;

        default:
            Q_ASSERT(false);
        }

        out << endl;
    }
    return true;
}

bool Code::dumpModule(QTextStream& out, Declaration* module)
{
    Declaration* d = module->subs;
    while(d)
    {
        if(d->kind == Declaration::Procedure)
            dumpProc(out, d);
        else if( d->kind == Declaration::TypeDecl && d->getType()->kind == Type::Object )
        {
            foreach( Declaration* sub, d->getType()->subs )
            {
                if(sub->kind == Declaration::Procedure)
                    dumpProc(out, sub);
            }
        }
        d = d->next;
    }
    return true;
}

bool Code::dumpAll(QTextStream& out)
{
    DeclList& modules = mdl->getModules();
    foreach( Declaration* module, modules )
        dumpModule(out, module);
    return true;
}

void Code::initMemory(char* mem, Type* t, bool doPointerInit )
{
    if( doPointerInit && t->pointerInit )
        memset(mem, 0, t->getByteSize(sizeof(void*)));
    if( !t->objectInit )
        return;
    if( t->kind == Type::Struct || t->kind == Type::Object )
    {
        if( t->kind == Type::Object )
        {
            Vtable* vt = getVtable(t);
            memcpy(mem, &vt, sizeof(vt));
            foreach( Declaration* d, t->subs )
            {
                if( d->kind == Declaration::Procedure )
                    translateProc(d);
            }
        }

        DeclList fields = t->getFieldList(true);
        foreach(Declaration* field, fields)
        {

            Type* tt = deref(field->getType());
            if( tt->objectInit )
                initMemory(mem + field->f.off, tt, false);
        }
    }else if( t->kind == Type::Array && t->len != 0)
    {
        Type* et = deref(t->getType());
        int off = 0;
        for(int i = 0; i < t->len; i++ )
        {
            initMemory(mem + off, et, false);
            off += et->getByteSize(sizeof(void*));
        }
    }
}
