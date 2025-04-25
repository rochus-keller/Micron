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
#include <QtDebug>
using namespace Mil;

enum OpArgCode {
    NoOpArgs,
    OffArg,
    SizeArg,
    IntArg,
    FloatArg,
    StrArg,
    ProcArg,
    JumpArg,
    OffSizeArgs
};

enum IL_op
{
    IL_invalid,

#define OPDEF(op, x) IL_##op
#include "MilVmOps.h"
#undef OPDEF

    IL_NUM_OF_OPS
};

static const char* op_names[] = {
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
    QByteArray locals;
    QByteArray stack; // intermediate, calls, aligns to quint64
};

struct Interpreter::Imp
{
    AstModel* mdl;
    QList<QByteArray> strings;
    QList<double> doubles;
    QList<qint64> ints;
    QList<Procedure> procs;
    QByteArray moduleData;
    Procedure* curProc;
    QList< QList<int> > loopStack;

    Imp(AstModel* mdl):mdl(mdl),curProc(0)
    {

    }

    bool translateModule(Declaration* m)
    {
        Q_ASSERT(m && m->kind == Declaration::Module);

        if( m->init )
            return true; // the module was already translated

        m->init = true;

        // look for the init procedure or synthesize one
        Declaration* init = m->findInitProc();
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
        proc->validated = true;

        const QByteArray path = proc->toPath();
        procs.append(Procedure());
        Procedure& cur = procs.back();
        cur.decl = proc;

        if( proc->init && !translateInit(cur) )
            // add a prefix which calls imports if not already called
            return false;
        return translateProc(cur);
    }

    int findProc(Declaration* proc) const
    {
        if( proc->kind == Declaration::Module )
        {
            Declaration* init = proc->findInitProc();
            if( init )
                proc = init;
        }
        for( int i = 0; i < procs.size(); i++ )
        {
            if( procs[i].decl == proc )
                return i;
        }
        return -1;
    }

    bool run(Declaration* proc)
    {
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

    int emitOp(Procedure& proc, IL_op op, quint32 v = 0, bool minus = false )
    {
        const int res = proc.ops.size();
        proc.ops.append(Operation(op, v, minus));
        return res;
    }

    Type* deref(Type* t)
    {
        if( t && t->kind == Type::NameRef )
            return deref(t->getType());
        else if( t )
            return t;
        else
            return mdl->getBasicType(Type::Undefined);
    }

    void inline branch_here(Procedure& proc, int pc)
    {
        Q_ASSERT(pc >= 0 && pc < proc.ops.size());
        proc.ops[pc].val = proc.ops.size() - pc - 1;
    }

    bool translateInit(Procedure& proc);
    bool translateProc(Procedure& proc);
    bool translateStatSeq(Procedure& proc, Statement* s);
    bool translateExprSeq(Procedure& proc, Expression* e);

    bool run(quint32) { return true; } // TODO
};

Interpreter::Interpreter(AstModel* mdl)
{
    imp = new Imp(mdl);
}

Interpreter::~Interpreter()
{
    delete imp;
}

bool Interpreter::precompile(Declaration* proc)
{
    Q_ASSERT(proc && (proc->kind == Declaration::Procedure || proc->kind == Declaration::Module));

    Declaration* module = proc->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    if( module->init )
        return true;

    imp->mdl->calcMemoryLayouts(sizeof(void*), 8);

    // TODO: if we run for the second time, we might reset the interpreter

    return imp->translateModule(module);
}

bool Interpreter::dumpProc(QTextStream& out, Declaration* proc)
{
    if( proc->forward )
        return false;
    const int i = imp->findProc(proc);
    if( i == -1 )
        return false;
    Procedure* p = &imp->procs[i];
    out << "proc " << p->decl->toPath() << endl;
    for( int pc = 0; pc < p->ops.size(); pc++ )
    {
        out << "    " << QString("%1: ").arg(pc,2) << op_names[p->ops[pc].op];
        switch(op_args[p->ops[pc].op])
        {
        case NoOpArgs:
            break;
        case OffArg:
            out << " " << p->ops[pc].val;
            break;
        case SizeArg:
            out << " " << p->ops[pc].val;
            break;
        case IntArg:
            out << " " << imp->ints[p->ops[pc].val];
            break;
        case FloatArg:
            out << " " << imp->doubles[p->ops[pc].val];
            break;
        case StrArg:
            out << " " << imp->strings[p->ops[pc].val];
            break;
        case ProcArg:
            out << " " << imp->procs[p->ops[pc].val].decl->toPath();
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
        default:
            Q_ASSERT(false);
        }

        out << endl;
    }
    return true;
}

bool Interpreter::dumpModule(QTextStream& out, Declaration* module)
{
    Declaration* d = module->subs;
    while(d)
    {
        if(d->kind == Declaration::Procedure)
            dumpProc(out, d);
        d = d->next;
    }
    return true;
}

bool Interpreter::dumpAll(QTextStream& out)
{
    DeclList& modules = imp->mdl->getModules();
    foreach( Declaration* module, modules )
        dumpModule(out, module);
    return true;
}

bool Interpreter::run(Declaration* proc)
{
    Q_ASSERT(proc && (proc->kind == Declaration::Procedure || proc->kind == Declaration::Module));

    Declaration* module = proc->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    if( !module->init && !precompile(module) )
        return true;

    if( proc->kind == Declaration::Module )
        return imp->run(proc);

    if( !proc->init )
    {
        if( !imp->run(module) )
            return false;
    }

    return imp->run(proc);
}

bool Interpreter::Imp::translateInit(Procedure& proc)
{
    Q_ASSERT( proc.decl && (proc.decl->kind == Declaration::Module || proc.decl->kind == Declaration::Procedure) );

    Declaration* module;
    if( proc.decl->kind == Declaration::Procedure )
        module = proc.decl->getModule();
    else
        module = proc.decl;

    DeclList vars = module->getVars();
    if( !vars.isEmpty() )
    {
        const int off = moduleData.size();
        const int len = vars.last()->off + vars.last()->getType()->getByteSize(sizeof(void*));
        moduleData.resize(off + len + AstModel::padding(len, sizeof(void*)));
        if( off )
        {
            // relocate module var addresses
            foreach( Declaration* d, vars )
                d->off += off;
        }
    }

    // first check if already called
    // TODO: this can likely be done at translation time instead of runtime!
#if 0
    emitOp(proc,IL_already_called);
    emitOp(proc,IL_brfalse_i4,1);
    emitOp(proc,IL_ret_void);
#endif

    Declaration* d = proc.decl->subs;
    while(d)
    {
        if( d->kind == Declaration::Import )
        {
            translateModule(d->imported);
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
    curProc = &proc;
    const bool res = translateStatSeq(proc, s);
    curProc = 0;
    return res;
}

bool Interpreter::Imp::translateStatSeq(Procedure& proc, Statement* s)
{
    while(s)
    {
        switch(s->kind)
        {
        case Tok_STARG:
            {
                Q_ASSERT(curProc);
                DeclList params = curProc->decl->getParams();
                Q_ASSERT(s->id < params.size());
                Type* t = deref(params[s->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_starg_i1,params[s->id]->off);
                    break;
                case Type::INT16:
                case Type::UINT16:
                    emitOp(proc, IL_starg_i2,params[s->id]->off);
                    break;
                case Type::INT32:
                case Type::UINT32:
                    emitOp(proc, IL_starg_i4,params[s->id]->off);
                    break;
                case Type::INT64:
                case Type::UINT64:
                    emitOp(proc, IL_starg_i8,params[s->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_starg_r4,params[s->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_starg_r8,params[s->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_starg_pp,params[s->id]->off);
                    else
                        emitOp(proc, IL_starg_p,params[s->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_starg_vt,params[s->id]->off);
                    emitOp(proc, IL_vt_size,t->getByteSize(sizeof(void*)));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_STLOC:
        case Tok_STLOC_S:
        case Tok_STLOC_0:
        case Tok_STLOC_1:
        case Tok_STLOC_2:
        case Tok_STLOC_3:
            {
                Q_ASSERT(curProc);
                DeclList locals = curProc->decl->getLocals();
                Q_ASSERT(s->id < locals.size());
                Type* t = deref(locals[s->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_stloc_i1,locals[s->id]->off);
                    break;
                case Type::INT16:
                case Type::UINT16:
                    emitOp(proc, IL_stloc_i2,locals[s->id]->off);
                    break;
                case Type::INT32:
                case Type::UINT32:
                    emitOp(proc, IL_stloc_i4,locals[s->id]->off);
                    break;
                case Type::UINT64:
                case Type::INT64:
                    emitOp(proc, IL_stloc_i8,locals[s->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_stloc_r4,locals[s->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_stloc_r8,locals[s->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_stloc_pp,locals[s->id]->off);
                    else
                        emitOp(proc, IL_stloc_p,locals[s->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_stloc_vt,locals[s->id]->off);
                    emitOp(proc, IL_vt_size, t->getByteSize(sizeof(void*)));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_STIND:
            emitOp(proc, IL_stind_vt,deref(s->d->getType())->getByteSize(sizeof(void*)));
            break;
        case Tok_STIND_I1:
            emitOp(proc, IL_stind_i1);
            break;
        case Tok_STIND_I2:
            emitOp(proc, IL_stind_i2);
            break;
        case Tok_STIND_I4:
            emitOp(proc, IL_stind_i4);
            break;
        case Tok_STIND_I8:
            emitOp(proc, IL_stind_i8);
            break;
        case Tok_STIND_R4:
            emitOp(proc, IL_stind_r4);
            break;
        case Tok_STIND_R8:
            emitOp(proc, IL_stind_r8);
            break;
        case Tok_STIND_IP:
            emitOp(proc, IL_stind_p);
            break;
        case Tok_STIND_IPP:
            emitOp(proc, IL_stind_pp);
            break;
        case Tok_STELEM:
            emitOp(proc, IL_stelem_vt, deref(s->d->getType())->getByteSize(sizeof(void*)));
            break;
        case Tok_STELEM_I1:
            emitOp(proc, IL_stelem_i1);
            break;
        case Tok_STELEM_I2:
            emitOp(proc, IL_stelem_i2);
            break;
        case Tok_STELEM_I4:
            emitOp(proc, IL_stelem_i4);
            break;
        case Tok_STELEM_I8:
            emitOp(proc, IL_stelem_i8);
            break;
        case Tok_STELEM_R4:
            emitOp(proc, IL_stelem_r4);
            break;
        case Tok_STELEM_R8:
            emitOp(proc, IL_stelem_r8);
            break;
        case Tok_STELEM_IP:
            emitOp(proc, IL_stelem_p);
            break;
        case Statement::ExprStat:
            translateExprSeq(proc, s->e);
            break;
        case Tok_STFLD:
            switch(deref(s->d->getType())->kind)
            {
            case Type::INT8:
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_stloc_i1,s->d->f.off);
                break;
            case Type::INT16:
            case Type::UINT16:
                emitOp(proc, IL_stloc_i2,s->d->f.off);
                break;
            case Type::INT32:
            case Type::UINT32:
                emitOp(proc, IL_stloc_i4,s->d->f.off);
                break;
            case Type::UINT64:
            case Type::INT64:
                emitOp(proc, IL_stloc_i8,s->d->f.off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_stloc_r4,s->d->f.off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_stloc_r8,s->d->f.off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(deref(s->d->getType())->typebound)
                    emitOp(proc, IL_stloc_pp,s->d->f.off);
                else
                    emitOp(proc, IL_stloc_p,s->d->f.off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_stloc_vt,s->d->f.off);
                emitOp(proc, IL_vt_size, deref(s->d->getType())->getByteSize(sizeof(void*)));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_STVAR:
            switch(deref(s->d->getType())->kind)
            {
            case Type::INT8:
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_stvar_i1,s->d->off);
                break;
            case Type::INT16:
            case Type::UINT16:
                emitOp(proc, IL_stvar_i2,s->d->off);
                break;
            case Type::INT32:
            case Type::UINT32:
                emitOp(proc, IL_stvar_i4,s->d->off);
                break;
            case Type::UINT64:
            case Type::INT64:
                emitOp(proc, IL_stvar_i8,s->d->off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_stvar_r4,s->d->off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_stvar_r8,s->d->off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(deref(s->d->getType())->typebound)
                    emitOp(proc, IL_stvar_pp,s->d->off);
                else
                    emitOp(proc, IL_stvar_p,s->d->off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_stvar_vt,s->d->off);
                emitOp(proc, IL_vt_size, deref(s->d->getType())->getByteSize(sizeof(void*)));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_IF:
            {
                translateExprSeq(proc, s->e);
                const int ifnot = emitOp(proc, deref(s->e->getType())->isInt64() ? IL_brfalse_i8 : IL_brfalse_i4);
                translateStatSeq(proc, s->body);
                const int after_if = emitOp(proc, IL_br);
                branch_here(proc, ifnot);
                if( s->next && s->next->kind == Tok_ELSE )
                {
                    s = s->next;
                    translateStatSeq(proc, s->body);
                }
                branch_here(proc,after_if);
            }
            break;
        case Tok_LOOP:
            {
                loopStack.push_back(QList<int>());
                translateStatSeq(proc, s->body);
                foreach( int pc, loopStack.back() )
                    branch_here(proc,pc);
                loopStack.pop_back();
            }
            break;
        case Tok_EXIT:
            loopStack.back() << emitOp(proc,IL_br);
            break;
        case Tok_REPEAT:
            {
                const int start = proc.ops.size();
                translateStatSeq(proc, s->body);
                translateExprSeq(proc, s->e);
                emitOp(proc, s->e->getType()->isInt64() ? IL_brfalse_i8 : IL_brfalse_i4, proc.ops.size()-start+1, true );
            }
            break;
        case Tok_WHILE:
            {
                const int start = proc.ops.size();
                translateExprSeq(proc, s->e);
                const int while_ = emitOp(proc, s->e->getType()->isInt64() ? IL_brfalse_i8 : IL_brfalse_i4 );
                translateStatSeq(proc, s->body);
                emitOp(proc, IL_br, proc.ops.size()-start+1, true);
                branch_here(proc, while_);
            }
            break;
        case Tok_POP:
            emitOp(proc, IL_pop, s->args->getType()->getByteSize(sizeof(void*)));
            break;
        case Tok_RET:
            emitOp(proc, IL_ret, s->args ? s->args->getType()->getByteSize(sizeof(void*)) : 0 );
            break;
        case Tok_SWITCH:
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
    static const int pointerWidth = sizeof(void*);
    while(e)
    {
        Type* t = deref(e->getType());
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
            else if( !t->isInt32OnStack() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_I8:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_i8_i4);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i8_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i8_r8);
            else if( !t->isInt64() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_R4:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_r4_i4);
            else if(t->isInt64() )
                emitOp(proc, IL_conv_r4_i8);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_r4_r8);
            else if( !t->isFloat() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_R8:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_conv_r8_i4);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_r8_r4);
            else if( t->isInt64() )
                emitOp(proc, IL_conv_r8_i8);
            else if( !t->isFloat() )
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
            else if( !t->isInt32OnStack() )
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
            {
                DeclList params = proc.decl->getParams();
                Q_ASSERT(e->id < params.size());
                Type* t = deref(params[e->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                    emitOp(proc, IL_ldarg_i1,params[e->id]->off);
                    break;
                case Type::INT16:
                    emitOp(proc, IL_ldarg_i2,params[e->id]->off);
                    break;
                case Type::INT32:
                    emitOp(proc, IL_ldarg_i4,params[e->id]->off);
                    break;
                case Type::INT64:
                    emitOp(proc, IL_ldarg_i8,params[e->id]->off);
                    break;
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_ldarg_u1,params[e->id]->off);
                    break;
                case Type::UINT16:
                    emitOp(proc, IL_ldarg_u2,params[e->id]->off);
                    break;
                case Type::UINT32:
                    emitOp(proc, IL_ldarg_u4,params[e->id]->off);
                    break;
                case Type::UINT64:
                    emitOp(proc, IL_ldarg_u8,params[e->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_ldarg_r4,params[e->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_ldarg_r8,params[e->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_ldarg_pp,params[e->id]->off);
                    else
                        emitOp(proc, IL_ldarg_p,params[e->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_ldarg_vt,params[e->id]->off);
                    emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_LDARGA_S:
        case Tok_LDARGA:
            {
                Q_ASSERT(curProc);
                DeclList params = curProc->decl->getParams();
                Q_ASSERT(e->id < params.size());
                emitOp(proc, IL_ldarga,params[e->id]->off);
            }
            break;
        case Tok_LDLOC_0:
        case Tok_LDLOC_1:
        case Tok_LDLOC_2:
        case Tok_LDLOC_3:
        case Tok_LDLOC_S:
        case Tok_LDLOC:
            {
                DeclList params = proc.decl->getLocals();
                Q_ASSERT(e->id < params.size());
                Type* t = deref(params[e->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                    emitOp(proc, IL_ldloc_i1,params[e->id]->off);
                    break;
                case Type::INT16:
                    emitOp(proc, IL_ldloc_i2,params[e->id]->off);
                    break;
                case Type::INT32:
                    emitOp(proc, IL_ldloc_i4,params[e->id]->off);
                    break;
                case Type::INT64:
                    emitOp(proc, IL_ldloc_i8,params[e->id]->off);
                    break;
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_ldloc_u1,params[e->id]->off);
                    break;
                case Type::UINT16:
                    emitOp(proc, IL_ldloc_u2,params[e->id]->off);
                    break;
                case Type::UINT32:
                    emitOp(proc, IL_ldloc_u4,params[e->id]->off);
                    break;
                case Type::UINT64:
                    emitOp(proc, IL_ldloc_u8,params[e->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_ldloc_r4,params[e->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_ldloc_r8,params[e->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_ldloc_pp,params[e->id]->off);
                    else
                        emitOp(proc, IL_ldloc_p,params[e->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_ldloc_vt,params[e->id]->off);
                    emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_LDLOCA_S:
        case Tok_LDLOCA:
            {
                DeclList locals = proc.decl->getLocals();
                Q_ASSERT(e->id < locals.size());
                emitOp(proc, IL_ldloca,locals[e->id]->off);
            }
            break;
        case Tok_LDIND_I1:
            emitOp(proc, IL_ldind_i1);
            break;
        case Tok_LDIND_I2:
            emitOp(proc, IL_ldind_i2);
            break;
        case Tok_LDIND_I4:
            emitOp(proc, IL_ldind_i4);
            break;
        case Tok_LDIND_I8:
            emitOp(proc, IL_ldind_i8);
            break;
        case Tok_LDIND_IP:
            emitOp(proc, IL_ldind_p);
            break;
        case Tok_LDIND_IPP:
            emitOp(proc, IL_ldind_pp);
            break;
        case Tok_LDIND_R4:
            emitOp(proc, IL_ldind_r4);
            break;
        case Tok_LDIND_R8:
            emitOp(proc, IL_ldind_r8);
            break;
        case Tok_LDIND_U1:
            emitOp(proc, IL_ldind_u1);
            break;
        case Tok_LDIND_U2:
            emitOp(proc, IL_ldind_u2);
            break;
        case Tok_LDIND_U4:
            emitOp(proc, IL_ldind_u2);
            break;
        case Tok_LDIND_U8:
            emitOp(proc, IL_ldind_u8);
            break;
        case Tok_LDIND:
            emitOp(proc, IL_ldind_vt,t->getByteSize(pointerWidth));
            break;
        case Tok_LDELEM_I1:
            emitOp(proc, IL_ldelem_i1);
            break;
        case Tok_LDELEM_I2:
            emitOp(proc, IL_ldelem_i2);
            break;
        case Tok_LDELEM_I4:
            emitOp(proc, IL_ldelem_i4);
            break;
        case Tok_LDELEM_I8:
            emitOp(proc, IL_ldelem_i8);
            break;
        case Tok_LDELEM_IP:
            emitOp(proc, IL_ldelem_p);
            break;
        case Tok_LDELEM_R4:
            emitOp(proc, IL_ldelem_r4);
            break;
        case Tok_LDELEM_R8:
            emitOp(proc, IL_ldelem_r8);
            break;
        case Tok_LDELEM_U1:
            emitOp(proc, IL_ldelem_u1);
            break;
        case Tok_LDELEM_U2:
            emitOp(proc, IL_ldelem_u2);
            break;
        case Tok_LDELEM_U4:
            emitOp(proc, IL_ldelem_u4);
            break;
        case Tok_LDELEM_U8:
            emitOp(proc, IL_ldelem_u8);
            break;
        case Tok_LDELEM:
            emitOp(proc, IL_ldelem_vt,t->getByteSize(pointerWidth));
            break;
        case Tok_LDELEMA:
            emitOp(proc, IL_ldelema);
            break;
        case Tok_LDFLD:
            switch(t->kind)
            {
            case Type::INT8:
                emitOp(proc, IL_ldfld_i1,e->d->f.off);
                break;
            case Type::INT16:
                emitOp(proc, IL_ldfld_i2,e->d->f.off);
                break;
            case Type::INT32:
                emitOp(proc, IL_ldfld_i4,e->d->f.off);
                break;
            case Type::INT64:
                emitOp(proc, IL_ldfld_i8,e->d->f.off);
                break;
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_ldfld_u1,e->d->f.off);
                break;
            case Type::UINT16:
                emitOp(proc, IL_ldfld_u2,e->d->f.off);
                break;
            case Type::UINT32:
                emitOp(proc, IL_ldfld_u4,e->d->f.off);
                break;
            case Type::UINT64:
                emitOp(proc, IL_ldfld_u8,e->d->f.off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_ldfld_r4,e->d->f.off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_ldfld_r8,e->d->f.off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(t->typebound)
                    emitOp(proc, IL_ldfld_pp,e->d->f.off);
                else
                    emitOp(proc, IL_ldfld_p,e->d->f.off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_ldfld_vt,e->d->f.off);
                emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_LDFLDA:
            emitOp(proc, IL_ldflda);
            break;
        case Tok_LDVAR:
            switch(t->kind)
            {
            case Type::INT8:
                emitOp(proc, IL_ldvar_i1,e->d->off);
                break;
            case Type::INT16:
                emitOp(proc, IL_ldvar_i2,e->d->off);
                break;
            case Type::INT32:
                emitOp(proc, IL_ldvar_i4,e->d->off);
                break;
            case Type::INT64:
                emitOp(proc, IL_ldvar_i8,e->d->off);
                break;
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_ldvar_u1,e->d->off);
                break;
            case Type::UINT16:
                emitOp(proc, IL_ldvar_u2,e->d->off);
                break;
            case Type::UINT32:
                emitOp(proc, IL_ldvar_u4,e->d->off);
                break;
            case Type::UINT64:
                emitOp(proc, IL_ldvar_u8,e->d->off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_ldvar_r4,e->d->off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_ldvar_r8,e->d->off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(t->typebound)
                    emitOp(proc, IL_ldvar_pp,e->d->off);
                else
                    emitOp(proc, IL_ldvar_p,e->d->off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_ldvar_vt,e->d->off);
                emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_LDVARA:
            emitOp(proc, IL_ldvara);
            break;
        case Tok_NEWOBJ:
            emitOp(proc,IL_alloc1, e->d->getType()->getByteSize(pointerWidth));
            break;
        case Tok_NEWARR:
            emitOp(proc,IL_allocN, e->d->getType()->getByteSize(pointerWidth)); // N is on stack
            break;
        case Tok_NOP:
        case Tok_CASTPTR:
            emitOp(proc,IL_nop);
            break;
        case Tok_DUP:
            emitOp(proc,IL_dup, e->getType()->getByteSize(pointerWidth));
            break;
        case Tok_CALL:
        case Tok_CALLI:
            {
                if( e->kind == Tok_CALL )
                    translateProc(e->d);
#if 0
                // TODO
                DeclList params = e->d->getParams();
                int len = 0;
                if( !params.isEmpty() )
                    len = params.last()->off + params.last()->getType()->getByteSize(pointerWidth);
                if( e->d->getType() )
                    len = qMax(len, (int)e->d->getType()->getByteSize(pointerWidth));
                len += AstModel::padding(len, 8);
#endif
                if(e->kind == Tok_CALL)
                    emitOp(proc, IL_call, findProc(e->d));
                else
                    emitOp(proc, IL_calli);
            }
            break;
        case Tok_IIF:
            {
                translateExprSeq(proc, e->e);
                const int iif = deref(e->lhs->getType())->isInt64() ?
                            emitOp(proc,IL_brfalse_i8) : emitOp(proc,IL_brfalse_i4);
                translateExprSeq(proc, e->next->e);
                const int end_of_then = emitOp(proc, IL_br);
                branch_here(proc,iif);
                translateExprSeq(proc, e->next->next->e);
                branch_here(proc,end_of_then);
                e = e->next->next;
            }
            break;
        case Tok_SIZEOF:
        case Tok_INITOBJ:
        case Tok_PTROFF:
        case Tok_NEWVLA:
        case Tok_ISINST:
        case Tok_CALLVI:
        case Tok_CALLVIRT:
        default:
            Q_ASSERT(false);
        }
        e = e->next;
    }
    return true;
}
