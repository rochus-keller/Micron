/*
* Copyright 2019-2025 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MilAstSerializer.h"
#include "MilRenderer.h"
#include "MilAst.h"
#include <QtDebug>
using namespace Mil;


static void renderProc( const Declaration* p, AbstractRenderer* r, AstSerializer::DbgInfo);

static void lineout(AbstractRenderer* r, const Mic::RowCol& pos, AstSerializer::DbgInfo dbi)
{
    if( dbi == AstSerializer::None || !pos.isValid())
        return;
    if( dbi == AstSerializer::RowsOnly )
        r->line(pos.d_row);
    else
        r->line(pos.packed());
}

static quint32 lineout(const Mic::RowCol& pos, AstSerializer::DbgInfo dbi)
{
    if( dbi == AstSerializer::None || !pos.isValid())
        return 0;
    if( dbi == AstSerializer::RowsOnly )
        return pos.d_row;
    else
        return pos.packed();
}

static Mil::Quali toQuali(Type* t)
{
    if( t )
        return t->toQuali();
    else
        return Quali("", "?");
}

static void renderType(const Declaration* d, AbstractRenderer* r, AstSerializer::DbgInfo dbi)
{
    Type* t = d->getType();
    Q_ASSERT(t);
    switch( t->kind )
    {
    case Type::Struct:
    case Type::Union:
    case Type::Object: {
            Quali base;
            if( t->getType() )
                base = t->getType()->toQuali();
            quint8 kind = 0;
            if( t->kind == Type::Struct )
                kind = Mil::EmiTypes::Struct;
            else if( t->kind == Type::Union )
                kind = Mil::EmiTypes::Union;
            else if( t->kind == Type::Object )
                kind = Mil::EmiTypes::Object;

            r->beginType(d->name,d->public_, kind, base);
            DeclList fields = t->getFieldList(false);
            foreach( Declaration* f, fields )
            {
                lineout(r, f->pos, dbi);
                r->addField(f->name,toQuali(f->getType()), f->public_, f->f.bw);
            }
            r->endType();
            DeclList methods = t->getMethodTable(false);
            foreach( Declaration* p, methods )
            {
                lineout(r, p->pos, dbi);
                renderProc(p, r, dbi);
            }
        } break;
    case Type::Proc:
        {
            Mil::ProcData proc;
            proc.name = d->name;
            proc.isPublic = d->public_;
            if( t->typebound )
                proc.kind = Mil::ProcData::MethType;
            else
                proc.kind = Mil::ProcData::ProcType;

            foreach( Declaration* sub, t->subs )
            {
                ProcData::Var param;
                param.name = sub->name;
                param.type = toQuali(sub->getType());
                param.line = lineout(sub->pos, dbi);
                proc.params.append(param);
            }

            if( t->getType() )
                proc.retType = toQuali(t->getType());
            r->addProcedure(proc);
        }
        break;
    case Type::NameRef:
        r->addType(d->name,d->public_,t->toQuali(),Mil::EmiTypes::Alias);
        break;
    case Type::Pointer:
        r->addType(d->name,d->public_,toQuali(t->getType()),Mil::EmiTypes::Pointer);
        break;
    case Type::Array:
        r->addType(d->name,d->public_,toQuali(t->getType()),Mil::EmiTypes::Array,t->len);
        break;
    case Type::Generic:
        r->addType(d->name,d->public_,Quali(), Mil::EmiTypes::Generic);
        break;
    default:
        Q_ASSERT(false);
    }
}

static void renderVar(const Declaration* v, AbstractRenderer* r)
{
    r->addVariable(toQuali(v->getType()),v->name, v->public_);
}

static void renderConst(const Declaration* v, AbstractRenderer* r)
{
    QVariant val;
    if( v->c )
        val = ConstrLiteral::toVariant(v->c, v->getType());
    r->addConst(toQuali(v->getType()),v->name,val);
}

static void renderPos(ProcData& proc, const Mic::RowCol& pos, quint32& line, AstSerializer::DbgInfo dbi)
{
    if( dbi == AstSerializer::None || !pos.isValid())
        return;
    const quint32 cur = (dbi == AstSerializer::RowsOnly ? pos.d_row : pos.packed() );
    if( cur != line )
    {
        line = cur;
        proc.body << ProcData::Op(IL_line, line);
    }
}

static void renderExprs(ProcData& proc, Expression* e, quint32& line, AstSerializer::DbgInfo dbi)
{
    while(e)
    {
        renderPos(proc, e->pos, line, dbi);
        switch(e->kind)
        {
        case IL_call:
        case IL_calli:
        case IL_callvi:
        case IL_castptr:
        case IL_initobj:
        case IL_isinst:
        case IL_ldelema:
        case IL_ldelem:
        case IL_ldind:
        case IL_ldproc:
        case IL_ldvar:
        case IL_ldvara:
        case IL_newobj:
        case IL_newvla:
        case IL_newarr:
        case IL_ptroff:
        case IL_sizeof: {
                Quali q = e->d->forwardToProc()->toQuali();
                proc.body << ProcData::Op(e->kind, QVariant::fromValue(q));
            } break;
        case IL_callvirt:
        case IL_ldfld:
        case IL_ldflda:
        case IL_ldmeth: {
                Trident td;
                td.second = e->d->forwardToProc()->name;
                td.first = e->d->outer->toQuali();
                proc.body << ProcData::Op(e->kind, QVariant::fromValue(td));
            } break;
        case IL_ldarg_s:
        case IL_ldarg:
        case IL_ldarga_s:
        case IL_ldarga:
        case IL_ldloc_s:
        case IL_ldloca_s:
        case IL_ldloca:
        case IL_ldloc:
            proc.body << ProcData::Op(e->kind, e->id);
            break;
        case IL_ldc_obj:
            proc.body << ProcData::Op(e->kind, ConstrLiteral::toVariant(e->c, e->getType()));
            break;
        case IL_ldstr:
            proc.body << ProcData::Op(e->kind, QByteArray(e->c->s));
            break;
        case IL_ldc_i4:
        case IL_ldc_i8:
        case IL_ldc_i4_s:
            proc.body << ProcData::Op(e->kind, e->i);
            break;
        case IL_ldc_r4:
        case IL_ldc_r8:
            proc.body << ProcData::Op(e->kind, e->f);
            break;
        case IL_iif: {
                proc.body << ProcData::Op(e->kind);

                Expression* if_ = e->e;
                Q_ASSERT(if_ && if_->kind == IL_if && if_->next->kind == IL_then &&
                         if_->next->next->kind == IL_else &&
                         if_->next->next->next == 0); // no IL_end

                Expression* then_ = if_->next;
                Expression* else_ = if_->next->next;

                renderExprs(proc, if_->e, line, dbi);
                proc.body << ProcData::Op(then_->kind);
                renderExprs(proc, then_->e, line, dbi);
                proc.body << ProcData::Op(else_->kind);
                renderExprs(proc, else_->e, line, dbi);
                proc.body << ProcData::Op(IL_end);
            } break;
        default:
            proc.body << ProcData::Op(e->kind);
            break;
        }

        e = e->next;
    }
}

static void renderStats(ProcData& proc, Statement* s, quint32& line, AstSerializer::DbgInfo dbi)
{
    while(s)
    {
        if( s->kind == Statement::ExprStat )
        {
            renderExprs(proc, s->e, line, dbi);
        }else
        {
            renderPos(proc, s->pos, line, dbi);
            switch( s->kind )
            {
            case IL_while:
                proc.body << ProcData::Op(IL_while);
                renderExprs(proc, s->e, line, dbi);
                proc.body << ProcData::Op(IL_do);
                renderStats(proc, s->body, line, dbi);
                proc.body << ProcData::Op(IL_end);
                break;
            case IL_repeat:
                proc.body << ProcData::Op(IL_repeat);
                renderStats(proc, s->body, line, dbi);
                proc.body << ProcData::Op(IL_until);
                renderExprs(proc, s->e, line, dbi);
                proc.body << ProcData::Op(IL_end);
                break;
            case IL_loop:
                proc.body << ProcData::Op(IL_loop);
                renderStats(proc, s->body, line, dbi);
                proc.body << ProcData::Op(IL_end);
                break;
            case IL_if:
                proc.body << ProcData::Op(IL_if);
                renderExprs(proc, s->e, line, dbi);
                proc.body << ProcData::Op(IL_then);
                renderStats(proc, s->body, line, dbi);
                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    proc.body << ProcData::Op(IL_else);
                    renderStats(proc, s->body, line, dbi);
                }
                proc.body << ProcData::Op(IL_end);
                break;
            case IL_switch:
                proc.body << ProcData::Op(IL_switch);
                renderExprs(proc, s->e, line, dbi);
                while( s->next && s->next->kind == IL_case )
                {
                    s = s->next;
                    CaseLabelList cll;
                    Expression* e = s->e;
                    while(e)
                    {
                        cll << e->i;
                        e = e->next;
                    }
                    proc.body << ProcData::Op(IL_case, QVariant::fromValue(cll));
                    renderStats(proc, s->body, line, dbi);
                }
                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    proc.body << ProcData::Op(IL_else);
                    renderStats(proc, s->body, line, dbi);
                }
                proc.body << ProcData::Op(IL_end);
                break;
            case IL_goto:
            case IL_label:
                proc.body << ProcData::Op(s->kind, s->name);
                break;
            case IL_line:
                // proc.body << ProcData::Op(s->kind, s->id);
                break;
            case IL_starg:
            case IL_starg_s:
            case IL_stloc:
            case IL_stloc_s:
                proc.body << ProcData::Op(s->kind, s->id);
                break;
            case IL_stelem:
            case IL_stind:
            case IL_stvar:
                proc.body << ProcData::Op(s->kind, QVariant::fromValue(s->d->toQuali()));
                break;
            case IL_stfld: {
                    Trident td;
                    td.second = s->d->name;
                    td.first = s->d->outer->toQuali();
                    proc.body << ProcData::Op(s->kind, QVariant::fromValue(td));
                } break;
            default:
                proc.body << ProcData::Op(s->kind);
                break;
            }
        }
        s = s->next;
    }
}

static void renderProc(const Declaration* p, AbstractRenderer* r, AstSerializer::DbgInfo dbi)
{
    p = p->forwardToProc();

    ProcData proc;
    proc.name = p->name;
    proc.isPublic = p->public_;
    if( p->typebound )
        proc.binding = p->outer->toQuali().second;

    if(p->pd )
        proc.endLine = lineout(p->pd->end, dbi);

    if( p->entryPoint )
        proc.kind = ProcData::ModuleInit;
    else if(p->extern_ )
        proc.kind = ProcData::Extern;
    else if(p->forward )
        proc.kind = ProcData::Forward;
    else if(p->inline_ )
        proc.kind = ProcData::Inline;
    else if(p->invar)
        proc.kind = ProcData::Invar;
    else
        proc.kind = ProcData::Normal;

    if( p->getType() )
        proc.retType = p->getType()->toQuali();

    Declaration* sub = p->subs;
    while(sub)
    {
        switch( sub->kind )
        {
        case Declaration::ParamDecl:{
                ProcData::Var param;
                param.name = sub->name;
                param.type = toQuali(sub->getType());
                param.line = lineout(sub->pos,dbi);
                proc.params.append(param);
            } break;
        case Declaration::LocalDecl: {
                ProcData::Var local;
                local.name = sub->name;
                local.type = toQuali(sub->getType());
                local.line = lineout(sub->pos,dbi);
                proc.locals.append(local);
            } break;
        }

        sub = sub->next;
    }

    quint32 line = 0;
    renderStats(proc, p->body, line, dbi);

    r->addProcedure(proc);
}

bool AstSerializer::render(AbstractRenderer* r, const Mil::Declaration* module, DbgInfo dbi)
{
    Q_ASSERT(r && module && module->kind == Declaration::Module);

    if( dbi != None && (module->md == 0 || module->md->source.isEmpty()) )
        qWarning() << "AstSerializer::render: trying to output debug info on a module without source information" <<
                      module->name;
    QString source;
    if( module->md == 0 || module->md->source.isEmpty() )
        dbi = None;
    else
        source = module->md->source;
    QByteArrayList metaParamNames;
    if( module->md != 0 && !module->md->metaParamNames.isEmpty() )
        metaParamNames = module->md->metaParamNames;

    lineout(r, module->pos, dbi);
    r->beginModule(module->name, source, metaParamNames);
    Declaration* sub = module->subs;
    while(sub)
    {
        lineout(r, sub->pos, dbi);
        switch(sub->kind)
        {
        case Declaration::TypeDecl:
            renderType(sub,r, dbi);
            break;
        case Declaration::VarDecl:
            renderVar(sub,r);
            break;
        case Declaration::Procedure:
            renderProc(sub, r, dbi);
            break;
        case Declaration::Import:
            r->addImport(sub->imported->name);
            break;
        case Declaration::ConstDecl:
            renderConst(sub,r);
            break;
        }
        sub = sub->next;
    }
    if( module->md && module->md->end.isValid() )
        lineout(r, module->md->end, dbi);
    r->endModule();
    return true;
}
