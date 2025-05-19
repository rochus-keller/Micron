/*
* Copyright 2019-2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MicMilLoader2.h"
#include "MicSymbol.h"
#include "MilOps.h"
#include "MilParser2.h"
#include "MilLexer.h"
#include "MilValidator.h"
#include <QtDebug>
using namespace Mic;
using namespace Mil;

class Lex : public Scanner2
{
public:
    Lexer lex;
    Token next()
    {
        return lex.nextToken();
    }

    Token peek(int offset)
    {
        return lex.peekToken(offset);
    }

    QString sourcePath() const
    {
        return lex.getSourcePath();
    }
};

static inline QByteArray format(const MilQuali& q )
{
    if( !q.first.isEmpty() )
        return q.first + "!" + q.second;
    else
        return q.second;
}

MilLoader2::MilLoader2()
{

}

Declaration* MilLoader2::loadFromFile(const QString& file)
{
    Lex lex;
    lex.lex.setStream(file);
    Parser2 p(&mdl, &lex, this);
    qDebug() << "**** parsing" << file;
    Declaration* module = 0;
    if( p.parseModule() ) // we only parse one module here
    {
        if( !p.errors.isEmpty() )
        {
            foreach( const Parser2::Error& e, p.errors )
                qCritical() << e.path << e.row << e.col << e.msg;
            p.errors.clear();
        }else
        {
            module = p.takeModule();
            qDebug() << "module" << module->name;
            Validator v(&mdl);
            if( !v.validate(module) )
            {
                foreach( const Validator::Error& e, v.errors )
                    qCritical() << e.where << e.pos.d_row << e.pos.d_col << e.pc << e.msg;
                v.errors.clear();
                delete module;
                module = 0;
            }
            if( module && !mdl.addModule(module) )
            {
                delete module;
                module = 0;
            }
        }
    }
    return 0;
}

static void visitImports(AstModel* loader, Declaration* top, QList<Declaration*>& res )
{
    Declaration* sub = top->subs;
    while(sub)
    {
        if( sub->kind == Declaration::Import && !res.contains(sub->imported) )
        {
            res.append(sub->imported);
            visitImports(loader, sub->imported, res);
        }
        sub = sub->next;
    }
}

QList<Declaration*> MilLoader2::getModulesInDependencyOrder()
{
    QList<Declaration*> res;
    DeclList modules = mdl.getModules();

    for( int i = 0; i < modules.size(); i++ )
    {
        visitImports(&mdl, modules[i], res);
        if( !res.contains(modules[i]) )
            res << modules[i];
    }
    return res;
}

InMemRenderer2::InMemRenderer2(MilLoader2* loader):loader(loader), module(0), type(0), curProc(0)
{
    Q_ASSERT( loader );
}

InMemRenderer2::~InMemRenderer2()
{
    if( module )
        delete module;
}

void InMemRenderer2::commit()
{
    if( hasError )
        delete module;
    else
    {
        foreach( Type* t, unresolved)
        {
            if( t->getType() == 0 && t->quali )
            {
                Declaration* d = resolve(*t->quali);
                if( d && d->kind != Declaration::TypeDecl )
                    qCritical() << "the reference is no type declaration";
                else if( d )
                    t->setType(d->getType());
            }
        }
        unresolved.clear();

        Validator v(&loader->mdl);
#if 1
        if( !v.validate(module) )
        {
            foreach( const Validator::Error& e, v.errors )
                qCritical() << e.where << e.pos.d_row << e.pos.d_col << e.pc << e.msg;
            delete module;
        }else
#endif
            if( !loader->mdl.addModule(module) )
        {
            error(QString("cannot add module: %1").arg(module->name.constData()) );
            delete module;
        }
    }

    module = 0;
}

void InMemRenderer2::beginModule(const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& mp)
{
    Q_ASSERT(module == 0);
    hasError = false;
    module = new Declaration();
    module->kind = Declaration::Module;
    module->name = moduleName;
    // TODO module->metaParams = mp;
}

void InMemRenderer2::endModule()
{
    // NOP, see commit/rollback
}

void InMemRenderer2::addImport(const QByteArray& path)
{
    Q_ASSERT(module);

    Declaration* imported = loader->mdl.findModuleByName(path);
    if( imported == 0 )
        error(QString("cannot import: %1").arg(path.constData()));
    else
    {
        Declaration* import = new Declaration();
        import->kind = Declaration::Import;
        import->name = path;
        module->appendSub(import);
        import->imported = imported;
        import->outer = module;
    }
}

void InMemRenderer2::addVariable(const MilQuali& typeRef, QByteArray name,  bool isPublic)
{
    Q_ASSERT(module);
    Declaration* var = new Declaration();
    var->kind = Declaration::VarDecl;
    var->name = name;
    var->public_ = isPublic;
    var->outer = module;
    module->appendSub(var);
    var->setType( derefType(typeRef) );
}

void InMemRenderer2::addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val)
{
    Q_ASSERT(module);
    Declaration* co = new Declaration();
    co->kind = Declaration::ConstDecl;
    co->name = name;
    co->public_ = true;
    co->outer = module;
    module->appendSub(co);
    co->setType( derefType(typeRef) );
    co->c = new Constant();
    switch( val.type() )
    {
    case QVariant::Double:
        co->c->kind = Constant::D;
        co->c->d = val.toDouble();
        break;
    case QVariant::Int:
    case QVariant::LongLong:
        co->c->kind = Constant::I;
        co->c->i = val.toLongLong();
        break;
    case QVariant::ByteArray:
        if( co->getType()->kind == Type::ByteArrayLit )
        {
            co->c->kind = Constant::B;
            co->c->b = new ByteString(val.toByteArray());
        }else
        {
            co->c->kind = Constant::S;
            const QByteArray str = val.toByteArray();
            co->c->s = (char*)malloc(str.size()+1);
            strcpy(co->c->s, str.data() );
        }
        break;
    default:
        qWarning() << "TODO InMemRenderer2::addConst type not yet supported" << val.type();
        break;
    }
}

void InMemRenderer2::addProcedure(const Mic::MilProcedure& proc)
{
    Q_ASSERT(module);
    if( proc.kind == Mic::MilProcedure::ProcType || proc.kind == Mic::MilProcedure::MethType )
    {
        Type* t = new Type();
        t->kind = Type::Proc;
        if( proc.kind == Mic::MilProcedure::MethType )
            t->typebound = true;
        t->setType(derefType(proc.retType));

        Declaration* decl = new Declaration();
        decl->kind = Declaration::TypeDecl;
        decl->name = proc.name;
        decl->public_ = proc.isPublic;
        decl->outer = module;
        module->appendSub(decl);
        decl->setType( t );
        t->decl = decl;

        foreach( const MilVariable& param, proc.params )
        {
            Declaration* p = new Declaration();
            p->kind = Declaration::ParamDecl;
            p->name = param.name;
            t->subs.append(p);
            p->outer = decl;
            p->setType( derefType(param.type) );
        }
    }else
    {
        Declaration* decl = new Declaration();
        decl->kind = Declaration::Procedure;
        decl->name = proc.name;
        decl->public_ = proc.isPublic;
        decl->outer = module;
        curProc = decl;

        decl->setType( derefType(proc.retType));

        switch(proc.kind)
        {
        case Mic::MilProcedure::Forward:
            decl->forward = true;
            break;
        case Mic::MilProcedure::Extern:
            decl->extern_ = true;
            break;
        case Mic::MilProcedure::Inline:
            decl->inline_ = true;
            break;
        case Mic::MilProcedure::Invar:
            decl->invar = true;
            break;
        case Mic::MilProcedure::ModuleInit:
            decl->init = true;
            break;
        }

        foreach( const MilVariable& param, proc.params )
        {
            Declaration* p = new Declaration();
            p->kind = Declaration::ParamDecl;
            p->name = param.name;
            decl->appendSub(p);
            p->outer = decl;
            p->setType( derefType(param.type) );
        }

        foreach( const MilVariable& local, proc.locals )
        {
            Declaration* p = new Declaration();
            p->kind = Declaration::LocalDecl;
            p->name = local.name;
            decl->appendSub(p);
            p->outer = decl;
            p->setType( derefType(local.type) );
        }

        if( !proc.binding.isEmpty() )
        {
            Declaration* receiver = module->findSubByName(proc.binding);
            if( receiver == 0 || receiver->kind != Declaration::TypeDecl || receiver->getType()->kind != Type::Object )
            {
                error(QString("invalid receiver: %1").arg(proc.binding.constData()));
                delete decl;
            }else
            {
                Type* rt = receiver->getType();
                Declaration* forward = rt->findSubByName(decl->name, false);
                if( forward && forward->forward && forward->kind == Declaration::Procedure )
                {
                    forward->name.clear();
                    forward->forwardTo = decl;
                }else if( forward )
                    error(QString("duplicate name: %1").arg(decl->name.constData()));
                rt->subs.append(decl);
                decl->outer = receiver;
            }
        }else
        {
            Declaration* forward = module->findSubByName(decl->name);
            if( forward && forward->forward && forward->kind == Declaration::Procedure )
            {
                forward->name.clear();
                forward->forwardTo = decl;
            }else if( forward )
                error(QString("duplicate name: %1").arg(decl->name.constData()));
            module->appendSub(decl);
        }

        quint32 pc = 0;
        decl->body = translateStat(proc.body, pc);
        curProc = 0;
    }
}

void InMemRenderer2::beginType(const QByteArray& name, bool isPublic, quint8 typeKind, const MilQuali& super)
{
    Q_ASSERT(module);

    Declaration* decl = new Declaration();
    decl->kind = Declaration::TypeDecl;
    decl->name = name;
    decl->public_ = isPublic;
    decl->outer = module;
    module->appendSub(decl);

    type = new Type();
    type->decl = decl;
    decl->setType(type);

    switch(typeKind)
    {
    case MilEmitter::Struct:
        type->kind = Type::Struct;
        break;
    case MilEmitter::Union:
        type->kind = Type::Union;
        break;
    case MilEmitter::Object:
        type->kind = Type::Object;
        type->setType(derefType(super));
        if( type->getType() == 0 && !super.second.isEmpty() )
            error(QString("cannot resolve base type: %1").arg(format(super).constData()));
        break;
    default:
        Q_ASSERT(false);
    }
}

void InMemRenderer2::endType()
{
    Q_ASSERT(module);
    type = 0;
}

void InMemRenderer2::addType(const QByteArray& name, bool isPublic, const MilQuali& baseType, quint8 typeKind, quint32 len)
{
    Q_ASSERT(module);
    Q_ASSERT(type == 0);
    Q_ASSERT(typeKind == MilEmitter::Alias || typeKind == MilEmitter::Array || typeKind == MilEmitter::Pointer);

    Declaration* decl = new Declaration();
    decl->kind = Declaration::TypeDecl;
    decl->name = name;
    decl->public_ = isPublic;
    decl->outer = module;
    module->appendSub(decl);

    Type* t = new Type();
    t->decl = decl;
    decl->setType(t);
    switch( typeKind )
    {
    case MilEmitter::Alias:
        t->kind = Type::NameRef;
        t->quali = new MilQuali();
        *t->quali = baseType;
        break;
    default:
        t->kind = typeKind == MilEmitter::Pointer ? Type::Pointer : Type::Array;
        t->len = len;
        t->setType(derefType(baseType));
        if( t->getType() == 0 && !baseType.second.isEmpty() )
            error(QString("cannot resolve base type: %1").arg(format(baseType).constData()));
        break;
    }
}

void InMemRenderer2::addField(const QByteArray& fieldName, const MilQuali& typeRef, bool isPublic, quint8 bits)
{
    Q_ASSERT(module);
    Q_ASSERT(type);
    Declaration* field = new Declaration();
    field->kind = Declaration::Field;
    field->name = fieldName;
    field->public_ = isPublic;
    field->setType( derefType(typeRef) );
    field->f.bw = bits;
    field->outer = type->decl;
    type->subs.append(field);
}

Type*InMemRenderer2::derefType(const Quali& q) const
{
    if( q.first.isEmpty() && q.second.isEmpty() )
        return 0;
    Declaration* type = resolve(q);
    if( type && type->kind != Declaration::TypeDecl )
    {
        error(QString("not a type declaration: %1").arg(type->toPath().constData()));
        return 0;
    }else if( type == 0 )
    {
        Type* ref = new Type();
        ref->kind = Type::NameRef;
        ref->quali = new Quali();
        *ref->quali = q;
        unresolved << ref;
        return ref;
    }else
        return type->getType();
}

Statement* InMemRenderer2::translateStat(const QList<MilOperation>& ops, quint32& pc)
{
    Statement* res = 0;
    while( pc < ops.size() )
    {
        const IL_op op = (IL_op)ops[pc].op;

        switch(op)
        {
        case IL_case:
        case IL_do:
        case IL_until:
        case IL_then:
        case IL_else:
        case IL_end:
            return res;
        }

        Statement* tmp = new Statement();
        tmp->kind = op;
        if( res )
            res->append(tmp);
        else
            res = tmp;

        if( op > IL_EXPRESSIONS && op < IL_STATEMENTS )
        {
            tmp->kind = (IL_op)Statement::ExprStat;
            tmp->e = translateExpr(ops, pc);
            continue;
        }

        switch(op)
        {
        case IL_while: {
                pc++;
                tmp->e = translateExpr(ops, pc);
                if( !expect(ops, pc, IL_do) )
                    return res;
                pc++;
                tmp->body = translateStat(ops, pc);
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_repeat: {
                pc++;
                tmp->body = translateStat(ops, pc);
                if( !expect(ops, pc, IL_until) )
                    return res;
                pc++;
                tmp->e = translateExpr(ops, pc);
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_loop: {
                pc++;
                tmp->body = translateStat(ops, pc);
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_if: {
                pc++;
                tmp->e = translateExpr(ops, pc);
                if( !expect(ops, pc, IL_then) )
                    return res;
                pc++;
                tmp->body = translateStat(ops, pc);
                if( pc < ops.size() && ops[pc].op == IL_else )
                {
                    expect(ops, pc, IL_else);
                    tmp = new Statement();
                    tmp->kind = IL_else;
                    res->append(tmp);
                    pc++;
                    tmp->body = translateStat(ops, pc);
                }
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_switch: {
                pc++;
                tmp->e = translateExpr(ops, pc);
                while( pc < ops.size() && ops[pc].op == IL_case )
                {
                    expect(ops, pc, IL_case);
                    CaseLabelList cll = ops[pc].arg.value<CaseLabelList>();
                    tmp = new Statement();
                    res->append(tmp);
                    tmp->kind = IL_case;
                    if( cll.isEmpty() )
                    {
                        error("empty case label list", pc);
                        return res;
                    }
                    tmp->e = new Expression();
                    tmp->e->kind = IL_case;
                    tmp->e->i = cll.first();
                    for( int i = 1; i < cll.size(); i++ )
                    {
                        Expression* e = new Expression();
                        e->kind = IL_case;
                        e->i = cll[i];
                        tmp->e->append(e);
                    }
                    pc++;
                    if( !expect(ops, pc, IL_then) )
                        return res;
                    pc++;
                    tmp->body = translateStat(ops, pc);
                    if( pc < ops.size() && ops[pc].op == IL_else )
                    {
                        expect(ops, pc, IL_else);
                        tmp = new Statement();
                        tmp->kind = IL_else;
                        res->append(tmp);
                        tmp->body = translateStat(ops, pc);
                    }
                    if( !expect(ops, pc, IL_end) )
                        return res;
                }
            }
            break;
        case IL_goto:
            tmp->name = ops[pc].arg.toByteArray();
            break;
        case IL_label:
            tmp->name = ops[pc].arg.toByteArray();
            break;
        case IL_line:
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_starg:
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_starg_s:
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_stelem:
            tmp->d = resolve(ops[pc].arg.value<MilQuali>());
            if( tmp->d == 0 || tmp->d->kind != Declaration::TypeDecl)
            {
                error("invalid type declaration reference",pc);
                return res;
            }
            break;
        case IL_stfld: {
                MilTrident td = ops[pc].arg.value<MilTrident>();
                Declaration* d = derefTrident(td);
                if( d == 0 || d->kind != Declaration::Field)
                {
                    error("invalid field declaration reference",pc);
                    return res;
                }
            } break;
        case IL_stind:
            tmp->d = resolve(ops[pc].arg.value<MilQuali>());
            if( tmp->d == 0 || tmp->d->kind != Declaration::TypeDecl)
            {
                error("invalid type declaration reference", pc);
                return res;
            }
            break;
        case IL_stloc:
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_stloc_s:
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_stvar:
            tmp->d = resolve(ops[pc].arg.value<MilQuali>());
            if( tmp->d == 0 || tmp->d->kind != Declaration::VarDecl)
            {
                error("invalid variable declaration reference",pc);
                return res;
            }
            break;
        case IL_stloc_1:
            tmp->id = 1;
            break;
        case IL_stloc_2:
            tmp->id = 2;
            break;
        case IL_stloc_3:
            tmp->id = 3;
            break;
        case IL_stloc_0:
        case IL_stind_i1:
        case IL_stind_i2:
        case IL_stind_i4:
        case IL_stind_i8:
        case IL_stind_r4:
        case IL_stind_r8:
        case IL_stind_ip:
        case IL_stind_ipp:
        case IL_stelem_i1:
        case IL_stelem_i2:
        case IL_stelem_i4:
        case IL_stelem_i8:
        case IL_stelem_r4:
        case IL_stelem_r8:
        case IL_stelem_ip:
        case IL_stelem_ipp:
        case IL_pop:
        case IL_ret:
        case IL_strcpy:
        case IL_free:
        case IL_exit:
            break;
        default:
            error(QString("unexpected operation '%1'").arg(s_opName[ops[pc].op]), pc);
            return res;
        }
        pc++;
    }
    return res;
}

Expression* InMemRenderer2::translateExpr(const QList<MilOperation>& ops, quint32& pc)
{
    Expression* res = 0;
    while(pc < ops.size() && isExprOp((IL_op)ops[pc].op) )
    {
        Expression* tmp = new Expression();
        tmp->kind = (IL_op)ops[pc].op;

        if( res )
            res->append(tmp);
        else
            res = tmp;

        switch(ops[pc].op)
        {
        case IL_iif: {
                pc++;
                tmp->e = translateExpr(ops, pc);
                if( !expect(ops, pc, IL_then) )
                    return res;
                pc++;
                Expression* res2 = new Expression();
                res2->kind = IL_then;
                res2->e = translateExpr(ops, pc);
                tmp->next = res2;

                if( !expect(ops, pc, IL_else) )
                    return res;
                pc++;
                Expression* res3 = new Expression();
                res3->kind = IL_else;
                res3->e = translateExpr(ops, pc);
                res2->next = res3;

                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
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
                MilQuali q = ops[pc].arg.value<MilQuali>();
                tmp->d = resolve(q);
                if(tmp->d == 0)
                {
                    error(QString("cannot resolve qualident: %1").arg(format(q).constData()),pc++);
                    return res;
                }
            } break;
        case IL_callvirt:
        case IL_ldfld:
        case IL_ldflda:
        case IL_ldmeth: {
                MilTrident td = ops[pc].arg.value<MilTrident>();
                tmp->d = derefTrident(td);
                if(tmp->d == 0 )
                {
                    error(QString("cannot resolve trident: %1.%2").arg(format(td.first).constData()).
                          arg(td.second.constData()), pc++);
                    return res;
                }
            } break;
        case IL_ldarg_s:
        case IL_ldarg:
        case IL_ldarga_s:
        case IL_ldarga:
        case IL_ldloc_s:
        case IL_ldloca_s:
        case IL_ldloca:
        case IL_ldloc:
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_ldobj:
            break; // MilObject TODO
        case IL_ldstr: {
            Constant* c = new Constant();
            c->kind = Constant::S;
            const QByteArray str = ops[pc].arg.toByteArray();
            c->s = (char*)malloc( str.size() + 1);
            strcpy(c->s, str.constData());
            tmp->c = c;
            }
            break;
        case IL_ldc_i4:
        case IL_ldc_i8:
        case IL_ldc_i4_s:
            tmp->i = ops[pc].arg.toLongLong();
            break;
        case IL_ldc_r4:
        case IL_ldc_r8:
            tmp->f = ops[pc].arg.toDouble();
            break;
        case IL_ldarg_1:
            tmp->id = 1;
            break;
        case IL_ldarg_2:
            tmp->id = 2;
            break;
        case IL_ldarg_3:
            tmp->id = 3;
            break;
        case IL_ldc_i4_1:
            tmp->i = 1;
            break;
        case IL_ldloc_1:
            tmp->id = 1;
            break;
        case IL_ldloc_2:
            tmp->id = 2;
            break;
        case IL_ldloc_3:
            tmp->id = 3;
            break;
        case IL_ldc_i4_2:
            tmp->i = 2;
            break;
        case IL_ldc_i4_3:
            tmp->i = 3;
            break;
        case IL_ldc_i4_4:
            tmp->i = 4;
            break;
        case IL_ldc_i4_5:
            tmp->i = 5;
            break;
        case IL_ldc_i4_6:
            tmp->i = 6;
            break;
        case IL_ldc_i4_7:
            tmp->i = 7;
            break;
        case IL_ldc_i4_8:
            tmp->i = 8;
            break;
        case IL_ldc_i4_m1:
            tmp->i = -1;
            break;
        }

        pc++;
    }
    return res;
}

bool InMemRenderer2::expect(const QList<MilOperation>& ops, quint32& pc, int op)
{
    // pc is not changed in here!
    if( pc >= ops.size() || ops[pc].op != op )
    {
        error(QString("expecting '%1', instead got '%2'").arg(s_opName[op]).arg(s_opName[ops[pc].op]), pc);
        return false;
    }else
        return true;
}

Declaration*InMemRenderer2::derefTrident(const MilTrident& td) const
{
    Declaration* d = resolve(td.first);
    if( d == 0 || d->kind != Declaration::TypeDecl || d->getType() == 0)
        return 0;
    d = d->getType()->findSubByName(td.second);
    return d;
}

Declaration*InMemRenderer2::resolve(const Quali& q) const
{
    if( q.first.isEmpty() )
    {
        Declaration* res = module->findSubByName(q.second);
        if( res == 0 )
            res = loader->mdl.getGlobals()->findSubByName(q.second);
        return res;
    }else
        return loader->mdl.resolve(q);
}

void InMemRenderer2::error(const QString& msg, int pc) const
{
    QByteArray where;
    if( curProc )
        where = curProc->toPath();
    else if( module )
        where = module->toPath();
    if( pc >= 0 )
        where += " pc " + QByteArray::number(pc);
    qCritical() << where.constData() << msg.toUtf8().constData();
    hasError = true;
}

static void renderType(const Declaration* d, MilRenderer* r)
{
    Type* t = d->getType();
    switch( t->kind )
    {
    case Type::Struct:
    case Type::Union:
    case Type::Object: {
            MilQuali base;
            if( t->getType() )
                base = t->getType()->toQuali();
            quint8 kind = 0;
            if( t->kind == Type::Struct )
                kind = MilEmitter::Struct;
            else if( t->kind == Type::Union )
                kind = MilEmitter::Union;
            else if( t->kind == Type::Object )
                kind = MilEmitter::Object;

            r->beginType(d->name,d->public_, kind, base);
            DeclList fields = t->getFieldList(false);
            foreach( Declaration* f, fields )
                r->addField(f->name,f->getType()->toQuali(), f->public_, f->f.bw);
            r->endType();
            DeclList methods = t->getMethodTable(false);
            foreach( Declaration* p, methods )
                ; // TODO r->addProcedure(p);
            } break;
    case Type::Proc:
        {
            Mic::MilProcedure proc;
            proc.name = d->name;
            proc.isPublic = d->public_;
            if( t->typebound )
                proc.kind = Mic::MilProcedure::MethType;
            else
                proc.kind = Mic::MilProcedure::ProcType;

            foreach( Declaration* sub, t->subs )
            {
                MilVariable param;
                param.name = sub->name;
                param.type = sub->getType()->toQuali();
                proc.params.append(param);
            }

            if( t->getType() )
                proc.retType = t->getType()->toQuali();
            r->addProcedure(proc);
        }
        break;
    case Type::NameRef:
        r->addType(d->name,d->public_,t->toQuali(),MilEmitter::Alias);
        break;
    case Type::Pointer:
        r->addType(d->name,d->public_,t->getType()->toQuali(),MilEmitter::Pointer);
        break;
    case Type::Array:
        r->addType(d->name,d->public_,t->getType()->toQuali(),MilEmitter::Array,t->len);
        break;
    default:
        Q_ASSERT(false);
    }
}

static void renderVar(const Declaration* v, MilRenderer* r)
{
    r->addVariable(v->getType()->toQuali(),v->name, v->public_);
}

static void renderConst(const Declaration* v, MilRenderer* r)
{
    r->addConst(v->getType()->toQuali(),v->name,v->c->toVariant()); // TODO: handle constructors
}

static void renderExprs(MilProcedure& proc, Expression* e)
{
    while(e)
    {
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
                MilQuali q = e->d->toQuali();
                proc.body << MilOperation(e->kind, QVariant::fromValue(q));
            } break;
        case IL_callvirt:
        case IL_ldfld:
        case IL_ldflda:
        case IL_ldmeth: {
                MilTrident td;
                td.second = e->d->name;
                td.first = e->d->outer->toQuali();
                proc.body << MilOperation(e->kind, QVariant::fromValue(td));
            } break;
        case IL_ldarg_s:
        case IL_ldarg:
        case IL_ldarga_s:
        case IL_ldarga:
        case IL_ldloc_s:
        case IL_ldloca_s:
        case IL_ldloca:
        case IL_ldloc:
            proc.body << MilOperation(e->kind, e->id);
            break;
        case IL_ldobj:
            proc.body << MilOperation(e->kind);
            break; // MilObject TODO
        case IL_ldstr:
            proc.body << MilOperation(e->kind, e->c->s);
            break;
        case IL_ldc_i4:
        case IL_ldc_i8:
        case IL_ldc_i4_s:
            proc.body << MilOperation(e->kind, e->i);
            break;
        case IL_ldc_r4:
        case IL_ldc_r8:
            proc.body << MilOperation(e->kind, e->f);
            break;
        case IL_iif:
            proc.body << MilOperation(e->kind);
            renderExprs(proc, e->e);
            Q_ASSERT(e->next && e->next->kind == IL_then);
            e = e->next;
            proc.body << MilOperation(e->kind);
            renderExprs(proc, e->e);
            Q_ASSERT(e->next && e->next->kind == IL_else);
            e = e->next;
            proc.body << MilOperation(e->kind);
            renderExprs(proc, e->e);
            proc.body << MilOperation(IL_end);
           break;
        default:
            proc.body << MilOperation(e->kind);
            break;
        }

        e = e->next;
    }
}

static void renderStats(MilProcedure& proc, Statement* s)
{
    while(s)
    {
        if( s->kind == Statement::ExprStat )
        {
            renderExprs(proc, s->e);
        }else
        {
            switch( s->kind )
            {
            case IL_while:
                proc.body << MilOperation(IL_while);
                renderExprs(proc, s->e);
                proc.body << MilOperation(IL_do);
                renderStats(proc, s->body);
                proc.body << MilOperation(IL_end);
                break;
            case IL_repeat:
                proc.body << MilOperation(IL_repeat);
                renderStats(proc, s->body);
                proc.body << MilOperation(IL_until);
                renderExprs(proc, s->e);
                proc.body << MilOperation(IL_end);
                break;
            case IL_loop:
                proc.body << MilOperation(IL_loop);
                renderStats(proc, s->body);
                proc.body << MilOperation(IL_end);
                break;
            case IL_if:
                proc.body << MilOperation(IL_if);
                renderExprs(proc, s->e);
                proc.body << MilOperation(IL_then);
                renderStats(proc, s->body);
                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    proc.body << MilOperation(IL_else);
                    renderStats(proc, s->body);
                }
                proc.body << MilOperation(IL_end);
                break;
            case IL_switch:
                proc.body << MilOperation(IL_repeat);
                renderExprs(proc, s->e);
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
                    proc.body << MilOperation(IL_case, QVariant::fromValue(cll));
                    renderStats(proc, s->body);
                }
                if( s->next && s->next->kind == IL_else )
                {
                    s = s->next;
                    proc.body << MilOperation(IL_else);
                    renderStats(proc, s->body);
                }
                proc.body << MilOperation(IL_end);
                break;
            case IL_goto:
            case IL_label:
                proc.body << MilOperation(s->kind, s->name);
                break;
            case IL_line:
            case IL_starg:
            case IL_starg_s:
            case IL_stloc:
            case IL_stloc_s:
                proc.body << MilOperation(s->kind, s->id);
                break;
            case IL_stelem:
            case IL_stind:
            case IL_stvar:
                proc.body << MilOperation(s->kind, QVariant::fromValue(s->d->toQuali()));
                break;
            case IL_stfld: {
                    MilTrident td;
                    td.second = s->d->name;
                    td.first = s->d->outer->toQuali();
                    proc.body << MilOperation(s->kind, QVariant::fromValue(td));
                } break;
            default:
                proc.body << MilOperation(s->kind);
                break;
            }
        }
        s = s->next;
    }
}

static void renderProc( const Declaration* p, MilRenderer* r)
{
    MilProcedure proc;
    proc.name = p->name;
    proc.isPublic = p->public_;
    // TODO proc.isVararg

    if( p->init )
        proc.kind = MilProcedure::ModuleInit;
    else if(p->extern_ )
        proc.kind = MilProcedure::Extern;
    else if(p->forward )
        proc.kind = MilProcedure::Forward;
    else if(p->inline_ )
        proc.kind = MilProcedure::Inline;
    else if(p->invar)
        proc.kind = MilProcedure::Invar;
    else
        proc.kind = MilProcedure::Normal;

    if( p->getType() )
        proc.retType = p->getType()->toQuali();

    Declaration* sub = p->subs;
    while(sub)
    {
        switch( sub->kind )
        {
        case Declaration::ParamDecl:{
                MilVariable param;
                param.name = sub->name;
                param.type = sub->getType()->toQuali();
                proc.params.append(param);
            } break;
        case Declaration::LocalDecl: {
                MilVariable local;
                local.name = sub->name;
                local.type = sub->getType()->toQuali();
                proc.locals.append(local);
            } break;
        }

        sub = sub->next;
    }

    renderStats(proc, p->body);

    r->addProcedure(proc);
}

bool MilLoader2::render(MilRenderer* r, const Mil::Declaration* module)
{
    Q_ASSERT(r && module);

    r->beginModule(module->name,"", QByteArrayList());
    Declaration* sub = module->subs;
    while(sub)
    {
        switch(sub->kind)
        {
        case Declaration::TypeDecl:
            renderType(sub,r);
            break;
        case Declaration::VarDecl:
            renderVar(sub,r);
            break;
        case Declaration::Procedure:
            renderProc(sub, r);
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
    r->endModule();
    return true;
}

Declaration*MilLoader2::loadModule(const Import& imp)
{
    // TODO in future parse and load mil file if not already here, using library search paths
    return mdl.findModuleByName(imp.moduleName);
}
