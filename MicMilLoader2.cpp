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
#include "MicMilOp.h"
#include <QtDebug>
using namespace Mic;
using namespace Mil;

MilLoader2::MilLoader2()
{

}

static void visitImports(AstModel* loader, Declaration* top, QList<Declaration*>& res )
{
    Declaration* sub = top->subs;
    while(sub)
    {
        if( res.contains(sub) )
            continue;
        visitImports(loader, sub, res);
        res.append(sub);
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

InMemRenderer2::InMemRenderer2(MilLoader2* loader):loader(loader), module(0), type(0)
{
    Q_ASSERT( loader );
}

void InMemRenderer2::beginModule(const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& mp)
{
    module = new Declaration();
    module->kind = Declaration::Module;
    if( !loader->mdl.addModule(module) )
        qWarning() << "cannot add module" << moduleName;
    module->name = moduleName;
    // TODO module->metaParams = mp;
}

void InMemRenderer2::endModule()
{
    if( module && loader->mdl.findModuleByName(module->name) != module )
        delete module;
    module = 0;
}

void InMemRenderer2::addImport(const QByteArray& path)
{
    Q_ASSERT(module);
    Declaration* import = new Declaration();
    import->kind = Declaration::Import;
    import->name = path;
    module->appendSub(import);
    import->imported = loader->mdl.findModuleByName(path);
    import->outer = module;
    if( import->imported == 0 )
        qWarning() << "cannot import" << path;
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

void InMemRenderer2::addProcedure(const Mic::MilProcedure& method)
{
    Q_ASSERT(module);
    if( method.kind == Mic::MilProcedure::ProcType || method.kind == Mic::MilProcedure::MethType )
    {
        Type* t = new Type();
        t->kind = Type::Proc;
        if( method.kind == Mic::MilProcedure::MethType )
            t->typebound = true;
        t->setType(derefType(method.retType));

        Declaration* decl = new Declaration();
        decl->kind = Declaration::TypeDecl;
        decl->name = method.name;
        decl->public_ = method.isPublic;
        decl->outer = module;
        module->appendSub(decl);
        decl->setType( t );
        t->decl = decl;

        foreach( const MilVariable& param, method.params )
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
        decl->name = method.name;
        decl->public_ = method.isPublic;
        decl->outer = module;

        foreach( const MilVariable& param, method.params )
        {
            Declaration* p = new Declaration();
            p->kind = Declaration::ParamDecl;
            p->name = param.name;
            decl->appendSub(p);
            p->outer = decl;
            p->setType( derefType(param.type) );
        }

        foreach( const MilVariable& local, method.locals )
        {
            Declaration* p = new Declaration();
            p->kind = Declaration::LocalDecl;
            p->name = local.name;
            decl->appendSub(p);
            p->outer = decl;
            p->setType( derefType(local.type) );
        }

        quint32 pc = 0;
        decl->body = translateStat(method.body, pc);

        if( !method.binding.isEmpty() )
        {
            Declaration* receiver = module->findSubByName(method.binding);
            if( receiver == 0 || receiver->kind != Declaration::TypeDecl || receiver->getType()->kind != Type::Object )
            {
                qCritical() << "InMemRenderer2::addProcedure: invalid receiver:" << method.binding;
                delete decl;
            }else
            {
                receiver->getType()->subs.append(decl);
                decl->outer = receiver;
            }
        }else
            module->appendSub(decl);
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
            qCritical() << "cannot resolve base type" << super;
        break;
    case MilEmitter::ProcType:
        type->kind = Type::Proc;
        break;
    case MilEmitter::MethType:
        type->kind = Type::Proc;
        type->typebound = true;
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
        t->kind = Type::Array;
        t->len = len;
        t->setType(derefType(baseType));
        if( t->getType() == 0 && !baseType.second.isEmpty() )
            qCritical() << "cannot resolve base type" << baseType;
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
    Declaration* type = loader->mdl.resolve(q);
    if( type && type->kind != Declaration::TypeDecl )
    {
        qWarning() << "not a type declaration:" << type->toPath();
        return 0;
    }else if( type == 0 )
    {
        Type* ref = new Type();
        ref->kind = Type::NameRef;
        ref->quali = new Quali();
        *ref->quali = q;
        return ref;
    }else
        return type->getType();
}

static inline TokenType opToTok(int op)
{
    switch( op )
    {
    case IL_add:
        return Tok_ADD;
    case IL_abs:
        return Tok_ABS;
    case IL_and:
        return Tok_AND;
    case IL_call:
        return Tok_CALL;
    case IL_calli:
        return Tok_CALLI;
    case IL_callvi:
        return Tok_CALLVI;
    case IL_callvirt:
        return Tok_CALLVIRT;
    case IL_castptr:
        return Tok_CASTPTR;
    case IL_ceq:
        return Tok_CEQ;
    case IL_cgt:
        return Tok_CGT;
    case IL_cgt_un:
        return Tok_CGT_UN;
    case IL_clt:
        return Tok_CLT;
    case IL_clt_un:
        return Tok_CLT_UN;
    case IL_conv_i1:
        return Tok_CONV_I1;
    case IL_conv_i2:
        return Tok_CONV_I2;
    case IL_conv_i4:
        return Tok_CONV_I4;
    case IL_conv_i8:
        return Tok_CONV_I8;
    case IL_conv_r4:
        return Tok_CONV_R4;
    case IL_conv_r8:
        return Tok_CONV_R8;
    case IL_conv_u1:
        return Tok_CONV_U1;
    case IL_conv_u2:
        return Tok_CONV_U2;
    case IL_conv_u4:
        return Tok_CONV_U4;
    case IL_conv_u8:
        return Tok_CONV_U8;
    case IL_div:
        return Tok_DIV;
    case IL_div_un:
        return Tok_DIV_UN;
    case IL_dup:
        return Tok_DUP;
    case IL_iif:
        return Tok_IIF;
    case IL_initobj:
        return Tok_INITOBJ;
    case IL_isinst:
        return Tok_ISINST;
    case IL_ldarg:
        return Tok_LDARG;
    case IL_ldarg_s:
        return Tok_LDARG_S;
    case IL_ldarg_0:
        return Tok_LDARG_0;
    case IL_ldarg_1:
        return Tok_LDARG_1;
    case IL_ldarg_2:
        return Tok_LDARG_2;
    case IL_ldarg_3:
        return Tok_LDARG_3;
    case IL_ldarga:
        return Tok_LDARGA;
    case IL_ldarga_s:
        return Tok_LDARGA_S;
    case IL_ldc_i4:
        return Tok_LDC_I4;
    case IL_ldc_i8:
        return Tok_LDC_I8;
    case IL_ldc_i4_s:
        return Tok_LDC_I4_S;
    case IL_ldc_r4:
        return Tok_LDC_R4;
    case IL_ldc_r8:
        return Tok_LDC_R8;
    case IL_ldc_i4_0:
        return Tok_LDC_I4_0;
    case IL_ldc_i4_1:
        return Tok_LDC_I4_1;
    case IL_ldc_i4_2:
        return Tok_LDC_I4_2;
    case IL_ldc_i4_3:
        return Tok_LDC_I4_3;
    case IL_ldc_i4_4:
        return Tok_LDC_I4_4;
    case IL_ldc_i4_5:
        return Tok_LDC_I4_5;
    case IL_ldc_i4_6:
        return Tok_LDC_I4_6;
    case IL_ldc_i4_7:
        return Tok_LDC_I4_7;
    case IL_ldc_i4_8:
        return Tok_LDC_I4_8;
    case IL_ldc_i4_m1:
        return Tok_LDC_I4_M1;
    case IL_ldobj:
        return Tok_LDOBJ;
    case IL_ldelem:
        return Tok_LDELEM;
    case IL_ldelema:
        return Tok_LDELEMA;
    case IL_ldelem_i1:
        return Tok_LDELEM_I1;
    case IL_ldelem_i2:
        return Tok_LDELEM_I2;
    case IL_ldelem_i4:
        return Tok_LDELEM_I4;
    case IL_ldelem_i8:
        return Tok_LDELEM_I8;
    case IL_ldelem_u1:
        return Tok_LDELEM_U1;
    case IL_ldelem_u2:
        return Tok_LDELEM_U2;
    case IL_ldelem_u4:
        return Tok_LDELEM_U4;
    case IL_ldelem_u8:
        return Tok_LDELEM_U8;
    case IL_ldelem_r4:
        return Tok_LDELEM_R4;
    case IL_ldelem_r8:
        return Tok_LDELEM_R8;
    case IL_ldelem_ip:
        return Tok_LDELEM_IP;
    case IL_ldelem_ipp:
        return Tok_LDELEM_IPP;
    case IL_ldfld:
        return Tok_LDFLD;
    case IL_ldflda:
        return Tok_LDFLDA;
    case IL_ldind_i1:
        return Tok_LDIND_I1;
    case IL_ldind_i2:
        return Tok_LDIND_I2;
    case IL_ldind_i4:
        return Tok_LDIND_I4;
    case IL_ldind_i8:
        return Tok_LDIND_I8;
    case IL_ldind_u1:
        return Tok_LDIND_U1;
    case IL_ldind_u2:
        return Tok_LDIND_U2;
    case IL_ldind_u4:
        return Tok_LDIND_U4;
    case IL_ldind_r4:
        return Tok_LDIND_R4;
    case IL_ldind_u8:
        return Tok_LDIND_U8;
    case IL_ldind_r8:
        return Tok_LDIND_R8;
    case IL_ldind_ip:
        return Tok_LDIND_IP;
    case IL_ldind_ipp:
        return Tok_LDIND_IPP;
    case IL_ldloc:
        return Tok_LDLOC;
    case IL_ldloc_s:
        return Tok_LDLOC_S;
    case IL_ldloca:
        return Tok_LDLOCA;
    case IL_ldloca_s:
        return Tok_LDLOCA_S;
    case IL_ldloc_0:
        return Tok_LDLOC_0;
    case IL_ldloc_1:
        return Tok_LDLOC_1;
    case IL_ldloc_2:
        return Tok_LDLOC_2;
    case IL_ldloc_3:
        return Tok_LDLOC_3;
    case IL_ldnull:
        return Tok_LDNULL;
    case IL_ldind:
        return Tok_LDIND;
    case IL_ldproc:
        return Tok_LDPROC;
    case IL_ldmeth:
        return Tok_LDMETH;
    case IL_ldstr:
        return Tok_LDSTR;
    case IL_ldvar:
        return Tok_LDVAR;
    case IL_ldvara:
        return Tok_LDVARA;
    case IL_mul:
        return Tok_MUL;
    case IL_neg:
        return Tok_NEG;
    case IL_newarr:
        return Tok_NEWARR;
    case IL_newvla:
        return Tok_NEWVLA;
    case IL_newobj:
        return Tok_NEWOBJ;
    case IL_not:
        return Tok_NOT;
    case IL_or:
        return Tok_OR;
    case IL_rem:
        return Tok_OR;
    case IL_rem_un:
        return Tok_REM_UN;
    case IL_shl:
        return Tok_SHL;
    case IL_shr:
        return Tok_SHR;
    case IL_shr_un:
        return Tok_SHR_UN;
    case IL_sizeof:
        return Tok_SIZEOF;
    case IL_sub:
        return Tok_SUB;
    case IL_xor:
        return Tok_XOR;
    case IL_ptroff:
        return Tok_PTROFF;
    case IL_nop:
        return Tok_NOP;
    default:
        return Tok_Invalid;
    }
}

Statement* InMemRenderer2::translateStat(const QList<MilOperation>& ops, quint32& pc)
{
    Statement* res = 0;
    while( pc < ops.size() )
    {
        Statement* tmp = new Statement();
        if( res )
            res->append(tmp);
        else
            res = tmp;
        if( opToTok(ops[pc].op) != Tok_Invalid )
        {
            tmp->kind = (TokenType)Statement::ExprStat;
            tmp->e = translateExpr(ops, pc);
            continue;
        }
        switch(ops[pc].op)
        {
        case IL_while:
            {
                tmp->kind = Tok_WHILE;
                tmp->e = translateExpr(ops, pc);
                if( !expect(ops, pc, IL_do) )
                    return res;
                tmp->body = translateStat(ops, pc);
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_repeat:
            {
                tmp->kind = Tok_REPEAT;
                tmp->body = translateStat(ops, pc);
                if( !expect(ops, pc, IL_until) )
                    return res;
                tmp->e = translateExpr(ops, pc);
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_loop:
            {
                tmp->kind = Tok_LOOP;
                tmp->body = translateStat(ops, pc);
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_if:
            {
                tmp->kind = Tok_IF;
                tmp->e = translateExpr(ops, pc);
                if( !expect(ops, pc, IL_then) )
                    return res;
                tmp->body = translateStat(ops, pc);
                if( pc+1 < ops.size() && ops[pc+1].op == IL_else )
                {
                    expect(ops, pc, IL_else);
                    tmp = new Statement();
                    tmp->kind = Tok_ELSE;
                    res->append(tmp);
                    tmp->body = translateStat(ops, pc);
                }
                if( !expect(ops, pc, IL_end) )
                    return res;
            }
            break;
        case IL_switch:
            {
                tmp->kind = Tok_SWITCH;
                tmp->e = translateExpr(ops, pc);
                while( pc+1 < ops.size() && ops[pc+1].op == IL_case )
                {
                    expect(ops, pc, IL_case);
                    CaseLabelList cll = ops[pc].arg.value<CaseLabelList>();
                    tmp = new Statement();
                    res->append(tmp);
                    tmp->kind = Tok_CASE;
                    if( cll.isEmpty() )
                    {
                        qCritical() << "empty case label list" << pc;
                        return res;
                    }
                    tmp->e = new Expression();
                    tmp->e->kind = Tok_CASE;
                    tmp->e->i = cll.first();
                    for( int i = 1; i < cll.size(); i++ )
                    {
                        Expression* e = new Expression();
                        e->kind = Tok_CASE;
                        e->i = cll[i];
                        tmp->e->append(e);
                    }
                    if( !expect(ops, pc, IL_then) )
                        return res;
                    tmp->body = translateStat(ops, pc);
                    if( pc+1 < ops.size() && ops[pc+1].op == IL_else )
                    {
                        expect(ops, pc, IL_else);
                        tmp = new Statement();
                        tmp->kind = Tok_ELSE;
                        res->append(tmp);
                        tmp->body = translateStat(ops, pc);
                    }
                    if( !expect(ops, pc, IL_end) )
                        return res;
                }
            }
            break;
        case IL_end:
            return res;
        case IL_free:
            tmp->kind = Tok_FREE;
            break;
        case IL_exit:
            tmp->kind = Tok_EXIT;
            break;
        case IL_goto:
            tmp->kind = Tok_GOTO;
            tmp->name = ops[pc].arg.toByteArray();
            break;
        case IL_label:
            tmp->kind = Tok_LABEL;
            tmp->name = ops[pc].arg.toByteArray();
            break;
        case IL_line:
            tmp->kind = Tok_LINE;
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_pop:
            tmp->kind = Tok_POP;
            break;
        case IL_ret:
            tmp->kind = Tok_RET;
            break;
        case IL_starg:
            tmp->kind = Tok_STARG;
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_starg_s:
            tmp->kind = Tok_STARG_S;
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_stelem:
            tmp->kind = Tok_STELEM;
            tmp->d = loader->mdl.resolve(ops[pc].arg.value<MilQuali>());
            if( tmp->d == 0 || tmp->d->kind != Declaration::TypeDecl)
            {
                qCritical() << "invalid type declaration reference at pc" << pc;
                return res;
            }
            break;
        case IL_stelem_i1:
            tmp->kind = Tok_STELEM_I1;
            break;
        case IL_stelem_i2:
            tmp->kind = Tok_STELEM_I2;
            break;
        case IL_stelem_i4:
            tmp->kind = Tok_STELEM_I4;
            break;
        case IL_stelem_i8:
            tmp->kind = Tok_STELEM_I8;
            break;
        case IL_stelem_r4:
            tmp->kind = Tok_STELEM_R4;
            break;
        case IL_stelem_r8:
            tmp->kind = Tok_STELEM_R8;
            break;
        case IL_stelem_ip:
            tmp->kind = Tok_STELEM_IP;
            break;
        case IL_stelem_ipp:
            tmp->kind = Tok_STELEM_IPP;
            break;
        case IL_stfld: {
                tmp->kind = Tok_STFLD;
                MilTrident td = ops[pc].arg.value<MilTrident>();
                Declaration* d = derefTrident(td);
                if( d == 0 || d->kind != Declaration::Field)
                {
                    qCritical() << "invalid field declaration reference at pc" << pc;
                    return res;
                }
            } break;
        case IL_stind:
            tmp->kind = Tok_STIND;
            tmp->d = loader->mdl.resolve(ops[pc].arg.value<MilQuali>());
            if( tmp->d == 0 || tmp->d->kind != Declaration::TypeDecl)
            {
                qCritical() << "invalid type declaration reference at pc" << pc;
                return res;
            }
            break;
        case IL_stind_i1:
            tmp->kind = Tok_STIND_I1;
            break;
        case IL_stind_i2:
            tmp->kind = Tok_STIND_I2;
            break;
        case IL_stind_i4:
            tmp->kind = Tok_STIND_I4;
            break;
        case IL_stind_i8:
            tmp->kind = Tok_STIND_I8;
            break;
        case IL_stind_r4:
            tmp->kind = Tok_STIND_R4;
            break;
        case IL_stind_r8:
            tmp->kind = Tok_STIND_R8;
            break;
        case IL_stind_ip:
            tmp->kind = Tok_STIND_IP;
            break;
        case IL_stind_ipp:
            tmp->kind = Tok_STIND_IPP;
            break;
        case IL_stloc:
            tmp->kind = Tok_STLOC;
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_stloc_s:
            tmp->kind = Tok_STLOC_S;
            tmp->id = ops[pc].arg.toUInt();
            break;
        case IL_stloc_0:
            tmp->kind = Tok_STLOC_0;
            break;
        case IL_stloc_1:
            tmp->kind = Tok_STLOC_1;
            break;
        case IL_stloc_2:
            tmp->kind = Tok_STLOC_2;
            break;
        case IL_stloc_3:
            tmp->kind = Tok_STLOC_3;
            break;
        case IL_strcpy:
            tmp->kind = Tok_STRCPY;
            break;
        case IL_stvar:
            tmp->kind = Tok_STVAR;
            tmp->d = loader->mdl.resolve(ops[pc].arg.value<MilQuali>());
            if( tmp->d == 0 || tmp->d->kind != Declaration::VarDecl)
            {
                qCritical() << "invalid variable declaration reference at pc" << pc;
                return res;
            }
            break;
        case IL_case:
        case IL_do:
        case IL_until:
        case IL_then:
        case IL_else:
        default:
            qCritical() << "unexpected operation" << s_opName[ops[pc].op] << "at pc" << pc;
            return res;
        }
    }
    return res;
}

Expression* InMemRenderer2::translateExpr(const QList<MilOperation>& ops, quint32& pc)
{
    Expression* res = 0;
    while(pc < ops.size() )
    {
        const TokenType tok = opToTok(ops[pc].op);
        if(tok == Tok_Invalid)
            return res;
        Expression* tmp = new Expression();
        tmp->kind = tok;

        if( res )
            res->append(tmp);
        else
            res = tmp;

        switch(ops[pc].op)
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
        case IL_sizeof:
            tmp->d = loader->mdl.resolve(ops[pc].arg.value<MilQuali>());
            if(tmp->d == 0)
            {
                qCritical() << "cannot resolve qualident at pc" << pc;
                return res;
            }
            break;
        case IL_callvirt:
        case IL_ldfld:
        case IL_ldflda:
        case IL_ldmeth:
            tmp->d = derefTrident(ops[pc].arg.value<MilTrident>());
            if(tmp->d == 0 )
            {
                qCritical() << "cannot resolve trident at pc" << pc;
                return res;
            }
            break;
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
            QByteArray str = ops[pc].arg.toByteArray();
            str = str.mid(1, str.size()-2); // dequote
            c->s = (char*)malloc( str.size() + 1);
            strcpy(c->s, str.constData());
            tmp->c = c;
            }
            break;
        }

        pc++;
    }
    return res;
}

bool InMemRenderer2::expect(const QList<MilOperation>& ops, quint32& pc, int op)
{
    pc++;
    if( pc >= ops.size() || ops[pc].op != op )
    {
        qCritical() << "expecting" << s_opName[op] << "at pc" << pc;
        return false;
    }else
        return true;
}

Declaration*InMemRenderer2::derefTrident(const MilTrident& td) const
{
    Declaration* d = loader->mdl.resolve(td.first);
    if( d == 0 || d->kind != Declaration::TypeDecl || d->getType() == 0)
        return 0;
    d = d->getType()->findSubByName(td.second);
    return d;
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
            // TODO proc.params = d->fields;
            if( t->getType() )
                proc.retType = t->getType()->toQuali();
            r->addProcedure(proc);
        }
        break;
    case Type::NameRef:
        r->addType(d->name,d->public_,t->getType()->toQuali(),MilEmitter::Alias);
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
            // TODO r->addProcedure(module->procs[i.second]);
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
