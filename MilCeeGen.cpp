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

#include "MilCeeGen.h"
#include "MilValidator.h"
#include "MilProject.h"
#include <QDateTime>
#include <QCoreApplication>
#include <QtDebug>
using namespace Mil;

CeeGen::CeeGen(AstModel* mdl):mdl(mdl)
{
    Q_ASSERT(mdl);
}

static QByteArray qualident(Declaration* d)
{
    if( d->outer )
        return qualident(d->outer) + "$" + d->forwardToProc()->name;
    else
        return d->name;
}

bool CeeGen::generate(Declaration* module, QIODevice* header, QIODevice* body)
{
    Q_ASSERT( module && header );
    curMod = module;
    hout.setDevice(header);
    QString dummy;
    if( body )
        bout.setDevice(body);
    else
        bout.setString(&dummy, QIODevice::WriteOnly);

    const QByteArray guard = "__" + module->name.toUpper() + "_INCLUDED__";
    const QString dedication = genDedication();

    hout << "#ifndef " << guard << endl;
    hout << "#define " << guard << endl << endl;
    hout << "// " << module->name << ".h" << endl;
    hout << dedication << endl << endl;

    bout << "// " << module->name << ".c" << endl;
    bout << dedication << endl << endl;
    bout << "#include \"" << Project::escapeFilename(module->name) << ".h\"" << endl;
    bout << "#include <stdlib.h>" << endl;
    bout << "#include <string.h>" << endl;
    bout << "#include <math.h>" << endl << endl;

    visitModule();

    hout << endl << "#endif // " << guard << endl << endl;
    return true;
}

bool CeeGen::requiresBody(Declaration* module)
{
    if( module == 0 )
        return false;
    Declaration* sub = module->subs;
    while(sub)
    {
        switch(sub->kind)
        {
        case Declaration::VarDecl:
            return true;
        case Declaration::Procedure:
            if( sub->forward )
                break;
            // TODO: inline and invar are implemented as normal procedures so far
            if( sub->invar || sub->inline_ )
                return true;
            if( !sub->extern_ && !sub->foreign_ )
                return true;
            break;
        }

        sub = sub->next;
    }
    return false;
}

QString CeeGen::genDedication()
{
    return "// this file was generated by " + QCoreApplication::applicationName() + " "
                                    + QCoreApplication::applicationVersion() + " on " + QDateTime::currentDateTime().toString();
}

void CeeGen::visitModule()
{
   Declaration* sub = curMod->subs;
   while( sub )
   {
       if( sub->kind == Declaration::Import )
       {
            hout << "#include \"" << Project::escapeFilename(sub->name) << ".h\"" << endl;
       }
       sub = sub->next;
   }
   hout << endl;

   sub = curMod->subs;
   while( sub )
   {
       if( sub->kind == Declaration::TypeDecl )
       {
           typeDecl(hout, sub);
           hout << ";" << endl;
           if( sub->getType()->objectInit && sub->getType()->isSOA() )
               emitInitializer(sub->getType());
       }
       sub = sub->next;
   }

   sub = curMod->subs;
   while( sub )
   {
       if( sub->kind == Declaration::ConstDecl )
       {
           hout << "#define " << qualident(sub);
           constValue(hout, sub->c);
           hout << endl << endl;
       }
       sub = sub->next;
   }
   bool initFound = false;
   sub = curMod->subs;
   while( sub )
   {
       switch( sub->kind )
       {
       case Declaration::TypeDecl:
           {
               Type* t = deref(sub->getType());
               if( t && t->kind == Type::Object )
               {
                   foreach( Declaration* p, t->subs )
                   {
                       if( p->kind == Declaration::Procedure )
                           visitProcedure(p);
                   }
                   visitMetaDecl(sub);
               }
           }
           break;
       case Declaration::VarDecl:
           variable(bout, sub);
           bout << ";" << endl << endl;
           hout << "extern ";
           variable(hout, sub);
           hout << ";" << endl;
           break;
       case Declaration::Procedure:
           visitProcedure(sub);
           if( sub->entryPoint )
               initFound = true;
           break;
       }

       sub = sub->next;
   }

   if( !initFound )
   {
        Declaration proc;
        proc.kind = Declaration::Procedure;
        proc.entryPoint = true;
        proc.outer = curMod;
        proc.name = "begin$";
        visitProcedure(&proc);
   }
}

static inline QByteArray ws(int level)
{
    return QByteArray((level+1)*4,' ');
}

void CeeGen::visitProcedure(Declaration* proc)
{
    curProc = proc;
    hout << "extern ";
    procHeader(hout, proc);
    hout << ";" << endl;
    if( !proc->forward && !proc->extern_ && !proc->foreign_ )
    {
        procHeader(bout, proc);
        bout << " {" << endl;
        if( proc->entryPoint && !curMod->nobody )
        {
            bout << ws(0) << "static int done$ = 0;" << endl;
            bout << ws(0) << "if(!done$) {" << endl;
            bout << ws(1) <<     "done$ = 1;" << endl;
            Declaration* sub = curMod->subs;
            while( sub )
            {
                if( sub->kind == Declaration::Import && !sub->imported->nobody )
                     bout << ws(1) << sub->imported->name << "$begin$();" << endl;
                else if( sub->kind == Declaration::VarDecl )
                    emitSoapInit(bout, qualident(sub), sub->getType(), 1 );
                sub = sub->next;
            }
            bout << ws(0) << "}" << endl;
        }
        bout << ws(0) << "void* _ptr$;" << endl;
        bout << ws(0) << "unsigned int _len$;" << endl;
        Declaration* sub = proc->subs;
        while(sub)
        {
            if( sub->kind == Declaration::LocalDecl )
            {
                bout << ws(0);
                parameter(bout, sub);
                bout << ";" << endl;
                emitSoapInit(bout, sub->name, sub->getType(), 0 );
            }
            sub = sub->next;
        }
        statementSeq(bout, proc->body);
        bout << "}" << endl << endl;
    }
    curProc = 0;
}

void CeeGen::visitMetaDecl(Declaration* d)
{
    const QByteArray className = qualident(d);
    hout << "struct " << className << "$Class$ {" << endl;
    bout << "struct " << className << "$Class$ " << className << "$class$ = { " << endl;

    Type* t = deref(d->getType());
    if( t->getType() )
    {
        hout << ws(0) << typeRef(t->getType()) << "$Class$* super$;" << endl;
        bout << ws(0) << "&" << typeRef(t->getType()) << "$class$," << endl;
    }else
    {
        hout << ws(0) << "void* super$;" << endl;
        bout << ws(0) << "0," << endl;
    }

    DeclList methods = t->getMethodTable();
    foreach( Declaration* p, methods )
    {
        Q_ASSERT( p->kind == Declaration::Procedure && !p->forward );
        bout << ws(0) << qualident(p) << ", " << endl;

        hout << ws(0) << typeRef(p->getType()) << " (*" << p->name << ")";
        hout << "(";
        DeclList params = p->getParams();
        for( int i = 0; i < params.size(); i++ )
        {
            if( i != 0 )
                hout << ", ";
            parameter(hout, params[i]);
        }
        hout << ");" << endl;
    }

    bout << "};" << endl << endl;
    hout << "};" << endl;
    hout << "extern struct " << className << "$Class$ " << className << "$class$;" << endl;

}

QByteArray CeeGen::typeRef(Type* t)
{
    if( t == 0 )
        return "void";
    t = t->deref();
    switch(t->kind)
    {
    case Type::Any:
        return "void";
    case Type::StringLit:
        return "const char*";
    case Type::ByteArrayLit:
        return "const unsigned char*";
    case Type::NIL:
        return "NULL";
    case Type::BOOL:
        return "unsigned char";
    case Type::CHAR:
        return "char";
    case Type::INT8:
        return "char";
    case Type::INT16:
        return "short";
    case Type::INT32:
        return "int";
    case Type::INT64:
        return "long long";
    case Type::UINT8:
        return "unsigned char";
    case Type::UINT16:
        return "unsigned short";
    case Type::UINT32:
        return "unsigned int";
    case Type::UINT64:
        return "unsigned long long";
    case Type::FLOAT32:
        return "float";
    case Type::FLOAT64:
        return "double";
    case Type::INTPTR:
        return "void*";
    case Type::DBLINTPTR:
        return "DBLINTPTR"; // TODO
    }

    if( t->kind == Type::Pointer )
    {
        Type* tt = deref(t->getType());
        if( tt->kind == Type::Array )
            tt = deref(tt->getType()); // we treat an array as a pointer to its elements
        QByteArray prefix;
        if( tt->kind == Type::Struct || tt->kind == Type::Object )
            prefix = "struct ";
        else if( tt->kind == Type::Union )
            prefix = "union ";
        return prefix + typeRef(tt) + "*";
    }else if( t->decl )
        return qualident(t->decl);
    else
        return "?TYPE";
}

void CeeGen::procHeader(QTextStream& out, Declaration* proc)
{
    out << typeRef(proc->getType()) << " ";
    out << qualident(proc->forwardToProc());
    out << "(";
    DeclList params = proc->getParams();
    for( int i = 0; i < params.size(); i++ )
    {
        if( i != 0 )
            out << ", ";
        parameter(out, params[i]);
    }
    out << ")";
}

void CeeGen::parameter(QTextStream& out, Declaration* param)
{
    out << typeRef(param->getType()) << " " << param->name;
}

void CeeGen::variable(QTextStream& out, Declaration* var)
{
    out << typeRef(var->getType()) << " " << qualident(var);
}

void CeeGen::typeDecl(QTextStream& out, Declaration* d)
{
    Type* t = deref(d->getType());
    if( t == 0 )
    {
        out << "// undeclared type " << d->name;
        return;
    }

    if( t->kind == Type::Object )
    {
        // forward declaration for class objects
        out << "typedef struct " << qualident(d) << "$Class$ " << qualident(d) << "$Class$;" << endl;
    }

    out << "typedef ";
    if( t->kind < Type::MaxBasicType )
        out << typeRef(t);
    else
        switch( t->kind )
        {
        case Type::Pointer:
            pointerTo(out, t);
            break;
        case Type::Proc:
            if( t->typebound )
            {
                out << "struct " << qualident(d) << " {" << endl;
                out << ws(0) << "void* self;" << endl;
                out << ws(0) << typeRef(t->getType()) << " (*proc)(void* self";
                DeclList params = t->subs;
                for( int i = 0; i < params.size(); i++ )
                {
                    if( t->typebound || i != 0 )
                        out << ", ";
                    parameter(out, params[i]);
                }
                out << ");" << endl;
                out << "}";
            }else
            {
                out << typeRef(t->getType()) << " (*";
                out << qualident(d);
                out << ")(";
                DeclList params = t->subs;
                for( int i = 0; i < params.size(); i++ )
                {
                    if( i != 0 )
                        out << ", ";
                    parameter(out, params[i]);
                }
                out << ")";
                return;
            }
            break;
        case Type::Array:
            out << typeRef(t->getType()) << " " << qualident(d) << "[";
            if( t->len != 0 )
                out << t->len;
            out << "]";
            return;

        case Type::Union:
        case Type::Struct:
            out << (t->kind == Type::Struct ? "struct " : "union ") << qualident(d) << " {" << endl;
            foreach( Declaration* field, t->subs )
            {
                if( field->kind == Declaration::Field )
                    out << ws(0) << typeRef(field->getType()) << " " << field->name << ";" << endl;
            }
            out << "}";
            break;
        case Type::Object: {
                out << "struct " << qualident(d) << " {" << endl;
                out << ws(0) << qualident(d) << "$Class$* class$;" << endl;
                QList<Declaration*> fields = t->getFieldList(true);
                foreach( Declaration* field, fields ) // TODO: was t->subs, but this cannot be correct
                {
                     out << ws(0) << typeRef(field->getType()) << " " << field->name << ";" << endl;
                }
                out << "}";
            } break;
        case Type::NameRef:
            out << typeRef(t->getType());
            break;
        }
    out << " " << qualident(d);
}

void CeeGen::pointerTo(QTextStream& out, Type* ptr)
{
    Type* to = ptr->getType();
    Type* to2 = deref(to);
    if( to2 && to2->kind == Type::Array )
    {
        // Pointer to array is translated to pointer to array element
        ptr = to2;
        to = ptr->getType();
        to2 = deref(to);
    }
    if( to2 && to2->isSUO() )
    {
        if( deref(to)->kind == Type::Union )
            out << "union ";
        else
            out << "struct ";
    }
    out << typeRef(ptr->getType()) << "*";
}

void CeeGen::constValue(QTextStream& out, Constant* c)
{
    if( c == 0 )
    {
        out << 0;
        return;
    }
    switch( c->kind )
    {
    case Constant::D:
        out << c->d;
        break;
    case Constant::I:
        out << c->i;
        break;
    case Constant::S:
        out << "\"" << c->s << "\"";
        break;
    case Constant::B:
        {
            const ByteString* ba = c->b;
            out << "{";
            for( int i = 0; i < ba->len; i++ )
            {
                if( i != 0 && i % 16 == 0 )
                    out << endl << "\t";
                out << "0x" << QByteArray::number(quint8(ba->b[i]),16) << ", ";
            }
            out << "}";
        }
        break;
    case Constant::R:
        constValue(out, c->r->c);
        break;
    case Constant::C:
        {
            if( c->c->type )
                out << "(" << typeRef(c->c->type) << ")";
            out << "{";
            Component* i = c->c->c;
            if( !i->name.isEmpty() )
                out << "." << i->name << "=";
            constValue(out, i->c);
            while( i->next )
            {
                out << ", ";
                i = i->next;
                if( !i->name.isEmpty() )
                    out << "." << i->name << "=";
                constValue(out, i->c);
            }
            out << "}";
        }
        break;
    }
}

void CeeGen::statementSeq(QTextStream& out, Statement* s, int level)
{
    while(s)
    {
        switch( s->kind )
        {
        case Statement::ExprStat:
            if( s->args )
            {
                out << ws(level);
                // NOTE: s->args points to the expr in a sequence after which the stack is empty
                expression(out, s->args, level);
                out << ";" << endl;
            }
            break;

        case IL_if:
            out << ws(level) << "if( ";
            expression(out, s->args, level+1);
            out << " ) {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "}";
            if( s->next && s->next->kind == IL_else )
            {
                s = s->next;
                out << " else {" << endl;
                statementSeq(out, s->body, level+1);
                out << ws(level) << "}";
            }
            out << endl;
            break;

        case IL_loop:
            out << ws(level) << "while( 1 ) {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "}" << endl;
            break;

        case IL_repeat:
            out << ws(level) << "do {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "} while( !";
            expression(out, s->args, level+1);
            out << " );" << endl;
            break;

        case IL_switch:
            out << ws(level) << "switch( ";
            expression(out, s->args, level+1);
            out << " ) {" << endl;
            while( s->next && s->next->kind == IL_case )
            {
                s = s->next;
                Expression* e = s->e;
                while(e)
                {
                    out << ws(level) << "case ";
                    expression(out, e, level+1 );
                    out << ":" << endl;
                    e = e->next;
                }
                out << ws(level+1) << "{" << endl;
                statementSeq(out, s->body, level+2);
                out << ws(level+1) << "} break;" << endl;
            }
            if( s->next && s->next->kind == IL_else )
            {
                s = s->next;
                out << "default:" << endl;
                out << ws(level+1) << "{" << endl;
                statementSeq(out, s->body, level+2);
                out << ws(level+1) << "} break;" << endl;
            }
            out << endl;
            break;

        case IL_while:
            out << ws(level) << "while( ";
            expression(out, s->args, level+1);
            out << " ) {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "}" << endl;
            break;
        case IL_exit:
            out << ws(level) << "break;" << endl;
            break;

        case IL_stloc:
        case IL_stloc_s:
        case IL_stloc_0:
        case IL_stloc_1:
        case IL_stloc_2:
        case IL_stloc_3:
            {
                DeclList locals = curProc->getLocals();
                Q_ASSERT(s->id < locals.size());
                out << ws(level) << locals[s->id]->name << " = ";
                expression(out, s->args, level + 1 );
                out << ";" << endl;
            }
            break;

        case IL_starg:
            {
                DeclList params = curProc->getParams();
                Q_ASSERT(s->id < params.size());
                out << ws(level) << params[s->id]->name << " = ";
                expression(out, s->args, level + 1 );
                out << ";" << endl;
            }
            break;

        case IL_stind:
        case IL_stind_i1:
        case IL_stind_i4:
        case IL_stind_i8:
        case IL_stind_r4:
        case IL_stind_r8:
        case IL_stind_ip:
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );
                out << ws(level) << "*";
                expression(out, s->args->lhs, level+1);
                out << " = ";
                expression(out, s->args->rhs, level+1);
                out << ";" << endl;
            }
            break;

        case IL_stind_ipp:
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );
                out << ws(level) << "*";
                expression(out, s->args->lhs, level+1);
                out << " = ";
                if( s->args->rhs->kind == IL_ldmeth )
                    out << "(" << typeRef(s->args->lhs->getType()->getType()) << ")";
                expression(out, s->args->rhs, level+1);
                out << ";" << endl;
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
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument &&
                          s->args->lhs && s->args->rhs &&
                          s->args->next && s->args->next->kind == Expression::Argument &&
                          s->args->next->rhs && s->args->next->lhs == 0);
                out << ws(level);
                expression(out, s->args->next->rhs, level+1);
                out << "[";
                expression(out, s->args->lhs, level+1);
                out << "] = ";
                expression(out, s->args->rhs, level+1);
                out << ";" << endl;
            }
            break;

        case IL_stfld:
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );
                out << ws(level) << "(";
                expression(out, s->args->lhs, level+1);
                out << ")->";
                out << s->d->name;
                out << " = ";
                expression(out, s->args->rhs, level+1);
                out << ";" << endl;
            }
            break;

        case IL_stvar:
            out << ws(level) << qualident(s->d);
            out << " = ";
            expression(out, s->args, level+1);
            out << ";" << endl;
            break;

        case IL_ret:
            out << ws(level) << "return";
            // TODO: return arrays by value, either via dynamc allocation or hidden parameter
            if( s->args )
            {
                out << " ";
                expression(out, s->args, level+1);
            }
            out << ";" << endl;
            break;

        case IL_pop:
            expression(out, s->args, level+1);
            break;

        case IL_free:
            out << ws(level) << "free(";
            expression(out, s->args, level+1);
            out << ");" << endl;
            break;

        case IL_label:
            out << ws(level) << s->name << ":" << endl;
            break;

        case IL_goto:
            out << ws(level) << "goto " << s->name << ";" << endl;
            break;

        case IL_line:
            // TODO
            break;

        default:
            Q_ASSERT(false);
        }

        s = s->next;
    }
}

void CeeGen::emitBinOP(QTextStream& out, Expression* e, const char* op, int level)
{
    out << "(";
    expression(out, e->lhs, level);
    out << " " << op << " ";
    expression(out, e->rhs, level);
    out << ")";
}

void CeeGen::emitRelOP(QTextStream& out, Expression* e, const char* op, int level)
{
    out << "(";
    expression(out, e->lhs, level+1);
    out << " " << op << " ";
    expression(out, e->rhs, level+1);
    out << ")";
}

void CeeGen::emitSoapInit(QTextStream& out, const QByteArray& name, Type* t, int level)
{
    t = deref(t);
    if( t->kind == Type::Pointer && t->pointerInit )
        out << ws(level) << name << " = NULL;" << endl;
    else if( t->isSUOA() )
    {
        if( t->pointerInit ) // it's cheaper to directly zero the whole thing
            out << ws(level) << "memset(" << (t->isSUO() ? "&" : "") << name << ", 0, sizeof(" << typeRef(t) << "));" << endl;
    }
    emitSoaInit(out, name, false, t, level);
}

void CeeGen::emitSoaInit(QTextStream& out, const QByteArray& name, bool nameIsPtr, Type* t, int level)
{
    t = deref(t);
    if( !t->objectInit )
        return;
    if( t->isSO() )
        out << ws(level) << qualident(t->decl) << "$init$(" << (nameIsPtr ? "" : "&") << name << ", 1);" << endl;
    else if( t->kind == Type::Array && t->len && deref(t->getType())->isSO() )
        out << ws(level) << qualident(deref(t->getType())->decl) << "$init$(" << name << ", " << t->len << ");" << endl;
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

Type*CeeGen::deref(Type* t)
{
    if( t && t->kind == Type::NameRef )
        return deref(t->getType());
    else if( t )
        return t;
    else
        return mdl->getBasicType(Type::Undefined);
}

void CeeGen::emitInitializer(Type* t)
{
    t = deref(t);
    hout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n);" << endl;
    bout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n) {" << endl;
    bout << ws(0) << "int i;" << endl;
    bout << ws(0) << "for( i = 0; i < n; i++ ) {" << endl;

    if( t->kind == Type::Object )
        bout << ws(1) << "obj[i].class$ = &" << qualident(t->decl) << "$class$;" << endl;
    else if( t->kind == Type::Array && t->len && t->objectInit )
    {
        Type* et = deref(t->getType());
        bout << ws(1) << qualident(et->decl) << "$init$(obj, " << t->len << ");" << endl;
    }

    foreach( Declaration* field, t->subs )
    {
        if( field->kind != Declaration::Field )
            continue;
        Type* tt = deref(field->getType());
        if( tt->isSO() )
        {
            bout << ws(1) << qualident(tt->decl) << "$init$(&obj->" << field->name << ", 1);" << endl;
        }else if( tt->kind == Type::Array && tt->len )
        {
            Type* et = deref(tt->getType());
            if( et->isSO() )
                bout << ws(1) << qualident(tt->decl) << "$init$(obj->" << field->name << ", " << tt->len << ");" << endl;
        }
    }

    bout << ws(0) << "}" << endl;
    bout << "}" << endl << endl;
}

void CeeGen::expression(QTextStream& out, Expression* e, int level)
{
    switch(e->kind)
    {
    case IL_add:
        emitBinOP(out, e, "+", level+1);
        break;
    case IL_div_un:
    case IL_div:
        emitBinOP(out, e, "/", level+1); // TODO: UN
        break;
    case IL_mul:
        emitBinOP(out, e, "*", level+1);
        break;
    case IL_rem:
    case IL_rem_un:
        emitBinOP(out, e, "%", level+1); // TODO: UN
        break;
    case IL_sub:
        emitBinOP(out, e, "-", level+1);
        break;

    case IL_and:
        emitBinOP(out, e, "&", level+1);
        break;
    case IL_or:
        emitBinOP(out, e, "|", level+1);
        break;
    case IL_xor:
        emitBinOP(out, e, "^", level+1);
        break;

    case IL_shl:
        emitBinOP(out, e, "<<", level+1);
        break;
    case IL_shr_un:
    case IL_shr:
        emitBinOP(out, e, ">>", level+1); // TODO: UN
        break;

    case IL_neg:
        out << "-";
        expression(out, e->lhs, level+1);
        break;

    case IL_abs:
        if( deref(e->lhs->getType())->isInteger() )
            out << "abs(";
        else
            out << "fabs(";
        expression(out, e->lhs, level+1);
        out << ")";
        break;

    case IL_not:
        out << "~";
        expression(out, e->lhs, level + 1);
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
        out << e->i;
        break;

    case IL_ldc_r4:
    case IL_ldc_r8:
        out << QByteArray::number(e->f,'e',16); // empirically optimized, 17 is too much
        break;

    case IL_ldnull:
        out << "NULL";
        break;

    case IL_ldstr:
    case IL_ldobj:
        constValue(out, e->c);
        break;

    case IL_conv_i1:
    case IL_conv_i2:
    case IL_conv_i4:
    case IL_conv_u1:
    case IL_conv_u2:
    case IL_conv_u4:
    case IL_conv_i8:
    case IL_conv_u8:
    case IL_conv_r4:
    case IL_conv_r8:
        out << "((" << typeRef(Validator::tokToBasicType(mdl, e->kind)) << ")";
        expression(out, e->lhs, level+1);
        out << ")";
        break;

    case IL_ceq:
        emitRelOP(out, e, "==", level + 1);
        break;
    case IL_cgt_un: // TODO: UN
    case IL_cgt:
        emitRelOP(out, e, ">", level + 1);
        break;
    case IL_clt_un: // TODO: UN
    case IL_clt:
        emitRelOP(out, e, "<", level + 1);
        break;

    case IL_ldvar:
        out << qualident(e->d);
        break;
    case IL_ldvara:
        out << "(" << ( !deref(e->getType())->isPtrToArray() ? "&" : "") << qualident(e->d) << ")";
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
            Q_ASSERT( e->id < params.size() );
            if( (e->kind == IL_ldarga_s || e->kind == IL_ldarga) && !deref(e->getType())->isPtrToArray() )
                out << "(&";
            out << params[e->id]->name;
            if( (e->kind == IL_ldarga_s || e->kind == IL_ldarga) && !deref(e->getType())->isPtrToArray() )
                out << ")";
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
            Q_ASSERT( e->id < locals.size() );
            if( (e->kind == IL_ldloca_s || e->kind == IL_ldloca) && !deref(e->getType())->isPtrToArray() )
                out << "(&";
            out << locals[e->id]->name;
            if( (e->kind == IL_ldloca_s || e->kind == IL_ldloca) && !deref(e->getType())->isPtrToArray() )
                out << ")";
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
        out << "*";
        expression(out, e->lhs, level+1);
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
    case IL_ldelema:
        if( e->kind == IL_ldelema && !deref(e->getType())->isPtrToArray() )
            out << "(&";
        expression(out, e->lhs, level + 1);
        out << "[";
        expression(out, e->rhs, level + 1);
        out << "]";
        if( e->kind == IL_ldelema && !deref(e->getType())->isPtrToArray() )
            out << ")";
        break;

    case IL_ldfld:
    case IL_ldflda:
        if( e->kind == IL_ldflda && !deref(e->getType())->isPtrToArray() )
            out << "(&";
        out << "(";
        expression(out, e->lhs, level+1 );
        out << "->" << e->d->name;
        out << ")";
        if( e->kind == IL_ldflda && !deref(e->getType())->isPtrToArray() )
            out << ")";
        break;

    case IL_ldproc:
        out << qualident(e->d);
        break;

    case IL_ldmeth:
        out << "{(_ptr$ = ";
        expression(out, e->lhs, level+1);
        out << ", _ptr$), ((" << typeRef(e->lhs->getType()) << ")_ptr$)";
        out << "->class$->" << e->d->name << "}";
        break;

    case IL_castptr:
        out << "((" << typeRef(e->d->getType()) << "*)";
        expression(out, e->lhs, level+1 );
        out << ")";
        break;

    case IL_call:
        {
            out << qualident(e->d) << "(";
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for( int i = 0; i < args.size(); i++ )
            {
                if( i != 0 )
                    out << ", ";
                expression(out, args[i], level+1);
            }
            out << ")";
        }
        break;

    case IL_callvirt:
        {
            out << "(_ptr$ = ";
            expression(out, e->lhs, level+1);
            out << ", ((" << typeRef(e->lhs->getType()) << ")_ptr$)";
            out << "->class$->" << e->d->name << "(_ptr$";
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for( int i = 0; i < args.size(); i++ )
            {
               out << ", ";
                expression(out, args[i], level+1);
            }
            out << "))";
        }
        break;

    case IL_calli:
        {
            expression(out, e->lhs, level+1 );
            out << "(";
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for( int i = 0; i < args.size(); i++ )
            {
                if( i != 0 )
                    out << ", ";
                expression(out, args[i], level+1);
            }
            out << ")";
        }
        break;

    case IL_callvi:
        {
            out << "(_ptr$ = &";
            expression(out, e->lhs, level+1 );
            out << ", ((" << typeRef(e->lhs->getType()) << "*)_ptr$)->proc(((";
            out << typeRef(e->lhs->getType()) << "*)_ptr$)->self";
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for( int i = 0; i < args.size(); i++ )
            {
                out << ", ";
                expression(out, args[i], level+1);
            }
            out << "))";
        }
        break;


    case IL_newobj:
        Q_ASSERT(e->getType()->kind == Type::Pointer);
        if( deref(e->getType())->objectInit )
        {
            const QByteArray name = typeRef(e->d->getType());
            out << "(_ptr$ = calloc(1, sizeof(";
            out << name;
            out << ")),\n\t\t";
            out << name << "$init$(" << "((" + name + "*)_ptr$)" << ", 1), (";
            out << typeRef(e->getType());
            out << ")_ptr$)";
        }else
        {
            out << "(";
            out << typeRef(e->getType());
            out << ")calloc(1, sizeof(";
            out << typeRef(e->getType()->getType());
            out << "))";
        }
        break;

    case IL_newarr:
        if( deref(e->getType())->objectInit )
        {
            Type* et = deref(e->d->getType());
            const QByteArray name = typeRef(et);
            out << "(_len$ = ";
            expression(out, e->lhs, level+1);
            out << ", _ptr$ = calloc(_len$,sizeof(";
            out << name << ")),\n\t\t";
            out << name << "$init$(" << "((" << name << "*)_ptr$)" << ", _len$), (";
            out << typeRef(et);
            out << "*)_ptr$)";
        }else
        {
            Type* et = deref(e->d->getType());
            out << "(";
            out << typeRef(et);
            out << "*)calloc(";
            expression(out, e->lhs, level+1);
            out << ",sizeof(";
            out << typeRef(et) << "))";
        }
        break;


    case IL_initobj:
        {
            Type* t = deref(e->d->getType());
            const QByteArray name = typeRef(t);
            out << "_ptr$ = ";
            expression(out, e->lhs, level+1 );
            out << "; memset(_ptr$, 0, sizeof(" << name << "));";
            emitSoaInit(out, "((" + name + "*)_ptr$)", true, t, level );
        }
        break;

    case IL_dup:
        expression(out, e->lhs, level+1);
        // TODO: temp instead of multiple eval of same exr
        break;

    case IL_nop:
        break;

    case IL_ptroff:
        expression(out, e->lhs, level + 1);
        out << " += ";
        expression(out, e->rhs, level+1);
        out << " * sizeof(" << typeRef(e->d->getType()) << ")";
        break;

    case IL_iif: {
            Expression* if_ = e->e;
            Q_ASSERT(if_ && if_->kind == IL_if && if_->next->kind == IL_then && if_->next->next->kind == IL_else &&
                     if_->next->next->next == 0); // no IL_end
            Expression* then_ = if_->next;
            Expression* else_ = if_->next->next;
            out << "(";
            expression(out, if_->lhs, level + 1);
            out << " ? ";
            expression(out, then_->lhs, level + 1);
            out << " : ";
            expression(out, else_->lhs, level + 1);
            out << ") ";
        } break;

    case IL_sizeof:
    case IL_newvla:
    case IL_isinst:
        out << "TODO: " << s_opName[e->kind];
        break;
    default:
        Q_ASSERT(false);
    }
}

