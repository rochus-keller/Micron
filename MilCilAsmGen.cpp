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

// derived from CeeGen; same logic; generates ECMA 335 IL asm text instead

#include "MilCilAsmGen.h"
#include "MilValidator.h"
#include "MilProject.h"
#include <QDateTime>
#include <QCoreApplication>
#include <QtDebug>

using namespace Mil;

CilAsmGen::CilAsmGen(AstModel* mdl) : mdl(mdl), labelCounter(0), maxStack(0), currentStack(0)
{
    Q_ASSERT(mdl);
}

static QByteArray indent(int level)
{
    return QByteArray(level * 4, ' ');
}

QByteArray CilAsmGen::declRef(Declaration * d)
{
    // remember MIL has no nested types or procedures!

    if( d == 0 )
        return QByteArray();

    switch(d->kind)
    {
    case Declaration::Module:
        if( d != curMod )
            return "['" + d->name + "']'" + d->name + "'";
        else
            return "'" + d->name + "'"; // since the MIL module is a CIL class, this name always remains.

    case Declaration::TypeDecl:
        Q_ASSERT(d->outer && d->outer->kind == Declaration::Module && d->getType() && d->getType()->isSUOA() );
        return declRef(d->outer) + "/'" + d->name + "'";

    case Declaration::Field:
        Q_ASSERT(d->outer && d->outer->kind == Declaration::TypeDecl);
        return declRef(d->outer) + "::'" + d->name + "'";

    case Declaration::VarDecl:
        Q_ASSERT(d->outer && d->outer->kind == Declaration::Module);
        return declRef(d->outer) + "::'" + d->name + "'";

    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
        return "'" + d->name + "'";

    case Declaration::Procedure:
        Q_ASSERT(d->outer && d->outer->kind == Declaration::Module);
        return declRef(d->outer) + "::'" + d->forwardToProc()->name + "'";

    default:
        Q_ASSERT(false);
    }
}

QByteArray CilAsmGen::declName(Declaration * d)
{
    if( d == 0 )
        return QByteArray();
    switch(d->kind)
    {
    case Declaration::Field:
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
    case Declaration::ConstDecl:
    case Declaration::Module:
        return "'" + d->name + "'";
    case Declaration::Procedure:
    case Declaration::TypeDecl:
        if (d->outer && d->outer->kind != Declaration::Module)
            return declName(d->outer) + "$" + d->forwardToProc()->name;
        else
            return "'" + d->forwardToProc()->name + "'";
    default:
        Q_ASSERT(false);
    }
}

bool CilAsmGen::generate(Declaration* module, QIODevice* device)
{
    Q_ASSERT(module && device);
    curMod = module;
    out.setDevice(device);
    
    const QString dedication = genDedication();
    
    out << "// " << declName(module) << ".il" << endl;
    out << dedication << endl << endl;
    

    out << ".assembly " << declName(module) << " {}" << endl;

    out << ".module '" << module->name << ".dll'" << endl << endl;

    out << "// imports" << endl;
    out << ".assembly extern mscorlib {}" << endl;
    out << ".assembly extern MIC$ {}" << endl;

    visitModule();
    
    return true;
}

bool CilAsmGen::generateMain(QIODevice *device, const QSet<Declaration *> &used)
{
    Q_ASSERT(device);

    out.setDevice(device);

    const QString dedication = genDedication();

    out << "// Main$.il" << endl;
    out << dedication << endl << endl;

    out << ".assembly Main$ {}" << endl;

    out << ".module 'Main$.exe'" << endl << endl;

    out << "// imports" << endl;
    foreach( Mil::Declaration* module, mdl->getModules() )
    {
        // if a module is not in "used", it is never imported and thus a root module
        if( !used.contains(module) && !module->nobody && !module->generic )
            out << ".assembly extern " << declName(module) << " {}" << endl;
    }

    out << endl;

    out << ".class public auto ansi sealed Main$ extends [mscorlib]System.Object {" << endl;

    out << ".method public static void Main() cil managed {" << endl;

    out << "    .entrypoint" << endl;
    foreach( Mil::Declaration* module, mdl->getModules() )
    {
        // if a module is not in "used", it is never imported and thus a root module
        if( !used.contains(module) && !module->nobody && !module->generic )
            out << "    call void ['" <<  module->name << "']'" << module->name << "'::begin$()" << endl;
    }
    out << "    ret" << endl;
    out << "}}" << endl;

    return true;
}

bool CilAsmGen::generateConfig(QIODevice *device)
{
    Q_ASSERT(device);

    out.setDevice(device);

    out << "{\"runtimeOptions\": {" << endl;
    out << "\"framework\": {" << endl;
    out << "\"name\": \"Microsoft.NETCore.App\"," << endl;
    out << "\"version\": \"3.0.0\"," << endl;
    out << "\"rollForward\": \"LatestMajor\"" << endl;
    out << "}}}" << endl;

    return true;
}

QString CilAsmGen::genDedication()
{
    return "// This file was generated by " + QCoreApplication::applicationName() + " "
           + QCoreApplication::applicationVersion() + " on " + QDateTime::currentDateTime().toString();
}

void CilAsmGen::visitModule()
{
    // Import external assemblies for imported modules
    Declaration* sub = curMod->subs;
    while (sub)
    {
        if (sub->kind == Declaration::Import)
        {
            out << ".assembly extern '" << sub->name << "' {}" << endl;
        }
        sub = sub->next;
    }
    out << endl;

    out << ".class public auto ansi sealed " << declName(curMod) << " extends [mscorlib]System.Object {" << endl;
    
    out << ".field static int32 initialized$" << endl;

    sub = curMod->subs;
    while (sub)
    {
        if (sub->kind == Declaration::TypeDecl)
        {
            typeDecl(sub);
        }
        sub = sub->next;
    }
    
    // Module-level static fields for variables
    sub = curMod->subs;
    while (sub)
    {
        if (sub->kind == Declaration::VarDecl)
        {
            out << ".field public static " << typeRef(sub->getType()) << " " << declName(sub) << endl;
        }
        sub = sub->next;
    }
    out << endl;
        
    // Process procedures and type methods
    bool initFound = false;
    sub = curMod->subs;
    while (sub)
    {
        switch (sub->kind)
        {
        case Declaration::TypeDecl:
            {
                Type* t = deref(sub->getType());
                if (t && t->kind == Type::Object)
                {
                    foreach (Declaration* p, t->subs)
                    {
                        if (p->kind == Declaration::Procedure)
                            visitProcedure(p);
                    }
                    visitMetaDecl(sub);
                }
                if (t && t->objectInit && t->isSOA())
                    emitInitializer(t);
            }
            break;
        case Declaration::Procedure:
            visitProcedure(sub);
            if (sub->entryPoint)
                initFound = true;
            break;
        }
        sub = sub->next;
    }
    
    // create module initializer if not present
    if (!initFound)
    {
        Declaration proc;
        proc.kind = Declaration::Procedure;
        proc.entryPoint = true;
        proc.outer = curMod;
        proc.name = "begin$";
        visitProcedure(&proc);
    }

    out << "}" << endl; // end module class
}

void CilAsmGen::visitProcedure(Declaration* proc)
{
    curProc = proc;
    labelCounter = 0;
    maxStack = 0;
    currentStack = 0;
    
    procHeader(proc);
    out << "{" << endl;
    
    if (proc->entryPoint && !curMod->nobody)
    {
        // Module initialization code
        out << indent(1) << ".maxstack 2" << endl;
        out << indent(1) << "ldsfld int32 " << declRef(curMod) << "::initialized$" << endl;
        out << indent(1) << "brtrue.s IL_INIT_DONE" << endl;
        out << indent(1) << "ldc.i4.1" << endl;
        out << indent(1) << "stsfld int32 " << declRef(curMod) << "::initialized$" << endl;
        
        Declaration* sub = curMod->subs;
        while (sub)
        {
            if (sub->kind == Declaration::Import && !sub->imported->nobody)
            {
                out << indent(1) << "call void ['" << sub->imported->name << "']'" << sub->imported->name << "'::begin$()" << endl;
            }
            else if (sub->kind == Declaration::VarDecl)
            {
                emitSoapInit(mangledName(sub), sub->getType(), true);
            }
            sub = sub->next;
        }
        out << indent(1) << "IL_INIT_DONE:" << endl;
    }
    
    if (!proc->forward && !proc->extern_ && !proc->foreign_)
    {
        // Local variables
        if (proc->subs)
        {
            out << indent(1) << ".locals init (" << endl;
            Declaration* sub = proc->subs;
            int localIdx = 0;
            bool first = true;
            while (sub)
            {
                if (sub->kind == Declaration::LocalDecl)
                {
                    if (!first)
                        out << "," << endl;
                    out << indent(2) << "[" << localIdx++ << "] " << typeRef(sub->getType()) << " '" << sub->name << "'";
                    first = false;
                }
                sub = sub->next;
            }
            out << endl << indent(1) << ")" << endl;
        }
        
        emitMaxStack();
        
        // Generate body
        statementSeq(proc->body);
        
        if (proc->getType() == 0 || deref(proc->getType())->kind == Type::Any)
        {
            out << indent(1) << "ret" << endl;
        }
    }
    
    out << "}" << endl << endl;
    curProc = 0;
}

void CilAsmGen::procHeader(Declaration* proc)
{
    out << ".method static ";

    if( proc->public_ || proc->entryPoint )
        out << "public ";
    
    out << typeRef(proc->getType()) << " " << declName(proc);
    out << "(";
    
    DeclList params = proc->getParams();
    for (int i = 0; i < params.size(); i++)
    {
        if (i != 0)
            out << ", ";
        parameter(params[i], i);
    }
    
    out << ") cil managed" << endl;
}

void CilAsmGen::parameter(Declaration* param, int index)
{
    out << typeRef(param->getType()) << " " << declName(param);
}

void CilAsmGen::emitField(Declaration *field, bool union_)
{
    out << indent(1) << ".field public ";
    if( union_ )
        out << "[0] ";
    out << typeRef(field->getType()) << " '" << field->name << "'" << endl;
    // TODO: bitfields?
}

void CilAsmGen::typeDecl(Declaration* d)
{
    Type* t = deref(d->getType());
    if (t == 0)
    {
        out << "// Undeclared type " << d->name << endl;
        return;
    }
        
    switch (t->kind)
    {
    case Type::Object:
    case Type::Struct: {
            out << ".class nested public sequential ansi sealed " << declName(d) << endl;
            out << "       extends [mscorlib]System.ValueType {" << endl;

            QList<Declaration*> fields;
            if( t->kind == Type::Object )
            {
                // Objects as structs with vtable pointer
                out << indent(1) << ".field public native int class$" << endl;  // vtable pointer
                fields = t->getFieldList(true);
            }else
                fields = t->subs;

            // For structs, fields are sequential
            foreach (Declaration* field, fields)
            {
                if (field->kind == Declaration::Field)
                    emitField(field, false);
            }
            out << "}" << endl << endl;
        } break;

    case Type::Union: {
            out << ".class nested public explicit ansi sealed " << declName(d) << endl;
            out << "       extends [mscorlib]System.ValueType {" << endl;

                // For unions, all fields start at offset 0
                foreach (Declaration* field, t->subs)
                {
                    if (field->kind == Declaration::Field)
                        emitField(field, true);
                }

            out << "}" << endl << endl;
        } break;
        
    case Type::Array:
        if( t->len )
        {
            const QByteArray et = typeRef(t->getType());
            out << ".class nested public sequential ansi sealed " << declName(d) << endl;
            out << "       extends [mscorlib]System.ValueType {" << endl;
            for( int i = 0; i < t->len; i++ )
            {
                out << indent(1) << ".field public ";
                out << et << " '#" << i << "'" << endl;
            }
            out << "}" << endl << endl;
        } // else
            // open arrays are just pointers to the first element, no type declaration needed
        break;
        
    case Type::Pointer:
        // Pointers are native int
        // No type declaration needed
        break;
        
    case Type::Proc:
        // Function pointers
        if (t->typebound)
        {
            out << ".class value explicit ansi sealed " << declName(d) << endl;
            out << "       extends [mscorlib]System.ValueType" << endl;
            out << "{" << endl;
            out << indent(1) << ".field native int self" << endl;
            out << indent(1) << ".field native int proc" << endl;
            out << "}" << endl << endl;
        } // else
            // plain function pointers are just native int, no type declaration needed
        break;
    }
}

QByteArray CilAsmGen::typeRef(Type* t)
{
    if (t == 0)
        return "void";
    
    t = t->deref();
    switch (t->kind)
    {
    case Type::BOOL:
        return "bool";
    case Type::CHAR:
        return "uint8"; // the CIL char has 16 bits
    case Type::INT8:
        return "int8";
    case Type::INT16:
        return "int16";
    case Type::INT32:
        return "int32";
    case Type::INT64:
        return "int64";
    case Type::UINT8:
        return "uint8";
    case Type::UINT16:
        return "uint16";
    case Type::UINT32:
        return "uint32";
    case Type::UINT64:
        return "uint64";
    case Type::FLOAT32:
        return "float32";
    case Type::FLOAT64:
        return "float64";
    case Type::INTPTR:
        return "native int";
    case Type::DBLINTPTR:
        return "native int";  // TODO: handle double pointer size
    case Type::Pointer: {
        Type* tt = deref(t->getType());
        if (tt->kind == Type::Array)
            tt = deref(tt->getType());

        return typeRef(tt) + "*";
    }
    case Type::Array:
        if( t->len )
            return "valuetype " + declRef(t->decl);
        else
        {
            Type* tt = deref(t->getType());
            return typeRef(tt) + "*"; // TODO
        }
    case Type::Struct:
    case Type::Object:
    case Type::Union:
        return "valuetype " + declRef(t->decl);
    case Type::Proc:
        if( t->typebound )
            return declRef(t->decl);
        else
            return "native int";
    default:
        Q_ASSERT(false);
        return "object"; // TODO
    }
}

QByteArray CilAsmGen::typeToken(Type* t)
{
    t = deref(t);
    switch (t->kind)
    {
    case Type::INT8:
        return "i1";
    case Type::INT16:
        return "i2";
    case Type::INT32:
        return "i4";
    case Type::INT64:
        return "i8";
    case Type::UINT8:
        return "u1";
    case Type::UINT16:
        return "u2";
    case Type::UINT32:
        return "u4";
    case Type::UINT64:
        return "u8";
    case Type::FLOAT32:
        return "r4";
    case Type::FLOAT64:
        return "r8";
    case Type::INTPTR:
    default:
        return "i";
    }
}

void CilAsmGen::statementSeq(Statement* s)
{
    while (s)
    {
        switch (s->kind)
        {
        case Statement::ExprStat:
            if (s->args)
            {
                expression(s->args);
                // Pop result if not used
                if (currentStack > 0)
                    out << indent(1) << "pop" << endl;
            }
            break;
            
        case IL_if:
            {
                QByteArray elseLabel = "IL_ELSE_" + QByteArray::number(labelCounter++);
                QByteArray endLabel = "IL_ENDIF_" + QByteArray::number(labelCounter++);
                
                expression(s->args);
                out << indent(1) << "brfalse.s " << elseLabel << endl;
                statementSeq(s->body);
                out << indent(1) << "br.s " << endLabel << endl;
                out << indent(1) << elseLabel << ":" << endl;
                
                if (s->next && s->next->kind == IL_else)
                {
                    s = s->next;
                    statementSeq(s->body);
                }
                out << indent(1) << endLabel << ":" << endl;
            }
            break;
            
        case IL_loop:
            {
                QByteArray loopLabel = "IL_LOOP_" + QByteArray::number(labelCounter++);
                out << indent(1) << loopLabel << ":" << endl;
                statementSeq(s->body);
                out << indent(1) << "br.s " << loopLabel << endl;
            }
            break;
            
        case IL_repeat:
            {
                QByteArray loopLabel = "IL_REPEAT_" + QByteArray::number(labelCounter++);
                out << indent(1) << loopLabel << ":" << endl;
                statementSeq(s->body);
                expression(s->args);
                out << indent(1) << "brfalse.s " << loopLabel << endl;
            }
            break;
            
        case IL_switch:
            {
                expression(s->args);
                
                // Count cases
                int caseCount = 0;
                Statement* cs = s->next;
                while (cs && cs->kind == IL_case)
                {
                    caseCount++;
                    cs = cs->next;
                }
                
                // Generate jump table
                out << indent(1) << "switch (";
                for (int i = 0; i < caseCount; i++)
                {
                    if (i > 0)
                        out << ", ";
                    out << "IL_CASE_" << i;
                }
                out << ")" << endl;
                
                // Default case
                QByteArray defaultLabel = "IL_DEFAULT_" + QByteArray::number(labelCounter++);
                out << indent(1) << "br.s " << defaultLabel << endl;
                
                // Generate cases
                int caseIdx = 0;
                while (s->next && s->next->kind == IL_case)
                {
                    s = s->next;
                    out << indent(1) << "IL_CASE_" << caseIdx++ << ":" << endl;
                    statementSeq(s->body);
                    out << indent(1) << "br.s IL_ENDSWITCH" << endl;
                }
                
                out << indent(1) << defaultLabel << ":" << endl;
                if (s->next && s->next->kind == IL_else)
                {
                    s = s->next;
                    statementSeq(s->body);
                }
                out << indent(1) << "IL_ENDSWITCH:" << endl;
            }
            break;
            
        case IL_while:
            {
                QByteArray testLabel = "IL_WHILE_TEST_" + QByteArray::number(labelCounter++);
                QByteArray loopLabel = "IL_WHILE_LOOP_" + QByteArray::number(labelCounter++);
                
                out << indent(1) << "br.s " << testLabel << endl;
                out << indent(1) << loopLabel << ":" << endl;
                statementSeq(s->body);
                out << indent(1) << testLabel << ":" << endl;
                expression(s->args);
                out << indent(1) << "brtrue.s " << loopLabel << endl;
            }
            break;
            
        case IL_exit:
            out << indent(1) << "br IL_ENDLOOP" << endl;  // Needs proper loop tracking
            break;
            
        case IL_stloc:
        case IL_stloc_s:
        case IL_stloc_0:
        case IL_stloc_1:
        case IL_stloc_2:
        case IL_stloc_3:
            expression(s->args);
            if (s->kind >= IL_stloc_0 && s->kind <= IL_stloc_3)
                out << indent(1) << "stloc." << (s->kind - IL_stloc_0) << endl;
            else
                out << indent(1) << "stloc " << s->id << endl;
            break;
            
        case IL_starg:
            expression(s->args);
            out << indent(1) << "starg " << s->id << endl;
            break;
            
        case IL_stind:
        case IL_stind_i1:
        case IL_stind_i4:
        case IL_stind_i8:
        case IL_stind_r4:
        case IL_stind_r8:
        case IL_stind_ip:
            Q_ASSERT(s->args && s->args->kind == Expression::Argument);
            expression(s->args->lhs);  // Address
            expression(s->args->rhs);  // Value
            out << indent(1) << "stind." << typeToken(s->args->rhs->getType()) << endl;
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
                Q_ASSERT(s->args && s->args->kind == Expression::Argument);
                expression(s->args->next->rhs);  // Array pointer
                expression(s->args->lhs);        // Index
                out << indent(1) << "conv.i" << endl;
                
                // Calculate element size based on type
                Type* elemType = deref(s->args->rhs->getType());
                int elemSize = 0;
                const char* storeOp = "stind.i";
                
                switch(s->kind) {
                case IL_stelem_i1:
                    elemSize = 1;
                    storeOp = "stind.i1";
                    break;
                case IL_stelem_i2:
                    elemSize = 2;
                    storeOp = "stind.i2";
                    break;
                case IL_stelem_i4:
                    elemSize = 4;
                    storeOp = "stind.i4";
                    break;
                case IL_stelem_i8:
                    elemSize = 8;
                    storeOp = "stind.i8";
                    break;
                case IL_stelem_r4:
                    elemSize = 4;
                    storeOp = "stind.r4";
                    break;
                case IL_stelem_r8:
                    elemSize = 8;
                    storeOp = "stind.r8";
                    break;
                case IL_stelem_ip:
                    out << indent(1) << "sizeof native int" << endl;
                    storeOp = "stind.i";
                    break;
                case IL_stelem:
                    out << indent(1) << "sizeof " << typeRef(elemType) << endl;
                    storeOp = elemType->isSUOA() ? "stobj" : "stind.i";
                    break;
                }
                
                if (elemSize > 0)
                    out << indent(1) << "ldc.i4." << elemSize << endl;
                
                out << indent(1) << "mul" << endl;
                out << indent(1) << "add" << endl;  // Calculate address
                expression(s->args->rhs);            // Value
                
                if (s->kind == IL_stelem && elemType->isSUO())
                    out << indent(1) << storeOp << " " << typeRef(elemType) << endl;
                else
                    out << indent(1) << storeOp << endl;
            }
            break;
            
        case IL_stfld:
            Q_ASSERT(s->args && s->args->kind == Expression::Argument);
            expression(s->args->lhs);  // Object
            expression(s->args->rhs);  // Value
            out << indent(1) << "stfld " << typeRef(s->args->rhs->getType()) << " ";
            out << mangledName(s->d->outer) << "::'" << s->d->name << "'" << endl;
            break;
            
        case IL_stvar:
            expression(s->args);
            // TODO out << indent(1) << "stsfld " << typeRef(s->args->getType()) << " ";
            out << indent(1) << "stsfld " << typeRef(s->d->getType()) << " ";
            out << mangledName(s->d) << endl;
            break;
            
        case IL_ret:
            if (s->args)
                expression(s->args);
            out << indent(1) << "ret" << endl;
            break;
            
        case IL_pop:
            expression(s->args);
            out << indent(1) << "pop" << endl;
            break;
            
        case IL_free:
            expression(s->args);
            out << indent(1) << "call void [mscorlib]System.Runtime.InteropServices.Marshal::FreeHGlobal(native int)" << endl;
            break;
            
        case IL_label:
            out << indent(1) << s->name << ":" << endl;
            break;
            
        case IL_goto:
            out << indent(1) << "br " << s->name << endl;
            break;
            
        case IL_line:
            // Line debug information
            break;
            
        case IL_strcpy:
            Q_ASSERT(s->args && s->args->kind == Expression::Argument);
            expression(s->args->lhs);  // to
            expression(s->args->rhs);  // from
            out << indent(1) << "call void [MIC$]MIC$::strcpy(uint8*,uint8*)" << endl;
            break;

        default:
            Q_ASSERT(false);
        }
        
        s = s->next;
    }
}

static void collectArgs(Expression* e, QList<Expression*>& args)
{
    if (e && e->kind == Expression::Argument)
    {
        if (e->next)
            collectArgs(e->next, args);
        if (e->lhs)
            args << e->lhs;
        args << e->rhs;
    }
    else if (e)
        args << e;
}

void CilAsmGen::expression(Expression* e)
{
    switch (e->kind)
    {
    case IL_add:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "add" << endl;
        break;
        
    case IL_div:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "div" << endl;
        break;
        
    case IL_div_un:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "div.un" << endl;
        break;
        
    case IL_mul:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "mul" << endl;
        break;
        
    case IL_rem:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "rem" << endl;
        break;
        
    case IL_rem_un:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "rem.un" << endl;
        break;
        
    case IL_sub:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "sub" << endl;
        break;
        
    case IL_and:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "and" << endl;
        break;
        
    case IL_or:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "or" << endl;
        break;
        
    case IL_xor:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "xor" << endl;
        break;
        
    case IL_shl:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "shl" << endl;
        break;
        
    case IL_shr:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "shr" << endl;
        break;
        
    case IL_shr_un:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "shr.un" << endl;
        break;
        
    case IL_neg:
        expression(e->lhs);
        out << indent(1) << "neg" << endl;
        break;
        
    case IL_not:
        expression(e->lhs);
        out << indent(1) << "not" << endl;
        break;
        
    case IL_ldc_i4_0:
        out << indent(1) << "ldc.i4.0" << endl;
        break;
    case IL_ldc_i4_1:
        out << indent(1) << "ldc.i4.1" << endl;
        break;
    case IL_ldc_i4_2:
        out << indent(1) << "ldc.i4.2" << endl;
        break;
    case IL_ldc_i4_3:
        out << indent(1) << "ldc.i4.3" << endl;
        break;
    case IL_ldc_i4_4:
        out << indent(1) << "ldc.i4.4" << endl;
        break;
    case IL_ldc_i4_5:
        out << indent(1) << "ldc.i4.5" << endl;
        break;
    case IL_ldc_i4_6:
        out << indent(1) << "ldc.i4.6" << endl;
        break;
    case IL_ldc_i4_7:
        out << indent(1) << "ldc.i4.7" << endl;
        break;
    case IL_ldc_i4_8:
        out << indent(1) << "ldc.i4.8" << endl;
        break;
    case IL_ldc_i4_m1:
        out << indent(1) << "ldc.i4.m1" << endl;
        break;
    case IL_ldc_i4_s:
        out << indent(1) << "ldc.i4.s " << e->i << endl;
        break;
    case IL_ldc_i4:
        out << indent(1) << "ldc.i4 " << e->i << endl;
        break;
    case IL_ldc_i8:
        out << indent(1) << "ldc.i8 " << e->i << endl;
        break;
        
    case IL_ldc_r4:
        out << indent(1) << "ldc.r4 " << QByteArray::number(e->f, 'e', 16) << endl;
        break;
    case IL_ldc_r8:
        out << indent(1) << "ldc.r8 " << QByteArray::number(e->f, 'e', 16) << endl;
        break;
        
    case IL_ldnull:
        out << indent(1) << "ldnull" << endl;
        break;
        
    case IL_ldstr:
        out << indent(1) << "ldstr \"" << e->c->s << "\"" << endl;
        out << indent(1) << "call uint8* [MIC$]MIC$::strlit(string)" << endl;
        break;
        
    case IL_ldc_obj:
        // Load value type
        // TODO: convert the constructor to a byte representation, then encode it with base64, then
        // "call uint8[] [mscorlib]System.Convert::FromBase64String(string)", then use cpblk.
        // NOTE: PeLib actually would support FieldRVA fields using DotNetPELib::Field::Bytes mode, but
        // it's less immediate than literals.

        // out << "ldstr \"" << toByteArray(e->c).toBase64() << "\"" << endl
        // out << "call uint8[] [mscorlib]System.Convert::FromBase64String(string)"
        out << indent(1) << "ldobj " << typeRef(e->getType()) << endl;
        break;
        
    case IL_conv_i1:
        expression(e->lhs);
        out << indent(1) << "conv.i1" << endl;
        break;
    case IL_conv_i2:
        expression(e->lhs);
        out << indent(1) << "conv.i2" << endl;
        break;
    case IL_conv_i4:
        expression(e->lhs);
        out << indent(1) << "conv.i4" << endl;
        break;
    case IL_conv_i8:
        expression(e->lhs);
        out << indent(1) << "conv.i8" << endl;
        break;
    case IL_conv_u1:
        expression(e->lhs);
        out << indent(1) << "conv.u1" << endl;
        break;
    case IL_conv_u2:
        expression(e->lhs);
        out << indent(1) << "conv.u2" << endl;
        break;
    case IL_conv_u4:
        expression(e->lhs);
        out << indent(1) << "conv.u4" << endl;
        break;
    case IL_conv_u8:
        expression(e->lhs);
        out << indent(1) << "conv.u8" << endl;
        break;
    case IL_conv_r4:
        expression(e->lhs);
        out << indent(1) << "conv.r4" << endl;
        break;
    case IL_conv_r8:
        expression(e->lhs);
        out << indent(1) << "conv.r8" << endl;
        break;
        
    case IL_ceq:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "ceq" << endl;
        break;
    case IL_cgt:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "cgt" << endl;
        break;
    case IL_cgt_un:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "cgt.un" << endl;
        break;
    case IL_clt:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "clt" << endl;
        break;
    case IL_clt_un:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "clt.un" << endl;
        break;
        
    case IL_ldvar:
        out << indent(1) << "ldsfld " << typeRef(e->d->getType()) << " ";
        out << mangledName(e->d) << endl;
        break;
        
    case IL_ldvara:
        out << indent(1) << "ldsflda " << typeRef(e->d->getType()) << " ";
        out << mangledName(e->d) << endl;
        break;
        
    case IL_ldarg_0:
        out << indent(1) << "ldarg.0" << endl;
        break;
    case IL_ldarg_1:
        out << indent(1) << "ldarg.1" << endl;
        break;
    case IL_ldarg_2:
        out << indent(1) << "ldarg.2" << endl;
        break;
    case IL_ldarg_3:
        out << indent(1) << "ldarg.3" << endl;
        break;
    case IL_ldarg_s:
        out << indent(1) << "ldarg.s " << e->id << endl;
        break;
    case IL_ldarg:
        out << indent(1) << "ldarg " << e->id << endl;
        break;
    case IL_ldarga_s:
        out << indent(1) << "ldarga.s " << e->id << endl;
        break;
    case IL_ldarga:
        out << indent(1) << "ldarga " << e->id << endl;
        break;
        
    case IL_ldloc_0:
        out << indent(1) << "ldloc.0" << endl;
        break;
    case IL_ldloc_1:
        out << indent(1) << "ldloc.1" << endl;
        break;
    case IL_ldloc_2:
        out << indent(1) << "ldloc.2" << endl;
        break;
    case IL_ldloc_3:
        out << indent(1) << "ldloc.3" << endl;
        break;
    case IL_ldloc_s:
        out << indent(1) << "ldloc.s " << e->id << endl;
        break;
    case IL_ldloc:
        out << indent(1) << "ldloc " << e->id << endl;
        break;
    case IL_ldloca_s:
        out << indent(1) << "ldloca.s " << e->id << endl;
        break;
    case IL_ldloca:
        out << indent(1) << "ldloca " << e->id << endl;
        break;
        
    case IL_ldind_i1:
        expression(e->lhs);
        out << indent(1) << "ldind.i1" << endl;
        break;
    case IL_ldind_i2:
        expression(e->lhs);
        out << indent(1) << "ldind.i2" << endl;
        break;
    case IL_ldind_i4:
        expression(e->lhs);
        out << indent(1) << "ldind.i4" << endl;
        break;
    case IL_ldind_i8:
        expression(e->lhs);
        out << indent(1) << "ldind.i8" << endl;
        break;
    case IL_ldind_ip:
        expression(e->lhs);
        out << indent(1) << "ldind.i" << endl;
        break;
    case IL_ldind_r4:
        expression(e->lhs);
        out << indent(1) << "ldind.r4" << endl;
        break;
    case IL_ldind_r8:
        expression(e->lhs);
        out << indent(1) << "ldind.r8" << endl;
        break;
    case IL_ldind_u1:
        expression(e->lhs);
        out << indent(1) << "ldind.u1" << endl;
        break;
    case IL_ldind_u2:
        expression(e->lhs);
        out << indent(1) << "ldind.u2" << endl;
        break;
    case IL_ldind_u4:
        expression(e->lhs);
        out << indent(1) << "ldind.u4" << endl;
        break;
    case IL_ldind_u8:
        expression(e->lhs);
        out << indent(1) << "ldind.u8" << endl;
        break;
    case IL_ldind:
        expression(e->lhs);
        out << indent(1) << "ldind.ref" << endl;
        break;
        
    case IL_ldelem_i1:
        expression(e->lhs);  // Array pointer
        expression(e->rhs);  // Index
        out << indent(1) << "conv.i" << endl;  // Ensure index is native int
        out << indent(1) << "add" << endl;     // Pointer arithmetic
        out << indent(1) << "ldind.i1" << endl;
        break;
    case IL_ldelem_i2:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.2" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.i2" << endl;
        break;
    case IL_ldelem_i4:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.4" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.i4" << endl;
        break;
    case IL_ldelem_i8:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.8" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.i8" << endl;
        break;
    case IL_ldelem_ip:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "sizeof native int" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.i" << endl;
        break;
    case IL_ldelem_r4:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.4" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.r4" << endl;
        break;
    case IL_ldelem_r8:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.8" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.r8" << endl;
        break;
    case IL_ldelem_u1:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.u1" << endl;
        break;
    case IL_ldelem_u2:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.2" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.u2" << endl;
        break;
    case IL_ldelem_u4:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.4" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.u4" << endl;
        break;
    case IL_ldelem_u8:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "conv.i" << endl;
        out << indent(1) << "ldc.i4.8" << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldind.u8" << endl;
        break;
    case IL_ldelem:
        {
            Type* elemType = deref(e->getType());
            expression(e->lhs);
            expression(e->rhs);
            out << indent(1) << "conv.i" << endl;
            out << indent(1) << "sizeof " << typeRef(elemType) << endl;
            out << indent(1) << "mul" << endl;
            out << indent(1) << "add" << endl;
            if (elemType->isSUO())
                out << indent(1) << "ldobj " << typeRef(elemType) << endl;
            else
                out << indent(1) << "ldind.ref" << endl;
        }
        break;
    case IL_ldelema:
        {
            Type* elemType = deref(e->getType()->getType());
            expression(e->lhs);
            expression(e->rhs);
            out << indent(1) << "conv.i" << endl;
            out << indent(1) << "sizeof " << typeRef(elemType) << endl;
            out << indent(1) << "mul" << endl;
            out << indent(1) << "add" << endl;
        }
        break;
        
    case IL_ldfld:
        expression(e->lhs);
        out << indent(1) << "ldfld " << typeRef(e->d->getType()) << " ";
        out << mangledName(e->d->outer) << "::'" << e->d->name << "'" << endl;
        break;
    case IL_ldflda:
        expression(e->lhs);
        out << indent(1) << "ldflda " << typeRef(e->d->getType()) << " ";
        out << mangledName(e->d->outer) << "::'" << e->d->name << "'" << endl;
        break;
        
    case IL_ldproc:
        out << indent(1) << "ldftn void " << mangledName(e->d) << "()" << endl;
        break;
        
    case IL_ldmeth:
        // Load method from vtable
        expression(e->lhs);
        out << indent(1) << "ldfld void* " << mangledName(e->lhs->getType()->decl) << "::class$" << endl;
        out << indent(1) << "ldfld void* " << mangledName(e->lhs->getType()->decl) << "_Class::'" << e->d->name << "'" << endl;
        break;
        
    case IL_castptr:
        expression(e->lhs);
        out << indent(1) << "castclass " << typeRef(e->d->getType()) << endl;
        break;
        
    case IL_call:
    case IL_callinst:
        emitCall(e);
        break;
        
    case IL_callvirt:
        {
            // Virtual call through vtable
            expression(e->lhs);  // Object
            out << indent(1) << "dup" << endl;
            out << indent(1) << "ldfld void* " << mangledName(e->lhs->getType()->decl) << "::class$" << endl;
            out << indent(1) << "ldfld void* " << mangledName(e->lhs->getType()->decl) << "_Class::'" << e->d->name << "'" << endl;
            
            // Push arguments
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for (int i = 0; i < args.size(); i++)
            {
                expression(args[i]);
            }
            
            out << indent(1) << "calli " << typeRef(e->d->getType()) << "(";
            out << typeRef(e->lhs->getType());
            DeclList params = e->d->getParams();
            for (int i = 1; i < params.size(); i++)  // Skip 'self'
            {
                out << ", " << typeRef(params[i]->getType());
            }
            out << ")" << endl;
        }
        break;
        
    case IL_calli:
        {
            // Indirect call
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for (int i = 0; i < args.size(); i++)
            {
                expression(args[i]);
            }
            
            expression(e->lhs);  // Function pointer
            
            out << indent(1) << "calli " << typeRef(e->getType()) << "(";
            // Need to determine parameter types from function pointer type
            out << ")" << endl;
        }
        break;
        
    case IL_callmi:
        {
            // Call through typebound procedure
            expression(e->lhs);  // Typebound proc struct
            out << indent(1) << "dup" << endl;
            out << indent(1) << "ldfld void* " << typeRef(e->lhs->getType()) << "::self" << endl;
            
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            for (int i = 0; i < args.size(); i++)
            {
                expression(args[i]);
            }
            
            out << indent(1) << "ldfld void* " << typeRef(e->lhs->getType()) << "::proc" << endl;
            out << indent(1) << "calli " << typeRef(e->getType()) << "(void*"; // TODO: interface method support
            // Add parameter types
            out << ")" << endl;
        }
        break;
        
    case IL_newobj:
        {
            Type* t = deref(e->getType()->getType());
            out << indent(1) << "sizeof " << typeRef(t) << endl;
            out << indent(1) << "call native int [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(int32)" << endl;
            
            if (t->objectInit)
            {
                out << indent(1) << "dup" << endl;
                out << indent(1) << "ldc.i4.1" << endl;
                out << indent(1) << "call void " << mangledName(t->decl) << "_init(native int, int32)" << endl;
            }
        }
        break;
        
    case IL_newarr:
        {
            Type* et = deref(e->d->getType());
            expression(e->lhs);  // Array size
            out << indent(1) << "dup" << endl;  // Keep size for later init
            out << indent(1) << "sizeof " << typeRef(et) << endl;
            out << indent(1) << "mul" << endl;
            out << indent(1) << "call native int [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(int32)" << endl;
            
            if (et->objectInit)
            {
                out << indent(1) << "dup" << endl;     // Duplicate pointer
                out << indent(1) << "ldloc.s size$" << endl;  // Get saved size
                out << indent(1) << "call void " << mangledName(et->decl) << "_init(native int, int32)" << endl;
            }
        }
        break;
        
    case IL_initobj:
        {
            expression(e->lhs);  // Address
            out << indent(1) << "initobj " << typeRef(e->d->getType()) << endl;
            
            Type* t = deref(e->d->getType());
            if (t->objectInit)
            {
                expression(e->lhs);
                out << indent(1) << "ldc.i4.1" << endl;
                out << indent(1) << "call void " << mangledName(t->decl) << "_init(native int, int32)" << endl;
            }
        }
        break;
        
    case IL_dup:
        expression(e->lhs);
        out << indent(1) << "dup" << endl;
        break;
        
    case IL_nop:
        out << indent(1) << "nop" << endl;
        break;
        
    case IL_ptroff:
        expression(e->lhs);
        expression(e->rhs);
        out << indent(1) << "sizeof " << typeRef(e->d->getType()) << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        break;
        
    case IL_iif:
        {
            // Conditional expression (ternary operator)
            Expression* if_ = e->e;
            Q_ASSERT(if_ && if_->kind == IL_if && if_->next->kind == IL_then && 
                     if_->next->next->kind == IL_else && if_->next->next->next == 0);
            
            QByteArray falseLabel = "IL_IIF_FALSE_" + QByteArray::number(labelCounter++);
            QByteArray endLabel = "IL_IIF_END_" + QByteArray::number(labelCounter++);
            
            expression(if_->lhs);  // Condition
            out << indent(1) << "brfalse.s " << falseLabel << endl;
            expression(if_->next->lhs);  // Then value
            out << indent(1) << "br.s " << endLabel << endl;
            out << indent(1) << falseLabel << ":" << endl;
            expression(if_->next->next->lhs);  // Else value
            out << indent(1) << endLabel << ":" << endl;
        }
        break;
        
    case IL_sizeof:
        out << indent(1) << "sizeof " << typeRef(e->d->getType()) << endl;
        break;
        
    case IL_newvla:
        // Variable length array - allocate on stack
        expression(e->lhs);  // Size
        out << indent(1) << "localloc" << endl;
        break;
        
    case IL_isinst:
        expression(e->lhs);
        out << indent(1) << "isinst " << typeRef(e->d->getType()) << endl;
        break;
        
    case IL_abs:
        expression(e->lhs);
        if (deref(e->lhs->getType())->isInteger())
            out << indent(1) << "call int32 [mscorlib]System.Math::Abs(int32)" << endl;
        else
            out << indent(1) << "call float64 [mscorlib]System.Math::Abs(float64)" << endl;
        break;
        
    default:
        Q_ASSERT(false);
    }
}

void CilAsmGen::emitCall(Expression* e)
{
    QList<Expression*> args;
    collectArgs(e->rhs, args);
    
    // Push arguments
    for (int i = 0; i < args.size(); i++)
    {
        expression(args[i]);
    }
    
    out << indent(1) << "call " << typeRef(e->d->getType()) << " " << mangledName(e->d) << "(";
    
    DeclList params = e->d->getParams();
    for (int i = 0; i < params.size(); i++)
    {
        if (i != 0)
            out << ", ";
        out << typeRef(params[i]->getType());
    }
    
    out << ")" << endl;
}

void CilAsmGen::constValue(Constant* c)
{
    if (c == 0)
    {
        out << "int32(0)";
        return;
    }
    
    switch (c->kind)
    {
    case Constant::D:
        out << "float64(" << c->d << ")";
        break;
    case Constant::I:
        out << "int32(" << c->i << ")";
        break;
    case Constant::S:
        out << "\"" << c->s << "\"";
        break;
    case Constant::B:
        {
            // Byte array initialization
            const ByteString* ba = c->b;
            out << "bytearray (";
            for (int i = 0; i < ba->len; i++)
            {
                if (i != 0)
                    out << " ";
                out << QByteArray::number(quint8(ba->b[i]), 16).rightJustified(2, '0').toUpper();
            }
            out << ")";
        }
        break;
    case Constant::R:
        constValue(c->r->c);
        break;
    case Constant::C:
        // Compound constant - needs special handling
        out << "/* compound constant */";
        break;
    }
}

void CilAsmGen::visitMetaDecl(Declaration* d)
{
    // Generate vtable structure for objects
    const QByteArray className = mangledName(d);
    Type* t = deref(d->getType());
    
    out << "// Vtable for " << className << endl;
    out << ".class value explicit ansi sealed " << className << "_Class" << endl;
    out << "       extends [mscorlib]System.ValueType" << endl;
    out << "{" << endl;
    
    if (t->getType())
    {
        out << indent(1) << ".field void* super" << endl;
    }
    else
    {
        out << indent(1) << ".field void* super" << endl;
    }
    
    DeclList methods = t->getMethodTable();
    foreach (Declaration* p, methods)
    {
        Q_ASSERT(p->kind == Declaration::Procedure && !p->forward);
        out << indent(1) << ".field void* " << p->name << endl;
    }
    
    out << "}" << endl << endl;
    
    // Generate static vtable instance
    out << ".field static valuetype " << className << "_Class " << className << "_class" << endl << endl;
}

void CilAsmGen::emitInitializer(Type* t)
{
    t = deref(t);
    const QByteArray typeName = mangledName(t->decl);
    
    out << ".method static void " << typeName << "_init(native int obj, int32 n) cil managed" << endl;
    out << "{" << endl;
    out << indent(1) << ".maxstack 4" << endl;
    out << indent(1) << ".locals init (int32 i)" << endl;
    
    out << indent(1) << "ldc.i4.0" << endl;
    out << indent(1) << "stloc.0" << endl;
    
    out << indent(1) << "IL_LOOP:" << endl;
    out << indent(1) << "ldloc.0" << endl;
    out << indent(1) << "ldarg.1" << endl;
    out << indent(1) << "bge.s IL_END" << endl;
    
    if (t->kind == Type::Object)
    {
        out << indent(1) << "ldarg.0" << endl;
        out << indent(1) << "ldloc.0" << endl;
        out << indent(1) << "sizeof " << typeRef(t) << endl;
        out << indent(1) << "mul" << endl;
        out << indent(1) << "add" << endl;
        out << indent(1) << "ldsflda valuetype " << typeName << "_Class " << typeName << "_class" << endl;
        out << indent(1) << "stfld void* " << typeName << "::class$" << endl;
    }
    
    // Initialize fields
    foreach (Declaration* field, t->subs)
    {
        if (field->kind != Declaration::Field)
            continue;
        
        Type* tt = deref(field->getType());
        if (tt->isSO())
        {
            out << indent(1) << "ldarg.0" << endl;
            out << indent(1) << "ldloc.0" << endl;
            out << indent(1) << "sizeof " << typeRef(t) << endl;
            out << indent(1) << "mul" << endl;
            out << indent(1) << "add" << endl;
            out << indent(1) << "ldflda " << typeRef(tt) << " " << typeName << "::'" << field->name << "'" << endl;
            out << indent(1) << "ldc.i4.1" << endl;
            out << indent(1) << "call void " << mangledName(tt->decl) << "_init(native int, int32)" << endl;
        }
    }
    
    out << indent(1) << "ldloc.0" << endl;
    out << indent(1) << "ldc.i4.1" << endl;
    out << indent(1) << "add" << endl;
    out << indent(1) << "stloc.0" << endl;
    out << indent(1) << "br.s IL_LOOP" << endl;
    
    out << indent(1) << "IL_END:" << endl;
    out << indent(1) << "ret" << endl;
    out << "}" << endl << endl;
}

void CilAsmGen::emitSoapInit(const QByteArray& name, Type* t, bool isStatic)
{
    t = deref(t);
    
    if (t->kind == Type::Pointer && t->pointerInit)
    {
        if (isStatic)
            out << indent(1) << "ldnull" << endl;
        else
            out << indent(1) << "ldloca " << name << endl;
        
        out << indent(1) << "stfld " << typeRef(t) << " " << name << endl;
    }
    else if (t->isSUOA() && t->pointerInit)
    {
        if (isStatic)
            out << indent(1) << "ldsflda " << typeRef(t) << " " << name << endl;
        else
            out << indent(1) << "ldloca " << name << endl;
        
        out << indent(1) << "initobj " << typeRef(t) << endl;
    }
    
    // Handle objects with initializers
    if (t->objectInit && t->isSO())
    {
        if (isStatic)
            out << indent(1) << "ldsflda " << typeRef(t) << " " << name << endl;
        else
            out << indent(1) << "ldloca " << name << endl;
        
        out << indent(1) << "ldc.i4.1" << endl;
        out << indent(1) << "call void " << mangledName(t->decl) << "_init(native int, int32)" << endl;
    }
}

void CilAsmGen::emitMaxStack()
{
    // TODO
    out << indent(1) << ".maxstack 8" << endl;
}

Type* CilAsmGen::deref(Type* t)
{
    if (t && t->kind == Type::NameRef)
        return deref(t->getType());
    else if (t)
        return t;
    else
        return mdl->getBasicType(Type::Undefined);
}

