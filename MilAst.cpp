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

#include "MilAst.h"
#include "MicSymbol.h"
#include "MilTokenType.h"
using namespace Mil;

static void addTypeDecl(Declaration* globals, Type* t, const QByteArray& name)
{
    Declaration* res = new Declaration();
    res->kind = Declaration ::TypeDecl;
    res->name = Mic::Symbol::getSymbol(name);
    res->public_ = true;
    res->outer = globals;
    res->setType(t);
    globals->appendSub(res);
}

AstModel::AstModel()
{
    for( int i = 0; i < Type::MaxBasicType; i++ )
    {
        Type* t = new Type();
        t->kind = (Type::Kind)i;
        t->owned = true;
        basicTypes[i] = t;
    }
    addTypeDecl(&globals, basicTypes[Type::Any], "ANY");
    addTypeDecl(&globals, basicTypes[Type::Any], "any");
    addTypeDecl(&globals, basicTypes[Type::BOOL], "BOOL");
    addTypeDecl(&globals, basicTypes[Type::BOOL], "bool");
    addTypeDecl(&globals, basicTypes[Type::CHAR], "CHAR");
    addTypeDecl(&globals, basicTypes[Type::CHAR], "char");
    addTypeDecl(&globals, basicTypes[Type::INT8], "INT8");
    addTypeDecl(&globals, basicTypes[Type::INT8], "int8");
    addTypeDecl(&globals, basicTypes[Type::INT8], "I1");
    addTypeDecl(&globals, basicTypes[Type::INT8], "i1");
    addTypeDecl(&globals, basicTypes[Type::INT16], "INT16");
    addTypeDecl(&globals, basicTypes[Type::INT16], "int16");
    addTypeDecl(&globals, basicTypes[Type::INT16], "I2");
    addTypeDecl(&globals, basicTypes[Type::INT16], "i2");
    addTypeDecl(&globals, basicTypes[Type::INT32], "INT32");
    addTypeDecl(&globals, basicTypes[Type::INT32], "int32");
    addTypeDecl(&globals, basicTypes[Type::INT32], "I4");
    addTypeDecl(&globals, basicTypes[Type::INT32], "i4");
    addTypeDecl(&globals, basicTypes[Type::INT64], "INT64");
    addTypeDecl(&globals, basicTypes[Type::INT64], "int64");
    addTypeDecl(&globals, basicTypes[Type::INT64], "I8");
    addTypeDecl(&globals, basicTypes[Type::INT64], "i8");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "UINT8");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "uint8");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "U1");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "u1");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "UINT16");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "uint16");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "U2");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "u2");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "UINT32");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "uint32");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "U4");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "u4");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "UINT64");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "uint64");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "U8");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "u8");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "FLOAT32");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "float32");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "R4");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "r4");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "FLOAT64");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "float64");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "R8");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "r8");
    addTypeDecl(&globals, basicTypes[Type::INTPTR], "INTPTR");
    addTypeDecl(&globals, basicTypes[Type::INTPTR], "intptr");
}

AstModel::~AstModel()
{
    clear();
    for( int i = 0; i < Type::MaxBasicType; i++ )
        delete basicTypes[i];
}

void AstModel::clear()
{
    foreach( Declaration* module, modules )
        delete module;
    modules.clear();
}

Declaration*AstModel::findModuleByName(const QByteArray& name) const
{
    // TODO: consider generic module instances with e.g. a deterministic suffix
    foreach( Declaration* module, modules )
    {
        if( module->name.constData() == name.constData() )
            return module;
    }
    return 0;
}

bool AstModel::addModule(Declaration* module)
{
    Q_ASSERT(module);
    if( findModuleByName(module->name) )
        return false;
    //else
    modules.append(module);
    return true;
}

Type* AstModel::getBasicType(quint8 k) const
{
    if( k < Type::MaxBasicType )
        return basicTypes[k];
    else
        return basicTypes[0];
}

Node::~Node()
{
    if( type && ownstype )
        delete type;
}

void Node::setType(Type* t)
{
    Q_ASSERT(type == 0);
    type = t;
    if( t && !t->owned )
    {
        ownstype = true;
        t->owned = true;
        if( meta == D && t->decl == 0 )
            t->decl = static_cast<Declaration*>(this);
    }
}

Declaration::~Declaration()
{
    if( kind == ConstDecl && c )
        delete c;
    if( subs )
        delete subs;
    if( body )
        delete body;
    if( next )
        delete next;
}

void Declaration::appendSub(Declaration* d)
{
    if( subs == 0 )
        subs = d;
    else
        append(subs,d);
}

Declaration* Declaration::findSubByName(const QByteArray& name) const
{
    Declaration* d = subs;
    while( d && d->name.constData() != name.constData() )
        d = d->next;
    if( d && d->name.constData() == name.constData())
        return d;
    else
        return 0;
}

QList<Declaration*> Declaration::getParams() const
{
    QList<Declaration*> res;
    Declaration* d = subs;
    while( d )
    {
        if( d->kind == Declaration::ParamDecl )
            res << d;
        d = d->next;
    }
    return res;
}

QList<Declaration*> Declaration::getLocals() const
{
    QList<Declaration*> res;
    Declaration* d = subs;
    while( d )
    {
        if( d->kind == Declaration::LocalDecl )
            res << d;
        d = d->next;
    }
    return res;
}

int Declaration::indexOf(Declaration* ref) const
{
    Declaration* d = subs;
    int i = 0;
    while( d )
    {
        if( d == ref )
            return i;
        d = d->next;
        i++;
    }
    return -1;
}

void Declaration::append(Declaration* list, Declaration* next)
{
    while( list && list->next )
        list = list->next;
    if( list )
    {
        Q_ASSERT(list->next == 0);
        list->next = next;
    }
}

QByteArray Declaration::toPath() const
{
    QByteArray res;
    if( (kind == Procedure && typebound) || kind == Field || kind == LocalDecl || kind == ParamDecl )
    {
        res = "." + name;
    }else if( kind != Module )
    {
        res = name;
        if( outer && outer->kind != Module )
            res = "$" + res;
    }else
        res = name + "!";
    if( outer )
        return outer->toPath() + res;
    else
        return res;
}

Declaration*Declaration::forwardToProc() const
{
    if( kind == Procedure && forward )
        return forwardTo->forwardToProc();
    else
        return const_cast<Declaration*>(this);
}

Expression::~Expression()
{
    if( (kind == Tok_LDOBJ || kind == Tok_LDSTR) && c != 0 )
        delete c;
    else if( (kind == Tok_IIF || kind == Tok_THEN || kind == Tok_ELSE ) && e != 0 )
        delete e;
    else if( rhs != 0 && rhs->kind == Expression::Argument )
        delete rhs;
    if( next )
        delete next;
}

void Expression::append(Expression* e)
{
    if( next == 0 )
    {
        next = e;
        return;
    }
    Expression* l = next;
    while( l && l->next )
        l = l->next;
    Q_ASSERT( l && l->next == 0 );
    l->next = e;
}

Component::~Component()
{
    if( c )
        delete c;
    if( next )
        delete next;
}

Constant::~Constant()
{
    switch( kind )
    {
    case S:
        free(s);
        break;
    case B:
        delete b;
        break;
    case C:
        delete c;
        break;
    }
}

ComponentList::~ComponentList()
{
    if( c )
        delete c;
    if( type )
        delete type;
}

ByteString::~ByteString()
{
    if( b )
        free(b);
}

Type::~Type()
{
    if( kind == NameRef && quali )
        delete quali;
    foreach( Declaration* sub, subs )
        delete sub;
}

Declaration*Type::findSubByName(const QByteArray& name, bool recursive) const
{
    for( int i = 0; i < subs.size(); i++ )
    {
        if( subs[i]->name.constData() == name.constData() )
            return subs[i];
    }
    if( getType() && recursive )
        return getType()->findSubByName(name,recursive);
    return 0;
}

Type*Type::deref() const
{
    if( kind == NameRef && type )
        return type->deref();
    else
        return const_cast<Type*>(this);
}

bool Type::isPtrToArray() const
{
    if( kind == Pointer )
    {
        Type* base = getType();
        if( base )
            base = base->deref();
        return base->kind == Array;
    }else
        return false;
}

Statement::~Statement()
{
    if( body )
        delete body;
    if( (kind == ExprStat || kind == Tok_IF || kind == Tok_SWITCH || kind == Tok_CASE ||
         kind == Tok_REPEAT || kind == Tok_WHILE) && e )
        delete e;
    if( next )
        delete next;
}

void Statement::append(Statement* s)
{
    if( next == 0 )
    {
        next = s;
        return;
    }
    Statement* l = next;
    while( l && l->next )
        l = l->next;
    Q_ASSERT( l && l->next == 0 );
    l->next = s;
}

