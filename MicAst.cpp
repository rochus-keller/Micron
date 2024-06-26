/*
** Copyright (C) 2024 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the Micron language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include "MicAst.h"
#include "MicToken.h"
#include <limits>
using namespace Mic;


AstModel::AstModel():universe(0),helper(0),helperId(0)
{
    openScope(0);

    types[BasicType::Undefined] = addType(BasicType::Undefined,1);
    types[BasicType::NoType] = addType(BasicType::NoType,1);
    types[BasicType::String] = addType(BasicType::String,1);
    types[BasicType::Nil] = addType(BasicType::Nil,1);
    types[BasicType::Any] = addType(BasicType::Any,1);

    types[BasicType::BOOLEAN] = addType("BOOLEAN", BasicType::BOOLEAN, 1 );
    types[BasicType::CHAR] = addType("CHAR", BasicType::CHAR, 1 );
    types[BasicType::UINT8] = addType("UINT8", BasicType::UINT8, 1 );
    addTypeAlias("BYTE", types[BasicType::UINT8] );
    types[BasicType::INT8] = addType("INT8", BasicType::INT8, 1 );
    types[BasicType::INT16] = addType("INT16", BasicType::INT16, 2 );
    types[BasicType::UINT16] = addType("UINT16", BasicType::UINT16, 2 );
    types[BasicType::INT32] = addType("INT32", BasicType::INT32, 4 );
    types[BasicType::UINT32] = addType("UINT32", BasicType::UINT32, 4 );
    types[BasicType::INT64] = addType("INT64", BasicType::INT64, 8 );
    types[BasicType::UINT64] = addType("UINT64", BasicType::UINT64, 8 );
    types[BasicType::REAL] = addType("REAL", BasicType::REAL, 4 );
    addTypeAlias("FLT32", types[BasicType::REAL] );
    types[BasicType::LONGREAL] = addType("LONGREAL", BasicType::LONGREAL, 8 );
    addTypeAlias("FLT64", types[BasicType::LONGREAL] );
    types[BasicType::SET] = addType("SET", BasicType::SET, 4 );
    types[BasicType::SYMBOL] = addType("SYMBOL",BasicType::SYMBOL,1);

    addTypeAlias("SHORTINT", types[BasicType::INT16] ); // TODO: variable type
    addTypeAlias("INTEGER", types[BasicType::INT32] );
    addTypeAlias("LONGINT", types[BasicType::INT64] );

    addBuiltin("ABS", Builtin::ABS);
    addBuiltin("CAP", Builtin::CAP);
    addBuiltin("BITAND", Builtin::BITAND);
    addBuiltin("BITASR", Builtin::BITASR);
    addBuiltin("BITNOT", Builtin::BITNOT);
    addBuiltin("BITOR", Builtin::BITOR);
    addBuiltin("BITS", Builtin::BITS);
    addBuiltin("BITSHL", Builtin::BITSHL);
    addBuiltin("BITSHR", Builtin::BITSHR);
    addBuiltin("BITXOR", Builtin::BITXOR);
    addBuiltin("CHR", Builtin::CHR);
    addBuiltin("DEFAULT", Builtin::DEFAULT);
    addBuiltin("FLOOR", Builtin::FLOOR);
    addBuiltin("FLT", Builtin::FLT);
    addBuiltin("GETENV", Builtin::GETENV);
    addBuiltin("LEN", Builtin::LEN);
    addBuiltin("LONG", Builtin::LONG);
    addBuiltin("MAX", Builtin::MAX);
    addBuiltin("MIN", Builtin::MIN);
    addBuiltin("ODD", Builtin::ODD);
    addBuiltin("ORD", Builtin::ORD);
    addBuiltin("SHORT", Builtin::SHORT);
    addBuiltin("SIGNED", Builtin::SIGNED);
    addBuiltin("SIZE", Builtin::SIZE);
    addBuiltin("STRLEN", Builtin::STRLEN);
    addBuiltin("UNSIGNED", Builtin::UNSIGNED);
    addBuiltin("VARARG", Builtin::VARARG);
    addBuiltin("VARARGS", Builtin::VARARGS);
    addBuiltin("ASSERT", Builtin::ASSERT);
    addBuiltin("DEC", Builtin::DEC);
    addBuiltin("DISPOSE", Builtin::DISPOSE);
    addBuiltin("EXCL", Builtin::EXCL);
    addBuiltin("HALT", Builtin::HALT);
    addBuiltin("INC", Builtin::INC);
    addBuiltin("INCL", Builtin::INCL);
    addBuiltin("NEW", Builtin::NEW);
    addBuiltin("PCALL", Builtin::PCALL);
    addBuiltin("PRINT", Builtin::PRINT);
    addBuiltin("PRINTLN", Builtin::PRINTLN);
    addBuiltin("RAISE", Builtin::RAISE);
    addBuiltin("SETENV", Builtin::SETENV);
}

void AstModel::openScope(Declaration* scope)
{
    if( scope == 0 )
    {
        scope = new Declaration();
        scope->mode = Declaration::Scope;
        scope->type = 0;
        scope->next = 0;
    }
    scope->scope = true;
    scopes.push_back(scope);
}

Declaration* AstModel::closeScope(bool takeMembers)
{
    Declaration* res = 0;

    if( takeMembers )
    {
        res = scopes.back()->link;
        scopes.back()->link = 0;
    }

    // TODO: delete topScope and successors

    scopes.pop_back();
    return res;
}

Declaration*AstModel::addDecl(const QByteArray& name, bool* doublette)
{
    if( doublette )
        *doublette = false;
    Declaration* scope = scopes.back();
    Declaration** obj = &scope->link;

    while(*obj != 0 && (*obj)->name.constData() != name.constData() )
        obj = &((*obj)->next);
    if((*obj) == 0 )
    {
        Declaration* decl = new Declaration();
        decl->link = 0;
        decl->next = 0;
        decl->name = name;
        if( scope->mode != Declaration::Scope )
            decl->outer = scope;
        *obj = decl;
        return decl;
    }else
    {
        if( doublette  )
            *doublette = true;
        return (*obj);
    }
}

Declaration*AstModel::addHelper()
{
    Declaration* decl = new Declaration();
    decl->link = 0;
    decl->next = helper;
    helper = decl;
    decl->name = "$" + QByteArray::number(++helperId);
    decl->outer = 0;
    return decl;
}

void AstModel::removeDecl(Declaration* del)
{
    Declaration* scope = scopes.back();
    Declaration** obj = &scope->link;
    while( (*obj) != del )
        obj = &((*obj)->next);
    obj = &(del->next);
    del->next = 0;
    delete del;
}

Declaration*AstModel::findDecl(const QByteArray& id) const
{
    for( int i = scopes.size() - 1; i >= 0; i-- )
    {
        Declaration* cur = scopes[i]->link;
        while( cur != 0 )
        {
            if( cur->name.constData() == id.constData() )
                return cur;
            else
                cur = cur->next;
        }
    }
    return 0;
}

Declaration*AstModel::findDecl(Declaration* import, const QByteArray& id) const
{
    if( import == 0 )
        return findDecl(id);
    Q_ASSERT(import && import->mode == Declaration::Import);
    Declaration* obj = import->link;
    while( obj != 0 && obj->name.constData() != id.constData() )
        obj = obj->next;
    return obj;
}

QByteArray AstModel::getTempName()
{
    return Token::getSymbol("$" + QByteArray::number(++helperId));
}

Type*AstModel::addType(int form, int size)
{
    Type* t = new Type();
    t->form = form;
    return t;
}

Type*AstModel::addType(const QByteArray& name, int form, int size)
{
    Type* t = addType(form, size);
    addTypeAlias(name.toUpper(), t);
    addTypeAlias(name.toLower(), t);
    return t;
}

void AstModel::addTypeAlias(const QByteArray& name, Type* t)
{
    Declaration* d = addDecl(Token::getSymbol(name.toUpper()));
    d->mode = Declaration::TypeDecl;
    d->type = t;
    if( t->decl == 0 )
        t->decl = d;
    Declaration* d2 = addDecl(Token::getSymbol(name.toLower()));
    d2->mode = Declaration::TypeDecl;
    d2->type = t;
}

void AstModel::addBuiltin(const QByteArray& name, Builtin::Type t)
{
    Declaration* d = addDecl(Token::getSymbol(name.toUpper()));
    d->mode = Declaration::Builtin;
    d->type = types[BasicType::NoType];
    d->level = t;
    d = addDecl(Token::getSymbol(name.toLower()));
    d->mode = Declaration::Builtin;
    d->type = types[BasicType::NoType];
    d->level = t;
}

Declaration*Type::findField(const QByteArray& name) const
{
    // TODO: search also through inlined records
    foreach( Declaration* d, subs)
    {
        if(d->name.constData() == name.constData())
            return d;
    }
    return 0;
}

QPair<int, int> Type::getFieldCount() const
{
    QPair<int, int> res;
    foreach( Declaration* d, subs)
    {
        if( d->mode == Declaration::Field )
            res.first++;
        else if( d->mode == Declaration::Variant )
            res.second++;
        d = d->next;
    }
    return res;
}

QVariant BasicType::getMax(BasicType::Type t)
{
    switch( t )
    {
    case BOOLEAN:
        return true;
    case CHAR:
    case UINT8:
        return std::numeric_limits<quint8>::max();
    case UINT16:
        return std::numeric_limits<quint16>::max();
    case UINT32:
        return std::numeric_limits<quint32>::max();
    case UINT64:
        return std::numeric_limits<quint64>::max();
    case SET:
        return 31;
    case INT8:
        return std::numeric_limits<qint8>::max();
    case INT16:
        return std::numeric_limits<qint16>::max();
    case INT32:
        return std::numeric_limits<qint32>::max();
    case INT64:
        return std::numeric_limits<qint64>::max();
    case REAL:
        return std::numeric_limits<float>::max();
    case LONGREAL:
        return std::numeric_limits<double>::max();
    }
    return QVariant();
}

QVariant BasicType::getMin(BasicType::Type t)
{
    switch( t )
    {
    case BOOLEAN:
        return false;
    case CHAR:
    case UINT8:
        return std::numeric_limits<quint8>::min();
    case UINT16:
        return std::numeric_limits<quint16>::min();
    case UINT32:
        return std::numeric_limits<quint32>::min();
    case UINT64:
        return std::numeric_limits<quint64>::min();
    case SET:
        return 31;
    case INT8:
        return std::numeric_limits<qint8>::min();
    case INT16:
        return std::numeric_limits<qint16>::min();
    case INT32:
        return std::numeric_limits<qint32>::min();
    case INT64:
        return std::numeric_limits<qint64>::min();
    case REAL:
        return std::numeric_limits<float>::min();
    case LONGREAL:
        return std::numeric_limits<double>::min();
    }
    return QVariant();
}


QList<Declaration*> Declaration::getParams() const
{
    Declaration* d = link;
    QList<Declaration*> res;
    while( d && d->mode == Declaration::ParamDecl )
    {
        res << d;
        d = d->next;
    }
    return res;
}

int Declaration::getIndexOf(Declaration* ref) const
{
    int idx = -1;
    Declaration* d = link;
    while( d )
    {
        if( d->mode == ref->mode )
            idx++;
        if( d == ref )
            return idx;
        d = d->next;
    }
    return -1;
}


bool Value::isCallable() const
{
    switch( mode )
    {
    case Declaration::Builtin:
    case Declaration::Procedure:
        return true;

    case Declaration::Field:
    case Declaration::Variant:
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
    case Value::Val:
        return type && type->form == Type::Proc;

    default:
        return false;
    }
}
