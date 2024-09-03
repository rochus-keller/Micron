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
#include <QtDebug>
using namespace Mic;

Declaration AstModel::globalScope;
Type* AstModel::types[BasicType::Max] = {0};


AstModel::AstModel():helper(0),helperId(0)
{
    openScope(&globalScope);

    if( globalScope.mode == Declaration::NoMode )
    {
        globalScope.mode = Declaration::Scope;
        types[BasicType::Undefined] = newType(BasicType::Undefined,1);
        types[BasicType::NoType] = newType(BasicType::NoType,1);
        types[BasicType::String] = newType(BasicType::String,1);
        types[BasicType::Nil] = newType(BasicType::Nil,1);
        types[BasicType::Any] = newType(BasicType::Any,1);

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
}

AstModel::~AstModel()
{
    for( int i = 1; i < scopes.size(); i++ ) // start with 1, 0 is globalScope
        delete scopes[i];
    scopes.clear();
    if( helper )
        delete helper;
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
        delete scopes.back();
    }else if( scopes.back()->mode == Declaration::Module )
    {
        Q_ASSERT(scopes.back()->next == 0);
        scopes.back()->next = helper;
        helper = 0;
    }
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
    while( (*obj) && (*obj) != del )
        obj = &((*obj)->next);
    *obj = del->next;
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

void AstModel::cleanupGlobals()
{
    if( globalScope.mode == Declaration::Scope )
    {
        if( globalScope.next )
            delete globalScope.next;
        globalScope.next = 0;
        if( globalScope.link )
            delete globalScope.link;
        globalScope.link = 0;
        globalScope.mode = Declaration::NoMode;
        for( int i = 0; i < BasicType::Max; i++ )
        {
            delete types[i];
            types[i] = 0;
        }
    }
}

Type*AstModel::newType(int form, int size)
{
    Type* t = new Type();
    t->form = form;
    return t;
}

Type*AstModel::addType(const QByteArray& name, int form, int size)
{
    Type* t = newType(form, size);
    addTypeAlias(name, t);
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
    d->id = t;
    d = addDecl(Token::getSymbol(name.toLower()));
    d->mode = Declaration::Builtin;
    d->type = types[BasicType::NoType];
    d->id = t;
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

Type::~Type()
{
    // all anonymous types are resolved, therefore base is no longer owned by a type
    //if( base )
    //    delete base;
    if( form != ConstEnum )
        for( int i = 0; i < subs.size(); i++ )
            delete subs[i];
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

const char* Declaration::s_mode[] = {
    "NoMode",
    "Scope", "Module", "TypeDecl", "Builtin", "ConstDecl", "Import", "Field", "Variant",
    "VarDecl", "LocalDecl",
    "Procedure", "ForwardDecl", "ParamDecl"
};

Declaration::~Declaration()
{
    if( next )
        delete next;
    if( link
            && mode != Declaration::Import  // imports are just referenced, not owned
            && !alias   // the original is deleted elswhere
            )
        delete link;
    if( type && ownstype )
        delete type;
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

bool Expression::isConst() const
{
    switch(kind)
    {
    case Addr:
    case Deref:
    case LocalVar:
    case ModuleVar:
    case Param:
        return false;
    case ConstDecl:
    case Literal:
        return true;
    }

    if( kind == Call )
    {
        if( lhs->kind == ProcDecl )
        {
            Declaration* d = lhs->val.value<Declaration*>();
            if( !d->invar )
                return false;
        }else if( lhs->kind == Builtin )
        {
            // TODO
        }else
            return false;

        ExpList args = val.value<ExpList>();

        for( int i = 0; i < args.size(); i++ )
            if( !args[i]->isConst() )
                return false;
        return true;
    }

    if( lhs && !lhs->isConst() )
        return false;
    if( rhs && !rhs->isConst() )
        return false;
    return true;
}

bool Expression::isLiteral() const
{
    return kind == Literal || kind == ConstDecl;
}

QVariant Expression::getLiteralValue() const
{
    if( kind == Literal )
        return val;
    else if( kind == ConstDecl )
        return val.value<Declaration*>()->data;
    else
        return QVariant();
}

DeclList Expression::getFormals() const
{
    if( kind == ProcDecl )
        return val.value<Declaration*>()->getParams();
    else if( type->form == Type::Proc )
        return type->subs;
    else
        return DeclList();
}

bool Expression::isLvalue() const
{
    // no, we need this function as a barrier for byVal propagation
    //if( byVal )
    //    return false;
    return kind == LocalVar || kind == Param || kind == ModuleVar ||
            kind == Select || kind == Index || kind == Deref;
}

void Expression::setByVal()
{
    // go back the desig leaving a ref to type on the stack and mark it to leave a value instead
    Expression* cur = this;
    while( cur && !cur->isLvalue() )
        cur = cur->lhs;
    if( cur )
        cur->byVal = true;
}

Expression*Expression::createFromToken(quint16 tt, const RowCol& rc)
{
    Kind k = Invalid;
    if( tt == Tok_Eq ) {
        k = Eq;
    } else if( tt == Tok_Hash ) {
        k = Neq;
    } else if( tt == Tok_Lt ) {
        k = Lt;
    } else if( tt == Tok_Leq ) {
        k = Leq;
    } else if( tt == Tok_Gt ) {
        k = Gt;
    } else if( tt == Tok_Geq ) {
        k = Geq;
    } else if( tt == Tok_IN ) {
        k = In;
    }	else if( tt == Tok_Plus ) {
        k = Add;
    } else if( tt == Tok_Minus ) {
        k = Sub;
    } else if( tt == Tok_OR ) {
        k = Or;
    } else if( tt == Tok_Star ) {
        k = Mul;
    } else if( tt == Tok_Slash ) {
        k = Fdiv;
    } else if( tt == Tok_DIV ) {
        k = Div;
    } else if( tt == Tok_MOD ) {
        k = Mod;
    } else if( tt == Tok_Amp ) {
        k = And;
    } else if( tt == Tok_AND ) {
        k = And;
    }
    return create(k,rc);
}

struct Expression::Arena {
    enum { len = 100 };
    Expression arena[len];
    Arena* next;
    Arena():next(0){}
    ~Arena() { if( next ) delete next; }
};

Expression::Arena* Expression::arena = 0;
quint32 Expression::used = 0;

Expression* Expression::create(Expression::Kind k, const RowCol& rc)
{
    Arena** cur = &arena;

    quint32 aidx = used;
    Expression* res = 0;
    while( true )
    {
        if( *cur == 0 )
            *cur = new Arena();
        if( aidx < Arena::len )
        {
            res = &(*cur)->arena[aidx];
            break;
        }
        aidx -= Arena::len;
        cur = &(*cur)->next;
    }
    used++;
    // use this instead of placement new because already initialized and destructor not called otherwise
    *res = Expression(k,rc);
    return res;
}

void Expression::deleteAllExpressions()
{
    used = 0;
}

void Expression::killArena()
{
    if( arena )
        delete arena;
    arena = 0;
}
