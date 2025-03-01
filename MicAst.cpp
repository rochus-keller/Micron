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
Type* AstModel::types[Type::MaxBasicType] = {0};


AstModel::AstModel():helper(0),helperId(0)
{
    openScope(&globalScope);

    if( globalScope.kind == Declaration::NoMode )
    {
        globalScope.kind = Declaration::Scope;
        types[Type::Undefined] = newType(Type::Undefined,1);
        types[Type::NoType] = newType(Type::NoType,1);
        types[Type::String] = newType(Type::String,1);
        types[Type::Nil] = newType(Type::Nil,1);
        types[Type::Any] = addType("ANY", Type::Any,1);

        types[Type::BOOL] = addType("BOOL", Type::BOOL, 1 );
        addTypeAlias("BOOLEAN", types[Type::BOOL] );

        types[Type::CHAR] = addType("CHAR", Type::CHAR, 1 );

        types[Type::UINT8] = addType("UINT8", Type::UINT8, 1 );
        addTypeAlias("BYTE", types[Type::UINT8] );
        addTypeAlias("U8", types[Type::UINT8] );

        types[Type::INT8] = addType("INT8", Type::INT8, 1 );
        addTypeAlias("I8", types[Type::INT8] );

        types[Type::INT16] = addType("INT16", Type::INT16, 2 );
        addTypeAlias("SHORTINT", types[Type::INT16] );
        addTypeAlias("I16", types[Type::INT16] );

        types[Type::UINT16] = addType("UINT16", Type::UINT16, 2 );
        addTypeAlias("U16", types[Type::UINT16] );

        types[Type::INT32] = addType("INT32", Type::INT32, 4 );
        addTypeAlias("INTEGER", types[Type::INT32] );
        addTypeAlias("I32", types[Type::INT32] );

        types[Type::UINT32] = addType("UINT32", Type::UINT32, 4 );
        addTypeAlias("U32", types[Type::UINT32] );

        types[Type::INT64] = addType("INT64", Type::INT64, 8 );
        addTypeAlias("LONGINT", types[Type::INT64] );
        addTypeAlias("I64", types[Type::INT64] );

        types[Type::UINT64] = addType("UINT64", Type::UINT64, 8 );
        addTypeAlias("U64", types[Type::UINT64] );

        types[Type::FLT32] = addType("FLT32", Type::FLT32, 4 );
        addTypeAlias("REAL", types[Type::FLT32] );

        types[Type::FLT64] = addType("FLT64", Type::FLT64, 8 );
        addTypeAlias("LONGREAL", types[Type::FLT64] );

        types[Type::SET] = addType("SET", Type::SET, 4 );

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
        scope->kind = Declaration::Scope;
        scope->next = 0;
    }
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
    }else if( scopes.back()->kind == Declaration::Module )
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

    if( !name.isEmpty() )
        while(*obj != 0 && (*obj)->name.constData() != name.constData() )
            obj = &((*obj)->next);
    else // if there is no name, just add a decl to the end
        while(*obj != 0 )
            obj = &((*obj)->next);
    if((*obj) == 0 )
    {
        Declaration* decl = new Declaration();
        decl->link = 0;
        decl->next = 0;
        decl->name = name;
        if( scope->kind != Declaration::Scope )
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
    decl->outer = getTopModule();
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

Declaration*AstModel::findDecl(const QByteArray& id, bool recursive) const
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
        if( !recursive )
            return 0;
    }
    return 0;
}

Declaration*AstModel::findDecl(Declaration* import, const QByteArray& id) const
{
    if( import == 0 )
        return findDecl(id);
    Q_ASSERT(import && import->kind == Declaration::Import);
    Declaration* obj = import->link;
    while( obj != 0 && obj->name.constData() != id.constData() )
        obj = obj->next;
    return obj;
}

QByteArray AstModel::getTempName()
{
    return Token::getSymbol("$" + QByteArray::number(++helperId));
}

Declaration*AstModel::getTopModule() const
{
    for( int i = 0; i < scopes.size(); i++ )
        if( scopes[i]->kind == Declaration::Module )
            return scopes[i];
    return 0;
}

void AstModel::cleanupGlobals()
{
    if( globalScope.kind == Declaration::Scope )
    {
        if( globalScope.next )
            delete globalScope.next;
        globalScope.next = 0;
        if( globalScope.link )
            delete globalScope.link;
        globalScope.link = 0;
        globalScope.kind = Declaration::NoMode;
        for( int i = 0; i < Type::MaxBasicType; i++ )
        {
            delete types[i];
            types[i] = 0;
        }
    }
}

Type*AstModel::newType(int form, int size)
{
    Type* t = new Type();
    t->kind = form;
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
    d->kind = Declaration::TypeDecl;
    d->setType(t);
    if( t->decl == 0 )
        t->decl = d;
    Declaration* d2 = addDecl(Token::getSymbol(name.toLower()));
    d2->kind = Declaration::TypeDecl;
    d2->setType(t);
}

void AstModel::addBuiltin(const QByteArray& name, Builtin::Type t)
{
    Declaration* d = addDecl(Token::getSymbol(name.toUpper()));
    d->kind = Declaration::Builtin;
    d->setType(types[Type::NoType]);
    d->id = t;
    d = addDecl(Token::getSymbol(name.toLower()));
    d->kind = Declaration::Builtin;
    d->setType(types[Type::NoType]);
    d->id = t;
}

Declaration*Type::findSub(const QByteArray& name) const
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
        if( d->kind == Declaration::Field )
            res.first++;
        else if( d->kind == Declaration::Variant )
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
    if( kind != ConstEnum )
        for( int i = 0; i < subs.size(); i++ )
            delete subs[i];
}

QVariant Type::getMax(Kind t)
{
    switch( t )
    {
    case BOOL:
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
    case FLT32:
        return std::numeric_limits<float>::max();
    case FLT64:
        return std::numeric_limits<double>::max();
    }
    return QVariant();
}

QVariant Type::getMin(Kind t)
{
    switch( t )
    {
    case BOOL:
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
    case FLT32:
        return std::numeric_limits<float>::min();
    case FLT64:
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
            && kind != Declaration::Import  // imports are just referenced, not owned
            )
        delete link;
    if( type && ownstype )
        delete type;
}

QList<Declaration*> Declaration::getParams() const
{
    Declaration* d = link;
    QList<Declaration*> res;
    while( d && d->kind == Declaration::ParamDecl )
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
        if( d->kind == ref->kind )
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
        return type && type->kind == Type::Proc;

    default:
        return false;
    }
}

static inline bool allConst( const ExpList& args )
{
    for( int i = 0; i < args.size(); i++ )
        if( !args[i]->isConst() )
            return false;
    return true;
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
        ExpList args = val.value<ExpList>();

        if( lhs->kind == ProcDecl )
        {
            Declaration* d = lhs->val.value<Declaration*>();
            if( !d->invar )
                return false;
            else
                return allConst(args);
        }else if( lhs->kind == Builtin )
        {
            // TODO
            switch( lhs->val.toInt() )
            {
            case Builtin::LEN:
                return true;
            case Builtin::MIN:
            case Builtin::MAX:
                return args.size() == 1 || allConst(args);
            case Builtin::GETENV:
            case Builtin::DEFAULT:
            case Builtin::SIZE:
                return true;
            case Builtin::CAST:
                return args.size() == 2 && args[1]->isConst();
            case Builtin::VARARG:
            case Builtin::VARARGS:
                return false;
            default:
                return allConst(args);
            }
        }else
            return false;

        for( int i = 0; i < args.size(); i++ )
            if( !args[i]->isConst() )
                return false;
        return true;
    }

    if( lhs && !lhs->isConst() )
        return false;
    if( rhs && !rhs->isConst() )
        return false;
    if( next && !next->isConst() ) // recursively iterates over all next
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
    else if( type && type->kind == Type::Proc )
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

void Expression::appendRhs(Expression* e)
{
    if( rhs == 0 )
        rhs = e;
    else
        append(rhs,e);
}

void Expression::append(Expression* list, Expression* elem)
{
    while( list && list->next )
        list = list->next;
    if( list )
    {
        Q_ASSERT(list->next == 0);
        list->next = elem;
    }
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

Node::~Node()
{
    // TODO
}

void Node::setType(Type* t)
{
    type = t;
}
