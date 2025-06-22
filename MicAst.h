#ifndef MICAST_H
#define MICAST_H

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

#include <QByteArray>
#include <Micron/MicRowCol.h>
#include <QVariant>

namespace Mic
{
    class Declaration;

    struct Builtin
    {
        enum Type {
            // functions
            ABS, CAP, BITAND, BITASR, BITNOT, BITOR, BITS, BITSHL, BITSHR,
            BITXOR, CAST, CHR, DEFAULT, FLOOR, FLT, GETENV, LEN, LONG, MAX,
            MIN, ODD, ORD, SHORT, SIGNED, SIZE, STRLEN, UNSIGNED, VARARG, VARARGS,
            // procedures
            ASSERT, DEC, DISPOSE, EXCL, HALT, INC,
            INCL, NEW, PCALL, PRINT, PRINTLN, RAISE, SETENV,
            // end
            Max
        };
        static const char* name[];
    };

    class Type;

    class Node
    {
    public:
        Node(quint8 m):
    #ifndef _DEBUG
            kind(0),
    #endif
            meta(m),deferred(false),anonymous(false),typebound(false),
            ownstype(false),inline_(false),invar(false),extern_(false),generic(false),byVal(false),
            owned(false),type(0),autoself(0),invalid(0),hasSubs(0){}
        ~Node();

        enum Meta { Inval, T, D, E };
        enum Visi { NA, Private, ReadOnly, ReadWrite };

#ifndef _DEBUG
        uint kind : 6;
#endif
        uint meta : 2;

        uint typebound : 1; // Type, Declaration
        uint ownstype : 1; // all
        uint visi : 2; // Declaration
        uint anonymous : 1;

        // Type
        uint deferred : 1;
        uint owned : 1;

        // Declaration:
        uint inline_ : 1;
        uint invar : 1;
        uint extern_ : 1; // extern name (if present) is in val
        uint generic : 1;
        uint autoself : 1;
        uint invalid : 1; // module
        uint hasSubs : 1; // class/method: has overrides; module: has clients

        // Expression
        uint byVal : 1; // option for LocalVar, Param, ModuleVar, Select, Index

        RowCol pos; // Declaration, Expression

        void setType(Type*t);
        Type* getType() const { return type; }

    protected:
        Type* type;
    };

    class Type : public Node
    {
    public:
        enum Kind {
            Undefined,
            NoType,
            String,
            Any,
            Nil,
            BOOL,
            CHAR,
            UINT8, UINT16, UINT32, UINT64,
            INT8, INT16, INT32, INT64,
            FLT32, FLT64,
            SET,
            MaxBasicType,
            Pointer, Proc, Array, Record, Object, ConstEnum, Generic };
        static const char* name[];
#ifdef _DEBUG
        Kind kind;
#endif

        quint32 len; // array length
        // type: array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl;

        bool isUInt() const { return kind >= Type::UINT8 && kind <= Type::UINT64; }
        bool isInt() const { return kind >= Type::INT8 && kind <= Type::INT64; }
        bool isNumber() const { return kind >= Type::UINT8 && kind <= Type::FLT64; }
        bool isReal() const { return kind == Type::FLT64 || kind == Type::FLT32; }
        bool isInteger() const { return kind >= Type::UINT8 && kind <= Type::INT64; }
        bool isSet() const { return kind == Type::SET; }
        bool isBoolean() const { return kind == Type::BOOL; }
        bool isSimple() const { return kind >= Type::String && kind < Type::MaxBasicType; }
        bool isText() const { return kind == Type::String || kind == Type::CHAR ||
                    ( kind == Array && type && type->kind == Type::CHAR ) ||
                    ( kind == Pointer && type && type->kind == Array && type->type && type->type->kind == Type::CHAR ); }
        bool isStructured() const { return kind == Array || kind == Record || kind == Object; }
        bool isCharArray() const { return kind == Array && getType() && getType()->kind == CHAR; }

        Declaration* findSub(const QByteArray& name) const;
        Declaration* findMember(const QByteArray& name, bool recurseSuper = false) const;
        QPair<int,int> getFieldCount() const; // fixed, variant

        static QVariant getMax(Kind);
        static QVariant getMin(Kind);
        static bool isSubtype(Type* super, Type* sub);

        Type():Node(T),len(0),decl(0){}
        ~Type();
    };

    class Declaration : public Node
    {
    public:
        enum Kind { NoMode, Scope, Module, TypeDecl, Builtin, ConstDecl, Import, Field, Variant,
                    VarDecl, LocalDecl,
                    Procedure, ForwardDecl, ParamDecl,
                    Max };
        static const char* s_mode[];
#ifdef _DEBUG
        Kind kind;
#endif
        Declaration* next; // list of all declarations in scope
        Declaration* link; // member list or imported module decl
        Declaration* outer; // the owning declaration to reconstruct the qualident
        QByteArray name;

        quint16 id; // used for built-in code and local/param number, and bit size of fields, and id of temparary locals
        // TODO super for super class declaration and overridden method declaration
        QVariant data; // value for Const and Enum, import data, name for Extern, decl for forward or super, module data


        Declaration():Node(D),next(0),link(0),id(0),outer(0){}
        ~Declaration();

        QList<Declaration*> getParams(bool includeReceiver = false) const;
        int getIndexOf(Declaration*) const;
        bool isLvalue() const { return kind == VarDecl || kind == LocalDecl || kind == ParamDecl; }
        bool isPublic() const { return visi >= ReadOnly; }
        Declaration* getModule();

    };
    typedef QList<Declaration*> DeclList;

    class Expression : public Node
    {
    public:
        enum Kind {
            Invalid,
            Plus, Minus, Not, // Unary
            Eq, Neq, Lt, Leq, Gt, Geq, In, Is, // Relation
            Add, Sub, Or, // AddOp
            Mul, Fdiv, Div, Mod, And, // MulOp
            Addr, // @, requires a ref to type and converts it to a pointer value to type
            Deref, // ^, requires a pointer value (not ref) and converts it to a ref of type.base
            LocalVar, Param, Builtin, // val is index of local or param or builtin
            ModuleVar, ProcDecl, ConstDecl, TypeDecl, // val is declaration
            Select, // f.g, val is field declaration
            MethDecl, // obj.proc, val is procedure declaration
            Index, // a[i]
            Cast, AutoCast,
            Call,
            Literal,
            Constructor, Range, NameValue, IndexValue,
            Super,   // ^ supercall
            MAX
        };
#ifdef _DEBUG
        Kind kind;
#endif
        QVariant val; // set elements and call args are ExpList embedded in val
        Expression* lhs; // for unary and binary ops
        Expression* rhs; // for binary ops
        Expression* next; // for args, set elems, and caselabellist

        bool isConst() const;
        bool isLiteral() const;
        QVariant getLiteralValue() const;
        DeclList getFormals(bool includeReceiver = false) const;
        bool isLvalue() const; // true if result of expression is usually a ref to type; can be changed with byVal
        void setByVal();
        void appendRhs(Expression*);
        static Expression* createFromToken(quint16,const RowCol&);
        static Expression* create(Kind k = Invalid, const RowCol& rc = RowCol());
        static void append(Expression* list, Expression* elem);
        static void lockArena();
        static void unlockArena();
        static void deleteAllExpressions();
        static void killArena();
    private:
        struct Arena;
        static Arena* arena;
        static quint32 used;
        static quint32 lock;
        Expression(Kind k = Invalid, const RowCol& rc = RowCol()):Node(E),lhs(0),rhs(0),next(0)
            {kind = k; pos = rc;}
        ~Expression() {}
    };
    typedef QList<Expression*> ExpList;

    struct Value {
        enum Mode { None, Val, Const, Builtin, Procedure, Method, VarDecl, LocalDecl, ParamDecl, TypeDecl };
        quint8 mode;
        quint8 visi;
        bool ref; // the value is a reference to the type
        Type* type;
        QVariant val;

        Value():mode(0),type(0),ref(false),visi(Declaration::Private){}
        Value(Type* t, const QVariant& v, Mode m):
            type(t),val(v),mode(m),ref(false),visi(0){}

        bool isConst() const { return mode == Const; }
        bool isLvalue() const { return mode == Declaration::VarDecl || mode == Declaration::LocalDecl ||
                    mode == Declaration::ParamDecl; }
        bool isCallable() const;
    };
    typedef QList<Value> MetaActualList;

    struct Import {
        QByteArrayList path; // full path incl. name
        MetaActualList metaActuals;
        Declaration* resolved; // points to imported module
        Declaration* importer;
        RowCol importedAt;
        Import():importer(0),resolved(0){}
        bool operator==(const Mic::Import& rhs) const;
    };

    typedef QList<Declaration*> MetaParamList;

    struct ModuleData {
        QString source;
        QByteArrayList path;
        MetaParamList metaParams;
        MetaActualList metaActuals;
        QByteArray suffix;
        QByteArray fullName; // path.join('/') + suffix as symbol
    };

    class Symbol
    {
    public:
        enum Kind { Invalid, End, Module, Decl, DeclRef, Lval };
        quint8 kind;
        quint8 len;
        RowCol pos;
        Declaration* decl;
        Symbol* next;

        Symbol():kind(Invalid),len(0),decl(0),next(0){}
        static void deleteAll(Symbol*);
    };

    typedef QList<Symbol*> SymList;

    struct Xref {
        Symbol* syms;
        QHash<Declaration*,SymList> uses;
        QHash<Declaration*,DeclList> subs;
        Xref():syms(0){}
    };

    class AstModel
    {
    public:
        AstModel();
        ~AstModel();

        void clear();

        void openScope(Declaration* scope);
        Declaration* closeScope(bool takeMembers = false);
        Declaration* addDecl(const QByteArray&, bool* doublette = 0 );
        Declaration* addHelper();
        void removeDecl(Declaration*);
        Declaration* findDecl(const QByteArray&, bool recursive = true) const;
        Declaration* findDecl(Declaration* import, const QByteArray&) const;
        Declaration* getTopScope() const { return scopes.back(); }
        QByteArray getTempName();
        Declaration* getTopModule() const;

        Type* getType(quint8 basicType) const { return types[basicType]; }

        static void cleanupGlobals();
    protected:
        Type* newType(Type::Kind form, int size);
        Type* addType(const QByteArray& name, Type::Kind form, int size);
        void addTypeAlias(const QByteArray& name, Type*);
        void addBuiltin(const QByteArray& name, Builtin::Type);
    private:
        QList<Declaration*> scopes;
        Declaration* helper;
        quint32 helperId;
        static Declaration globalScope;
        static Type* types[Type::MaxBasicType];

    };

    class Importer {
    public:
        virtual Declaration* loadModule( const Import& imp ) = 0;
        virtual QByteArray moduleSuffix( const MetaActualList& imp ) = 0;
        virtual QByteArray modulePath( const QByteArrayList& imp ) = 0;
    };

}

Q_DECLARE_METATYPE(Mic::Import)
Q_DECLARE_METATYPE(Mic::Declaration*)
Q_DECLARE_METATYPE(Mic::Expression*)
Q_DECLARE_METATYPE(Mic::ExpList)
Q_DECLARE_METATYPE(Mic::ModuleData)
Q_DECLARE_METATYPE(Mic::Symbol*)


#endif // MICAST_H
