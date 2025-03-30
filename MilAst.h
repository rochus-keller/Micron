#ifndef MILAST_H
#define MILAST_H

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


#include <QByteArray>
#include <Micron/MicRowCol.h>
#include <Micron/MilTokenType.h>
#include <QVariant>

namespace Mil
{
    class Type;
    class Declaration;

    class Node
    {
    public:
        Node(quint8 m):kind(0),meta(m),deferred(false),anonymous(false),selfref(false),typebound(false),
            ownstype(false),inline_(false),invar(false),extern_(false),generic(false),byVal(false),
            type(0),autoself(0),public_(0),init(0) {}
        ~Node();

        enum Meta { Inval, T, D, E, S };
        enum Visi { NA, Private, ReadOnly, ReadWrite };

        uint kind : 6;
        uint meta : 3;

        uint typebound : 1; // Type, Declaration
        uint ownstype : 1; // all
        uint public_ : 1; // Declaration
        uint anonymous : 1; // Type, Field

        // Type
        uint deferred : 1;
        uint selfref : 1;

        // Declaration:
        uint inline_ : 1;
        uint invar : 1;
        uint extern_ : 1; // extern name (if present) is in val
        uint generic : 1;
        uint autoself : 1;
        uint init : 1;

        // Expression
        uint byVal : 1; // option for LocalVar, Param, ModuleVar, Select, Index

        Mic::RowCol pos; // Declaration, Expression

        void setType(Type*t);
        Type* getType() const { return type; }

    protected:
        Type* type;
    };

    typedef QPair<QByteArray,QByteArray> Quali;

    class Type : public Node
    {
    public:
        enum Kind {
            Undefined,
            Any,
            StringLit,
            NIL,
            BOOL,
            CHAR,
            INT8, INT16, INT32, INT64,
            UINT8, UINT16, UINT32, UINT64,
            FLOAT32, FLOAT64,
            INTPTR,
            MaxBasicType,
            Pointer, Proc, Array, Struct, Union, Object, NameRef, Generic };
        union {
            quint32 len; // array length
            Quali* quali; // unresolved NameRef
        };
        // type: array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl;

        Type():Node(T),len(0),decl(0){}
        ~Type();

        Declaration* findSubByName(const QByteArray& name, bool recursive = true) const;
    };

    struct Constant;
    class Statement;

    class Declaration : public Node
    {
    public:
        enum Kind { NoMode, Module, TypeDecl, ConstDecl, Import,
                    Field, VarDecl, LocalDecl, ParamDecl,
                    Procedure,
                    Max };
        Declaration* next; // list of all declarations in outer scope
        Declaration* link; // member list or imported module decl
        Declaration* outer; // the owning declaration to reconstruct the qualident
        Statement* body;
        QByteArray name;
        union {
            Constant* c; // ConstDecl
            struct {
                uint bw : 8; // Field bitwidth
                uint off : 24; // Field offset in bytes
            } f;
            int off;
        };
        // QVariant data; // value for Const, path for Import, name for Extern
        Declaration():Node(D),next(0),link(0),outer(0),c(0),body(0){}
        ~Declaration();
        void appendSub(Declaration*);
        Declaration* findSubByName(const QByteArray&) const;
        int indexOf(Declaration*) const;
        static void append(Declaration* list, Declaration* next);

    };
    typedef QList<Declaration*> DeclList;

    struct Component
    {
        QByteArray name; // optional field name
        Constant* c;
        Component* next;

        Component():c(0),next(0){}
        ~Component();
    };

    struct ComponentList
    {
        Type* type; // optional type, owned
        Component* c; // owned

        ComponentList():type(0),c(0) {}
        ~ComponentList();
    };

    struct ByteString
    {
        quint32 len;
        unsigned char* b; // hexstring

        ByteString():len(0),b(0){}
        ~ByteString();
    };

    struct Constant
    {
        enum Kind { Invalid, D, I, S, B, R, C } kind;
        union {
            double d;
            qint64 i;
            char* s;    // string
            ByteString* b; // hexstring
            Declaration* r; // reference to const decl
            ComponentList* c;
        };
        Constant():kind(Invalid) {}
        ~Constant();
    };

    class Expression : public Node
    {
    public:
        // Kind corresponds to the TokenType
        Expression* next;
        union {
            Expression* e; // IIF/THEN/ELSE
            Declaration* d; // not owned
            quint32 id;
            qint64 i;
            double f;
            Constant* c; // owned
        };

        Expression():Node(E), next(0), i(0) {}
        ~Expression();
        void append(Expression*);
    };

    class Statement : public Node
    {
    public:
        enum Kind { ExprStat = TT_Specials,  };
        // Kind corresponds to the TokenType
        Statement* next;
        Statement* body;

        union {
            Expression* e; // owned
            Declaration* d; // not owned
            const char* name;
            qint64 i;
            double f;
            quint32 id;
        };

        Statement():Node(S),next(0), body(0), e(0) {}
        ~Statement();
        void append(Statement*);
    };

    typedef QList<Expression*> MetaActualList;

    struct Import {
        QByteArray moduleName;
        MetaActualList metaActuals;
    };

    class Importer {
    public:
        virtual Declaration* loadModule( const Import& imp ) = 0;
    };

    class AstModel
    {
    public:
        AstModel();
        ~AstModel();

        void clear();

        Declaration* findModuleByName( const QByteArray& ) const;
        bool addModule(Declaration*);
        const Declaration* getGlobals() const { return &globals; }

    protected:
        void createMIC();

    private:
        DeclList modules;
        Declaration globals;
        Type* basicTypes[Type::MaxBasicType];
    };
}

Q_DECLARE_METATYPE(Mil::Expression*)

#endif // MILAST_H
