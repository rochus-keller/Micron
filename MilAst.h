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
            owned(false),type(0),autoself(0){}
        ~Node();

        enum Meta { Inval, T, D, E };
        enum Visi { NA, Private, ReadOnly, ReadWrite };

        uint kind : 6;
        uint meta : 2;

        uint typebound : 1; // Type, Declaration
        uint ownstype : 1; // all
        uint visi : 2; // Declaration

        // Type
        uint deferred : 1;
        uint anonymous : 1;
        uint selfref : 1;
        uint owned : 1;

        // Declaration:
        uint inline_ : 1;
        uint invar : 1;
        uint extern_ : 1; // extern name (if present) is in val
        uint generic : 1;
        uint autoself : 1;

        // Expression
        uint byVal : 1; // option for LocalVar, Param, ModuleVar, Select, Index

        Mic::RowCol pos; // Declaration, Expression

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
            Nil,
            BOOL,
            CHAR,
            INTEGER,
            FLOAT,
            MaxBasicType,
            Pointer, Proc, Array, Record, Object, NameRef, Generic };
        quint32 len; // array length
        // type: array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl;

        Type():Node(T),len(0),decl(0){}
    };

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
        QVariant data; // value for Const and Enum, path for Import, name for Extern
        Declaration():Node(D),next(0),link(0),outer(0){}
    };
    typedef QList<Declaration*> DeclList;

    class Ast
    {
    public:
        Ast();
    };
}

#endif // MILAST_H
