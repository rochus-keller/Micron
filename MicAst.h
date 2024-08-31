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

    struct BasicType
    {
        enum Type {
               Undefined, NoType,
               Nil, Any, String,
               BOOLEAN,
               CHAR,
               UINT8, UINT16, UINT32, UINT64,
               INT8, INT16, INT32, INT64,
               REAL, LONGREAL,
               SET,
               Max
             };

        static QVariant getMax(Type);
        static QVariant getMin(Type);
    };

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
    };

    class Type
    {
    public:
        enum Form { Pointer = BasicType::Max, Proc, Array, Record, ConstEnum, NameRef };
        quint8 form;
        bool deferred;
        bool anonymous;
        bool selfref;
        quint32 len; // array length
        Type* base; // array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl;

        bool isUInt() const { return form >= BasicType::UINT8 && form <= BasicType::UINT64; }
        bool isInt() const { return form >= BasicType::INT8 && form <= BasicType::INT64; }
        bool isNumber() const { return form >= BasicType::UINT8 && form <= BasicType::LONGREAL; }
        bool isReal() const { return form == BasicType::LONGREAL || form == BasicType::REAL; }
        bool isInteger() const { return form >= BasicType::UINT8 && form <= BasicType::INT64; }
        bool isSet() const { return form == BasicType::SET; }
        bool isBoolean() const { return form == BasicType::BOOLEAN; }
        bool isSimple() const { return form >= BasicType::String && form < BasicType::Max; }
        bool isText() const { return form == BasicType::String || form == BasicType::CHAR ||
                    ( form == Array && base && base->form == BasicType::CHAR ) ||
                    ( form == Pointer && base && base->form == Array && base->base->form == BasicType::CHAR ); }
        bool isStructured() const { return form == Array || form == Record; }

        Declaration* findField(const QByteArray& name) const;
        QPair<int,int> getFieldCount() const; // fixed, variant

        Type():form(0),len(0),base(0),decl(0),deferred(false),anonymous(false),selfref(false){}
        ~Type();
    };

    class Declaration
    {
    public:
        enum Mode { NoMode, Scope, Module, TypeDecl, Builtin, ConstDecl, Import, Field, Variant,
                    VarDecl, LocalDecl,
                    Procedure, ForwardDecl, ParamDecl,
                    Max };
        static const char* s_mode[];
        Declaration* next; // list of all declarations in outer scope
        Declaration* link; // member list or alias proc or imported module decl
        Declaration* outer; // the owning declaration to reconstruct the qualident
        Type* type;
        QByteArray name;
        uint row : RowCol::ROW_BIT_LEN; // supports 524k lines
        uint col : RowCol::COL_BIT_LEN; // supports 4k chars per line
        enum Visi { NA, Private, ReadOnly, ReadWrite };
        uint visi : 2;
        uint inline_ : 1;
        uint invar : 1;
        uint extern_ : 1; // extern name (if present) is in val
        uint alias : 1; // the original is in link
        uint symbol : 1;
        uint scope : 1;
        uint ownstype : 1;
        uint mode : 5;
        uint level : 8; // also used for built-in code and local number
        QVariant data; // value for Const and Enum, path for Import, name for Extern
        Declaration():next(0),link(0),type(0),row(0),col(0),level(0),mode(0),visi(0),ownstype(false),
            inline_(false),invar(false),extern_(false),symbol(false),scope(false),outer(0),alias(0){}
        ~Declaration();

        QList<Declaration*> getParams() const;
        int getIndexOf(Declaration*) const;
        bool isLvalue() const { return mode == VarDecl || mode == LocalDecl || mode == ParamDecl; }
        bool isPublic() const { return visi >= ReadOnly; }
    };

    struct Value {
        enum Mode { Val = Declaration::Max, Const };
        quint8 mode;
        quint8 visi;
        bool ref; // the value is a reference to the type
        Type* type;
        QVariant val;

        Value():mode(0),type(0),ref(false),visi(Declaration::Private){}

        bool isConst() const { return mode == Declaration::ConstDecl || mode == Const; }
        bool isLvalue() const { return mode == Declaration::VarDecl || mode == Declaration::LocalDecl ||
                    mode == Declaration::ParamDecl; }
        bool isCallable() const;
   };

    struct Import {
        QByteArrayList path;
        QList<Value> metaActuals;
    };

    class AstModel
    {
    public:
        AstModel();
        ~AstModel();

        void openScope(Declaration* scope);
        Declaration* closeScope(bool takeMembers = false);
        Declaration* addDecl(const QByteArray&, bool* doublette = 0 );
        Declaration* addHelper();
        void removeDecl(Declaration*);
        Declaration* findDecl(const QByteArray&) const;
        Declaration* findDecl(Declaration* import, const QByteArray&) const;
        Declaration* getTopScope() const { return scopes.back(); }
        QByteArray getTempName();

        Type* getType(quint8 basicType) const { return types[basicType]; }

        static void cleanupGlobals();
    protected:
        Type* newType(int form, int size);
        Type* addType(const QByteArray& name, int form, int size);
        void addTypeAlias(const QByteArray& name, Type*);
        void addBuiltin(const QByteArray& name, Builtin::Type);
    private:
        QList<Declaration*> scopes;
        Declaration* helper;
        quint32 helperId;
        static Declaration globalScope;
        static Type* types[BasicType::Max];

    };
}

Q_DECLARE_METATYPE(Mic::Import)
Q_DECLARE_METATYPE(Mic::Declaration*)

#endif // MICAST_H
