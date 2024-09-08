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

#include "MicEiGen.h"
#include "MicToken.h"
#include <QTextStream>
#include <QCoreApplication>
#include <QDateTime>
#include <QtDebug>
using namespace Mic;

class EiGen::Imp
{
public:
    struct Type;

    struct Field
    {
        QByteArray name;
        Type* type;
        quint32 offset;
        Field():type(0),offset(0){}
    };

    struct Type
    {
        enum Kind { None, u1, u2, u4, u8, s1, s2, s4, s8, f4, f8, ptr, rec, array, alias };
        QByteArray name;
        quint8 kind;
        quint32 len;
        quint32 size;
        Type* base;
        QList<Field*> fields;
        Type(quint8 kind = None):kind(kind),len(0),base(0),size(0){}
        ~Type()
        {
            for( int i = 0; i < fields.size(); i++ )
                delete fields[i];
        }
        Type* deref()
        {
            if( kind == alias && base )
                return base->deref();
            else
                return this;
        }
        quint8 getAlig() const
        {
            quint8 alig = 0;
            switch( kind )
            {
            case rec:
                for( int i = 0; i < fields.size(); i++ )
                {
                    const quint8 a = fields[i]->type->getAlig();
                    if( a > alig )
                        alig = a;
                }
                break;
            case array:
                alig = base->deref()->getAlig();
            default:
                alig = size;
                break;
            }
            return alig;
        }
        QByteArray getName(const QByteArray& default_ = QByteArray() ) const
        {
            if( name.isEmpty() )
            {
                if( !default_.isEmpty() )
                    return "\"" + default_ + "\"";
                else
                    return "?";
            }
            if( kind <= f8 )
                return name;
            else
                return "\"" + name + "\"";
        }
    };

    Imp(QIODevice* out):out(out)
    {
        //basic[Type::None].name = "???";
        basic[Type::u1].kind = Type::u1;
        basic[Type::u1].name = "u1";
        basic[Type::u1].size = 1;
        basic[Type::u2].kind = Type::u2;
        basic[Type::u2].name = "u2";
        basic[Type::u2].size = 2;
        basic[Type::u4].kind = Type::u4;
        basic[Type::u4].name = "u4";
        basic[Type::u4].size = 4;
        basic[Type::u8].kind = Type::u8;
        basic[Type::u8].name = "u8";
        basic[Type::u8].size = 8;
        basic[Type::s1].kind = Type::s1;
        basic[Type::s1].name = "s1";
        basic[Type::s1].size = 1;
        basic[Type::s2].kind = Type::s2;
        basic[Type::s2].name = "s2";
        basic[Type::s2].size = 2;
        basic[Type::s4].kind = Type::s4;
        basic[Type::s4].name = "s4";
        basic[Type::s4].size = 4;
        basic[Type::s8].kind = Type::s8;
        basic[Type::s8].name = "s8";
        basic[Type::s8].size = 8;
        basic[Type::f4].kind = Type::f4;
        basic[Type::f4].name = "f4";
        basic[Type::f4].size = 4;
        basic[Type::f8].kind = Type::f8;
        basic[Type::f8].name = "f8";
        basic[Type::f8].size = 8;

        for( int i = Type::u1; i <= Type::f8; i++ )
            byName[basic[i].name] = &basic[i];

        ptrSize = sizeof(void*);
        stackMinAlign = stackMaxAlign = 0;
    }
    ~Imp()
    {
        for( int i = 0; i < toDelete.size(); i++ )
            delete toDelete[i];
    }

    QTextStream out;
    QByteArray modName;
    QString sourceFile;

    Type basic[Type::f8+1];

    QHash<QByteArray,Type*> byName, notFound;
    QList<Type*> toDelete;

    QList< QPair<QByteArray,QByteArray> > fields;
    QByteArray typeName;
    quint8 typeKind;

    quint8 ptrSize; // byte width of pointer and function pointer
    quint8 stackMinAlign, stackMaxAlign;

    Type* type(const QByteArray& name, bool noWarning = false )
    {
        const QByteArray namelc = name.toLower();
        // TODO: performance
        if( namelc == "bool" || namelc == "char" || namelc == "uint8" )
            return &basic[Type::u1];
        else if( namelc == "uint16" )
            return &basic[Type::u2];
        else if( namelc == "uint32" )
            return &basic[Type::u4];
        else if( namelc == "uint64" )
            return &basic[Type::u8];
        else if( namelc == "int8" )
            return &basic[Type::s1];
        else if( namelc == "int16" )
            return &basic[Type::s2];
        else if( namelc == "int32" )
            return &basic[Type::s4];
        else if( namelc == "int64" )
            return &basic[Type::s8];
        else if( namelc == "float32" )
            return &basic[Type::f4];
        else if( namelc == "float64" )
            return &basic[Type::f8];

        Type* res = byName.value(name);
        if( res == 0 )
        {
            if( !noWarning )
                qWarning() << "unknown type:" << name;
            res = &basic[Type::None];
        }
        return res;
    }
    void add(Type* t)
    {
        byName.insert(t->name, t);
        toDelete.append(t);
        QHash<QByteArray,Type*>::iterator i = notFound.find(t->name);
        if( i != notFound.end() )
        {
            i.value()->base = t;
            notFound.erase(i);
        }
    }
    void render(const MilProcedure& method)
    {
        out << "    loc \"" << sourceFile << "\", 1, 1" << endl; // TODO
        if( method.d_retType.isEmpty() )
            out << "    void" << endl;
        else
            out << "    " << type(method.d_retType)->getName(method.d_retType) << endl;

#if 0 // TODO
        for( int i = 0; i < method.d_args.size(); i++ )
            out << "    " << type(method.d_args[i].first)->getName(method.d_args[i].first) << endl;
#endif


    }
};

EiGen::EiGen(QIODevice* out)
{
    Q_ASSERT(out && out->isOpen());
    imp = new Imp(out);
}

EiGen::~EiGen()
{
    delete imp;
}

void EiGen::beginModule(const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& mp)
{
    imp->out << "; this is Eigen IR, see https://github.com/EigenCompilerSuite/" << endl;
    imp->out << "; generated by " << qApp->applicationName();
    if( !qApp->applicationVersion().isEmpty() )
     imp->out << " " << qApp->applicationVersion();
    imp->out << " on " << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;

    if( !mp.isEmpty() )
        qWarning() << "generic modules not yet supported";

    imp->modName = moduleName;
    imp->sourceFile = sourceFile;

    imp->notFound.clear();

#if 0
    imp->out << ".type bool" << endl << "    type u1" << endl;
    imp->out << ".type char" << endl << "    type u1" << endl;
    imp->out << ".type float32" << endl << "    type f4" << endl;
    imp->out << ".type float64" << endl << "    type f8" << endl;
    imp->out << ".type int8" << endl << "    type s1" << endl;
    imp->out << ".type int16" << endl << "    type s2" << endl;
    imp->out << ".type int32" << endl << "    type s4" << endl;
    imp->out << ".type int64" << endl << "    type s8" << endl;
    imp->out << ".type uint8" << endl << "    type u1" << endl;
    imp->out << ".type uint16" << endl << "    type u2" << endl;
    imp->out << ".type uint32" << endl << "    type u4" << endl;
    imp->out << ".type uint64" << endl << "    type u8" << endl;
#endif
}

void EiGen::endModule()
{
    if( !imp->notFound.isEmpty() )
    {
        QHash<QByteArray,Imp::Type*>::const_iterator i;
        for( i = imp->notFound.begin(); i != imp->notFound.end(); ++i )
            qWarning() << "could not resolve type:" << i.key();
    }
}

void EiGen::addImport(const QByteArray& path, const QByteArray& name)
{
    qWarning() << "imports not yet supported: " << path << name;
}

void EiGen::addVariable(const QByteArray& typeRef, QByteArray name, bool isPublic)
{
    imp->out << ".data " << name << endl;
    Imp::Type* t = imp->type(typeRef);
    imp->out << "    .alignment " << t->getAlig() << endl;
    imp->out << "    loc \"" << imp->sourceFile << "\", 1, 1" << endl;
    imp->out << "    type " << t->getName(typeRef) << endl;
    imp->out << "    res " << t->size << endl;
}

void EiGen::addProcedure(const MilProcedure& method)
{
    switch( method.d_kind )
    {
    case MilProcedure::ProcType:
        imp->out << ".type " << method.d_name << endl;
        imp->out << "    func +" << method.d_args.size() + 1 << endl;
        if( method.d_retType.isEmpty() )
            imp->out << "    void" << endl;
        else
            imp->out << "    " << imp->type(method.d_retType)->getName(method.d_retType) << endl;
        for( int i = 0; i < method.d_args.size(); i++ )
            imp->out << "    " << imp->type(method.d_args[i].first)->getName(method.d_args[i].first) << endl;
        break;
    case MilProcedure::Extern:
        // NOP
        break;
    case MilProcedure::Normal:
    case MilProcedure::Invar:
    case MilProcedure::Inline: // TODO: currently treated like a normal proc
        imp->out << ".code " << method.d_name << endl;
        imp->render(method);
        break;
    case MilProcedure::ModuleInit:
        imp->out << ".initcode " << method.d_name << endl;
        imp->render(method);
        break;
    }
    // else TODO
}

void EiGen::beginType(const QByteArray& name, bool isPublic, quint8 typeKind)
{
    Q_ASSERT( typeKind == MilEmitter::Struct || typeKind == MilEmitter::Union );
    imp->fields.clear();
    imp->typeName = name;
    imp->typeKind = typeKind;
}

static quint32 padding( quint32 alig, quint32 size )
{
    return alig ? (alig - (size % alig)) % alig : 0;
}

void EiGen::endType()
{
    Q_ASSERT( imp->typeKind == MilEmitter::Struct || imp->typeKind == MilEmitter::Union );

    Imp::Type* t = new Imp::Type(Imp::Type::rec);
    t->name = imp->typeName;
    quint32 maxAlig = 0;
    for( int i = 0; i < imp->fields.size(); i++ )
    {
        Imp::Field* f = new Imp::Field();
        f->name = imp->fields[i].first;
        f->type = imp->type(imp->fields[i].second);
        t->fields.append(f);

        const quint32 alig = f->type->getAlig();
        if( imp->typeKind == MilEmitter::Struct )
        {
            f->offset = t->size;
            t->size += f->type->size;
            // https://en.wikipedia.org/wiki/Data_structure_alignment#Computing_padding
            const int pad = padding(alig,t->size);
            t->size += pad;
        }else
        {
            if( alig > maxAlig )
                maxAlig = alig;
            f->offset = 0;
            if( t->size < f->type->size )
                t->size = f->type->size; // union size of biggest field
        }
    }
    if( imp->typeKind == MilEmitter::Union )
        t->size += padding(maxAlig, t->size);
    imp->add(t);

    imp->out << ".type " << t->name << endl;
    imp->out << "    rec +" << t->fields.size() * 3 << ", " << t->size << endl;
    for( int i = 0; i < t->fields.size(); i++ )
    {
        imp->out << "    field \"" << t->fields[i]->name << "\", " << t->fields[i]->type->size << ", s4 0" << endl;
        imp->out << "    loc \"" << imp->sourceFile << "\", 1, 1" << endl;
        imp->out << "    type " << t->fields[i]->type->getName() << endl;
    }
}

void EiGen::addType(const QByteArray& name, bool isPublic, const QByteArray& baseType, quint8 typeKind, quint32 len)
{
    Q_ASSERT( typeKind == MilEmitter::Alias || typeKind == MilEmitter::Pointer || typeKind == MilEmitter::Array);
    imp->out << ".type " << name << endl;
    imp->out << "    ";
    Imp::Type* t = new Imp::Type();
    t->name = name;
    t->base = imp->type(baseType, typeKind == MilEmitter::Pointer ); // pointer type resolution is deferred

    switch(typeKind)
    {
    case MilEmitter::Alias:
        t->kind = Imp::Type::alias;
        t->size = t->deref()->size;
        imp->add(t);
        imp->out << "type " << t->base->getName(baseType) << endl;
        break;
    case MilEmitter::Pointer:
        t->kind = Imp::Type::ptr;
        t->size = imp->ptrSize;
        if( t->base->kind == Imp::Type::None )
            imp->notFound.insert(baseType,t);
        imp->add(t);
        imp->out << "ptr" << endl;
        imp->out << "    type " << t->base->getName(baseType) << endl;
        break;
    case MilEmitter::Array:
        t->kind = Imp::Type::array;
        t->len = len;
        t->size = t->base->size * len;
        imp->add(t);
        imp->out << "array 0, " << len << endl;
        imp->out << "    type " << t->base->getName(baseType) << endl;
        break;
    }
}

void EiGen::addField(const QByteArray& fieldName, const QByteArray& typeRef, bool isPublic)
{
    imp->fields.append(qMakePair(fieldName,typeRef));
}

