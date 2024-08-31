#ifndef MICMILLOADER_H
#define MICMILLOADER_H

/*
* Copyright 2019-2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MicMilEmitter.h"
#include <QHash>

namespace Mic
{

struct MilVariable
{
    QByteArray name;
    QByteArray type;
    bool isPublic;
};

struct MilType
{
    QByteArray name;
    quint8 kind;
    bool isPublic;
    QByteArray base;
    quint32 len;
    QList<MilVariable> fields;
};

struct MilImport
{
    QByteArray name;
    QByteArray path;
};

struct MilModule
{
    QByteArray name;

    enum What { Type, Variable, Proc, Import };
    QHash<QByteArray,QPair<What,quint32> >  symbols;
    QList<QPair<What,quint32> > order;
    QList<MilType> types;
    QList<MilProcedure> procs;
    QList<MilImport> imports;
    QList<MilVariable> vars;
};

class MilLoader
{
public:
    MilLoader();

    const MilModule* getModule(const QByteArray& name) const;
private:
    friend class InMemRenderer;
    QList<MilModule> modules;
};

class InMemRenderer : public Mic::MilRenderer
{
public:
    InMemRenderer(MilLoader*);

    void beginModule( const QByteArray& moduleName, const QString& sourceFile, const Mic::MilMetaParams& );
    void endModule();

    void addImport( const QByteArray& path, const QByteArray& name );

    void addVariable(const QByteArray& typeRef, QByteArray name , bool isPublic);
    void addProcedure(const Mic::MilProcedure& method );

    void beginType(const QByteArray& name, bool isPublic, quint8 typeKind);
    void endType();
    void addType( const QByteArray& name, bool isPublic, const QByteArray& baseType,
                  quint8 typeKind, quint32 len = 0);

    void addField( const QByteArray& fieldName, // on top level or in class
                   const QByteArray& typeRef,
                   bool isPublic = true );
private:
    MilLoader* loader;
    MilModule* module;
    MilType* type;
};

}

#endif // MICMILLOADER_H
