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

class MilLoader
{
public:
    MilLoader();

    const MilModule* getModule(const QByteArray& name) const;
    const QList<MilModule>& getModules() const { return modules; }
    static bool render(MilRenderer*, const MilModule*);
private:
    friend class InMemRenderer;
    QList<MilModule> modules;
};

class InMemRenderer : public Mic::MilRenderer
{
public:
    InMemRenderer(MilLoader*);

    void beginModule( const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& );
    void endModule();

    void addImport(const QByteArray& path);

    void addVariable(const MilQuali& typeRef, QByteArray name , bool isPublic);
    void addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val );
    void addProcedure(const Mic::MilProcedure& method );

    void beginType(const QByteArray& name, bool isPublic, quint8 typeKind);
    void endType();
    void addType( const QByteArray& name, bool isPublic, const MilQuali& baseType,
                  quint8 typeKind, quint32 len = 0);

    void addField( const QByteArray& fieldName, // on top level or in class
                   const MilQuali& typeRef,
                   bool isPublic = true );

    MilModule* getCurrentModule() const { return module; }
    MilType* getCurrentType() const { return type; }
private:
    MilLoader* loader;
    MilModule* module;
    MilType* type;
};

}

#endif // MICMILLOADER_H
