#ifndef MICMILLOADER2_H
#define MICMILLOADER2_H

/*
* Copyright 2019-2025 Rochus Keller <mailto:me@rochus-keller.ch>
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
#include "MilAst.h"
#include <QHash>

namespace Mic
{

class MilLoader2 : public Mil::Importer
{
public:
    MilLoader2();

    Mil::Declaration* loadFromFile( const QString& path);
    Mil::AstModel& getModel() { return mdl; }
    QList<Mil::Declaration*> getModulesInDependencyOrder();
    static bool render(MilRenderer*, const Mil::Declaration* module);
protected:
    Mil::Declaration* loadModule( const Mil::Import& imp );

private:
    friend class InMemRenderer2;
    Mil::AstModel mdl;
};

class InMemRenderer2 : public MilRenderer
{
public:
    InMemRenderer2(MilLoader2*);
    ~InMemRenderer2();

    bool commit();

    void beginModule( const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& );
    void endModule();

    void addImport(const QByteArray& path);

    void addVariable(const MilQuali& typeRef, QByteArray name , bool isPublic);
    void addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val );
    void addProcedure(const MilProcedure& method );

    void beginType(const QByteArray& name, bool isPublic, quint8 typeKind, const MilQuali& super);
    void endType();
    void addType( const QByteArray& name, bool isPublic, const MilQuali& baseType,
                  quint8 typeKind, quint32 len = 0);

    void addField( const QByteArray& fieldName, // on top level or in class
                   const MilQuali& typeRef,
                   bool isPublic = true, quint8 bits = 0 );

    Mil::Declaration* getCurrentModule() const { return module; }
    Mil::Type* getCurrentType() const { return type; }

protected:
    Mil::Type* derefType(const Mil::Quali& ) const;
    Mil::Statement* translateStat(const QList<MilOperation>& ops, quint32& pc);
    Mil::Expression* translateExpr(const QList<MilOperation>& ops, quint32& pc);
    bool expect(const QList<MilOperation>& ops, quint32& pc, int op);
    Mil::Declaration* derefTrident(const MilTrident& ) const;
    Mil::Declaration* resolve(const Mil::Quali&) const;
    void error(const QString&, int pc = -1) const;

private:
    MilLoader2* loader;
    Mil::Declaration* module;
    Mil::Type* type;
    Mil::Declaration* curProc;
    mutable QList<Mil::Type*> unresolved;
    mutable bool hasError;
};

}

#endif // MICMILLOADER_H
