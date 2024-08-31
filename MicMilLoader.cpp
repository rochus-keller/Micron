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

#include "MicMilLoader.h"
using namespace Mic;

MilLoader::MilLoader()
{

}

const MilModule*MilLoader::getModule(const QByteArray& name) const
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].name == name )
            return &modules[i];
    }
    return 0;
}

InMemRenderer::InMemRenderer(MilLoader* loader):loader(loader), module(0), type(0)
{
    Q_ASSERT( loader );
}

void InMemRenderer::beginModule(const QByteArray& moduleName, const QString& sourceFile, const Mic::MilMetaParams&)
{
    loader->modules.append(MilModule());
    module = &loader->modules.back();
}

void InMemRenderer::endModule()
{
    module = 0;
}

void InMemRenderer::addImport(const QByteArray& path, const QByteArray& name)
{
    Q_ASSERT(module);
    MilImport imp;
    imp.name = name;
    imp.path = path;
    module->imports.append(imp);
    module->symbols[name] = qMakePair(MilModule::Import,module->imports.size()-1);
    module->order.append(qMakePair(MilModule::Import,module->imports.size()-1) );
}

void InMemRenderer::addVariable(const QByteArray& typeRef, QByteArray name,  bool isPublic)
{
    Q_ASSERT(module);
    MilVariable var;
    var.name = name;
    var.type = typeRef;
    var.isPublic = isPublic;
    module->vars.append(var);
    module->symbols[name] = qMakePair(MilModule::Variable,module->vars.size()-1);
    module->order.append(qMakePair(MilModule::Variable,module->vars.size()-1) );
}

void InMemRenderer::addProcedure(const Mic::MilProcedure& method)
{
    Q_ASSERT(module);
    module->procs.append(method);
    module->symbols[method.d_name] = qMakePair(MilModule::Proc,module->procs.size()-1);
    module->order.append(qMakePair(MilModule::Proc,module->procs.size()-1) );
}

void InMemRenderer::beginType(const QByteArray& name, bool isPublic, quint8 typeKind)
{
    Q_ASSERT(module);
    MilType t;
    t.name = name;
    t.isPublic = isPublic;
    t.kind = typeKind;
    module->types.append(t);
    module->symbols[name] = qMakePair(MilModule::Type,module->types.size()-1);
    module->order.append(qMakePair(MilModule::Type,module->types.size()-1) );
    type = &module->types.back();
}

void InMemRenderer::endType()
{
    Q_ASSERT(module);
    type = 0;
}

void InMemRenderer::addType(const QByteArray& name, bool isPublic, const QByteArray& baseType, quint8 typeKind, quint32 len)
{
    Q_ASSERT(module);
    MilType t;
    t.name = name;
    t.base = baseType;
    t.isPublic = isPublic;
    t.kind = typeKind;
    t.len = len;
    module->types.append(t);
    module->symbols[name] = qMakePair(MilModule::Type,module->types.size()-1);
    module->order.append(qMakePair(MilModule::Type,module->types.size()-1));
}

void InMemRenderer::addField(const QByteArray& fieldName, const QByteArray& typeRef, bool isPublic)
{
    Q_ASSERT(module);
    Q_ASSERT(type);
    MilVariable f;
    f.name = fieldName;
    f.type = typeRef;
    f.isPublic = isPublic;
    type->fields.append(f);
}
