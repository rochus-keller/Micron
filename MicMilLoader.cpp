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

MilModule*MilLoader::getModule(const QByteArray& fullName)
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].fullName.constData() == fullName.constData() )
            return &modules[i];
    }
    return 0;
}

InMemRenderer::InMemRenderer(MilLoader* loader):loader(loader), module(0), type(0)
{
    Q_ASSERT( loader );
}

void InMemRenderer::beginModule(const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& mp)
{
    loader->modules.append(MilModule());
    module = &loader->modules.back();
    module->fullName = moduleName;
    module->metaParams = mp;
}

void InMemRenderer::endModule()
{
    module = 0;
}

void InMemRenderer::addImport(const QByteArray& path)
{
    Q_ASSERT(module);
    module->imports.append(path);
    module->symbols[path.constData()] = qMakePair(MilModule::Import,module->imports.size()-1);
    module->order.append(qMakePair(MilModule::Import,module->imports.size()-1) );
}

void InMemRenderer::addVariable(const MilQuali& typeRef, QByteArray name,  bool isPublic)
{
    Q_ASSERT(module);
    MilVariable var;
    var.name = name;
    var.type = typeRef;
    var.isPublic = isPublic;
    module->vars.append(var);
    module->symbols[name.constData()] = qMakePair(MilModule::Variable,module->vars.size()-1);
    module->order.append(qMakePair(MilModule::Variable,module->vars.size()-1) );
}

void InMemRenderer::addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val)
{
    Q_ASSERT(module);
    MilConst c;
    c.name = name;
    c.type = typeRef;
    c.val = val;
    module->consts.append(c);
    module->symbols[name.constData()] = qMakePair(MilModule::Const,module->consts.size()-1);
    module->order.append(qMakePair(MilModule::Const,module->consts.size()-1) );
}

void InMemRenderer::addProcedure(const Mic::MilProcedure& method)
{
    Q_ASSERT(module);
    if( method.kind == Mic::MilProcedure::ProcType )
    {
        MilType t;
        t.name = method.name;
        t.isPublic = method.isPublic;
        t.kind = MilEmitter::ProcType;
        t.base = method.retType;
        t.fields = method.params;
        module->types.append(t);
        module->symbols[method.name.constData()] = qMakePair(MilModule::Type,module->types.size()-1);
        module->order.append(qMakePair(MilModule::Type,module->types.size()-1) );
    }else
    {
        module->procs.append(method);
        module->symbols[method.name.constData()] = qMakePair(MilModule::Proc,module->procs.size()-1);
        module->order.append(qMakePair(MilModule::Proc,module->procs.size()-1) );
    }
}

void InMemRenderer::beginType(const QByteArray& name, bool isPublic, quint8 typeKind)
{
    Q_ASSERT(module);
    MilType t;
    t.name = name;
    t.isPublic = isPublic;
    t.kind = typeKind;
    module->types.append(t);
    module->symbols[name.constData()] = qMakePair(MilModule::Type,module->types.size()-1);
    module->order.append(qMakePair(MilModule::Type,module->types.size()-1) );
    type = &module->types.back();
}

void InMemRenderer::endType()
{
    Q_ASSERT(module);
    type = 0;
}

void InMemRenderer::addType(const QByteArray& name, bool isPublic, const MilQuali& baseType, quint8 typeKind, quint32 len)
{
    Q_ASSERT(module);
    MilType t;
    t.name = name;
    t.base = baseType;
    t.isPublic = isPublic;
    t.kind = typeKind;
    t.len = len;
    module->types.append(t);
    module->symbols[name.constData()] = qMakePair(MilModule::Type,module->types.size()-1);
    module->order.append(qMakePair(MilModule::Type,module->types.size()-1));
}

void InMemRenderer::addField(const QByteArray& fieldName, const MilQuali& typeRef, bool isPublic)
{
    Q_ASSERT(module);
    Q_ASSERT(type);
    MilVariable f;
    f.name = fieldName;
    f.type = typeRef;
    f.isPublic = isPublic;
    type->fields.append(f);
}

static void render_(const MilType* t, MilRenderer* r)
{
    switch( t->kind )
    {
    case MilEmitter::Struct:
    case MilEmitter::Union:
    case MilEmitter::ProcType:
        r->beginType(t->name,t->isPublic,t->kind);
        foreach( const MilVariable& v, t->fields )
            r->addField(v.name,v.type,v.isPublic);
        r->endType();
        break;
    case MilEmitter::Alias:
    case MilEmitter::Pointer:
    case MilEmitter::Array:
        r->addType(t->name,t->isPublic,t->base,t->kind,t->len);
        break;
    }
}

static void render_(const MilVariable* v, MilRenderer* r)
{
    r->addVariable(v->type,v->name, v->isPublic);
}

static void render_(const MilProcedure* p, MilRenderer* r)
{
    r->addProcedure(*p);
}

bool MilLoader::render(MilRenderer* r, const MilModule* m)
{
    Q_ASSERT(r);
   // MilEmitter e(r);

    r->beginModule(m->fullName,"", m->metaParams);
    foreach( const MilModule::Order& i, m->order )
    {
        switch(i.first)
        {
        case MilModule::Type:
            render_(&m->types[i.second],r);
            break;
        case MilModule::Variable:
            render_(&m->vars[i.second],r);
            break;
        case MilModule::Proc:
            render_(&m->procs[i.second],r);
            break;
        case MilModule::Import:
            // TODO render_(&m->imports[i.second],r);
            break;
            // TODO Const
        }
    }
    r->endModule();
    return true;
}
