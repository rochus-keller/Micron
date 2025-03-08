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
#include <QtDebug>
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
    if( method.kind == Mic::MilProcedure::ProcType || method.kind == Mic::MilProcedure::MethType )
    {
        MilType t;
        t.name = method.name;
        t.isPublic = method.isPublic;
        t.kind = method.kind == Mic::MilProcedure::ProcType ? MilEmitter::ProcType : MilEmitter::MethType;
        t.base = method.retType;
        t.fields = method.params;
        module->types.append(t);
        module->symbols[method.name.constData()] = qMakePair(MilModule::Type,module->types.size()-1);
        module->order.append(qMakePair(MilModule::Type,module->types.size()-1) );
    }else
    {
        if( !method.binding.isEmpty() )
        {
            QPair<MilModule::What,quint32> what = module->symbols.value(method.binding.constData());
            if( what.first != MilModule::Type )
            {
                qCritical() << "InMemRenderer::addProcedure: receiver type not found:" << method.binding;
                return;
            }
            MilType& type = module->types[what.second];
            if( type.kind != MilEmitter::Object )
            {
                qCritical() << "InMemRenderer::addProcedure: only object types can be referenced as receivers:"
                            << method.binding << type.name;
                return;
            }
            type.methods.append(method);
        }else
        {
            module->procs.append(method);
            module->symbols[method.name.constData()] = qMakePair(MilModule::Proc,module->procs.size()-1);
            module->order.append(qMakePair(MilModule::Proc,module->procs.size()-1) );
        }
    }
}

void InMemRenderer::beginType(const QByteArray& name, bool isPublic, quint8 typeKind, const MilQuali& super)
{
    Q_ASSERT(module);
    MilType t;
    t.name = name;
    t.isPublic = isPublic;
    t.kind = typeKind;
    if( !super.second.isEmpty() )
    {
        if( typeKind != MilEmitter::Object )
        {
            qCritical() << "InMemRenderer::beginType: only object types can have a super type:" << name;
            return;
        }
        MilModule* m = module;
        if( !super.first.isEmpty() )
        {
            m = loader->getModule(super.first);
            if( m == 0 )
            {
                qCritical() << "InMemRenderer::beginType: module of super type not found:" << super.first;
                return;
            }
        }
        QPair<MilModule::What,quint32> what = m->symbols.value(super.second.constData());
        if( what.first != MilModule::Type )
        {
            qCritical() << "InMemRenderer::beginType: super type not found:" << super.first << super.second;
            return;
        }
        MilType& type = m->types[what.second];
        if( type.kind != MilEmitter::Object )
        {
            qCritical() << "InMemRenderer::beginType: only object types can be referenced as super type:"
                        << super.first << super.second;
            return;
        }
        t.base = super;
    }
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

void InMemRenderer::addField(const QByteArray& fieldName, const MilQuali& typeRef, bool isPublic, quint8 bits)
{
    Q_ASSERT(module);
    Q_ASSERT(type);
    MilVariable f;
    f.name = fieldName;
    f.type = typeRef;
    f.isPublic = isPublic;
    f.bits = bits;
    type->fields.append(f);
}

static void render_(const MilType* t, MilRenderer* r)
{
    switch( t->kind )
    {
    case MilEmitter::Struct:
    case MilEmitter::Union:
    case MilEmitter::Object:
        r->beginType(t->name,t->isPublic,t->kind, t->base);
        foreach( const MilVariable& v, t->fields )
            r->addField(v.name,v.type,v.isPublic,v.bits);
        r->endType();
        foreach( const MilProcedure& p, t->methods )
        {
            r->addProcedure(p);
        }
        break;
    case MilEmitter::ProcType:
    case MilEmitter::MethType:
        {
            Mic::MilProcedure proc;
            proc.name = t->name;
            proc.isPublic = t->isPublic;
            proc.kind = t->kind == MilEmitter::ProcType ? Mic::MilProcedure::ProcType : Mic::MilProcedure::MethType;
            proc.params = t->fields;
            proc.retType = t->base;
            r->addProcedure(proc);
        }
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
