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

#include "MicCilGen.h"
#include <PeLib/PublicApi.h>
#include <PeLib/PEMetaTables.h>
using namespace Mic;
using namespace DotNetPELib;

struct Thing
{
    enum Type { Module, Struct, Union, ProcType, Pointer, Array, Field, Var, Const, Proc } type;

    Thing(Type t, Resource* r):type(t),me(r){}
    ~Thing()
    {
        QMap<const char*,Thing*>::const_iterator i;
        for( i = subs.begin(); i != subs.end(); ++i )
            delete i.value();
    }

    Resource* me;
    QMap<const char*,Thing*> subs;

};

class CilGen::Imp
{
public:
    MilLoader* loader;
    InMemRenderer imr;
    PELib* out;
    typedef QMap<const char*,Thing*> Imports;
    Imports imports;
    Thing* module;
    QList<Thing*> current;

    Imp(MilLoader*l):loader(l),imr(l),out(0),module(0)
    {

    }

    ~Imp()
    {
        if( module )
            delete module;
    }

    void instantiate( const MilType& type, DataContainer* dc)
    {
        Type* t;
        Thing* base = 0;
        if( !type.base.second.isEmpty() )
            base = find(type.base);
        switch(type.kind)
        {
        // TODO
        case MilEmitter::Struct:
        case MilEmitter::Union:
            {
                Class* cls = new Class(type.name.constData(),Qualifiers::Public | Qualifiers::Value,-1,-1);
                dc->Add(cls);
                current.back()->subs.insert(type.name, new Thing(type.kind == MilEmitter::Struct ?
                                                                     Thing::Struct : Thing::Union,t));
            }
            break;
        case MilEmitter::ProcType:
            break;
        case MilEmitter::Alias:
            // TODO: call recursively for resolved base
            break;
        case MilEmitter::Pointer:
        case MilEmitter::Array:
            if( Type* primitive = dynamic_cast<Type*>(base->me) )
                t = new Type(primitive->GetBasicType());
            else
                t = new Type(dynamic_cast<DataContainer*>(base->me));
            if( type.kind == MilEmitter::Array )
                t->ArrayLevel(1);
            else
                t->PointerLevel(1);
            // TODO: use plain memory for array instead of built-in array type
            //dc->Add(t);
            current.back()->subs.insert(type.name, new Thing(Thing::Array,t));
            break;
        default:
            Q_ASSERT(false);
        }
    }

    void instantiate( const MilProcedure& p, DataContainer* dc)
    {
        // TODO
    }

    void instantiate( const MilVariable& v, DataContainer* dc, bool module)
    {
        Thing* type = find(v.type);
        Qualifiers q = Qualifiers::Public;
        if( module )
            q |= Qualifiers::Static;
        Field* f = new Field(v.name.constData(),dynamic_cast<Type*>(type->me), q);
        dc->Add(f);
        current.back()->subs.insert(v.name, new Thing(Thing::Var,f));
    }

    void instantiate( const MilConst& c, DataContainer* dc)
    {
        // NOP ?
    }

    void instantiate( const QByteArray& module)
    {
        fetchImport(module);
    }

    void instantiate(const MilModule* m, DataContainer* dc )
    {
        for( int i = 0; i < m->order.size(); i++ )
        {
            const MilModule::Order& o = m->order[i];
            switch( o.first )
            {
            case MilModule::Type:
                instantiate(m->types[o.second],dc);
                break;
            case MilModule::Variable:
                instantiate(m->vars[o.second],dc,true);
                break;
            case MilModule::Proc:
                instantiate(m->procs[o.second],dc);
                break;
            case MilModule::Import:
                instantiate(m->imports[o.second]);
                break;
            case MilModule::Const:
                instantiate(m->consts[o.second],dc);
                break;
            }
        }
    }

    Thing* fetchImport( const QByteArray& module )
    {
        const MilModule* m = loader->getModule(module);
        if( m == 0 )
            return 0;
        AssemblyDef* a = out->AddExternalAssembly(module.constData());
        Thing* res = new Thing(Thing::Module, a);
        imports.insert(module,res);
        current.push_back(res);
        instantiate(m,a);
        current.pop_back();
        return res;
    }

    static void cannotResolve( const QString& what )
    {
        throw QString("cannot resolve '%1'").arg(what);
    }

    Thing* find(const MilTrident& what)
    {
        Thing* type = find(what.first);
        Thing* res = type->subs.value(what.second.constData());
        if( res == 0 )
            cannotResolve(what.second);
        return res;
    }

    Thing* find(const MilQuali& what)
    {
        Thing* m = module;
        if( !what.first.isEmpty() )
        {
            Thing* t = imports.value(what.first.constData());
            if( t == 0 )
            {
                t = fetchImport(what.first);
                if( t == 0 )
                    cannotResolve(what.first);
                imports.insert(what.first.constData(), t );
            }
            m = t;
        }
        Thing* res = m->subs.value(what.second.constData());
        if( res == 0 )
            cannotResolve(what.second);
        return res;
    }

    void addLabelOp( Method* m, quint8 op, const QByteArray& label )
    {
        m->AddInstruction(new Instruction((Instruction::iop)op, new Operand( label.constData() ) ) );
    }

    Thing* addTypeOp( Method* m, quint8 op, const MilQuali& typeRef )
    {
        Thing* type = find(typeRef);
        Type* t = dynamic_cast<Type*>(type->me);
        Q_ASSERT( t );
        m->AddInstruction(new Instruction((Instruction::iop)op, new Operand( new Value(t))));
        return type;
    }

    void addMethodOp( Method* m, quint8 op, const MilQuali& methodRef )
    {
        Thing* node = find(methodRef);
        Q_ASSERT(node);
        MethodSignature* sig = 0;
        if( Method* meth = dynamic_cast<Method*>(node->me) )
            sig = meth->Signature();
        else
            sig = dynamic_cast<MethodSignature*>(node->me); // happens only for vararg signatures
        Q_ASSERT( sig );
        m->AddInstruction(new Instruction((Instruction::iop)op, new Operand( new MethodName( sig ))));
    }

    void addFieldOp( Method* m, quint8 op, const MilTrident& fieldRef )
    {
        Thing* node = find(fieldRef);
        Q_ASSERT(node);
        Field* f = dynamic_cast<Field*>(node->me);
        Q_ASSERT( f );
        m->AddInstruction( new Instruction((Instruction::iop)op, new Operand( new FieldName(f))));
    }

    void addOperand( Method* m, quint8 op, Operand* v )
    {
        m->AddInstruction( new Instruction((Instruction::iop)op, v));
    }

    void addLocalOp( Method* m, quint8 op, const QByteArray& local )
    {
        m->AddInstruction( new Instruction((Instruction::iop)op,
                                    new Operand( m->getLocal(local.toInt() ))));
    }

    void addArgOp( Method* m, quint8 op, const QByteArray& arg )
    {
        int i = arg.toInt();
        MethodSignature* sig = m->Signature();
        m->AddInstruction( new Instruction((Instruction::iop)op,new Operand(sig->getParam(i))));
    }
};

CilGen::CilGen(MilLoader* l)
{
    imp = new Imp(l);
}

CilGen::~CilGen()
{
    delete imp;
}

void CilGen::beginModule(const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& mp)
{
    imp->imr.beginModule(moduleName, sourceFile, mp);
    imp->out = new PELib(moduleName.constData(), PELib::ilonly);

    if( imp->module )
        delete imp->module;

    // PELib implicitly associates a CIL assembly with exactly one CIL module. The WorkingAssembly is
    // automatically created by PELib; we use it as a MIC module. CIL supports creating fields and
    // methods directly in the CIL module.
    imp->module = new Thing(Thing::Module,imp->out->WorkingAssembly());
    imp->current.append(imp->module);
}

void CilGen::endModule()
{
    imp->imr.endModule();
    MetaBase::dump();
    delete imp->out;
    imp->out = 0;
}

void CilGen::addImport(const QByteArray& path)
{
    imp->imr.addImport(path);
}

void CilGen::addVariable(const MilQuali& typeRef, QByteArray name, bool isPublic)
{
    imp->imr.addVariable(typeRef, name, isPublic);
}

void CilGen::addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val)
{
    imp->imr.addConst(typeRef, name, val);
}

void CilGen::addProcedure(const MilProcedure& method)
{
    imp->imr.addProcedure(method);
}

void CilGen::beginType(const QByteArray& name, bool isPublic, quint8 typeKind)
{
    imp->imr.beginType(name, isPublic, typeKind);
}

void CilGen::endType()
{
    imp->imr.endType();
}

void CilGen::addType(const QByteArray& name, bool isPublic, const MilQuali& baseType, quint8 typeKind, quint32 len)
{
    imp->imr.addType(name, isPublic, baseType, typeKind, len);
}

void CilGen::addField(const QByteArray& fieldName, const MilQuali& typeRef, bool isPublic)
{
    imp->imr.addField(fieldName, typeRef, isPublic);
}
