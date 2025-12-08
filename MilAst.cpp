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

#include "MilAst.h"
#include "MicAtom.h"
#include <QtDebug>
#include <algorithm>
using namespace Mil;

static void addTypeDecl(Declaration* globals, Type* t, const QByteArray& name)
{
    Declaration* res = new Declaration();
    res->kind = Declaration ::TypeDecl;
    res->name = Mic::Atom::getAtom(name);
    res->public_ = true;
    // NO, otherwise INT32 gets a $ prefix: res->outer = globals;
    res->setType(t);
    if( t->decl == 0 )
        t->decl = res;
    globals->appendSub(res);
}

AstModel::AstModel()
{
    for( int i = 0; i < Type::MaxBasicType; i++ )
    {
        Type* t = new Type();
        t->kind = (Type::Kind)i;
        t->owned = true;
        basicTypes[i] = t;
    }
    addTypeDecl(&globals, basicTypes[Type::Any], "any");
    addTypeDecl(&globals, basicTypes[Type::Any], "ANY");
    addTypeDecl(&globals, basicTypes[Type::BOOL], "bool");
    addTypeDecl(&globals, basicTypes[Type::BOOL], "BOOL");
    addTypeDecl(&globals, basicTypes[Type::CHAR], "char");
    addTypeDecl(&globals, basicTypes[Type::CHAR], "CHAR");
    addTypeDecl(&globals, basicTypes[Type::INT8], "int8");
    addTypeDecl(&globals, basicTypes[Type::INT8], "INT8");
    addTypeDecl(&globals, basicTypes[Type::INT8], "I1");
    addTypeDecl(&globals, basicTypes[Type::INT8], "i1");
    addTypeDecl(&globals, basicTypes[Type::INT16], "int16");
    addTypeDecl(&globals, basicTypes[Type::INT16], "INT16");
    addTypeDecl(&globals, basicTypes[Type::INT16], "I2");
    addTypeDecl(&globals, basicTypes[Type::INT16], "i2");
    addTypeDecl(&globals, basicTypes[Type::INT32], "int32");
    addTypeDecl(&globals, basicTypes[Type::INT32], "INT32");
    addTypeDecl(&globals, basicTypes[Type::INT32], "I4");
    addTypeDecl(&globals, basicTypes[Type::INT32], "i4");
    addTypeDecl(&globals, basicTypes[Type::INT64], "int64");
    addTypeDecl(&globals, basicTypes[Type::INT64], "INT64");
    addTypeDecl(&globals, basicTypes[Type::INT64], "I8");
    addTypeDecl(&globals, basicTypes[Type::INT64], "i8");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "uint8");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "UINT8");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "U1");
    addTypeDecl(&globals, basicTypes[Type::UINT8], "u1");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "uint16");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "UINT16");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "U2");
    addTypeDecl(&globals, basicTypes[Type::UINT16], "u2");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "uint32");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "UINT32");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "U4");
    addTypeDecl(&globals, basicTypes[Type::UINT32], "u4");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "uint64");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "UINT64");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "U8");
    addTypeDecl(&globals, basicTypes[Type::UINT64], "u8");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "float32");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "FLOAT32");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "R4");
    addTypeDecl(&globals, basicTypes[Type::FLOAT32], "r4");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "float64");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "FLOAT64");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "R8");
    addTypeDecl(&globals, basicTypes[Type::FLOAT64], "r8");
    addTypeDecl(&globals, basicTypes[Type::INTPTR], "intptr");
    addTypeDecl(&globals, basicTypes[Type::INTPTR], "INTPTR");
}

AstModel::~AstModel()
{
    clear();
    for( int i = 0; i < Type::MaxBasicType; i++ )
        delete basicTypes[i];
}

void AstModel::clear()
{
    foreach( Declaration* module, modules )
        delete module;
    modules.clear();
}

Declaration*AstModel::findModuleByName(const QByteArray& name) const
{
    // TODO: consider generic module instances with e.g. a deterministic suffix
    foreach( Declaration* module, modules )
    {
        if( module->name.constData() == name.constData() )
            return module;
    }
    return 0;
}

bool AstModel::addModule(Declaration* module)
{
    Q_ASSERT(module && module->kind == Declaration::Module);
    if( findModuleByName(module->name) )
        return false;
    //else
    modules.append(module);
    return true;
}

Type* AstModel::getBasicType(quint8 k) const
{
    if( k < Type::MaxBasicType )
        return basicTypes[k];
    else
        return basicTypes[0];
}

Declaration*AstModel::resolve(const Quali& q) const
{
    if( q.first.isEmpty() || q.second.isEmpty() )
        return 0;
    Declaration* module = findModuleByName(q.first);
    if( module == 0 )
        return 0;

    return module->findSubByName(q.second);
}

void AstModel::calcMemoryLayouts(quint8 pointerWidth, quint8 stackAlignment, quint8 firstParamOffset)
{
    varOff = 0;
    DeclList done;
    foreach( Declaration* m, modules )
    {
        if( done.contains(m) )
            continue;
        done << m;
        walkImports(m, done, pointerWidth, stackAlignment, firstParamOffset);
        calcMemoryLayoutOf(m, pointerWidth, stackAlignment, firstParamOffset);
    }
}

void AstModel::walkImports(Declaration* m, DeclList& done, quint8 pointerWidth, quint8 stackAlignment, quint8 firstParamOffset)
{
    Declaration* sub = m->subs;
    while(sub)
    {
        if( sub->kind == Declaration::Import )
        {
            if( sub->imported && !done.contains(sub->imported) )
            {
                done << sub->imported;
                calcMemoryLayoutOf(sub->imported, pointerWidth, stackAlignment, firstParamOffset);
            }
        }
        sub = sub->next;
    }
}

AstModel::BitFieldUnit AstModel::collectBitFields(const DeclList& fields, int start, quint8 pointerWidth)
{
    Q_ASSERT( start < fields.size() && fields[start]->kind == Declaration::Field &&
              fields[start]->f.bw && fields[start]->f.bw <= 64 );
    BitFieldUnit res;

    res.start = start;
    if( fields[start]->f.bw >= pointerWidth )
    {
        // special case where bw is larger than 32 bits on a 32 bit machine
        res.len = 1;
        res.bits = fields[start]->f.bw;
        res.bytelen = 8;
        return res;
    } // else

    res.len = 0;
    res.bits = fields[start]->f.bw;
    for(int i = start + 1; i < fields.size(); i++ )
    {
        Declaration* field = fields[i];
        if( field->kind != Declaration::Field ||
                (res.bits + field->f.bw) > pointerWidth ||
                field->f.bw == 0 )
            break;
        res.len++;
        res.bits += field->f.bw;
    }
    if( res.bits <= 8 )
        res.bytelen = 8;
    else if( res.bits <= 16 )
        res.bytelen = 16;
    else if( res.bits <= 32 )
        res.bytelen = 32;
    else
        res.bytelen = 64;
    return res;
}

void AstModel::calcMemoryLayoutOf(Declaration* module, quint8 pointerWidth, quint8 stackAlignment, quint8 firstParamOffset)
{
    // RISK: this function assumes that declaration order reflects dependency order, besides pointers
    Declaration* sub = module->subs;
    while(sub)
    {
        Type* type = sub->getType();
        switch(sub->kind)
        {
        case Declaration::TypeDecl:
            switch( type->kind )
            {
            case Type::Struct:
                {
                    int off = 0;
                    int maxAlig = 1;
                    int i = 0;
                    while(i < type->subs.size())
                    {
                        Declaration* field = type->subs[i];
                        if( field->kind == Declaration::Field )
                        {
                            int alig = 0;
                            if( field->f.bw )
                            {
                                // bitfield
                                const BitFieldUnit unit = collectBitFields(type->subs, i, pointerWidth);
                                alig = unit.bytelen;
                                if( i != 0 )
                                    off += AstModel::padding(off, alig);
                                for( int j = unit.start; j < unit.start + unit.len; j++ )
                                    type->subs[j]->f.off = off;
                                i += unit.len-1;
                            }else
                            {
                                // a normal field
                                Type* t = field->getType();
                                const int size = t->getByteSize(pointerWidth);
                                alig = t->getAlignment(pointerWidth);
                                if( i != 0 )
                                    off += AstModel::padding(off, alig);
                                field->f.off = off;
                                off += size;
                            }
                            if( alig > maxAlig )
                                maxAlig = alig;
                        }
                        i++;
                    }
                    type->bytesize = off + AstModel::padding(off, maxAlig);
                }
                break;
            case Type::Union:
                {
                    int size = 0;
                    foreach( Declaration* field, type->subs)
                    {
                        if( field->kind == Declaration::Field )
                        {
                            Type* t = field->getType();
                            const int s = t->getByteSize(pointerWidth);
                            if( s > size )
                                size = s;
                        }
                    }
                    type->bytesize = size;
                }
                break;
            case Type::Object:
                {
                    int off = pointerWidth; // spare room for the vtbl pointer
                    int maxAlig = off;
                    DeclList fields = type->getFieldList(true);
                    foreach( Declaration* field, fields)
                    {
                        Q_ASSERT( field->kind == Declaration::Field );
                        Type* t = field->getType();
                        const int size = t->getByteSize(pointerWidth);
                        const int alig = t->getAlignment(pointerWidth);
                        if( alig > maxAlig )
                            maxAlig = alig;
                        off += AstModel::padding(off, alig);
                        field->f.off = off;
                        off += size;
                    }
                    type->bytesize = off + AstModel::padding(off, maxAlig);
                    foreach( Declaration* sub, type->subs )
                    {
                        if( sub->kind == Declaration::Procedure )
                            calcParamsLocalsLayout(sub, pointerWidth, stackAlignment, firstParamOffset);
                    }
                }
                break;
            case Type::Pointer:
                type->bytesize = pointerWidth;
                break;
            }
            break;
        case Declaration::Procedure:
            calcParamsLocalsLayout(sub, pointerWidth, stackAlignment, firstParamOffset);
            break;
        case Declaration::VarDecl:
            {
                Type* t = sub->getType();
                const int size = t->getByteSize(pointerWidth);
                const int alig = t->getAlignment(pointerWidth);
                varOff += AstModel::padding(varOff, alig);
                sub->off = varOff;
                varOff += size;
            }
            break;
        }

        sub = sub->next;
    }
}

void AstModel::calcParamsLocalsLayout(Declaration* proc, quint8 pointerWidth, quint8 stackAlignment, quint8 firstParamOffset)
{
    int off_l = 0;
    QList<Declaration*> params;
    Declaration* subsub = proc->subs;
    while(subsub)
    {
        Type* t = subsub->getType();
        switch( subsub->kind )
        {
        case Declaration::ParamDecl:
            params.append(subsub);
            break;
        case Declaration::LocalDecl:
            {
                const int size = t->getByteSize(pointerWidth);
                const int alig = t->getAlignment(pointerWidth);
                off_l += AstModel::padding(off_l, alig);
                subsub->off = off_l;
                off_l += size;
            }
            break;
        }
        subsub = subsub->next;
    }
    int off_p = firstParamOffset;
#if 0
    // NOTE: this is wrong; they are pushed in reverse order, but the offsets are in original order
    if( reverseParamOrder )
        std::reverse(params.begin(), params.end());
#endif
    foreach( Declaration* p, params )
    {
        Type* t = p->getType();
        const int size = t->getByteSize(pointerWidth);
        const int alig = t->getAlignment(pointerWidth);
        off_p += AstModel::padding(off_p, alig);
        p->off = off_p;
        off_p += qMax(size,(int)stackAlignment);
    }
}

Node::~Node()
{
    if( type && ownstype )
        delete type;
}

void Node::setType(Type* t)
{
    if( type && ownstype )
        delete type;
    type = t;
    if( t && !t->owned )
    {
        ownstype = true;
        t->owned = true;
        if( meta == D && t->decl == 0 )
            t->decl = static_cast<Declaration*>(this);
    }
}

Declaration::~Declaration()
{
    if( kind == ConstDecl && c )
        delete c;
    else if( kind == Procedure && !forward && pd )
        delete pd;
    else if( kind == Module && md )
        delete md;
    if( subs )
        delete subs;
    if( body )
        delete body;
    if( next )
        delete next;
}

void Declaration::appendSub(Declaration* d)
{
    if( subs == 0 )
        subs = d;
    else
        append(subs,d);
}

Declaration* Declaration::findSubByName(const QByteArray& name) const
{
    Declaration* d = subs;
    while( d && d->name.constData() != name.constData() )
        d = d->next;
    if( d && d->name.constData() == name.constData())
        return d;
    else
        return 0;
}

QList<Declaration*> Declaration::getParams(bool includeSelf) const
{
    QList<Declaration*> res;
    Declaration* d = subs;
    while( d )
    {
        if( d->kind == Declaration::ParamDecl && (includeSelf || !d->typebound) )
            res << d;
        d = d->next;
    }
    return res;
}

QList<Declaration*> Declaration::getLocals() const
{
    QList<Declaration*> res;
    Declaration* d = subs;
    while( d )
    {
        if( d->kind == Declaration::LocalDecl )
            res << d;
        d = d->next;
    }
    return res;
}

QList<Declaration*> Declaration::getVars() const
{
    QList<Declaration*> res;
    Declaration* d = subs;
    while( d )
    {
        if( d->kind == Declaration::VarDecl )
            res << d;
        d = d->next;
    }
    return res;
}

int Declaration::indexOf(Declaration* ref) const
{
    Declaration* d = subs;
    int i = 0;
    while( d )
    {
        if( d == ref )
            return i;
        d = d->next;
        i++;
    }
    return -1;
}

void Declaration::append(Declaration* list, Declaration* next)
{
    while( list && list->next )
        list = list->next;
    if( list )
    {
        Q_ASSERT(list->next == 0);
        list->next = next;
    }
}

QByteArray Declaration::toPath() const
{
    QByteArray res;
    if( (kind == Procedure && typebound) || kind == Field || kind == LocalDecl || kind == ParamDecl )
    {
        res = "." + name;
    }else if( kind != Module )
    {
        res = name;
        if( outer && outer->kind != Module )
            res = "$" + res;
    }else
        res = name;
    if( outer )
    {
        if( outer->kind == Module )
            res = "!" + res;
        return outer->toPath() + res;
    }else
        return res;
}

Quali Declaration::toQuali() const
{
    Quali res;
    if( (kind == Procedure && typebound) || kind == Field || kind == LocalDecl || kind == ParamDecl )
    {
        res.second = "." + name;
    }else if( kind == Module )
        res.first = name;
    else if( outer && outer->kind != Module )
        res.second = "$" + name;
    else
        res.second = name;

    if( outer )
    {
        Quali tmp = outer->toQuali();
        res.first = tmp.first + res.first;
        res.second = tmp.second + res.second;
    }
    return res;
}

Declaration*Declaration::forwardToProc() const
{
    if( kind == Procedure && forward )
        return forwardTo->forwardToProc();
    else
        return const_cast<Declaration*>(this);
}

Declaration*Declaration::getModule() const
{
    Declaration* m = const_cast<Declaration*>(this);
    while( m )
    {
        if( m->kind == Declaration::Module )
            return m;
        m = m->outer;
    }
    return 0;
}

Declaration*Declaration::findInitProc() const
{
    if( kind != Module )
        return 0;
    Declaration* init = subs;
    while(init)
    {
        if( init->entryPoint )
            return init;
        init = init->next;
    }
    return 0;
}

ProcedureData *Declaration::getPd()
{
    if( kind == Procedure && !forward )
    {
        if( pd == 0 )
            pd = new ProcedureData();
        return pd;
    }else
        return 0;
}

Expression::~Expression()
{
    if( (kind == IL_ldc_obj || kind == IL_ldstr) && c != 0 )
        delete c;
    else if( (kind == IL_iif || kind == IL_if || kind == IL_then || kind == IL_else ) && e != 0 )
        delete e;
    if( next )
        delete next;
}

void Expression::append(Expression* e)
{
    if( next == 0 )
    {
        next = e;
        return;
    }
    Expression* l = next;
    while( l && l->next )
        l = l->next;
    Q_ASSERT( l && l->next == 0 );
    l->next = e;
}

Component::~Component()
{
    if( c )
        delete c;
}

Constant::~Constant()
{
    switch( kind )
    {
    case S:
        free(s);
        break;
    case B:
        delete b;
        break;
    case C:
        delete c;
        break;
    }
}

ComponentList::~ComponentList()
{
    if( type && !type->owned )
        delete type;
}

ByteString::ByteString(const QByteArray& ba)
{
    b = (unsigned char*)malloc(ba.size());
    memcpy(b, ba.data(), ba.size());
    len = ba.size();
}

ByteString::~ByteString()
{
    if( b )
        free(b);
}

Type::~Type()
{
    if( kind == NameRef && quali )
        delete quali;
    if( !subsborrowed )
        foreach( Declaration* sub, subs )
            delete sub;
}

bool Type::isInt32OnStack() const
{
    switch(kind)
    {
    case BOOL:
    case CHAR:
    case INT8:
    case UINT8:
    case INT16:
    case UINT16:
    case INT32:
    case UINT32:
        return true;
    }
    return false;
}

bool Type::isFuncOnStack() const
{
    return (kind == Proc && !typebound) || kind == NIL || kind == INTPTR;
}

bool Type::isMethOnStack() const
{
    return (kind == Proc && typebound) || kind == NIL || kind == INTPTR;
}

Declaration*Type::findSubByName(const QByteArray& name, bool recursive) const
{
    if( name.isEmpty() )
        return 0;
    for( int i = 0; i < subs.size(); i++ )
    {
        if( subs[i]->name.constData() == name.constData() )
            return subs[i];
    }
    if( getType() && recursive )
        return getType()->findSubByName(name,recursive);
    return 0;
}

int Type::numOfNonFwdNonOverrideProcs() const
{
    int count = 0;
    if( kind != Object )
        return count;
    Type* base = getBaseObject();
    if( base )
        count = base->numOfNonFwdNonOverrideProcs();
    for( int i = 0; i < subs.size(); i++ )
    {
        if( subs[i]->kind == Declaration::Procedure && !subs[i]->forward && !subs[i]->override_ )
            count++;
    }
    return count;
}

Type* Type::getBaseObject() const
{
    if( kind != Object )
        return 0;
    Type* base = getType();
    if( base == 0 )
        return 0;
    base = base->deref();
    if( base->kind == Pointer )
    {
        base = base->getType();
        if( base == 0 )
            return 0;
        base = base->deref();
    }
    if( base && base->kind == Object )
        return base;
    else
        return 0;
}

QList<Declaration*> Type::getMethodTable(bool recursive) const
{
    DeclList tbl;
    if( kind != Object && kind != Struct && kind != Interface )
        return tbl;
    if( recursive )
    {
        Type* base = getBaseObject();
        if( base )
            tbl = base->getMethodTable();
    }
    for( int i = 0; i < subs.size(); i++ )
    {
        Declaration* p = subs[i];
        if( p->kind != Declaration::Procedure || p->forward )
            continue;
        if( recursive && p->override_)
        {
            bool found = false;
            for( int j = 0; j < tbl.size(); j++ )
            {
                if( tbl[j]->name.constData() == p->name.constData() )
                {
                    tbl[j] = p;
                    found = true;
                    break;
                }
            }
            Q_ASSERT(found);
        }else
            tbl.append(p);
    }
    return tbl;
}

QList<Declaration*> Type::getFieldList(bool recursive) const
{
    DeclList res;
    if( recursive && getType() && getType()->kind == Type::Object )
        res = getType()->getFieldList(recursive);
    foreach(Declaration* f, subs)
    {
        if( f->kind == Declaration::Field )
            res << f;
    }
    return res;
}

Type*Type::deref() const
{
    if( kind == NameRef && type )
        return type->deref();
    else
        return const_cast<Type*>(this);
}

bool Type::isPtrToArray() const
{
    if( kind == Pointer )
    {
        Type* base = getType();
        if( base )
            base = base->deref();
        return base->kind == Array;
    }else
        return false;
}

bool Type::isPtrToFixArray() const
{
    if( kind == Pointer )
    {
        Type* base = getType();
        if( base )
            base = base->deref();
        return base->kind == Array && base->len != 0;
    }else
        return false;
}

bool Type::isPtrToOpenArray() const
{
    if( kind == Pointer )
    {
        Type* base = getType();
        if( base )
            base = base->deref();
        return base->kind == Array && base->len == 0;
    }else
        return false;
}

bool Type::isPtrToOpenCharArray() const
{
    if( kind == Pointer )
    {
        Type* to = getType();
        if( to )
            to = to->deref();
        if( to->kind == Array )
        {
            Type* et = to->getType();
            if( et )
                et = et->deref();
            if( et->kind == Type::CHAR && to->len == 0 )
                return true;
        }
    } // else
    return false;
}

quint32 Type::getByteSize(quint8 pointerWidth) const
{
    switch( kind )
    {
    case NIL:
        return 0;
    case BOOL:
    case CHAR:
    case INT8:
    case UINT8:
        return 1;
    case INT16:
    case UINT16:
        return 2;
    case INT32:
    case UINT32:
    case FLOAT32:
        return 4;
    case INT64:
    case UINT64:
    case FLOAT64:
        return 8;
    case Array:
        {
            Type* et = getType();
            if( et ) et = et->deref();
            return len * et->getByteSize(pointerWidth);
        }
    case Pointer:
    case Proc:
        return pointerWidth;
    case NameRef:
        if( getType() )
            return getType()->getByteSize(pointerWidth);
        break;
    }
    return deref()->bytesize;
}

quint32 Type::getAlignment(quint8 pointerWidth) const
{
    quint32 res = 0;
    int i;
    switch( kind )
    {
    case Struct:
        i = 0;
        while( i < subs.size() )
        {
            Declaration* f = subs[i];
            if( f->kind == Declaration::Field )
            {
                quint32 a = 0;
                if( f->f.bw )
                {
                    AstModel::BitFieldUnit unit = AstModel::collectBitFields(subs, i, pointerWidth);
                    i += unit.len-1;
                    a = unit.bytelen;
                }else
                    a = f->getType()->getAlignment(pointerWidth);
                if( a > res )
                    res = a;
            }
            i++;
        }
        break;
    case Object:
        if( getType() )
            res = getType()->getAlignment(pointerWidth);
        else
            res = pointerWidth; // the vtbl pointer
        foreach( Declaration* f, subs )
        {
            if( f->kind != Declaration::Field )
                continue;
            const quint32 a = f->getType()->getAlignment(pointerWidth);
            if( a > res )
                res = a;
        }
        break;
    case Union:
        foreach( Declaration* f, subs )
        {
            if( f->kind != Declaration::Field )
                continue;
            const quint32 a = f->getType()->getAlignment(pointerWidth);
            if( a > res )
                res = a;
        }
        break;
    case Array:
        if( len )
            res = getType()->getAlignment(pointerWidth);
        break;
    default:
        res = deref()->getByteSize(pointerWidth);
        break;
    }
    return res;
}

bool Type::isA(Type* sub, Type* super)
{
    if( sub == 0 || super == 0 || sub->kind != Object || super->kind != Object)
        return false;
    while(sub)
    {
        if( sub == super )
            return true;
        sub = sub->getType();
        if( sub ) sub = sub->deref();
    }
    return false;
}

Quali Type::toQuali() const
{
    if( kind == NameRef )
    {
        if( quali )
            return *quali;
        else if( type )
            return type->toQuali();
    }
    if( decl )
        return decl->toQuali();
    else
        return Quali();
}

Statement::~Statement()
{
    if( body )
        delete body;
    if( (kind == ExprStat || kind == IL_if || kind == IL_switch || kind == IL_case ||
         kind == IL_repeat || kind == IL_while) && e )
        delete e;
    if( next )
        delete next;
}

void Statement::append(Statement* s)
{
    if( next == 0 )
    {
        next = s;
        return;
    }
    Statement* l = next;
    while( l && l->next )
        l = l->next;
    Q_ASSERT( l && l->next == 0 );
    l->next = s;
}

ToDelete::~ToDelete()
{
    if( arg )
        delete arg;
    if( next )
        delete next;
}

void ToDelete::append(ToDelete* td)
{
    if( next == 0 )
    {
        next = td;
        return;
    }
    ToDelete* l = next;
    while( l && l->next )
        l = l->next;
    Q_ASSERT( l && l->next == 0 );
    l->next = td;
}

ModuleData::~ModuleData()
{
    if( toDelete )
        delete toDelete;
}

