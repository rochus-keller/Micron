/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MicMilInterpreter.h"
#include "MicMilOp.h"
#include "MicToken.h"
#include <QElapsedTimer>
#include <QVector>
#include <QFile>
#include <QtDebug>
#include <math.h>
using namespace Mic;

// NOTE: this is now obsolete, see MilInterpreter instead

#define _USE_GETTIMEOFDAY
#ifdef _USE_GETTIMEOFDAY
#if defined(_WIN32) && !defined(__GNUC__)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
// Source: https://stackoverflow.com/questions/10905892/equivalent-of-gettimeday-for-windows/26085827

// MSVC defines this in winsock2.h!?
typedef struct timeval {
    long tv_sec;
    long tv_usec;
} timeval;

int gettimeofday(struct timeval * tp, struct timezone * tzp)
{
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
}
#else
#include <sys/time.h>
#endif
#endif

struct ModuleData;

struct ProcData
{
    MilProcedure* proc;
    ModuleData* module;
    ProcData(MilProcedure* p, ModuleData* m):proc(p),module(m) {}
};

struct FlattenedType
{
    const MilType* type;
    quint32 len : 31; // flattened multi-dim-arrays
    quint32 flattened : 1;
    QList<MilVariable*> fields;
    QList<ProcData> vtable;
    ModuleData* module;
    FlattenedType():type(0),len(0),flattened(0),module(0) {}
    int lastIndexOfField(const QByteArray& name)
    {
        for( int i = fields.size()-1; i >= 0; i-- )
        {
            if( fields[i]->name.constData() == name.constData() )
                return i;
        }
        return -1;
    }
    int lastIndexOfMethod(const QByteArray& name)
    {
        for( int i = vtable.size()-1; i >= 0; i-- )
        {
            if( vtable[i].proc->name.constData() == name.constData() )
                return i;
        }
        return -1;
    }
};

struct MemSlot;

struct MethRef
{
    MemSlot* obj; // points to a MemSlot::Record slot
    ProcData* proc;
    MethRef():obj(0),proc(0){}
};

// NOTE: replaced original design based on MemSlots(QVector) by MemSlot[] and integrated
// all embedded arrays and structs into the same MemSlot[] so that constructs like
// struct Permute { Benchmark base; int count; } (from Permute.c) can be represented.

struct MemSlot
{
    union {
        qint64 i;
        quint64 u;
        double f;
        MemSlot* p;
        ProcData* pp;
        FlattenedType* tt;
        MethRef* m;
    };
    enum Type { Invalid, I, U, F,
                Record, Array,  // value semantics, as a pointer to sequence of MemSlot, owned
                Pointer, // ref semantics, as pointer to MemSlot (sequence, variable, parameter, field, element), not owned
                Procedure, // pointer to procedure/module, not owned
                TypeTag,  // pointer to FlattenedType, tt, not owned
                Method,   // MethRef, m, owned
                Header // the header slot of a record or array; the pointer points to the next slot; u is size
              };
#ifdef _DEBUG
    Type t;
#else
    quint8 t;
#endif
    bool hw; // half width for i, u or f
    bool embedded;

    MemSlot():u(0),t(Invalid),hw(false),embedded(false) {}
    MemSlot(quint64 u, bool h = false):u(u),t(U),hw(h),embedded(false) { if(hw) u &= 0xffffffff; }
    MemSlot(qint64 i, bool h = false):i(i),t(I),hw(h),embedded(false) { if(hw) i = (qint32)i; }
    MemSlot(int i, bool h = true):i(i),t(I),hw(h),embedded(false) {}
    MemSlot(double d,bool h):f(d),t(F),hw(h),embedded(false) {}
    MemSlot(MemSlot* s):p(s),t(Pointer), hw(false),embedded(false) {}
    MemSlot(ProcData* p):pp(p),t(Procedure),hw(false),embedded(false) {}
    MemSlot(FlattenedType* tt):tt(tt),t(TypeTag), hw(false),embedded(false) {}
    MemSlot(const MemSlot& rhs):t(Invalid),u(0),embedded(false) { *this = rhs; }
    MemSlot(MethRef* m):m(m),t(Method), hw(false),embedded(false) {}
    ~MemSlot();
    void clear();
    MemSlot& operator=(const MemSlot& rhs);
    void move( MemSlot& rhs );
    void copyOf(const MemSlot* rhs, bool record);
    void copyOf(const MemSlot* rhs, quint32 off, quint32 len, bool record);
    static void dispose(MemSlot*);
};

typedef QVector<MemSlot> MemSlotList;

#ifdef _MIC_MEM_CHECK
static QHash<MemSlot*,bool> dynamicSeqs;
#endif

static MemSlot* createSequence(int size)
{
    MemSlot* s = new MemSlot[size+1];
    s->t = MemSlot::Header;
    s->u = size;
    s++; // point to the second element which is the actual first element of the sequence
#ifdef _MIC_MEM_CHECK
    dynamicSeqs.insert(s,false);
#endif
    return s;
}

void MemSlot::copyOf(const MemSlot* rhs, bool record)
{
    clear();
    t = record ? Record : Array;
    p = 0;
    if( rhs == 0 )
        return;
    const MemSlot* header = rhs - 1;
    Q_ASSERT(header->t == MemSlot::Header);
    p = createSequence(header->u);
    for(int i = 0; i < header->u; i++ )
        p[i] = rhs[i];
}

void MemSlot::copyOf(const MemSlot* rhs, quint32 off, quint32 len, bool record)
{
    clear();
    t = record ? Record : Array;
    if( rhs == 0 )
        return;
    if( len == 0 )
    {
        const MemSlot* header = rhs - 1;
        Q_ASSERT(off == 0 && header->t == MemSlot::Header);
        len = header->u;
    }
    p = createSequence(len);
    for(int i = 0; i < len; i++ )
        p[i] = rhs[i+off];
}


MemSlot& MemSlot::operator=(const MemSlot& rhs)
{
    if( rhs.t == Array || rhs.t == Record )
        copyOf(rhs.p, rhs.t == Record);
    else
    {
        clear();
        u = rhs.u;
        t = rhs.t;
        hw = rhs.hw;
        if( rhs.t == Method )
        {
            m = new MethRef();
            *m = *rhs.m;
        }
    }
    return *this;
}

void MemSlot::move( MemSlot& rhs )
{
    clear();
    u = rhs.u;
    t = rhs.t;
    hw = rhs.hw;
    embedded = rhs.embedded;
    if( rhs.t == Record || rhs.t == Array || rhs.t == Method )
        rhs.p = 0;
}

void MemSlot::clear()
{
    if( t == Record || t == Array )
        dispose(p);
    else if( t == Method )
        delete m;
    t = Invalid;
    u = 0;
    hw = 0;
}

MemSlot::~MemSlot()
{
    clear();
}

void MemSlot::dispose(MemSlot* s)
{
    if( s == 0 )
        return;
#ifdef _MIC_MEM_CHECK
    if( !dynamicSeqs.contains(s) )
        qCritical() << "not dynamically allocated";
    else if( dynamicSeqs.value(s) )
        qCritical() << "double delete";
    else
        dynamicSeqs[s] = true;
#endif

    MemSlot* header = s - 1;
    Q_ASSERT(header->t == MemSlot::Header);
    delete[] header;
}

struct ModuleData
{
    MilModule* module;
    MemSlotList variables;
    QMap<const char*,ProcData*> procs;

    ModuleData():module(0){}
    ~ModuleData()
    {
        QMap<const char*,ProcData*>::const_iterator i;
        for( i = procs.begin(); i != procs.end(); ++i )
            delete i.value();
    }
};

class MilInterpreter::Imp
{
public:
    Imp():loader(0),out(stdout) {}

    MilLoader* loader;

    QHash<const char*, ModuleData*> modules; // moduleFullName -> data
    QHash<const MilType*,FlattenedType> flattened;
    typedef QList< QList<int> > LoopStack;
    struct MilLabel
    {
        quint32 labelPc;
        QList<quint32> gotoPcs;
        MilLabel():labelPc(0){}
    };
    typedef QHash<const char*,MilLabel> Labels;
    typedef QHash<QByteArray,MemSlot*> Strings;
    Strings strings; // internalized strings
    QByteArray intrinsicMod, outMod, inputMod, mathlMod;
    typedef QHash<const char*, MilProcedure> Intrinsics;
    Intrinsics intrinsics;
    QTextStream out;
#ifdef _USE_GETTIMEOFDAY
    struct timeval start; // 57732 us for Bounce 1500, i.e. 23 times worse than Lua 5.4.7 without jump table
#else
    QElapsedTimer timer; // NOTE: QElapsedTimer and gettimeofday are virtually identical
#endif

    ~Imp()
    {
        Strings::iterator i;
        for( i = strings.begin(); i != strings.end(); ++i )
        {
            MemSlot::dispose(i.value());
            i.value() = 0;
        }
    }

    void dump(const MemSlot& s)
    {
        //out << "[" << (void*) &s << "] ";

        switch( s.t )
        {
        case MemSlot::I:
            out << s.i;
            break;
        case MemSlot::U:
            out << s.u;
            break;
        case MemSlot::F:
            out << s.f;
            break;
        case MemSlot::Pointer:
            if( s.p == 0 )
                out << "nil";
            else
            {
                // out << (void*)s.sp;
                out << "->";
                if( (s.p-1)->t == MemSlot::Header && (s.p->t != MemSlot::Record || s.p->t != MemSlot::Array ) )
                {
                    out << "[";
                    for( int i = 0; i < (s.p - 1)->u; i++ )
                    {
                        if( i != 0 )
                            out << " ";
                        dump(s.p[i]);
                    }
                    out << "]";
                    break;
                }else
                    dump(*s.p);
            }
            break;
        case MemSlot::Record:
        case MemSlot::Array:
            out << "(";
            for( int i = 0; i < (s.p - 1)->u; i++ )
            {
                if( i != 0 )
                    out << " ";
                dump(s.p[i]);
            }
            out << ")";
            break;
        case MemSlot::Procedure:
            if( s.pp == 0 )
                out << "none";
            else
                out << s.pp->module->module->fullName << "!" << s.pp->proc->name;
            break;
        default:
            out << "?";
        }
    }
    void dump( const MemSlotList& l, const QString& title)
    {
        out << "*** " << title << endl;
        for( int i = 0; i < l.size(); i++ )
        {
            out << i << ": ";
            dump(l[i]);
            out << endl;
        }
    }
    void dump(const QList<MemSlot>& stack)
    {
        out << "*** stack:" << endl;
        for( int i = 0; i < stack.size(); i++ )
        {
            out << (i - stack.size() + 1) << ": ";
            dump(stack[i]);
            out << endl;
        }
    }

    MemSlot* internalize(const QByteArray& str)
    {
        MemSlot* s = strings.value(str);
        if( s == 0 )
        {
            s = createSequence(str.size());
            for( int i = 0; i < str.size(); i++ )
            {
                s[i].t = MemSlot::U;
                s[i].u = (quint8)str[i];
            }
            strings[str] = s;
        }
        return s;
    }

    static inline MemSlot::Type fromSymbol(const MilQuali& type)
    {
        MilEmitter::Type tt = MilEmitter::fromSymbol(type.second);
        switch(tt)
        {
        case MilEmitter::I1: case MilEmitter::I2:
        case MilEmitter::I4: case MilEmitter::I8:
            return MemSlot::I;
        case MilEmitter::R4: case MilEmitter::R8:
            return MemSlot::F;
        case MilEmitter::U1: case MilEmitter::U2:
        case MilEmitter::U4: case MilEmitter::U8:
            return MemSlot::U;
        case MilEmitter::IntPtr:
            return MemSlot::Pointer;
        }
        return MemSlot::Invalid;
    }

    void initSlot(ModuleData* module, MemSlot& s, const MilQuali& type )
    {
        FlattenedType* t = getFlattenedType(module, type);
        if( t == 0 )
        {
            // maybe intrinsic type
            s.t = fromSymbol(type);
            return;
        }
        switch( t->type->kind )
        {
        // TODO Alias
        case MilEmitter::Struct:
        case MilEmitter::Object:
            s.clear();
            s.t = MemSlot::Record;
            s.p = createSequence(t->fields.size() + (t->type->kind == MilEmitter::Object ? 1 : 0));
            initFields(module, s.p, t->fields);
            if( t->type->kind == MilEmitter::Object )
                s.p[0] = t;
            break;
        case MilEmitter::Array:
            s.clear();
            s.t = MemSlot::Array;
            s.p = createSequence(t->len);
            initArray(module, s.p, t->type->base);
            break;
        case MilEmitter::Pointer:
            s.t = MemSlot::Pointer;
            break;
        case MilEmitter::ProcType:
            s.t = MemSlot::Procedure;
            break;
        }

    }

    void initArray(ModuleData* module, MemSlot* ss, const MilQuali& etype )
    {
        MemSlot* header = ss-1;
        Q_ASSERT( header->t == MemSlot::Header );
        MilQuali q = etype;
        FlattenedType* t = getFlattenedType(module,q);
        while( t && t->type->kind == MilEmitter::Array )
        {
            // skip embedded arrays since multi-dim were flattened already and
            // we just initialize the flattened one with the ultimate element type
            q = t->type->base;
            t = getFlattenedType(module,q);
        }
        //if( t == 0 || (t->type->kind != MilEmitter::Struct && t->type->kind != MilEmitter::Union) )
        //    return; // scalar or no struct/union type, no initialisation required
        for( int i = 0; i < header->u; i++ )
            initSlot(module,ss[i],q);
    }

    void initVars(ModuleData* module, MemSlot* ss, const QList<MilVariable>& types )
    {
        for(int i = 0; i < types.size(); i++ )
            // slot can either be a flattened array or struct or union, or a scalar
            initSlot(module, ss[i], types[i].type);
    }

    void initFields( ModuleData* module, MemSlot* ss, const QList<MilVariable*>& types )
    {
        for(int i = 0; i < types.size(); i++ )
        {
            FlattenedType* t = getFlattenedType(module,types[i]->type);
            //if( t == 0 || t->type->kind != MilEmitter::Array )
                // skip embedded structs/unions since they were flattened already
            //    continue;
            initSlot(module, ss[types[i]->offset], types[i]->type);
        }
    }

    ModuleData* loadModule(const QByteArray& fullName)
    {
        ModuleData* md = modules.value(fullName.constData());
        if( md )
            return md;
        if( fullName.constData() == intrinsicMod.constData() )
        {
            md = new ModuleData();
            modules.insert(fullName.constData(), md);
            return md;
        }
        MilModule* module = loader->getModule(fullName);
        if( module == 0 )
            return 0;
        md = new ModuleData();
        md->module = module;
        md->variables.resize(module->vars.size());
        initVars(md, md->variables.data(),module->vars);
        modules.insert(fullName.constData(), md);

        for( int i = 0; i < module->imports.size(); i++ )
        {
            ModuleData* imp = loadModule(module->imports[i]);
            if( imp == 0 )
                return 0;
        }

        MilProcedure* init = 0;
        for( int i = 0; i < module->procs.size(); i++ )
        {
            if( module->procs[i].kind == MilProcedure::ModuleInit )
            {
                init = &module->procs[i];
                break;
            }
        }
        if( init )
        {
            MemSlotList args;
            MemSlot ret;
            execute(md, init, args, ret);
        }
        return md;
    }

    void assureValid(ModuleData* module, const MilProcedure* proc, int start, int pc, int op1, int op2 = 0, int op3 = 0)
    {
        if( pc >= proc->body.size() )
        {
            QString what = s_opName[op1];
            if( op2 )
                what += QString(" | %1").arg(s_opName[op2]);
            if( op3 )
                what += QString(" | %1").arg(s_opName[op3]);

            throw QString("%1!%2 statement %3 at pc %4 premature end when looking for %5")
                .arg(module->module->fullName.constData())
                .arg(proc->name.constData()).arg(s_opName[proc->body[start].op]).arg(start).arg(what);
        }

        if( proc->body[pc].op == op1 ||
                (op2 && proc->body[pc].op == op2) ||
                (op3 && proc->body[pc].op == op3))
            return;

        QString what = s_opName[op1];
        if( op2 )
            what += QString(" | %1").arg(s_opName[op2]);
        if( op3 )
            what += QString(" | %1").arg(s_opName[op3]);

        throw QString("%1!%2 statement %3 at pc %4 missing %5 at pc %6")
                .arg(module->module->fullName.constData())
                .arg(proc->name.constData()).arg(s_opName[proc->body[start].op]).arg(start)
                .arg(what).arg(pc);
    }

    void execError(ModuleData* module, const MilProcedure* proc, int pc, const QString& msg = QString())
    {
        throw QString("%1!%2 error at statement '%3' at pc %4 %5")
            .arg(module->module->fullName.constData())
            .arg(proc->name.constData()).arg(s_opName[proc->body[pc].op]).arg(pc).arg(msg);
    }

    void execError(ModuleData* module, const MilProcedure* proc, const QString& msg = QString())
    {
        Q_ASSERT(module);
        Q_ASSERT(proc);
        QByteArray modName;
        if( module->module )
            modName = module->module->fullName;
        else
            modName = intrinsicMod;
        throw QString("%1!%2 error: %3")
            .arg(modName.constData())
            .arg(proc->name.constData()).arg(msg);
    }

    QPair<MilType*,ModuleData*> getType(ModuleData* module, const MilQuali& q)
    {
        // this is only for custom types defined with Emitter::addType or begin/endType, not for intrinsic types
        ModuleData* m = q.first.isEmpty() ? module : loadModule(q.first);
        if( m == 0 )
            return QPair<MilType*,ModuleData*>();
        QPair<MilModule::What,quint32> what = m->module->symbols.value(q.second.constData());
        if( what.first == MilModule::Type )
            return qMakePair(&m->module->types[what.second], m);
        // else
        return QPair<MilType*,ModuleData*>();
    }

    bool isA( ModuleData* module, FlattenedType* sub, FlattenedType* super )
    {
        if( sub == 0 || super == 0 )
            return false;
        while(sub)
        {
            if( sub == super )
                return true;
            sub = getFlattenedType(module, sub->type->base );
        }
        return false;
    }

    FlattenedType* getFlattenedType(ModuleData* module, const MilQuali& q)
    {
        // we mostly need flattened types because of embedded structs (which are resolved to slots)
        // and objects (where inherited fields and methods are linearized).
        QPair<MilType*,ModuleData*> mt = getType(module,q);
        if( mt.first == 0 )
            return 0;
        MilType* ty = mt.first;
        FlattenedType& out = flattened[ty];
        if( out.type == 0 )
        {
            out.module = mt.second;
            if( ty->kind == MilEmitter::Struct || ty->kind == MilEmitter::Object )
            {
                if( ty->kind == MilEmitter::Object )
                {
                    FlattenedType* baseType = getFlattenedType(module, ty->base);
                    if( baseType )
                    {
                        out.fields << baseType->fields;
                        out.vtable = baseType->vtable;
                    }
                }
                for( int i = 0; i < ty->fields.size(); i++ )
                {
                    FlattenedType* fieldType = getFlattenedType(module, ty->fields[i].type);
                    // fieldType is null e.g. in case of int32
                    ty->fields[i].offset = out.fields.size();
                    if( ty->kind == MilEmitter::Object )
                        ty->fields[i].offset++; // make room for the vtable pointer in index 0
                    if( fieldType && !fieldType->fields.isEmpty() )
                    {
                        // field is itself a struct or union, use the flattened fields
                        out.fields << fieldType->fields;
                        out.flattened = true;
                    }else
                        out.fields << &ty->fields[i]; // scalars, arrays, and fieldless structs or unions
                }
                for( int i = 0; i < ty->methods.size(); i++ )
                {
                    const char* name = ty->methods[i].name.constData();
                    ty->methods[i].offset = out.vtable.size(); // preset for new method
                    bool found = false;
                    for(int j = 0; j < out.vtable.size(); j++ )
                    {
                        // look up the method in the inherited vtable and replace it there if it's an override
                        if( out.vtable[j].proc->name.constData() == name )
                        {
                            ty->methods[i].offset = out.vtable[j].proc->offset;
                            out.vtable[j].proc = &ty->methods[i];
                            out.vtable[j].module = mt.second;
                            found = true;
                            break;
                        }
                    }
                    if( !found )
                        // otherwise it's a new method, append it to the vtable
                        out.vtable << ProcData(&ty->methods[i], mt.second);
                }
            }else if( ty->kind == MilEmitter::Union )
            {
                for( int i = 0; i < ty->fields.size(); i++ )
                {
                    FlattenedType* fieldType = getFlattenedType(module, ty->fields[i].type);
                    ty->fields[i].offset = 0;
                    if( fieldType && !fieldType->fields.isEmpty() )
                    {
                        // field is itself a struct or union, use the flattened fields
                        if( out.fields.size() < fieldType->fields.size() )
                            out.fields = fieldType->fields; // use the largest field set
                        out.flattened = true;
                    }else if( out.fields.isEmpty() )
                        out.fields << &ty->fields[i]; // scalars, arrays, and fieldless structs or unions
                }
            }else if( ty->kind == MilEmitter::Array )
            {
                FlattenedType* elemType = getFlattenedType(module, ty->base);
                out.len = ty->len ? ty->len : 1;
                if( elemType && elemType->type->kind == MilEmitter::Array )
                {
                    out.len *= elemType->len;
                    out.flattened = true;
                }
            }
            out.type = ty;
        }
        return &out;
    }

    ProcData* getProc(ModuleData* module, const MilQuali& q)
    {
        ModuleData* m = q.first.isEmpty() ? module : loadModule(q.first);
        if( m == 0 )
            return 0; // error?
        ProcData* res = m->procs.value(q.second.constData());
        if( res )
            return res;
        MilProcedure* proc = 0;
        if( m->module == 0 )
        {
            // intrinsic module
            Intrinsics::iterator i = intrinsics.find(q.second.constData());
            if( i == intrinsics.end() )
                return 0;
            proc = &i.value();
        }else
        {
            QPair<MilModule::What,quint32> what = m->module->symbols.value(q.second.constData());
            if( what.first != MilModule::Proc )
                return 0; // error?
            proc = &m->module->procs[what.second];
        }
        res = new ProcData(proc, m);
        m->procs.insert(q.second.constData(), res);
        return res;
    }

    void prepareBytecode(ModuleData* module, MilProcedure* proc, qint32& pc, Labels& labels, LoopStack& loopStack)
    {
        QList<MilOperation>& ops = proc->body;
        const int start = pc;
        switch(ops[pc].op)
        {
        case IL_while:
            {
                // end -> while+1, then -> end+1
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_do )
                    prepareBytecode(module, proc,pc, labels, loopStack);
                assureValid(module,proc,start,pc,IL_do);
                const int then = pc;
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_end )
                    prepareBytecode(module, proc, pc, labels, loopStack); // look for nested statements
                assureValid(module,proc,start,pc,IL_end);
                ops[pc].index = start+1; // end jumps to while+1
                pc++;
                ops[then].index = pc; // then jumps to end+1
            }
            break;
        case IL_switch:
            {
                // switch -> case -> case -> else -> end
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_case &&
                       ops[pc].op != IL_else && ops[pc].op != IL_end )
                    prepareBytecode(module, proc,pc, labels, loopStack);
                assureValid(module,proc,start,pc,IL_case, IL_else, IL_end);
                int prev = start;
                QList<quint32> thenList;
                while( ops[pc].op == IL_case )
                {
                    ops[prev].index = pc;
                    prev = pc;
                    pc++;
                    assureValid(module,proc,start,pc,IL_then);
                    thenList.append(pc);
                    pc++;
                    while( pc < ops.size() && ops[pc].op != IL_case &&
                           ops[pc].op != IL_else && ops[pc].op != IL_end )
                        prepareBytecode(module, proc, pc, labels, loopStack); // look for nested statements
                    assureValid(module,proc,start,pc,IL_case, IL_else, IL_end);
                }
                if( ops[pc].op == IL_else )
                {
                    ops[prev].index = pc;
                    prev = pc;
                    thenList.append(pc);
                    pc++;
                    while( pc < ops.size() && ops[pc].op != IL_end )
                        prepareBytecode(module, proc, pc, labels, loopStack);
                }
                assureValid(module,proc,start,pc,IL_end);
                foreach( quint32 off, thenList )
                    ops[off].index = pc; // all then and else point to end
                pc++;
            }
            break;
        case IL_if:
            {
                // if -> then -> else -> end
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_then )
                    prepareBytecode(module, proc,pc, labels, loopStack);
                assureValid(module,proc,start,pc, IL_then);
                int then = pc;
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_else && ops[pc].op != IL_end)
                    prepareBytecode(module, proc, pc, labels, loopStack); // then statements
                assureValid(module,proc,start,pc, IL_else, IL_end);
                if( ops[pc].op == IL_else )
                {
                    ops[then].index = pc+1; // then -> else+1
                    const int else_ = pc;
                    pc++;
                    while( pc < ops.size() && ops[pc].op != IL_end )
                        prepareBytecode(module, proc, pc, labels, loopStack); // else statements
                    ops[else_].index = pc; // else -> end
                }else
                    ops[then].index = pc; // then -> end
                assureValid(module,proc,start,pc,IL_end);
                pc++;
            }
            break;
        case IL_iif:
            {
                // iif -> then -> else -> end
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_then )
                    prepareBytecode(module, proc,pc, labels, loopStack);
                assureValid(module,proc,start,pc, IL_then);
                int then = pc;
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_else)
                    prepareBytecode(module, proc, pc, labels, loopStack); // then statements
                assureValid(module,proc,start,pc, IL_else);
                ops[then].index = pc+1; // then -> else+1
                const int else_ = pc;
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_end )
                    prepareBytecode(module, proc, pc, labels, loopStack); // else statements
                ops[else_].index = pc; // else -> end
                assureValid(module,proc,start,pc,IL_end);
                pc++;
            }
            break;
        case IL_repeat:
            {
                // end -> repeat+1
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_until )
                    prepareBytecode(module, proc, pc, labels, loopStack);
                assureValid(module,proc,start,pc, IL_until);
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_end )
                    prepareBytecode(module, proc,pc, labels, loopStack);
                assureValid(module,proc,start,pc, IL_end);
                ops[pc].index = start+1;
                pc++;
            }
            break;
        case IL_loop:
            {
                // end -> loop
                loopStack.push_back(QList<int>());
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_end )
                    prepareBytecode(module, proc, pc, labels, loopStack);
                assureValid(module,proc,start,pc, IL_end);
                ops[pc].index = start+1;
                pc++;
                foreach( int exit_, loopStack.back() )
                    ops[exit_].index = pc;
                loopStack.pop_back();
            }
            break;
        case IL_exit:
            if( loopStack.isEmpty() )
                execError(module, proc, pc, "operation not expected here");
            loopStack.back().append(pc);
            pc++;
            break;
        case IL_label:
            labels[ops[pc].arg.toByteArray().constData()].labelPc = pc;
            pc++;
            break;
        case IL_goto:
            labels[ops[pc].arg.toByteArray().constData()].gotoPcs.append(pc);
            pc++;
            break;
        case IL_ldfld:
        case IL_ldflda:
        case IL_stfld:
            {
                MilTrident td = ops[pc].arg.value<MilTrident>();
                FlattenedType* ty = getFlattenedType(module, td.first);
                if( ty == 0 )
                    execError(module, proc, pc, QString("unknown type '%1'").
                              arg(MilEmitter::toString(td.first).constData()));
                const int idx = ty->type->indexOfField(td.second);
                if( idx < 0 )
                    break; // error?
                ops[pc].index = ty->type->fields[idx].offset;;
            }
            pc++;
            break;
        case IL_ldmeth:
            {
                MilTrident td = ops[pc].arg.value<MilTrident>();
                FlattenedType* ty = getFlattenedType(module, td.first);
                if( ty == 0 )
                    execError(module, proc, pc, QString("unknown type '%1'").
                              arg(MilEmitter::toString(td.first).constData()));
                const int idx = ty->lastIndexOfMethod(td.second); // TODO
                if( idx < 0 )
                    break; // error?
                ops[pc].index = ty->vtable[idx].proc->offset;
            }
            pc++;
            break;
        case IL_ldvar:
        case IL_ldvara:
        case IL_stvar:
            {
                MilQuali q = ops[pc].arg.value<MilQuali>();
                MilModule* m = q.first.isEmpty() ? module->module : loader->getModule(q.first);
                if( m == 0 )
                    break; // error?
                const int idx = m->indexOfVar(q.second);
                if( idx < 0 )
                    break; // error?
                // since the module is not a struct we don't flatten structs and the offset is the index in the field list
                ops[pc].index = idx;
            }
            pc++;
            break;
        default:
            pc++;
            break;
        }
    }

    void prepareBytecode(ModuleData* module, MilProcedure* proc, qint32& pc)
    {
        Labels labels; // name -> label pos, goto poss
        LoopStack loopStack;

        while( pc < proc->body.size() )
            prepareBytecode(module, proc, pc, labels, loopStack);

        Labels::const_iterator i;
        for( i = labels.begin(); i != labels.end(); ++i )
        {
            for(int j = 0; j < i.value().gotoPcs.size(); j++ )
                proc->body[i.value().gotoPcs[j]].index = i.value().labelPc;
        }
    }

    void inline convertTo( QList<MemSlot>& stack, MemSlot::Type to, quint8 size )
    {
        MemSlot s = stack.takeLast();
        switch(s.t)
        {
        case MemSlot::F:
            if( to != s.t )
                s.i = s.f;
            break;
        case MemSlot::I:
        case MemSlot::U:
            if( to == MemSlot::F )
                s.f = s.i;
            else if( to == MemSlot::I && size )
            {
                s.hw = size && size != 8;
                switch(size)
                {
                case 1:
                    {
                        qint8 tmp = s.i;
                        s.i = tmp;
                        break;
                    }
                case 2:
                    {
                        qint16 tmp = s.i;
                        s.i = tmp;
                        break;
                    }
                case 4:
                    {
                        qint32 tmp = s.i;
                        s.i = tmp;
                        break;
                    }
                }
            }else if( to == MemSlot::U && size )
            {
                s.hw = size && size != 8;
                switch(size)
                {
                case 1:
                    {
                        quint8 tmp = s.u;
                        s.u = tmp;
                        break;
                    }
                case 2:
                    {
                        quint16 tmp = s.u;
                        s.u = tmp;
                        break;
                    }
                case 4:
                    {
                        quint32 tmp = s.u;
                        s.u = tmp;
                        break;
                    }
                }
            }
            break;
        case MemSlot::Pointer:
            if( to == MemSlot::F )
                s.f = s.i;
            break;
        default:
            s.u = 0;
            if( size && size < 8 )
                s.hw = true;
            break;
        }
        s.t = to;
        stack.push_back(s);
    }

    void convert(MemSlot& out, const QVariant& data )
    {
        out.clear();
        if( data.canConvert<MilRecordLiteral>() )
        {
            // TODO: flatten embedded elements
            MilRecordLiteral m = data.value<MilRecordLiteral>();
            out.p = createSequence(m.size());
            out.t = MemSlot::Record;
            for( int i = 0; i < m.size(); i++ )
                convert(out.p[i], m[i].second);
            return;
        }
        switch( data.type() )
        {
        case QVariant::List:
            {
                // TODO: flatten multi-dim arrays
                QVariantList l = data.toList();
                out.p = createSequence(l.size());
                out.t = MemSlot::Array;
                for( int i = 0; i < l.size(); i++ )
                    convert(out.p[i], l[i]);
            }
            break;
        case QVariant::ByteArray:
            out.p = internalize(data.toByteArray());
            out.t = MemSlot::Pointer;
            break;
        case QVariant::String:
            out.p = internalize(data.toString().toLatin1());
            out.t = MemSlot::Pointer;
            break;
        case QVariant::LongLong:
        case QVariant::Int:
        case QVariant::Bool:
            out.i = data.toLongLong();
            out.t = MemSlot::I; // TODO: hw
            break;
        case QVariant::ULongLong:
        case QVariant::UInt:
            out.u = data.toULongLong();
            out.t = MemSlot::U; // TODO: hw
            break;
        case QVariant::Double:
            out.f = data.toDouble();
            out.t = MemSlot::F;
            break;
        default:
            Q_ASSERT(false);
        }
    }

    inline void makeCall(QList<MemSlot>& stack, MilProcedure* proc, ModuleData* md, MemSlot* self = 0)
    {
        // TODO: support varargs
        MemSlotList args(proc->params.size());
        for( int i = proc->params.size()-1; i >= 0; i-- )
        {
            if(self && i == 0)
            {
                // NOTE: a bound proc comes with receiver as explicit first param
                Q_ASSERT(!proc->binding.isEmpty());
                args[i] = MemSlot(self);
                break;
            }
            if( stack.isEmpty() )
                execError(md,proc,"not enough actual parameters");
            args[i].move(stack.back());
            stack.pop_back();
        }
        MemSlot ret;
        execute(md, proc, args, ret);
        if( !proc->retType.second.isEmpty() )
        {
            stack.push_back(MemSlot());
            stack.back().move(ret);
        }
    }

    static inline QByteArray toStr(const MemSlot& s)
    {
        Q_ASSERT( (s.t == MemSlot::Pointer || s.t == MemSlot::Array) && s.p );
#if 0
        MemSlot* header = s.sp - 1;
        Q_ASSERT( header->t == MemSlot::Header );
        QByteArray str(header->u, ' ');
        for( int i = 0; i < str.size(); i++ )
            str[i] = (char)(quint8)s.sp[i].u;
#else
        QByteArray str;
        MemSlot* p = s.p;
        while( p->u )
        {
            str += (char)(quint8)p->u;
            p++;
        }
#endif
        return str;
    }

    static int MIC_relop1(const char* l, const char* r, int op)
    {
        switch( op )
        {
        case 1: // EQ
            return strcmp(l,r) == 0;
        case 2: // NEQ
            return strcmp(l,r) != 0;
        case 3: // LT
            return strcmp(l,r) < 0;
        case 4: // LEQ
            return strcmp(l,r) <= 0;
        case 5: // GT
            return strcmp(l,r) > 0;
        case 6: // GEQ
            return strcmp(l,r) >= 0;
        }
        return 0;
    }

    void nyiError(ModuleData* module, MilProcedure* proc)
    {
        throw QString("%1!%2 not yet implemented")
                .arg(module->module->fullName.constData())
                .arg(proc->name.constData());
    }

    void callExtern(ModuleData* module, MilProcedure* proc, MemSlotList& args, MemSlot& ret)
    {
        static QByteArray symbols[10];
        if( symbols[0].isEmpty() )
        {
            symbols[0] = Token::getSymbol("Open");
            symbols[1] = Token::getSymbol("Char");
            symbols[2] = Token::getSymbol("String");
            symbols[3] = Token::getSymbol("Int");
            symbols[4] = Token::getSymbol("Real");
            symbols[5] = Token::getSymbol("LongReal");
            symbols[6] = Token::getSymbol("Ln");
            symbols[7] = Token::getSymbol("Time");
            symbols[8] = Token::getSymbol("sqrt");
        }
        const char* mn = module->module->fullName.constData();
        const char* pn = proc->name.constData();
        if( mn == outMod.constData() )
        {
            if( pn == symbols[2].constData() ) // Out.String(const str: POINTER TO ARRAY OF CHAR)
            {
                Q_ASSERT(args.size() == 1);
                out << toStr(args.first()).constData() << flush;
            }else if( pn == symbols[1].constData() ) // Out.Char(c: CHAR)
            {
                Q_ASSERT(args.size() == 1);
                out << (char)(quint8)args.first().i << flush;
            }else if( pn == symbols[3].constData() ) // Out.Int(i: INT64;n: INT32)
            {
                Q_ASSERT(args.size() == 2);
                out << args.first().i << flush;
            }else if( pn == symbols[6].constData() ) // Out.Ln
            {
                out << endl << flush;
            }else
                nyiError(module,proc);
        }else if( mn == inputMod.constData() )
        {
            if( pn == symbols[7].constData() ) // Input.Time
            {
                ret.t = MemSlot::I;
#ifdef _USE_GETTIMEOFDAY
                static struct timeval now;
                gettimeofday(&now, 0);
                const long seconds = now.tv_sec - start.tv_sec;
                const long microseconds = now.tv_usec - start.tv_usec;
                ret.i = seconds*1000000 + microseconds;
#else
                ret.i = timer.nsecsElapsed() / 1000;
#endif
            }else
                nyiError(module,proc);
        }else if( mn == mathlMod.constData() )
        {
            if( pn == symbols[8].constData() ) // MathL.sqrt (x : LONGREAL) : LONGREAL
            {
                Q_ASSERT(args.size() == 1);
                ret.t = MemSlot::F;
                ret.f = sqrt(args.first().f);
            }else
                nyiError(module,proc);
        }else
            nyiError(module,proc);
    }

    void callIntrinsic(MilProcedure* proc, MemSlotList& args, MemSlot& ret)
    {
        switch(proc->offset)
        {
        case 1: // relop1
            Q_ASSERT(args.size()==3);
            ret.t = MemSlot::U;
            ret.u = MIC_relop1(toStr(args[0]).constData(), toStr(args[1]).constData(), args[2].u);
            break;
        case 2: // relop2
            {
                Q_ASSERT(args.size()==3);
                char tmp[2] = ".";
                tmp[0] = (char)(quint8)args[1].u;
                ret.t = MemSlot::U;
                ret.u = MIC_relop1(toStr(args[0]).constData(), tmp, args[2].u);
            }
            break;
        case 3: // relop3
            {
                Q_ASSERT(args.size()==3);
                char tmp[2] = ".";
                tmp[0] = (char)(quint8)args[0].u;
                ret.t = MemSlot::U;
                ret.u = MIC_relop1(tmp, toStr(args[1]).constData(), args[2].u);
            }
            break;
        case 4: // relop4
            {
                Q_ASSERT(args.size()==3);
                char l[2] = ".";
                l[0] = (char)(quint8)args[0].u;
                char r[2] = ".";
                r[0] = (char)(quint8)args[1].u;
                ret.t = MemSlot::U;
                ret.u = MIC_relop1(l, r, args[2].u);
            }
            break;
        case 5: // SetDiv
            Q_ASSERT(args.size()==2);
            ret.t = MemSlot::U;
            ret.u = ~(quint32)( args.first().u & args.last().u ) & ( args.first().u | args.last().u );
            break;
        case 6: // SetIn
            Q_ASSERT(args.size()==2);
            ret.t = MemSlot::U;
            ret.u = ((1 << args.first().u) & args.last().u) != 0;
            break;
        case 7: // printI8
            Q_ASSERT(args.size()==1);
            out << args.first().i << flush;
            break;
        case 8: // printU8
            Q_ASSERT(args.size()==1);
            out << args.first().u << flush;
            break;
        case 9: // printF8
            Q_ASSERT(args.size()==1);
            out << args.first().f << flush;
            break;
        case 10: // printStr
            Q_ASSERT(args.size()==1 );
            out << toStr(args.first()).constData() << flush;
            break;
        case 11: // printCh
            Q_ASSERT(args.size()==1);
            out << (char)(quint8)args.first().u << flush;
            break;
        case 12: // printBool
            Q_ASSERT(args.size()==1);
            out << (args.first().u ? "true" : "false") << flush;
            break;
        case 13: // printSet
            Q_ASSERT(args.size()==1);
            out << QByteArray::number(args.first().u,2).constData() << flush;
            break;
        case 14: // strcopy
            Q_ASSERT(args.size()==2 && args.first().p && args.last().p );
            for( int i = 0; ; i++ )
            {
                args.first().p[i] = args.last().p[i];
                if( args.last().p[i].u == 0 )
                    break;
            }
            break;
        case 15: // assert
            Q_ASSERT(args.size()==3);
            if( args.first().u == 0 )
                throw QString("assertion failed at %1:%2").arg(toStr(args[2]).constData()).arg(args[1].u);
            break;
        default:
            throw QString("intrinsic proc 'MIC$!%1' not yet implemented").arg(proc->name.constData());
        }
    }

    void storeVariable(ModuleData* module, MilProcedure* proc, MemSlot& lhs, MemSlot& rhs )
    {
        if( lhs.t == MemSlot::Record || lhs.t == MemSlot::Array )
        {
            if( rhs.t != lhs.t )
                execError(module,proc,"incompatible value type");
            MemSlot* lh = lhs.p - 1;
            MemSlot* rh = rhs.p - 1;
            Q_ASSERT( lh->t == MemSlot::Header );
            Q_ASSERT( rh->t == MemSlot::Header );
            if( rh->u > lh->u )
                execError(module,proc,"value too large");
            if( rh->u == lh->u )
                lhs.move(rhs);
            else
            {
                for( int i = 0; i < rh->u; i++ )
                    lhs.p[i].move(rhs.p[i]);
            }
        }else
            lhs = rhs;
    }

    static inline MemSlot* findHeader(MemSlot* s)
    {
        while( s && s->t != MemSlot::Header )
            s--;
        return s;
    }

    void boundsCheck(ModuleData* module, MilProcedure* proc, quint32 pc, MemSlot* array, int index)
    {
        MemSlot* lh = findHeader(array);
        Q_ASSERT(lh != 0 && lh->t == MemSlot::Header);
        const quint32 start = array - lh - 1;
        if( index + start >= lh->u )
            execError(module,proc,pc,"index out of upper bound");
    }

    void store(ModuleData* module, MilProcedure* proc, quint32 pc, MemSlot* lhs, bool embedded, MemSlot& rhs)
    {
        if( rhs.t == MemSlot::Record || rhs.t == MemSlot::Array )
        {
            if( embedded )
            {
                MemSlot* lh = findHeader(lhs);
                Q_ASSERT(lh != 0 && lh->t == MemSlot::Header);
                const quint32 border = lhs - lh - 1;
                MemSlot* rh = rhs.p - 1;
                Q_ASSERT(rh->t == MemSlot::Header);
                if( rh->u > (lh->u - border) )
                    execError(module,proc,pc,"value slot width too large");
                for( int i = 0; i < rh->u; i++ )
                    lhs[i].move(rhs.p[i]);
            }else if(lhs->t == MemSlot::Record || lhs->t == MemSlot::Array)
                storeVariable(module, proc, *lhs,rhs);
            else
                execError(module,proc,pc,"cannot copy a structured to a scalar value");
        }else
            lhs->move(rhs);
    }

#define vmdispatch(o)	switch(o)
#define vmcase(l)	case l:
#define vmbreak		break

    void execute(ModuleData* module, MilProcedure* proc, MemSlotList& args, MemSlot& ret)
    {
        if( proc->kind == MilProcedure::Intrinsic )
        {
            callIntrinsic(proc,args,ret);
            return;
        }
        if( proc->kind == MilProcedure::Extern )
        {
            callExtern(module, proc,args,ret);
            return;
        }
        qint32 pc = 0;
        if( !proc->compiled )
        {
            prepareBytecode(module, proc, pc);

#if 0
            QFile out;
            out.open(stdout, QIODevice::WriteOnly);
            Mic::IlAsmRenderer r(&out);
            Mic::MilLoader::render(&r,module->module);
            out.putChar('\n');
#endif

            proc->compiled = true;
        }

//#define _USE_JUMP_TABLE
        // the debugger becomes veeeery slow because of local var display
        // the threaded interpreter when compiled with GCC 4.8 is only about 2% faster than the switch version

#ifdef _USE_JUMP_TABLE
#undef vmdispatch
#undef vmcase
#undef vmbreak

#define vmdispatch(x)     goto *disptab[x];

#define vmcase(l)     L_##l:

#define vmbreak		 if( pc >= proc->body.size() ) return; vmdispatch(proc->body[pc].op);

        static const void *const disptab[IL_NUM_OF_OPS] = {
            &&L_IL_invalid,
            &&L_IL_add, &&L_IL_abs, &&L_IL_and,
            &&L_IL_call, &&L_IL_calli,
            &&L_IL_callvi, &&L_IL_callvirt, &&L_IL_castptr,
            &&L_IL_ceq, &&L_IL_cgt, &&L_IL_cgt_un, &&L_IL_clt, &&L_IL_clt_un,
            &&L_IL_conv_i1, &&L_IL_conv_i2, &&L_IL_conv_i4, &&L_IL_conv_i8, &&L_IL_conv_r4, &&L_IL_conv_r8,
            &&L_IL_conv_u1, &&L_IL_conv_u2, &&L_IL_conv_u4, &&L_IL_conv_u8, &&L_IL_conv_ip,
            &&L_IL_div, &&L_IL_div_un, &&L_IL_dup, &&L_IL_iif, &&L_IL_initobj, &&L_IL_isinst, &&L_IL_ldarg, &&L_IL_ldarg_s,
            &&L_IL_ldarg_0, &&L_IL_ldarg_1, &&L_IL_ldarg_2, &&L_IL_ldarg_3,
            &&L_IL_ldarga, &&L_IL_ldarga_s,
            &&L_IL_ldc_i4, &&L_IL_ldc_i8, &&L_IL_ldc_i4_s, &&L_IL_ldc_r4, &&L_IL_ldc_r8,
            &&L_IL_ldc_i4_0, &&L_IL_ldc_i4_1, &&L_IL_ldc_i4_2, &&L_IL_ldc_i4_3, &&L_IL_ldc_i4_4, &&L_IL_ldc_i4_5,
            &&L_IL_ldc_i4_6, &&L_IL_ldc_i4_7, &&L_IL_ldc_i4_8, &&L_IL_ldc_i4_m1, &&L_IL_ldobj,
            &&L_IL_ldelem, &&L_IL_ldelema, &&L_IL_ldelem_i1, &&L_IL_ldelem_i2,
            &&L_IL_ldelem_i4, &&L_IL_ldelem_i8, &&L_IL_ldelem_u1, &&L_IL_ldelem_u2,
            &&L_IL_ldelem_u4, &&L_IL_ldelem_u8, &&L_IL_ldelem_r4, &&L_IL_ldelem_r8, &&L_IL_ldelem_ip,
            &&L_IL_ldfld, &&L_IL_ldflda,
            &&L_IL_ldind_i1, &&L_IL_ldind_i2, &&L_IL_ldind_i4, &&L_IL_ldind_i8, &&L_IL_ldind_u1, &&L_IL_ldind_u2,
            &&L_IL_ldind_u4, &&L_IL_ldind_r4, &&L_IL_ldind_u8, &&L_IL_ldind_r8, &&L_IL_ldind_ip, &&L_IL_ldind_ipp,
            &&L_IL_ldloc, &&L_IL_ldloc_s, &&L_IL_ldloca, &&L_IL_ldloca_s,
            &&L_IL_ldloc_0, &&L_IL_ldloc_1, &&L_IL_ldloc_2, &&L_IL_ldloc_3, &&L_IL_ldnull,
            &&L_IL_ldind, &&L_IL_ldproc, &&L_IL_ldmeth, &&L_IL_ldstr,
            &&L_IL_ldvar, &&L_IL_ldvara, &&L_IL_mul, &&L_IL_neg,
            &&L_IL_newarr, &&L_IL_newvla, &&L_IL_newobj,
            &&L_IL_not, &&L_IL_or, &&L_IL_rem, &&L_IL_rem_un, &&L_IL_shl, &&L_IL_shr, &&L_IL_shr_un,
            &&L_IL_sizeof, &&L_IL_sub, &&L_IL_xor, &&L_IL_ptroff, &&L_IL_nop,
            &&L_IL_free, &&L_IL_repeat, &&L_IL_until,
            &&L_IL_exit, &&L_IL_goto, &&L_IL_if, &&L_IL_then, &&L_IL_else, &&L_IL_end,
            &&L_IL_label, &&L_IL_line, &&L_IL_loop, &&L_IL_pop, &&L_IL_ret,
            &&L_IL_starg, &&L_IL_starg_s,
            &&L_IL_stelem, &&L_IL_stelem_i1, &&L_IL_stelem_i2, &&L_IL_stelem_i4, &&L_IL_stelem_i8,
            &&L_IL_stelem_r4, &&L_IL_stelem_r8, &&L_IL_stelem_ip, &&L_IL_stfld,
            &&L_IL_stind_i1, &&L_IL_stind_i2, &&L_IL_stind_i4, &&L_IL_stind_i8, &&L_IL_stind_r4, &&L_IL_stind_r8, &&L_IL_stind_ip, &&L_IL_stind_ipp,
            &&L_IL_stloc, &&L_IL_stloc_s, &&L_IL_stloc_0, &&L_IL_stloc_1, &&L_IL_stloc_2, &&L_IL_stloc_3,
            &&L_IL_stind, &&L_IL_stvar, &&L_IL_switch, &&L_IL_case, &&L_IL_while, &&L_IL_do
        };

#endif
        MemSlotList locals(proc->locals.size());
        initVars(module, locals.data(), proc->locals);
        ret = MemSlot();
        pc = 0;
        QList<MemSlot> stack;
        MemSlot lhs, rhs;
        MilObject mo;
        QList<MemSlot> switchExpr;
        QList<quint8> curStatement;

        //out << "***** " << module->module->fullName << "!" << proc->name << ":" << endl;
        //dump(args,"args");

        while(true)
        {
            if( pc >= proc->body.size() )
            {
#if 0
                out << "***** " << module->module->fullName << "!" << proc->name << ":" << endl;
                //dump(module->variables,"module");
                dump(args,"args");
                //dump(locals,"locals");
#endif
                return;
            }

            // NOTE: jump tables (i.e. &&label, computed gotos) makes little sense, since GCC is able
            // to derive it automatically in principle; e.g. Lua 5.4.7 has exactly same
            // performance with or without jump tables; but the VM has a different architecture than
            // the present one.
            vmdispatch(proc->body[pc].op)
            {
            vmcase(IL_invalid)
                    pc++;
                    vmbreak;
            vmcase(IL_add)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i + rhs.i, lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u + rhs.u, lhs.hw));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(lhs.f + rhs.f, lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_abs)
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(qAbs(lhs.i),lhs.hw));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(qAbs(lhs.f),lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_and) // TODO short-circuit evaluation
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i & rhs.i, lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u & rhs.u, lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_call)
                {
                    ProcData* pd = getProc(module, proc->body[pc].arg.value<MilQuali>());
                    if( pd == 0 )
                        execError(module, proc, pc, QString("cannot resolve procedure %1").
                                  arg(MilEmitter::toString(proc->body[pc].arg.value<MilQuali>()).constData()));
                    makeCall(stack, pd->proc, pd->module);
                }
                pc++;
                vmbreak;
            vmcase(IL_calli)
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::Procedure || lhs.pp == 0 )
                    execError(module, proc, pc, "top of stack is not a procedure");
                makeCall(stack, lhs.pp->proc, lhs.pp->module);
                pc++;
                vmbreak;
            vmcase(IL_callvi)
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::Method || lhs.m == 0 || lhs.m->proc == 0 ||
                        lhs.m->obj == 0 || lhs.m->obj->t != MemSlot::Record )
                    execError(module, proc, pc, "top of stack is not a valid methref");
                makeCall(stack, lhs.m->proc->proc, lhs.m->proc->module, lhs.m->obj);
                pc++;
                vmbreak;
            vmcase(IL_callvirt)
                {
                    MilTrident tri = proc->body[pc].arg.value<MilTrident>();
                    FlattenedType* rec = getFlattenedType(module, tri.first);
                    if( rec == 0 )
                        execError(module, proc, pc, "invalid object type");
                    const int midx = rec->lastIndexOfMethod(tri.second);
                    if( midx < 0 )
                        execError(module, proc, pc, "unknown method");
                    else
                    {
                        MilProcedure* proc = rec->vtable[midx].proc;
                        makeCall(stack, proc, rec->module);
                    }
                }
                pc++;
                vmbreak;
            vmcase(IL_castptr)
                // NOP
                if( stack.back().t != MemSlot::Pointer || stack.back().p == 0 )
                    execError(module, proc, pc, "top of stack is not a pointer");
                pc++;
                vmbreak;
            vmcase(IL_ceq)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                stack.push_back(MemSlot(qint64(lhs.u == rhs.u),true));
                pc++;
                vmbreak;
            vmcase(IL_cgt)
            vmcase(IL_cgt_un)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(qint64(lhs.i > rhs.i),true));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(qint64(lhs.f > rhs.f),true));
                    break;
                default:
                    stack.push_back(MemSlot(qint64(lhs.u > rhs.u),true));
                    break;
                }
                pc++;
                vmbreak;
            vmcase(IL_clt)
            vmcase(IL_clt_un)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(qint64(lhs.i < rhs.i),true));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(qint64(lhs.f < rhs.f),true));
                    break;
                default:
                    stack.push_back(MemSlot(qint64(lhs.u < rhs.u),true));
                    break;
                }
                pc++;
                vmbreak;
            vmcase(IL_conv_i1)
                convertTo(stack, MemSlot::I, 1);
                pc++;
                vmbreak;
            vmcase(IL_conv_i2)
                convertTo(stack, MemSlot::I, 2);
                pc++;
                vmbreak;
            vmcase(IL_conv_i4)
                convertTo(stack, MemSlot::I, 4);
                pc++;
                vmbreak;
            vmcase(IL_conv_i8)
                convertTo(stack, MemSlot::I, 0);
                pc++;
                vmbreak;
            vmcase(IL_conv_r4)
            vmcase(IL_conv_r8)
                convertTo(stack, MemSlot::F, 0);
                pc++;
                vmbreak;
            vmcase(IL_conv_u1)
                convertTo(stack, MemSlot::U, 1);
                pc++;
                vmbreak;
            vmcase(IL_conv_u2)
                convertTo(stack, MemSlot::U, 2);
                pc++;
                vmbreak;
            vmcase(IL_conv_u4)
                convertTo(stack, MemSlot::U, 4);
                pc++;
                vmbreak;
            vmcase(IL_conv_u8)
                convertTo(stack, MemSlot::U, 0);
                pc++;
                vmbreak;
            vmcase(IL_conv_ip)
                convertTo(stack, MemSlot::Pointer, 0);
                pc++;
                vmbreak;
            vmcase(IL_div)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i / rhs.i,lhs.hw));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(lhs.f / rhs.f,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_div_un)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u / rhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_dup)
                stack.push_back(stack.back());
                pc++;
                vmbreak;
            vmcase(IL_initobj)
                // NOP
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::Pointer || lhs.p == 0 )
                    execError(module, proc, pc, "top of stack is not a pointer");
                pc++;
                vmbreak;
            vmcase(IL_isinst) {
                    lhs.move(stack.back());
                    stack.pop_back();
                    if( (lhs.t != MemSlot::Pointer) )
                        execError(module, proc, pc, "invalid pointer");
                    if( lhs.p == 0 )
                        stack.push_back(MemSlot(0)); // IS of null is false
                    else
                    {
                        MilQuali q = proc->body[pc].arg.value<MilQuali>();
                        FlattenedType* reftype = getFlattenedType(module, q);
                        if( reftype == 0 )
                            execError(module, proc, pc, "invalid reference object type");
                        const bool res = isA(module, lhs.p[0].tt, reftype);
                        stack.push_back(MemSlot(res));
                    }
                }
                pc++;
                vmbreak;
            vmcase(IL_ldarg)
            vmcase(IL_ldarg_s)
                stack.push_back(args.at(proc->body[pc].arg.toUInt()));
                pc++;
                vmbreak;
            vmcase(IL_ldarg_0)
                stack.push_back(args.at(0));
                pc++;
                vmbreak;
            vmcase(IL_ldarg_1)
                stack.push_back(args.at(1));
                pc++;
                vmbreak;
            vmcase(IL_ldarg_2)
                stack.push_back(args.at(2));
                pc++;
                vmbreak;
            vmcase(IL_ldarg_3)
                stack.push_back(args.at(3));
                pc++;
                vmbreak;
            vmcase(IL_ldarga)
            vmcase(IL_ldarga_s)
                stack.push_back(&args[proc->body[pc].arg.toUInt()]);
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4)
            vmcase(IL_ldc_i4_s)
                stack.push_back(MemSlot(proc->body[pc].arg.toLongLong(),true));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i8)
                stack.push_back(MemSlot(proc->body[pc].arg.toLongLong()));
                pc++;
                vmbreak;
            vmcase(IL_ldc_r4)
                stack.push_back(MemSlot(proc->body[pc].arg.toDouble(),true));
                pc++;
                vmbreak;
            vmcase(IL_ldc_r8)
                stack.push_back(MemSlot(proc->body[pc].arg.toDouble(),false));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_0)
                stack.push_back(MemSlot(0));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_1)
                stack.push_back(MemSlot(1));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_2)
                stack.push_back(MemSlot(2));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_3)
                stack.push_back(MemSlot(3));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_4)
                stack.push_back(MemSlot(4));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_5)
                stack.push_back(MemSlot(5));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_6)
                stack.push_back(MemSlot(6));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_7)
                stack.push_back(MemSlot(7));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_8)
                stack.push_back(MemSlot(8));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4_m1)
                stack.push_back(MemSlot(-1));
                pc++;
                vmbreak;
            vmcase(IL_ldobj)
                mo = proc->body[pc].arg.value<MilObject>();
                convert(lhs, mo.data);
                stack.push_back(MemSlot());
                stack.back().move(lhs);
                // now a direct seqval is on the stack
                pc++;
                vmbreak;
            vmcase(IL_ldelem)
            vmcase(IL_ldelem_i1)
            vmcase(IL_ldelem_i2)
            vmcase(IL_ldelem_i4)
            vmcase(IL_ldelem_i8)
            vmcase(IL_ldelem_u1)
            vmcase(IL_ldelem_u2)
            vmcase(IL_ldelem_u4)
            vmcase(IL_ldelem_u8)
            vmcase(IL_ldelem_r4)
            vmcase(IL_ldelem_r8)
            vmcase(IL_ldelem_ip)
                rhs = stack.takeLast();
                lhs.move(stack.back());
                stack.pop_back();
                if( (lhs.t != MemSlot::Pointer) || lhs.p == 0 )
                    execError(module, proc, pc, "invalid array");
                if( !lhs.embedded && lhs.p->t == MemSlot::Array )
                    lhs.p = lhs.p->p;
                // TODO: bounds check
                if( rhs.i < 0 )
                    execError(module,proc,pc,"index out of lower bound");
                if( proc->body[pc].op == IL_ldelem )
                {
                    FlattenedType* ety = getFlattenedType(module, proc->body[pc].arg.value<MilQuali>());
                    if( ety && ety->len )
                        rhs.u *= ety->len; // multi-dim elem types are here by value
                    boundsCheck(module,proc,pc,lhs.p,rhs.u);
                    if( ety && ety->len > 1 )
                    {
                        // in case of an array by value element make a value copy
                        // TODO: why don't we use ldobj here?
                        stack.push_back(MemSlot());
                        boundsCheck(module,proc,pc,lhs.p,rhs.u + ety->len - 1);
                        stack.back().copyOf(lhs.p, rhs.u, ety->len, false);
                    }else
                        stack.push_back(lhs.p[rhs.u]);
                }else
                {
                    boundsCheck(module,proc,pc,lhs.p,rhs.u);
                    stack.push_back(lhs.p[rhs.u]);
                }
                pc++;
                vmbreak;
            vmcase(IL_ldelema) {
                rhs.move(stack.back());
                stack.pop_back();
                lhs.move(stack.back());
                stack.pop_back();
                if( lhs.t != MemSlot::Pointer || lhs.p == 0 )
                    execError(module, proc, pc, "invalid array");
                if( !lhs.embedded && lhs.p->t == MemSlot::Array )
                    lhs.p = lhs.p->p;
                MilQuali ety = proc->body[pc].arg.value<MilQuali>();
                FlattenedType* ty = getFlattenedType(module, ety);
                if( ty && ty->len )
                    rhs.u *= ty->len; // multi-dim elem types are all in the same flattened array
                if( rhs.u < 0 )
                    execError(module, proc, pc, "index out of lower bound");
                boundsCheck(module,proc,pc, lhs.p,rhs.u);
                stack.push_back(&lhs.p[rhs.u]);
                /* if the array is multi-dim and the terminal array element is a struct, the pointer to a
                 * single element in the flattened array is ambiguous, i.e. we don't know if the actual
                 * target of the pointer is represented by the SlotPtr on the stack, or the SeqVal pointed
                 * to by the SlotPtr. We therefore mark the SlotPtr on the stack, if it directly represents
                 * the target value */
                stack.back().embedded = ty && ty->type->kind == MilEmitter::Array;
                pc++;
                vmbreak;
            }
            vmcase(IL_ldfld) {
                lhs.move(stack.back());
                stack.pop_back();
                if( (lhs.t != MemSlot::Pointer) || lhs.p == 0 )
                    execError(module, proc, pc, "invalid record or field");
                if( !lhs.embedded && lhs.p->t == MemSlot::Record )
                    lhs.p = lhs.p->p;
                MilTrident tri = proc->body[pc].arg.value<MilTrident>();
                FlattenedType* rec = getFlattenedType(module, tri.first);
                if( rec == 0 )
                    execError(module, proc, pc, "invalid record or union type");
                const MilVariable* field = rec->type->findField(tri.second);
                if( field == 0 )
                    execError(module, proc, pc, "unknown field");
                FlattenedType* ft = getFlattenedType(module, field->type);
                boundsCheck(module,proc,pc,lhs.p,proc->body[pc].index);
                if( ft && !ft->fields.isEmpty() )
                {
                    // embedded struct by value
                    stack.push_back(MemSlot());
                    boundsCheck(module,proc,pc,lhs.p,proc->body[pc].index+ft->fields.size()-1);
                    stack.back().copyOf(lhs.p, proc->body[pc].index, ft->fields.size(), true);
                }else
                    stack.push_back(lhs.p[proc->body[pc].index]);
                pc++;
                vmbreak;
            }
            vmcase(IL_ldflda) {
                lhs.move(stack.back());
                stack.pop_back();
                if( (lhs.t != MemSlot::Pointer) || lhs.p == 0 )
                    execError(module, proc, pc, "invalid record or field");
                if( !lhs.embedded && lhs.p->t == MemSlot::Record )
                    lhs.p = lhs.p->p;
                MilTrident tri = proc->body[pc].arg.value<MilTrident>();
                FlattenedType* rec = getFlattenedType(module, tri.first);
                if( rec == 0 )
                    execError(module, proc, pc, "invalid record or union type");
                const MilVariable* field = rec->type->findField(tri.second);
                if( field == 0 )
                    execError(module, proc, pc, "unknown field");
                FlattenedType* ft = getFlattenedType(module, field->type);
                boundsCheck(module,proc,pc,lhs.p,proc->body[pc].index);
                stack.push_back(&lhs.p[proc->body[pc].index]);
                /* if the struct/union has embedded structs/unions and the (flattened) field pointed to is an
                 * embedded array, the pointer is ambigous, i.e. we don't know if the actual target of the
                 * pointer is represented by the SlotPtr on the stack, or the SeqVal pointed to by the SlotPtr.
                 * We therefore mark the SlotPtr on the stack, if it directly represents the target value */
                stack.back().embedded = ft && !ft->fields.isEmpty();
                pc++;
                vmbreak;
            }
            vmcase(IL_ldind_i1)
            vmcase(IL_ldind_i2)
            vmcase(IL_ldind_i4)
            vmcase(IL_ldind_i8)
            vmcase(IL_ldind_u1)
            vmcase(IL_ldind_u2)
            vmcase(IL_ldind_u4)
            vmcase(IL_ldind_u8)
            vmcase(IL_ldind_r4)
            vmcase(IL_ldind_r8)
            vmcase(IL_ldind_ip)
            vmcase(IL_ldind_ipp)
                lhs = stack.takeLast();
                if( lhs.p == 0 || lhs.t != MemSlot::Pointer)
                    execError(module, proc, pc, "invalid pointer on stack");
                if( lhs.embedded || lhs.p->t == MemSlot::Record || lhs.p->t == MemSlot::Array )
                    execError(module, proc, pc, "incompatible type on stack");
                stack.push_back(*lhs.p);
                pc++;
                vmbreak;
            vmcase(IL_ldloc)
            vmcase(IL_ldloc_s)
                stack.push_back(locals.at(proc->body[pc].arg.toUInt()));
                pc++;
                vmbreak;
            vmcase(IL_ldloca)
            vmcase(IL_ldloca_s)
                stack.push_back(&locals[proc->body[pc].arg.toUInt()]);
                pc++;
                vmbreak;
            vmcase(IL_ldloc_0)
                stack.push_back(locals.at(0));
                pc++;
                vmbreak;
            vmcase(IL_ldloc_1)
                stack.push_back(locals.at(1));
                pc++;
                vmbreak;
            vmcase(IL_ldloc_2)
                stack.push_back(locals.at(2));
                pc++;
                vmbreak;
            vmcase(IL_ldloc_3)
                stack.push_back(locals.at(3));
                pc++;
                vmbreak;
            vmcase(IL_ldmeth) {
                    lhs.move(stack.back());
                    stack.pop_back();
                    if( (lhs.t != MemSlot::Pointer) || lhs.p == 0 )
                        execError(module, proc, pc, "invalid pointer to object");
                    if( (lhs.p->t != MemSlot::Record) || lhs.p->p == 0 || lhs.p->p->t != MemSlot::TypeTag )
                        execError(module, proc, pc, "invalid record");
                    if( proc->body[pc].index >= lhs.p->p->tt->vtable.size() )
                        execError(module, proc, pc, "invalid vtable index");
                    MethRef* m = new MethRef();
                    m->obj = lhs.p;
                    m->proc = &lhs.p->p->tt->vtable[proc->body[pc].index];
                    stack.push_back(MemSlot(m));
                }
                pc++;
                vmbreak;
            vmcase(IL_ldnull)
                stack.push_back(MemSlot((MemSlot*)0));
                pc++;
                vmbreak;
            vmcase(IL_ldind)
                lhs.move(stack.back());
                stack.pop_back();
                if( lhs.t != MemSlot::Pointer || lhs.p == 0 )
                    execError(module, proc, pc, "invalid pointer on stack");
                /* the structured value is either in a SeqVal pointed to by the SlotPtr on the stack,
                 * or directly represented by the SlotPtr on the stack; the latter case is marked by
                 * "embedded" */
                if( lhs.embedded )
                {
                    MilQuali q = proc->body[pc].arg.value<MilQuali>();
                    if( q.second.isEmpty() )
                    {
                        // we expect an intrinsic string on the stack
                        stack.push_back(MemSlot());
                        stack.back().copyOf(lhs.p, 0, 0, false );
                    }else
                    {
                        FlattenedType* ty = getFlattenedType(module, q);
                        if( ty == 0 )
                            execError(module, proc, pc, "invalid type");
                        stack.push_back(MemSlot());
                        if( ty->type->kind == MilEmitter::Struct || ty->type->kind == MilEmitter::Union
                                || ty->type->kind == MilEmitter::Object)
                            stack.back().copyOf(lhs.p, 0, ty->fields.size(), true );
                        else
                        {
                            Q_ASSERT( ty->type->kind == MilEmitter::Array && ty->type->len != 0 );
                            stack.back().copyOf(lhs.p, 0, ty->len, false );
                        }
                    }
                }else
                {
                    if( (lhs.p->t != MemSlot::Record && lhs.p->t != MemSlot::Array) || lhs.p->p == 0 )
                        execError(module, proc, pc, "the pointer doesn't point to a structured value");
                    stack.push_back(MemSlot());
                    stack.back().copyOf(lhs.p->p, lhs.p->t == MemSlot::Record);
                }
                pc++;
                vmbreak;
            vmcase(IL_ldproc)
                stack.push_back(MemSlot(getProc(module,proc->body[pc].arg.value<MilQuali>())));
                pc++;
                vmbreak;
            vmcase(IL_ldstr) {
                MemSlot* str = internalize(proc->body[pc].arg.toByteArray());
                stack.push_back(str);
                stack.back().embedded = true; // the SeqPtr on the stack points directly to the string value
                pc++;
                vmbreak;
            }
            vmcase(IL_ldvar)
                if( module->variables.size() <= proc->body[pc].index )
                    execError(module, proc, pc, "invalid variable reference");
                stack.push_back(module->variables.at(proc->body[pc].index));
                pc++;
                vmbreak;
            vmcase(IL_ldvara)
                if( module->variables.size() <= proc->body[pc].index )
                    execError(module, proc, pc, "invalid variable reference");
                stack.push_back(&module->variables[proc->body[pc].index]);
                pc++;
                vmbreak;
            vmcase(IL_mul)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i * rhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u * rhs.u,lhs.hw));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(lhs.f * rhs.f,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_neg)
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(-lhs.i,lhs.hw));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(-lhs.f,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_newarr)
            vmcase(IL_newvla)
                {
                    lhs = stack.takeLast();
                    if( lhs.u == 0 )
                        execError(module, proc, pc, "invalid array size");
                    MilQuali ety = proc->body[pc].arg.value<MilQuali>();
                    FlattenedType* ty = getFlattenedType(module, ety);
                    // multi-dim arrays are flattened
                    MemSlot* array = createSequence(lhs.u * ( ty && ty->len ? ty->len : 1 ) );
                    initArray(module, array, ety);
                    stack.push_back(MemSlot(array));
                }
                pc++;
                vmbreak;
            vmcase(IL_newobj)
                {
                    const MilQuali q = proc->body[pc].arg.value<MilQuali>();
                    FlattenedType* ty = getFlattenedType(module, q);
                    if( ty == 0 )
                        execError(module, proc, pc, QString("unknown type '%1'").
                                  arg(MilEmitter::toString(q).constData()));
                    if( ty->type->kind != MilEmitter::Struct && ty->type->kind != MilEmitter::Union
                            && ty->type->kind != MilEmitter::Object)
                        execError(module, proc, pc, "operation not available for given type");
                    int size = ty->fields.size();
                    if( ty->type->kind == MilEmitter::Object )
                        size++;
                    MemSlot* record = createSequence(size); // use the flattened version of the record or union
                    initFields(module, record, ty->fields);
                    if( ty->type->kind == MilEmitter::Object )
                        record[0] = ty;
                    stack.push_back(MemSlot( record ) );
                }
                pc++;
                vmbreak;
            vmcase(IL_not)
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(~lhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(~lhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_or) // TODO short-circuit evaluation
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i | rhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u | rhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_rem)
            vmcase(IL_rem_un)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i % rhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u % rhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_shl)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i << rhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u << rhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_shr)
                switch( lhs.t )
                {
                case MemSlot::I:
                    if( lhs.hw )
                    {
                        qint32 l = lhs.i;
                        stack.push_back(MemSlot(l >> rhs.i,lhs.hw));
                    }else
                        stack.push_back(MemSlot(lhs.i >> rhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u >> rhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_shr_un)
                stack.push_back(MemSlot(lhs.u >> rhs.u,lhs.hw));
                pc++;
                vmbreak;
            vmcase(IL_sizeof)
                stack.push_back(MemSlot(1)); // RISK
                pc++;
                vmbreak;
            vmcase(IL_sub)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i - rhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u - rhs.u,lhs.hw));
                    break;
                case MemSlot::F:
                    stack.push_back(MemSlot(lhs.f - rhs.f,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_xor)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i ^ rhs.i,lhs.hw));
                    break;
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u ^ rhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                vmbreak;
            vmcase(IL_free) {
                lhs = stack.takeLast();
                if( (lhs.t != MemSlot::Pointer) || lhs.p == 0 )
                    execError(module, proc, pc, "invalid pointer");
                MemSlot* header = lhs.p-1;
                if(header->t != MemSlot::Header)
                    execError(module, proc, pc, "cannot free this object");
                delete[] header;
                pc++;
                vmbreak;
            }
            vmcase(IL_repeat) // repeat(1) until(1) end(5)
                curStatement.push_back(IL_repeat);
                pc++;
                vmbreak;
            vmcase(IL_until)
                pc++;
                vmbreak;
            vmcase(IL_exit)
                while( !curStatement.isEmpty() && curStatement.back() != IL_loop )
                    curStatement.pop_back(); // exit everything up to the closest loop statement
                if( curStatement.isEmpty() || curStatement.back() != IL_loop )
                    execError(module, proc, pc, "operation not allowed here");
                curStatement.pop_back();
                pc = proc->body[pc].index;
                vmbreak;
            vmcase(IL_goto)
                pc = proc->body[pc].index;
                vmbreak;
            vmcase(IL_if) // if(1) then(2) else(2) end(5)
                curStatement.push_back(IL_if);
                pc++;
                vmbreak;
            vmcase(IL_iif) // iif(1) then(2) else(2) end(5)
                curStatement.push_back(IL_iif);
                pc++;
                vmbreak;
            vmcase(IL_then)
                if( curStatement.isEmpty() )
                    execError(module, proc, pc, "operation not expected here");
                if( curStatement.back() == IL_if || curStatement.back() == IL_iif )
                {
                    lhs = stack.takeLast();
                    if( lhs.u == 0 )
                        pc = proc->body[pc].index; // jump to else+1 or end
                    else
                        pc++;
                }else // we never execute then in a switch statement
                    execError(module, proc, pc, "operation not expected here");
                vmbreak;
            vmcase(IL_else)
                if( curStatement.isEmpty() )
                    execError(module, proc, pc, "operation not expected here");
                if( curStatement.back() == IL_switch )
                {
                    if( switchExpr.back().t == MemSlot::Procedure )
                        pc = proc->body[pc].index; // done, else points to end
                    else
                        pc++; // execute else
                }else if( curStatement.back() == IL_if || curStatement.back() == IL_iif )
                    pc = proc->body[pc].index; // else points to end, only hit after then is executed
                vmbreak;
            vmcase(IL_end)
                if( curStatement.isEmpty() )
                    execError(module, proc, pc, "operation not expected here");
                if( curStatement.back() == IL_repeat )
                {
                    // repeat until end
                    lhs = stack.takeLast();
                    if( lhs.u != 0 )
                    {
                        curStatement.pop_back();
                        pc++;
                        break;
                    }
                }else if( curStatement.back() == IL_switch )
                    switchExpr.pop_back();
                if( proc->body[pc].index )
                    pc = proc->body[pc].index;
                else
                {
                    curStatement.pop_back();
                    pc++;
                }
                vmbreak;
            vmcase(IL_label)
                // NOP
                pc++;
                vmbreak;
            vmcase(IL_line)
                // NOP
                pc++;
                vmbreak;
            vmcase(IL_loop) // loop(1) end(5)
                curStatement.push_back(IL_loop);
                pc++;
                vmbreak;
            vmcase(IL_pop)
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_ptroff)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::Pointer || rhs.t != MemSlot::I )
                    execError(module, proc, pc, "invalid argument types");
                lhs.p += rhs.i;
                stack.push_back(lhs);
                pc++;
                vmbreak;
            vmcase(IL_ret)
                if( !proc->retType.second.isEmpty() )
                {
                    ret.move(stack.back());
                    stack.pop_back();
                }
                if( !stack.isEmpty() )
                    execError(module, proc, pc, "stack must be empty at this place");
                return;

            vmcase(IL_starg)
            vmcase(IL_starg_s)
                storeVariable(module, proc, args[proc->body[pc].arg.toUInt()], stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stelem)
            vmcase(IL_stelem_i1)
            vmcase(IL_stelem_i2)
            vmcase(IL_stelem_i4)
            vmcase(IL_stelem_i8)
            vmcase(IL_stelem_r4)
            vmcase(IL_stelem_r8)
            vmcase(IL_stelem_ip) {
                    rhs.move(stack.back());
                    stack.pop_back();
                    MemSlot index = stack.takeLast();
                    lhs.move(stack.back());
                    stack.pop_back();
                    if( lhs.t != MemSlot::Pointer || lhs.p == 0 )
                        execError(module, proc, pc, "invalid array pointer");
                    if( index.t != MemSlot::I && index.t != MemSlot::U )
                        execError(module, proc, pc, "invalid index type");
                    if( rhs.i < 0 )
                        execError(module, proc, pc, "index out of lower bound");
                    if( proc->body[pc].op == IL_stelem )
                    {
                        MilQuali ety = proc->body[pc].arg.value<MilQuali>();
                        FlattenedType* ty = getFlattenedType(module, ety);
                        if( ty && ty->len )
                            index.u *= ty->len; // multi-dim elem types are here by value
                        boundsCheck(module,proc,pc,lhs.p,index.i);
                        store(module,proc,pc,lhs.p + index.u, lhs.embedded, rhs );
                    }else
                    {
                        if( rhs.t != MemSlot::I && rhs.t != MemSlot::U && rhs.t != MemSlot::F &&
                                rhs.t != MemSlot::Pointer && rhs.t != MemSlot::Procedure )
                            execError(module, proc, pc, "invalid value type");
                        boundsCheck(module,proc,pc,lhs.p,index.i);
                        lhs.p[index.u] = rhs;
                    }
                pc++;
                vmbreak;
            }
            vmcase(IL_stfld)
                rhs.move(stack.back());
                stack.pop_back();
                lhs.move(stack.back());
                stack.pop_back();
                if( lhs.t != MemSlot::Pointer || lhs.p == 0 )
                    execError(module, proc, pc, "invalid object pointer");
                store(module,proc,pc,lhs.p, lhs.embedded, rhs);
                pc++;
                vmbreak;
            vmcase(IL_stind_i1)
            vmcase(IL_stind_i2)
            vmcase(IL_stind_i4)
            vmcase(IL_stind_i8)
            vmcase(IL_stind_r4)
            vmcase(IL_stind_r8)
            vmcase(IL_stind_ip)
            vmcase(IL_stind_ipp)
                rhs.move(stack.back());
                stack.pop_back();
                // TODO: default scalar value it t == 0
                if( rhs.t != MemSlot::I && rhs.t != MemSlot::U && rhs.t != MemSlot::F
                        && rhs.t != MemSlot::Pointer && rhs.t != MemSlot::Procedure && rhs.t != MemSlot::Method )
                    execError(module, proc, pc, "incompatible value");
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::Pointer || lhs.p == 0 )
                    execError(module, proc, pc, "invalid pointer");
                lhs.p->move(rhs);
                pc++;
                vmbreak;
            vmcase(IL_stloc)
            vmcase(IL_stloc_s)
                storeVariable(module, proc, locals[proc->body[pc].arg.toUInt()], stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_0)
                storeVariable(module, proc, locals[0], stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_1)
                storeVariable(module, proc, locals[1], stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_2)
                storeVariable(module, proc, locals[2], stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_3)
                storeVariable(module, proc, locals[3], stack.back());
                stack.pop_back();
               pc++;
                vmbreak;
            vmcase(IL_stind)
                Q_ASSERT( stack.size() >= 2 );
                rhs.move(stack.back());
                stack.pop_back();
                lhs.move(stack.back());
                stack.pop_back();
                if( lhs.t != MemSlot::Pointer || lhs.p == 0 )
                    execError(module, proc, pc, "invalid destination pointer");
                store(module,proc,pc,lhs.p,lhs.embedded,rhs);
                pc++;
                vmbreak;
            vmcase(IL_stvar)
                storeVariable(module, proc, module->variables[proc->body[pc].index], stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_switch) // switch(1) {case(1) then(2)} [else(2)] end(5)
                curStatement.push_back(IL_switch);
                switchExpr.append(MemSlot());
                pc++;
                vmbreak;
            vmcase(IL_case)
                if( switchExpr.back().t == MemSlot::Procedure )
                {
                    // done
                    pc++;
                    if( proc->body.size() <= pc || proc->body[pc].op != IL_then )
                        execError(module, proc, pc, "expecting 'then' operation");
                    pc = proc->body[pc].index; // then points to end
                    break;
                }
                if( switchExpr.back().t == MemSlot::Invalid )
                    switchExpr.back() = stack.takeLast();
                if( switchExpr.back().t != MemSlot::I )
                    execError(module, proc, pc+1, "switch expression has invalid type");
                if( proc->body[pc].arg.value<CaseLabelList>().contains( switchExpr.back().i ) )
                {
                    switchExpr.back().t = MemSlot::Procedure; // mark as done
                    pc += 2; // skip then
                }else
                    pc = proc->body[pc].index;
                vmbreak;
            vmcase(IL_while) // while(1) do(1) end(5)
                curStatement.push_back(IL_while);
                pc++;
                vmbreak;
            vmcase(IL_do)
                lhs = stack.takeLast();
                if( lhs.u == 0 )
                {
                    curStatement.pop_back();
                    pc = proc->body[pc].index; // jump to end+1
                }else
                    pc++;
                vmbreak;
            vmcase(IL_nop)
                pc++;
                vmbreak;
#ifndef _USE_JUMP_TABLE
            default:
                throw QString("operator not implemented: %1").arg(s_opName[proc->body[pc].op]);
#endif
            }
        }
    }
};

static inline MilProcedure createIntrinsic(const QByteArray& name, int code, int pars, bool ret )
{
    MilProcedure p;
    p.name = name;
    p.kind = MilProcedure::Intrinsic;
    p.offset = code;
    if(ret)
        p.retType.second = "?";
    for( int i = 0; i < pars; i++ )
        p.params.append( MilVariable( MilQuali("","?"),QByteArray("?")));
    return p;
}

MilInterpreter::MilInterpreter(MilLoader* l)
{
    imp = new Imp;
    imp->loader = l;
#ifdef _USE_GETTIMEOFDAY
    gettimeofday(&imp->start, 0);
#else
    imp->timer.start();
#endif

    imp->intrinsicMod = Token::getSymbol("MIC$");
    imp->outMod = Token::getSymbol("Out");
    imp->inputMod = Token::getSymbol("Input");
    imp->mathlMod = Token::getSymbol("MathL");
    QByteArray name;
    name = Token::getSymbol("relop1");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,1,3,true));
    name = Token::getSymbol("relop2");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,2,3,true));
    name = Token::getSymbol("relop3");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,3,3,true));
    name = Token::getSymbol("relop4");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,4,3,true));
    name = Token::getSymbol("SetDiv");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,5,2,true));
    name = Token::getSymbol("SetIn");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,6,2,true));
    name = Token::getSymbol("printI8");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,7,1,false));
    name = Token::getSymbol("printU8");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,8,1,false));
    name = Token::getSymbol("printF8");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,9,1,false));
    name = Token::getSymbol("printStr");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,10,1,false));
    name = Token::getSymbol("printCh");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,11,1,false));
    name = Token::getSymbol("printBool");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,12,1,false));
    name = Token::getSymbol("printSet");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,13,1,false));
    name = Token::getSymbol("strcopy");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,14,1,false));
    name = Token::getSymbol("assert");
    imp->intrinsics.insert(name.constData(), createIntrinsic(name,15,3,false));
}

MilInterpreter::~MilInterpreter()
{
    delete imp;
}

void MilInterpreter::run(const QByteArray& module)
{
    try
    {
        ModuleData* m = imp->loadModule(module);
        if( m == 0 )
            qCritical() << "module" << module << "not found";
    }catch(const QString& str)
    {
        qCritical() << str;
    }
}

