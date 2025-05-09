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

#include "MilInterpreter.h"
#include "MicSymbol.h"
extern "C" {
#include "runtime/MIC+.h"
}
#include <QVector>
#include <QtDebug>
#include <deque>
using namespace Mil;

enum OpArgCode {
    NoOpArgs,
    OffArg,
    SizeArg,
    IntArg,
    FloatArg,
    StrArg,
    ByteArrayArg,
    ProcArg,
    JumpArg,
    OffSizeArgs
};

enum IL_op
{
    IL_invalid,

#define OPDEF(op, x) IL_##op
#include "MilVmOps.h"
#undef OPDEF

    IL_NUM_OF_OPS
};

static const char* op_names[] = {
    "<invalid>",
    #define OPDEF(op, x) #op
    #include "MilVmOps.h"
    #undef OPDEF
};

static const int op_args[] = {
    0,
    #define OPDEF(op, n) n
    #include "MilVmOps.h"
    #undef OPDEF
};

struct Operation
{
    uint val : 22;
    uint minus : 1;
    uint op : 9;
    Operation(IL_op op = IL_invalid, quint32 val = 0, bool minus = false):val(val),minus(minus),op(op){}
};

#define _USE_JUMP_TABLE // instead of a big switch
//#define _CHECK_HEAP_ADDRESSES

enum { PreAllocSize = 1024 };

class ByteArray
{
    char* d;
    quint32 s : 31;
    quint32 borrowed : 1;
public:
    // according to Valgrind, QByteArray.detach() is very expensive; so here a custom imp.
    ByteArray():d(0),s(0),borrowed(0) {}
    ~ByteArray()
    {
        if( d && !borrowed )
            free(d);
    }
    void init(void* mem, quint32 size)
    {
        if( d && !borrowed )
            free(d);
        d = (char*)mem;
        s = size;
        borrowed = 1;
    }
    void resize(int len)
    {
        if(len <= s)
            return;
        if( d )
        {
            if( borrowed )
            {
                d = (char*)malloc(len);
                borrowed = 0;
            }else
                d = (char*)realloc(d, len);
        }else
            d = (char*)malloc(len);
        s = len;
    }
    inline char* data() { return d; }
    inline int size() const { return s; }
};

struct Procedure
{
    std::vector<Operation> ops;
    // according to Valgrind, QVector.detach() is very expensive
    Declaration* decl;
    Interpreter::FfiProc ffi;
    bool called;
    bool init;
    quint32 localsSize;
    quint32 fixArgSize, returnSize; // stackAligned
    Procedure():decl(0),called(false),localsSize(0),fixArgSize(0),returnSize(0),ffi(0),ops(0),init(false){}
};

struct Vtable
{
    Vtable* parent;
    Type* type;
    std::vector<Procedure*> methods;
};

struct MethRef
{
    void* obj;
    Procedure* proc;
    MethRef(Vtable* o = 0, Procedure* p = 0):obj(o),proc(p){}
};

class Frame
{
public:
    enum { stackAlig = 8 };
    Procedure* proc;
    Frame* outer;
    ByteArray locals;
    ByteArray stack; // intermediate, calls, aligns to quint64
    int sp;
    Frame():proc(0),outer(0),sp(0){}
    void* alloc(int len)
    {
        if( len < stackAlig )
            len = stackAlig;
        if( sp + len > stack.size() )
            stack.resize(len);
        void* res = stack.data()+sp;
        sp += len;
        return res;
    }
    inline void push(void* what, int len)
    {
        void* ptr = alloc(len);
        memcpy(ptr,what,len);
    }
    inline void pushI4(qint32 v)
    {
        push(&v,sizeof(qint32));
    }
    inline void pushI8(qint64 v)
    {
        push(&v,sizeof(qint64));
    }
    inline void pushR4(float v)
    {
        push(&v,sizeof(float));
    }
    inline void pushR8(double v)
    {
        push(&v,sizeof(double));
    }
    inline void pushP(void* v)
    {
        push(&v,sizeof(void*));
    }
    inline void pop(int len)
    {
        sp -= len;
    }
    qint32 popI4()
    {
        qint32 res;
        copy(&res, -stackAlig, sizeof(qint32));
        pop(stackAlig);
        return res;
    }
    float popR4()
    {
        float res;
        copy(&res, -stackAlig, sizeof(float));
        pop(stackAlig);
        return res;
    }
    qint64 popI8()
    {
        qint64 res;
        copy(&res, -stackAlig, sizeof(qint64));
        pop(stackAlig);
        return res;
    }
    double popR8()
    {
        double res;
        copy(&res, -stackAlig, sizeof(double));
        pop(stackAlig);
        return res;
    }
    void* popP()
    {
        void* res;
        copy(&res, -stackAlig, sizeof(void*));
        pop(stackAlig);
        return res;
    }
    MethRef popPP()
    {
        MethRef res;
        const int sos = Interpreter::stackAligned(sizeof(MethRef));
        copy(&res, -sos, sizeof(MethRef));
        pop(sos);
        return res;
    }
    void insert(int off, void* p)
    {
        Q_ASSERT(off <= 0);
        // make room for one slot as the first argument and
        // move all existing arguments by one slot to the right
        quint8* to = (quint8*)(stack.data()+sp+off);
        quint8* from = (quint8*)(stack.data()+sp);
        while( from >= to )
        {
            from[Interpreter::StackAlign] = *from;
            from--;
        }
        *(void**)to = p;
        sp -= off;
    }

    inline void copy(void* to, int off, int len)
    {
        Q_ASSERT(off <= 0);
        memcpy(to,stack.data()+sp+off,len);
    }
    inline void* slot(int off)
    {
        return stack.data()+sp+off*stackAlig;
    }
private:
    static void* operator new(size_t);      // Block scalar heap allocation
    static void* operator new[](size_t);    // Block array heap allocation
};

static bool MIC_relop1(void* args, void* ret)
{
    // int MIC$$relop1(const char* l, const char* r, int op)
    const int res = MIC$$relop1((char*)Interpreter::toP(args, 0),
                                (char*)Interpreter::toP(args, sizeof(void*)),
                                Interpreter::toI4(args, 2*Interpreter::stackAligned(sizeof(void*))));
    Interpreter::retI4(ret, res);
    return true;
}

static bool MIC_relop2(void* args, void* ret)
{
    // int MIC$$relop2(const char* lhs, char rhs, int op)
    const int res = MIC$$relop2((char*)Interpreter::toP(args, 0),
                                Interpreter::toI4(args, sizeof(void*)),
                                Interpreter::toI4(args, 2*Interpreter::stackAligned(sizeof(void*))));
    Interpreter::retI4(ret, res);
    return true;
}

static bool MIC_relop3(void* args, void* ret)
{
    // int MIC$$relop3(char lhs, const char* rhs, int op)
    const int res = MIC$$relop3(Interpreter::toI4(args, 0),
                                (char*)Interpreter::toP(args, sizeof(int)),
                                Interpreter::toI4(args, 2*Interpreter::stackAligned(sizeof(void*))));
    Interpreter::retI4(ret, res);
    return true;
}

static bool MIC_relop4(void* args, void* ret)
{
    // int MIC$$relop4(char lhs, char rhs, int op)
    const int res = MIC$$relop4(Interpreter::toI4(args, 0),
                                Interpreter::toI4(args, sizeof(void*)),
                                Interpreter::toI4(args, 2*Interpreter::stackAligned(sizeof(void*))));
    Interpreter::retI4(ret, res);
    return true;
}


static bool MIC_SetDiv(void* args, void* ret)
{
    // uint32_t MIC$$SetDiv( uint32_t lhs, uint32_t rhs )
    quint32 res = MIC$$SetDiv(Interpreter::toI4(args,0), Interpreter::toI4(args,8) );
    Interpreter::retI4(ret,res);
    return true;
}

static bool MIC_SetIn(void* args, void* ret)
{
    // uint32_t MIC$$SetIn( uint32_t lhs, uint32_t rhs )
    quint32 res = MIC$$SetIn(Interpreter::toI4(args,0), Interpreter::toI4(args,8) );
    Interpreter::retI4(ret,res);
    return true;
}

static bool MIC_printI8(void* args, void* ret)
{
    MIC$$printI8(*(qint64*)args);
    return true;
}

static bool MIC_printU8(void* args, void* ret)
{
    MIC$$printU8(*(qint64*)args);
    return true;
}

static bool MIC_printF8(void* args, void* ret)
{
    MIC$$printF8(*(double*)args);
    return true;
}

static bool MIC_printStr(void* args, void* ret)
{
    MIC$$printStr(*(char**)args);
    return true;
}

static bool MIC_printCh(void* args, void* ret)
{
    MIC$$printCh(*(qint32*)args);
    return true;
}

static bool MIC_printBool(void* args, void* ret)
{
    MIC$$printBool(*(qint32*)args);
    return true;
}

static bool MIC_printSet(void* args, void* ret)
{
    MIC$$printBool(*(qint32*)args);
    return true;
}

static bool MIC_assert(void* args, void* ret)
{
    // void MIC$$assert(uint8_t cond, uint32_t line, const char* file)
    if( !Interpreter::toI4(args,0) )
    {
        throw QString("assertion failed at %1:%2").arg((char*)Interpreter::toP(args,16)).
            arg(Interpreter::toI4(args, 8));
    }

    return true;
}

struct Template
{
    Type* type;
    std::vector<char> mem; // preinitialized memory for type
    Template():type(0){}
};

struct Interpreter::Imp
{
    AstModel* mdl;
    // accessing std::vector is cheaper than QVector or QByteArray
    std::vector<std::string> strings;
    std::vector<QByteArray> objects;
    std::vector<double> doubles;
    std::vector<qint64> ints;
    std::vector<Procedure*> procs;
    std::vector<Vtable*> vtables;
    std::vector<char> moduleData;
    std::vector<Template> templates;
    Procedure* curProc;
    QList< QList<int> > loopStack;
    const char* MIC$;
    QMap<const char*,QMap<const char*, FfiProc> > ffiProcs;

    Imp(AstModel* mdl):mdl(mdl),curProc(0)
    {
#if 0
        QTextStream out(stdout);
        for(int i = 1; i < IL_NUM_OF_OPS; i++ )
            out << "vmcase(" << op_names[i] << ")" << endl;
#endif
        MIC$ = Mic::Symbol::getSymbol("MIC$").constData();
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("relop1").constData(), MIC_relop1);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("relop2").constData(), MIC_relop2);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("relop3").constData(), MIC_relop3);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("relop4").constData(), MIC_relop4);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("SetDiv").constData(), MIC_SetDiv);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("SetIn").constData(), MIC_SetIn);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("printI8").constData(), MIC_printI8);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("printU8").constData(), MIC_printU8);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("printF8").constData(), MIC_printF8);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("printStr").constData(), MIC_printStr);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("printCh").constData(), MIC_printCh);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("printBool").constData(), MIC_printBool);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("printSet").constData(), MIC_printSet);
        ffiProcs[MIC$].insert(Mic::Symbol::getSymbol("assert").constData(), MIC_assert);
    }
    ~Imp()
    {
        for( int i = 0; i < procs.size(); i++ )
            delete procs[i];
        for( int i = 0; i < vtables.size(); i++ )
            delete vtables[i];
    }

    void downcopy(Vtable* vt)
    {
        // make sure that each vtable is filled with inherited methods as far as used
        // TODO avoid multiple scans of same vt
        if( vt->parent )
        {
            downcopy(vt->parent);
            for( int i = 0; i < vt->parent->methods.size(); i++ )
            {
                if( vt->parent->methods[i] && vt->methods[i] == 0 )
                    vt->methods[i] = vt->parent->methods[i];
            }
        }
    }

    bool translateModule(Declaration* m)
    {
        Q_ASSERT(m && m->kind == Declaration::Module);

        if( m->init )
            return true; // the module was already translated

        m->init = true; // re/misuse this flag to indicate that we already translated

        Declaration* sub = m->subs;
        while(sub)
        {
            if( sub->kind == Declaration::TypeDecl && sub->getType() && sub->getType()->kind == Type::Object )
            {
                Vtable* vt = new Vtable();
                vtables.push_back(vt);
                vt->type = sub->getType();
                if( sub->getType()->getType() )
                {
                    const int i = findVtable(deref(sub->getType()->getType()));
                    if( i != -1 )
                        vt->parent = vtables[i];
                }
                vt->methods.resize(vt->type->getMethodTable().size());
            }
            sub = sub->next;
        }

        // look for the init procedure or synthesize one
        Declaration* init = m->findInitProc();
        if(init == 0)
        {
            // no init proc was found, so we synthesize a minimal one
            Procedure* cur = new Procedure();
            procs.push_back(cur);
            cur->decl = m;
            if( !translateInit(*cur, procs.size()-1) )
                return false;
        }else if( !translateProc(init) )
            return false;
        return true;
    }

    bool translateProc(Declaration* proc)
    {
        if( proc->validated )
            return true; // the proc was already translated
        proc->validated = true;
        if( proc->forward )
            return translateProc(proc->forwardTo);

        Q_ASSERT( proc->kind == Declaration::Procedure );

        Procedure* cur = new Procedure();
        procs.push_back(cur);
        cur->decl = proc;

        if( proc->init && !translateInit(*cur, procs.size()-1) )
            // add a prefix which calls imports if not already called
            return false;
        if( proc->typebound )
        {
            const int off = findVtable(proc->outer->getType());
            Q_ASSERT(off != -1);
            vtables[off]->methods[proc->off] = cur;
            if(proc->override_)
            {
                // go up the inheritance chain and assure all super methods are translated
                Type* super = deref(proc->outer->getType()->getType());
                Q_ASSERT(super && super->kind == Type::Object);
                Declaration* baseproc = super->findSubByName(proc->name, true);
                if( baseproc )
                {
                    Q_ASSERT(proc->off == baseproc->off );
                    translateProc(baseproc);
                }
            }
        }
        return translateProc(*cur);
    }

    int findVtable(Type* object) const
    {
        for( int i = 0; i < vtables.size(); i++ )
        {
            if( vtables[i]->type == object )
                return i;
        }
        return -1;
    }

    Vtable* getVtable(Type* object) const
    {
        for( int i = 0; i < vtables.size(); i++ )
        {
            if( vtables[i]->type == object )
                return vtables[i];
        }
        return 0;
    }

    int findProc(Declaration* proc) const
    {
        while( proc->kind == Declaration::Procedure && proc->forward )
            proc = proc->forwardTo;
        if( proc->kind == Declaration::Module )
        {
            Declaration* init = proc->findInitProc();
            if( init )
                proc = init;
        }
        for( int i = 0; i < procs.size(); i++ )
        {
            if( procs[i]->decl == proc )
                return i;
        }
        return -1;
    }

    bool run(Declaration* proc)
    {
        const int i = findProc(proc);
        if( i >= 0 )
            return run(i);
        // proc not found
        return false;
    }

    template<typename T>
    int appendUnique(std::vector<T>& vec, const T& val)
    {
          for (size_t i = 0; i < vec.size(); ++i) {
              if (vec[i] == val)
                  return i;
          }
          vec.push_back(val);
          return vec.size() - 1;
    }

    quint32 addInt(qint64 i)
    {
        return appendUnique(ints,i);
    }

    quint32 addFloat(double f)
    {
        return appendUnique(doubles, f);
    }

    quint32 addString(const char* str)
    {
        return appendUnique(strings, std::string(str));
    }

    void render(char* data, quint32 off, Type* t, Constant* c)
    {
        switch(c->kind)
        {
        case Constant::D:
            if( t->kind == Type::FLOAT32 )
            {
                float tmp = c->d;
                memcpy(data+off, &tmp, sizeof(float));
            }else
            {
                Q_ASSERT(t->kind == Type::FLOAT64);
                memcpy(data+off, &c->d, sizeof(double));
            }
            break;
        case Constant::I:
            switch(t->kind)
            {
            case Type::BOOL:
            case Type::CHAR:
            case Type::UINT8:
            case Type::INT8: {
                    qint8 tmp = c->i;
                    *(data+off) = tmp;
                    break;
                }
            case Type::UINT16:
            case Type::INT16: {
                    qint16 tmp = c->i;
                    memcpy(data+off, &tmp, sizeof(qint16));
                    break;
                }
            case Type::UINT32:
            case Type::INT32: {
                    qint32 tmp = c->i;
                    memcpy(data+off, &tmp, sizeof(qint32));
                    break;
                }
            case Type::UINT64:
            case Type::INT64:
                memcpy(data+off, &c->i, sizeof(qint64));
                break;
            default:
                Q_ASSERT(false);
            }
            break;
        case Constant::S:
            Q_ASSERT(t->kind == Type::Array && deref(t->getType())->kind == Type::CHAR && t->len);
            strncpy(data+off, c->s, t->len-1);
            *(data+off+t->len-1) = 0;
            break;
        case Constant::B:
            Q_ASSERT(t->kind == Type::Array && deref(t->getType())->kind == Type::UINT8 &&
                     t->len == c->b->len);
            memcpy(data+off, c->b, t->len);
            break;
        case Constant::R:
            Q_ASSERT(c->r->kind == Declaration::ConstDecl);
            render(data, off, t, c->r->c);
            break;
        case Constant::C:
            if( c->c->type == 0 )
                c->c->type = t;
            render(data, off, c->c);
            break;
        default:
            Q_ASSERT(false);
        }
    }

    void render(char* data, quint32 start, ComponentList* cl )
    {
        Type* t = deref(cl->type);
        if( t->kind == Type::Array )
        {
            Type* et = deref(t->getType());
            int off = start;
            Component* c = cl->c;
            while( c )
            {
                render(data, off, et, c->c);
                off += et->getByteSize(sizeof(void*));
                c = c->next;
            }
        }else
        {
            qWarning() << "TODO record literals not yet implemented";
        }
    }

    quint32 addObject(Constant* c)
    {
        QByteArray obj;
        if( c->kind == Constant::B )
            obj = QByteArray::fromRawData((const char*)c->b->b, c->b->len);
        else
        {
            ComponentList* cl = c->c;
            Q_ASSERT( cl->type );
            obj.resize(deref(cl->type)->getByteSize(sizeof(void*)));
            render(obj.data(), 0, cl);
        }
        const quint32 id = objects.size();
        objects.push_back(obj);
        return id;
    }

    int emitOp(Procedure& proc, IL_op op, quint32 v = 0, bool minus = false )
    {
        const int res = proc.ops.size();
        proc.ops.push_back(Operation(op, v, minus));
        return res;
    }

    Type* deref(Type* t)
    {
        if( t && t->kind == Type::NameRef )
            return deref(t->getType());
        else if( t )
            return t;
        else
            return mdl->getBasicType(Type::Undefined);
    }

    void inline branch_here(Procedure& proc, int pc)
    {
        Q_ASSERT(pc >= 0 && pc < proc.ops.size());
        proc.ops[pc].val = proc.ops.size() - pc - 1;
    }

    static inline int stackAligned(int off)
    {
        return AstModel::align(off, Frame::stackAlig );
    }

    int findTemplate(Type* t) const
    {
        for( int i = 0; i < templates.size(); i++ )
            if( templates[i].type == t )
                return i;
        return -1;
    }

    void initMemory(char* mem, Type* t, bool doPointerInit )
    {
        if( doPointerInit && t->pointerInit )
            memset(mem, 0, t->getByteSize(sizeof(void*)));
        if( !t->objectInit )
            return;
        if( t->kind == Type::Object )
        {
            *((Vtable**) mem) = getVtable(t);
        }
        if( t->kind == Type::Struct || t->kind == Type::Object )
        {
            DeclList fields = t->getFieldList(true);
            int off = 0;
            foreach(Declaration* field, fields)
            {

                Type* tt = deref(field->getType());
                if( tt->objectInit )
                    initMemory(mem + off, tt, false);
                off += field->f.off;
            }
        }else if( t->kind == Type::Array && t->len != 0)
        {
            Type* et = deref(t->getType());
            int off = 0;
            for(int i = 0; i < t->len; i++ )
            {
                initMemory(mem + off, et, false);
                off += et->getByteSize(sizeof(void*));
            }
        }
    }

    bool translateInit(Procedure& proc, quint32 id);
    bool translateProc(Procedure& proc);
    bool translateStatSeq(Procedure& proc, Statement* s);
    bool translateExprSeq(Procedure& proc, Expression* e);

    bool run(quint32 proc);
    bool execute(Frame*);
    bool call(Frame* frame, int pc, Procedure* proc, void* local, void* stack);
};

Interpreter::Interpreter(AstModel* mdl)
{
    imp = new Imp(mdl);
}

Interpreter::~Interpreter()
{
    delete imp;
}

qint32 Interpreter::toI4(void* args, int off)
{
    qint32 res;
    memcpy(&res, args + Imp::stackAligned(off), sizeof(res));
    return res;
}

qint64 Interpreter::toI8(void* args, int off)
{
    qint64 res;
    memcpy(&res, args + Imp::stackAligned(off), sizeof(res));
    return res;
}

float Interpreter::toR4(void* args, int off)
{
    float res;
    memcpy(&res, args + Imp::stackAligned(off), sizeof(res));
    return res;
}

double Interpreter::toR8(void* args, int off)
{
    double res;
    memcpy(&res, args + Imp::stackAligned(off), sizeof(res));
    return res;
}

void*Interpreter::toP(void* args, int off)
{
    void* res;
    memcpy(&res, args + Imp::stackAligned(off), sizeof(res));
    return res;
}

void Interpreter::retI4(void* ret, qint32 val)
{
    memcpy(ret, &val, sizeof(qint32));
}

void Interpreter::retI8(void* ret, qint64 val)
{
    memcpy(ret, &val, sizeof(qint64));
}

void Interpreter::retR4(void* ret, float val)
{
    memcpy(ret, &val, sizeof(float));
}

void Interpreter::retR8(void* ret, double val)
{
    memcpy(ret, &val, sizeof(double));
}

void Interpreter::retP(void* ret, void* val)
{
    memcpy(ret, &val, sizeof(void*));
}

int Interpreter::stackAligned(int off)
{
    return Imp::stackAligned(off);
}

void Interpreter::registerProc(const QByteArray& module, const QByteArray& procName, Interpreter::FfiProc proc)
{
    imp->ffiProcs[Mic::Symbol::getSymbol(module).constData()].insert(
                Mic::Symbol::getSymbol(procName).constData(), proc);
}

bool Interpreter::precompile(Declaration* proc)
{
    Q_ASSERT(proc && (proc->kind == Declaration::Procedure || proc->kind == Declaration::Module));

    Declaration* module = proc->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    const bool res = imp->translateModule(module);
    if(res)
    {
        foreach(Vtable* vt, imp->vtables )
            imp->downcopy(vt);
    }
    return res;
}

bool Interpreter::dumpProc(QTextStream& out, Declaration* proc)
{
    if( proc->forward )
        return false;
    const int i = imp->findProc(proc);
    if( i == -1 )
        return false; // there is no implementation for this proc, not an error
    Procedure* p = imp->procs[i];
    out << "proc " << p->decl->toPath() << endl;
    for( int pc = 0; pc < p->ops.size(); pc++ )
    {
        out << "    " << QString("%1: ").arg(pc,2) << op_names[p->ops[pc].op];
        switch(op_args[p->ops[pc].op])
        {
        case NoOpArgs:
            break;
        case OffArg:
            out << " " << p->ops[pc].val;
            break;
        case SizeArg:
            out << " " << p->ops[pc].val;
            break;
        case IntArg:
            out << " " << imp->ints[p->ops[pc].val];
            break;
        case FloatArg:
            out << " " << imp->doubles[p->ops[pc].val];
            break;
        case StrArg:
            out << " \"" << imp->strings[p->ops[pc].val].c_str() << "\"";
            break;
        case ByteArrayArg:
            out << " $" << imp->objects[p->ops[pc].val].toHex().left(40) << " (" <<
                   imp->objects[p->ops[pc].val].size() << ")";
            break;
        case ProcArg:
            if( p->ops[pc].val < imp->procs.size() )
                out << " " << imp->procs[p->ops[pc].val]->decl->toPath();
            else
                out << " invalid proc " << p->ops[pc].val;
            break;
        case JumpArg:
            out << " " << (p->ops[pc].minus ? "-" : "") << p->ops[pc].val << " -> "
                << QString("%1").arg(pc + 1 + (p->ops[pc].minus?-1:1) * p->ops[pc].val);
            break;
        case OffSizeArgs:
            Q_ASSERT(pc+1 < p->ops.size());
            out << " " << p->ops[pc].val;
            out << " " << p->ops[pc+1].val;
            pc++;
           break;
        default:
            Q_ASSERT(false);
        }

        out << endl;
    }
    return true;
}

bool Interpreter::dumpModule(QTextStream& out, Declaration* module)
{
    Declaration* d = module->subs;
    while(d)
    {
        if(d->kind == Declaration::Procedure)
            dumpProc(out, d);
        d = d->next;
    }
    return true;
}

bool Interpreter::dumpAll(QTextStream& out)
{
    DeclList& modules = imp->mdl->getModules();
    foreach( Declaration* module, modules )
        dumpModule(out, module);
    return true;
}

bool Interpreter::run(Declaration* proc)
{
    Q_ASSERT(proc && (proc->kind == Declaration::Procedure || proc->kind == Declaration::Module));

    Declaration* module = proc->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    if( !module->init && !precompile(module) )
        return false;

    try
    {
        if( proc->kind == Declaration::Module )
            return imp->run(proc);

        if( !proc->init )
        {
            if( !imp->run(module) )
                return false;
        }

        return imp->run(proc);
    }catch( const QString& err )
    {
        qCritical() << err.toUtf8().constData();
        return false;
    }
}

bool Interpreter::Imp::translateInit(Procedure& proc, quint32 id)
{
    Q_ASSERT( proc.decl && (proc.decl->kind == Declaration::Module || proc.decl->kind == Declaration::Procedure) );

    Declaration* module;
    if( proc.decl->kind == Declaration::Procedure )
        module = proc.decl->getModule();
    else
        module = proc.decl;

    DeclList vars = module->getVars();
    if( !vars.isEmpty() )
    {
        const int off = moduleData.size();
        const int len = vars.last()->off + vars.last()->getType()->getByteSize(sizeof(void*));
        moduleData.resize(off + len + AstModel::padding(len, sizeof(void*)));
        // relocate module var addresses, init vars if necessary
        foreach( Declaration* d, vars )
        {
            Type* t = deref(d->getType());
            d->off += off;
            initMemory(moduleData.data()+d->off, t,true);
        }
    }
    // first check if already called
    proc.init = true;

    Declaration* d = proc.decl->subs;
    while(d)
    {
        if( d->kind == Declaration::Import )
        {
            translateModule(d->imported);
            const int p = findProc(d->imported);
            if( p < 0 )
            {
                qCritical() << "module initializer not found" << d->imported->toPath();
                return false; // procedure not found
            }
            emitOp(proc,IL_call, p);
        }
        // TODO: initialize structs, arrays and objects for value objects vtables
        d = d->next;
    }
    return true;
}

bool Interpreter::Imp::translateProc(Procedure& proc)
{
    Q_ASSERT( proc.decl && proc.decl->kind == Declaration::Procedure );
    const DeclList locals = proc.decl->getLocals();
    if( !locals.isEmpty() )
        proc.localsSize = locals.last()->off + locals.last()->getType()->getByteSize(sizeof(void*));
    const DeclList params = proc.decl->getParams();
    if( !params.isEmpty() )
        // this always includes SELF
        proc.fixArgSize = stackAligned(params.last()->off + params.last()->getType()->getByteSize(sizeof(void*)));
    if( proc.decl->getType() )
        proc.returnSize = stackAligned(proc.decl->getType()->getByteSize(sizeof(void*)));
    if( proc.decl->extern_ )
    {
        if(proc.decl->typebound)
        {
            qCritical() << "typebound external implementations not yet supported" << proc.decl->toPath();
            return false;
        }
        Declaration* module = proc.decl->getModule();

        proc.ffi = ffiProcs.value(module->name.constData()).value(proc.decl->name.constData());
        if( proc.ffi == 0 )
        {
            qCritical() << "cannot find external implementation for" << proc.decl->toPath();
            return false;
        }
        return true;
    }else
    {
        Statement* s = proc.decl->body;
        Procedure* oldProc = curProc;
        curProc = &proc;
        const bool res = translateStatSeq(proc, s);
        curProc = oldProc;
        return res;
    }
}

bool Interpreter::Imp::translateStatSeq(Procedure& proc, Statement* s)
{
    while(s)
    {
        switch(s->kind)
        {
        case Tok_STARG:
            {
                Q_ASSERT(curProc);
                DeclList params = curProc->decl->getParams();
                Q_ASSERT(s->id < params.size());
                Type* t = deref(params[s->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_starg_i1,params[s->id]->off);
                    break;
                case Type::INT16:
                case Type::UINT16:
                    emitOp(proc, IL_starg_i2,params[s->id]->off);
                    break;
                case Type::INT32:
                case Type::UINT32:
                    emitOp(proc, IL_starg_i4,params[s->id]->off);
                    break;
                case Type::INT64:
                case Type::UINT64:
                    emitOp(proc, IL_starg_i8,params[s->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_starg_r4,params[s->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_starg_r8,params[s->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_starg_pp,params[s->id]->off);
                    else
                        emitOp(proc, IL_starg_p,params[s->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_starg_vt,params[s->id]->off);
                    emitOp(proc, IL_vt_size,t->getByteSize(sizeof(void*)));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_STLOC:
        case Tok_STLOC_S:
        case Tok_STLOC_0:
        case Tok_STLOC_1:
        case Tok_STLOC_2:
        case Tok_STLOC_3:
            {
                Q_ASSERT(curProc);
                DeclList locals = curProc->decl->getLocals();
                Q_ASSERT(s->id < locals.size());
                Type* t = deref(locals[s->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_stloc_i1,locals[s->id]->off);
                    break;
                case Type::INT16:
                case Type::UINT16:
                    emitOp(proc, IL_stloc_i2,locals[s->id]->off);
                    break;
                case Type::INT32:
                case Type::UINT32:
                    emitOp(proc, IL_stloc_i4,locals[s->id]->off);
                    break;
                case Type::UINT64:
                case Type::INT64:
                    emitOp(proc, IL_stloc_i8,locals[s->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_stloc_r4,locals[s->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_stloc_r8,locals[s->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_stloc_pp,locals[s->id]->off);
                    else
                        emitOp(proc, IL_stloc_p,locals[s->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_stloc_vt,locals[s->id]->off);
                    emitOp(proc, IL_vt_size, t->getByteSize(sizeof(void*)));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_STIND:
            emitOp(proc, IL_stind_vt,deref(s->d->getType())->getByteSize(sizeof(void*)));
            break;
        case Tok_STIND_I1:
            emitOp(proc, IL_stind_i1);
            break;
        case Tok_STIND_I2:
            emitOp(proc, IL_stind_i2);
            break;
        case Tok_STIND_I4:
            emitOp(proc, IL_stind_i4);
            break;
        case Tok_STIND_I8:
            emitOp(proc, IL_stind_i8);
            break;
        case Tok_STIND_R4:
            emitOp(proc, IL_stind_r4);
            break;
        case Tok_STIND_R8:
            emitOp(proc, IL_stind_r8);
            break;
        case Tok_STIND_IP:
            emitOp(proc, IL_stind_p);
            break;
        case Tok_STIND_IPP:
            emitOp(proc, IL_stind_vt,sizeof(MethRef));
            break;
        case Tok_STELEM_IPP:
            emitOp(proc, IL_stelem_vt, sizeof(MethRef));
            break;
        case Tok_STELEM:
            emitOp(proc, IL_stelem_vt, deref(s->d->getType())->getByteSize(sizeof(void*)));
            break;
        case Tok_STELEM_I1:
            emitOp(proc, IL_stelem_i1);
            break;
        case Tok_STELEM_I2:
            emitOp(proc, IL_stelem_i2);
            break;
        case Tok_STELEM_I4:
            emitOp(proc, IL_stelem_i4);
            break;
        case Tok_STELEM_I8:
            emitOp(proc, IL_stelem_i8);
            break;
        case Tok_STELEM_R4:
            emitOp(proc, IL_stelem_r4);
            break;
        case Tok_STELEM_R8:
            emitOp(proc, IL_stelem_r8);
            break;
        case Tok_STELEM_IP:
            emitOp(proc, IL_stelem_p);
            break;
        case Statement::ExprStat:
            if( !translateExprSeq(proc, s->e) )
                return false;
            break;
        case Tok_STFLD:
            switch(deref(s->d->getType())->kind)
            {
            case Type::INT8:
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_stfld_i1,s->d->f.off);
                break;
            case Type::INT16:
            case Type::UINT16:
                emitOp(proc, IL_stfld_i2,s->d->f.off);
                break;
            case Type::INT32:
            case Type::UINT32:
                emitOp(proc, IL_stfld_i4,s->d->f.off);
                break;
            case Type::UINT64:
            case Type::INT64:
                emitOp(proc, IL_stfld_i8,s->d->f.off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_stfld_r4,s->d->f.off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_stfld_r8,s->d->f.off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(deref(s->d->getType())->typebound)
                    emitOp(proc, IL_stfld_pp,s->d->f.off);
                else
                    emitOp(proc, IL_stfld_p,s->d->f.off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_stfld_vt,s->d->f.off);
                emitOp(proc, IL_vt_size, deref(s->d->getType())->getByteSize(sizeof(void*)));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_STVAR:
            switch(deref(s->d->getType())->kind)
            {
            case Type::INT8:
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_stvar_i1,s->d->off);
                break;
            case Type::INT16:
            case Type::UINT16:
                emitOp(proc, IL_stvar_i2,s->d->off);
                break;
            case Type::INT32:
            case Type::UINT32:
                emitOp(proc, IL_stvar_i4,s->d->off);
                break;
            case Type::UINT64:
            case Type::INT64:
                emitOp(proc, IL_stvar_i8,s->d->off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_stvar_r4,s->d->off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_stvar_r8,s->d->off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(deref(s->d->getType())->typebound)
                    emitOp(proc, IL_stvar_pp,s->d->off);
                else
                    emitOp(proc, IL_stvar_p,s->d->off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_stvar_vt,s->d->off);
                emitOp(proc, IL_vt_size, deref(s->d->getType())->getByteSize(sizeof(void*)));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_IF:
            {
                if( !translateExprSeq(proc, s->e) )
                    return false;
                const int ifnot = emitOp(proc, deref(s->e->getType())->isInt64() ? IL_brfalse_i8 : IL_brfalse_i4);
                if( !translateStatSeq(proc, s->body) )
                    return false;
                const int after_if = emitOp(proc, IL_br);
                branch_here(proc, ifnot);
                if( s->next && s->next->kind == Tok_ELSE )
                {
                    s = s->next;
                    if( !translateStatSeq(proc, s->body) )
                        return false;
                }
                branch_here(proc,after_if);
            }
            break;
        case Tok_LOOP:
            {
                loopStack.push_back(QList<int>());
                if( !translateStatSeq(proc, s->body) )
                    return false;
                foreach( int pc, loopStack.back() )
                    branch_here(proc,pc);
                loopStack.pop_back();
            }
            break;
        case Tok_EXIT:
            loopStack.back() << emitOp(proc,IL_br);
            break;
        case Tok_REPEAT:
            {
                const int start = proc.ops.size();
                if( !translateStatSeq(proc, s->body) )
                    return false;
                if( !translateExprSeq(proc, s->e) )
                    return false;
                emitOp(proc, s->e->getType()->isInt64() ? IL_brfalse_i8 : IL_brfalse_i4, proc.ops.size()-start+1, true );
            }
            break;
        case Tok_WHILE:
            {
                const int start = proc.ops.size();
                if( !translateExprSeq(proc, s->e) )
                    return false;
                const int while_ = emitOp(proc, s->e->getType()->isInt64() ? IL_brfalse_i8 : IL_brfalse_i4 );
                if( !translateStatSeq(proc, s->body) )
                    return false;
                emitOp(proc, IL_br, proc.ops.size()-start+1, true);
                branch_here(proc, while_);
            }
            break;
        case Tok_POP:
            emitOp(proc, IL_pop, s->args->getType()->getByteSize(sizeof(void*)));
            break;
        case Tok_STRCPY:
            emitOp(proc, IL_strcpy, deref(s->args->lhs->getType()->getType())->len);
            break;
        case Tok_RET:
            emitOp(proc, IL_ret, s->args ? s->args->getType()->getByteSize(sizeof(void*)) : 0 );
            break;
        case Tok_FREE:
            emitOp(proc, IL_free );
            break;
        case Tok_SWITCH:
        case Tok_LABEL:
        case Tok_GOTO:
            qCritical() << "ERROR: not yet implemented in interpreter:" << tokenTypeString(s->kind);
            return false;
       default:
            Q_ASSERT(false);
        }

        s = s->next;
    }
    return true;
}

bool Interpreter::Imp::translateExprSeq(Procedure& proc, Expression* e)
{
    static const int pointerWidth = sizeof(void*);
    while(e)
    {
        Type* t = deref(e->getType());
        Type* lhsT = deref(e->lhs ? e->lhs->getType() : 0);
        switch(e->kind)
        {
        case Tok_ADD:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_add_i4);
            else if( t->isInt64())
                emitOp(proc, IL_add_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_add_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_add_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SUB:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_sub_i4);
            else if( t->isInt64())
                emitOp(proc, IL_sub_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_sub_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_sub_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_DIV:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_div_i4);
            else if( t->isInt64())
                emitOp(proc, IL_div_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_div_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_div_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_DIV_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_div_un_i4);
            else if( t->isInt64())
                emitOp(proc, IL_div_un_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_MUL:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_mul_i4);
            else if( t->isInt64())
                emitOp(proc, IL_mul_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_mul_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_mul_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_REM:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_rem_i4);
            else if( t->isInt64())
                emitOp(proc, IL_rem_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_REM_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_rem_un_i4);
            else if( t->isInt64())
                emitOp(proc, IL_rem_un_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_ABS:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_abs_i4);
            else if( t->isInt64())
                emitOp(proc, IL_abs_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_abs_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_abs_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_NEG:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_neg_i4);
            else if( t->isInt64())
                emitOp(proc, IL_neg_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_neg_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_neg_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_AND:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_and_i4);
            else if( t->isInt64())
                emitOp(proc, IL_and_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_OR:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_or_i4);
            else if( t->isInt64())
                emitOp(proc, IL_or_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_XOR:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_xor_i4);
            else if( t->isInt64())
                emitOp(proc, IL_xor_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SHL:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_shl_i4);
            else if( t->isInt64())
                emitOp(proc, IL_shl_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SHR_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_shr_un_i4);
            else if( t->isInt64())
                emitOp(proc, IL_shr_un_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_SHR:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_shr_i4);
            else if( t->isInt64())
                emitOp(proc, IL_shr_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_NOT:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_not_i4);
            else if( t->isInt64())
                emitOp(proc, IL_not_i8);
            else
                Q_ASSERT(false);
            break;
        case Tok_LDC_I4_0:
            emitOp(proc, IL_ldc_i4_0);
            break;
        case Tok_LDC_I4_1:
            emitOp(proc, IL_ldc_i4_1);
            break;
        case Tok_LDC_I4_2:
            emitOp(proc, IL_ldc_i4_2);
            break;
        case Tok_LDC_I4_3:
            emitOp(proc, IL_ldc_i4_3);
            break;
        case Tok_LDC_I4_4:
            emitOp(proc, IL_ldc_i4_4);
            break;
        case Tok_LDC_I4_5:
            emitOp(proc, IL_ldc_i4_5);
            break;
        case Tok_LDC_I4_6:
            emitOp(proc, IL_ldc_i4_6);
            break;
        case Tok_LDC_I4_7:
            emitOp(proc, IL_ldc_i4_7);
            break;
        case Tok_LDC_I4_8:
            emitOp(proc, IL_ldc_i4_8);
            break;
        case Tok_LDC_I4_M1:
            emitOp(proc, IL_ldc_i4_m1);
            break;
        case Tok_LDC_I4_S:
        case Tok_LDC_I4:
            emitOp(proc, IL_ldc_i4, addInt(e->i));
            break;
        case Tok_LDC_I8:
            emitOp(proc, IL_ldc_i8, addInt(e->i));
            break;
        case Tok_LDC_R4:
            emitOp(proc, IL_ldc_r4, addFloat(e->f) );
            break;
        case Tok_LDC_R8:
            emitOp(proc, IL_ldc_r8, addFloat(e->f) );
            break;
        case Tok_LDNULL:
            emitOp(proc, IL_ldnull);
            break;
        case Tok_LDSTR:
            emitOp(proc, IL_ldstr, addString(e->c->s) );
            break;
        case Tok_LDOBJ:
            emitOp(proc, IL_ldobj, addObject(e->c) );
            break;
        case Tok_LDPROC:
            if( !translateProc(e->d) )
                return false;
            emitOp(proc, IL_ldproc, findProc(e->d));
            break;
        case Tok_LDMETH:
            if( !translateProc(e->d) )
                return false;
            emitOp(proc, IL_ldmeth, e->d->off);
            break;
        case Tok_CONV_I1:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_i1_i4);
            else if( lhsT->isInt64())
                emitOp(proc, IL_conv_i1_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i1_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i1_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_I2:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_i2_i4);
            else if( lhsT->isInt64())
                emitOp(proc, IL_conv_i2_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i2_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i2_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_I4:
            if( lhsT->isInt64())
                emitOp(proc, IL_conv_i4_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i4_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i4_r8);
            else if( !lhsT->isInt32OnStack() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_I8:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_i8_i4);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_i8_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_i8_r8);
            else if( !lhsT->isInt64() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_R4:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_r4_i4);
            else if(lhsT->isInt64() )
                emitOp(proc, IL_conv_r4_i8);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_r4_r8);
            else if( !lhsT->isFloat() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_R8:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_r8_i4);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_r8_r4);
            else if( lhsT->isInt64() )
                emitOp(proc, IL_conv_r8_i8);
            else if( !lhsT->isFloat() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_U1:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_u1_i4);
            else if( lhsT->isInt64())
                emitOp(proc, IL_conv_u1_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u1_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u1_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_U2:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_u2_i4);
            else if( lhsT->isInt64())
                emitOp(proc, IL_conv_u2_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u2_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u2_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CONV_U4:
            if( lhsT->isInt64() )
                emitOp(proc, IL_conv_u4_i8);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u4_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u4_r8);
            else if( !lhsT->isInt32OnStack() )
                Q_ASSERT(false);
            break;
        case Tok_CONV_U8:
            if( lhsT->isInt32OnStack() )
                emitOp(proc, IL_conv_u8_i4);
            else if(lhsT->kind == Type::FLOAT32)
                emitOp(proc, IL_conv_u8_r4);
            else if(lhsT->kind == Type::FLOAT64)
                emitOp(proc, IL_conv_u8_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CEQ:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_ceq_i4);
            else if( t->isInt64())
                emitOp(proc, IL_ceq_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_ceq_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_ceq_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CGT:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_cgt_i4);
            else if( t->isInt64())
                emitOp(proc, IL_cgt_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_cgt_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_cgt_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CGT_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_cgt_u4);
            else if( t->isInt64())
                emitOp(proc, IL_cgt_u8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CLT:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_clt_i4);
            else if( t->isInt64())
                emitOp(proc, IL_clt_i8);
            else if(t->kind == Type::FLOAT32)
                emitOp(proc, IL_clt_r4);
            else if(t->kind == Type::FLOAT64)
                emitOp(proc, IL_clt_r8);
            else
                Q_ASSERT(false);
            break;
        case Tok_CLT_UN:
            if( t->isInt32OnStack() )
                emitOp(proc, IL_clt_u4);
            else if( t->isInt64())
                emitOp(proc, IL_clt_u8);
            else
                Q_ASSERT(false);
            break;
        case Tok_LDARG_0:
        case Tok_LDARG_1:
        case Tok_LDARG_2:
        case Tok_LDARG_3:
        case Tok_LDARG_S:
        case Tok_LDARG:
            {
                DeclList params = proc.decl->getParams();
                Q_ASSERT(e->id < params.size());
                Type* t = deref(params[e->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                    emitOp(proc, IL_ldarg_i1,params[e->id]->off);
                    break;
                case Type::INT16:
                    emitOp(proc, IL_ldarg_i2,params[e->id]->off);
                    break;
                case Type::INT32:
                    emitOp(proc, IL_ldarg_i4,params[e->id]->off);
                    break;
                case Type::INT64:
                    emitOp(proc, IL_ldarg_i8,params[e->id]->off);
                    break;
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_ldarg_u1,params[e->id]->off);
                    break;
                case Type::UINT16:
                    emitOp(proc, IL_ldarg_u2,params[e->id]->off);
                    break;
                case Type::UINT32:
                    emitOp(proc, IL_ldarg_u4,params[e->id]->off);
                    break;
                case Type::UINT64:
                    emitOp(proc, IL_ldarg_u8,params[e->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_ldarg_r4,params[e->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_ldarg_r8,params[e->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_ldarg_pp,params[e->id]->off);
                    else
                        emitOp(proc, IL_ldarg_p,params[e->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_ldarg_vt,params[e->id]->off);
                    emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_LDARGA_S:
        case Tok_LDARGA:
            {
                Q_ASSERT(curProc);
                DeclList params = curProc->decl->getParams();
                Q_ASSERT(e->id < params.size());
                emitOp(proc, IL_ldarga,params[e->id]->off);
            }
            break;
        case Tok_LDLOC_0:
        case Tok_LDLOC_1:
        case Tok_LDLOC_2:
        case Tok_LDLOC_3:
        case Tok_LDLOC_S:
        case Tok_LDLOC:
            {
                DeclList params = proc.decl->getLocals();
                Q_ASSERT(e->id < params.size());
                Type* t = deref(params[e->id]->getType());
                switch(t->kind)
                {
                case Type::INT8:
                    emitOp(proc, IL_ldloc_i1,params[e->id]->off);
                    break;
                case Type::INT16:
                    emitOp(proc, IL_ldloc_i2,params[e->id]->off);
                    break;
                case Type::INT32:
                    emitOp(proc, IL_ldloc_i4,params[e->id]->off);
                    break;
                case Type::INT64:
                    emitOp(proc, IL_ldloc_i8,params[e->id]->off);
                    break;
                case Type::UINT8:
                case Type::BOOL:
                case Type::CHAR:
                    emitOp(proc, IL_ldloc_u1,params[e->id]->off);
                    break;
                case Type::UINT16:
                    emitOp(proc, IL_ldloc_u2,params[e->id]->off);
                    break;
                case Type::UINT32:
                    emitOp(proc, IL_ldloc_u4,params[e->id]->off);
                    break;
                case Type::UINT64:
                    emitOp(proc, IL_ldloc_u8,params[e->id]->off);
                    break;
                case Type::FLOAT32:
                    emitOp(proc, IL_ldloc_r4,params[e->id]->off);
                    break;
                case Type::FLOAT64:
                    emitOp(proc, IL_ldloc_r8,params[e->id]->off);
                    break;
                case Type::Pointer:
                case Type::Proc:
                    if(t->typebound)
                        emitOp(proc, IL_ldloc_pp,params[e->id]->off);
                    else
                        emitOp(proc, IL_ldloc_p,params[e->id]->off);
                    break;
                case Type::Struct:
                case Type::Union:
                case Type::Object:
                case Type::Array:
                    emitOp(proc, IL_ldloc_vt,params[e->id]->off);
                    emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }
            break;
        case Tok_LDLOCA_S:
        case Tok_LDLOCA:
            {
                DeclList locals = proc.decl->getLocals();
                Q_ASSERT(e->id < locals.size());
                emitOp(proc, IL_ldloca,locals[e->id]->off);
            }
            break;
        case Tok_LDIND_I1:
            emitOp(proc, IL_ldind_i1);
            break;
        case Tok_LDIND_I2:
            emitOp(proc, IL_ldind_i2);
            break;
        case Tok_LDIND_I4:
            emitOp(proc, IL_ldind_i4);
            break;
        case Tok_LDIND_I8:
            emitOp(proc, IL_ldind_i8);
            break;
        case Tok_LDIND_IP:
            emitOp(proc, IL_ldind_p);
            break;
        case Tok_LDIND_IPP:
            emitOp(proc, IL_ldind_vt,sizeof(MethRef));
            break;
        case Tok_LDIND_R4:
            emitOp(proc, IL_ldind_r4);
            break;
        case Tok_LDIND_R8:
            emitOp(proc, IL_ldind_r8);
            break;
        case Tok_LDIND_U1:
            emitOp(proc, IL_ldind_u1);
            break;
        case Tok_LDIND_U2:
            emitOp(proc, IL_ldind_u2);
            break;
        case Tok_LDIND_U4:
            emitOp(proc, IL_ldind_u2);
            break;
        case Tok_LDIND_U8:
            emitOp(proc, IL_ldind_u8);
            break;
        case Tok_LDIND:
            if( lhsT && (lhsT->kind == Type::StringLit || lhsT->isPtrToOpenCharArray()) )
                emitOp(proc, IL_ldind_str,t->getByteSize(pointerWidth));
            else
                emitOp(proc, IL_ldind_vt,t->getByteSize(pointerWidth));
            break;
        case Tok_LDELEM_I1:
            emitOp(proc, IL_ldelem_i1);
            break;
        case Tok_LDELEM_I2:
            emitOp(proc, IL_ldelem_i2);
            break;
        case Tok_LDELEM_I4:
            emitOp(proc, IL_ldelem_i4);
            break;
        case Tok_LDELEM_I8:
            emitOp(proc, IL_ldelem_i8);
            break;
        case Tok_LDELEM_IP:
            emitOp(proc, IL_ldelem_p);
            break;
        case Tok_LDELEM_R4:
            emitOp(proc, IL_ldelem_r4);
            break;
        case Tok_LDELEM_R8:
            emitOp(proc, IL_ldelem_r8);
            break;
        case Tok_LDELEM_U1:
            emitOp(proc, IL_ldelem_u1);
            break;
        case Tok_LDELEM_U2:
            emitOp(proc, IL_ldelem_u2);
            break;
        case Tok_LDELEM_U4:
            emitOp(proc, IL_ldelem_u4);
            break;
        case Tok_LDELEM_U8:
            emitOp(proc, IL_ldelem_u8);
            break;
        case Tok_LDELEM_IPP:
            emitOp(proc, IL_ldelem_vt,sizeof(MethRef));
            break;
        case Tok_LDELEM:
            emitOp(proc, IL_ldelem_vt,t->getByteSize(pointerWidth));
            break;
        case Tok_LDELEMA:
            emitOp(proc, IL_ldelema,t->getType()->getByteSize(pointerWidth)); // deref pointer for et
            break;
        case Tok_LDFLD:
            switch(t->kind)
            {
            case Type::INT8:
                emitOp(proc, IL_ldfld_i1,e->d->f.off);
                break;
            case Type::INT16:
                emitOp(proc, IL_ldfld_i2,e->d->f.off);
                break;
            case Type::INT32:
                emitOp(proc, IL_ldfld_i4,e->d->f.off);
                break;
            case Type::INT64:
                emitOp(proc, IL_ldfld_i8,e->d->f.off);
                break;
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_ldfld_u1,e->d->f.off);
                break;
            case Type::UINT16:
                emitOp(proc, IL_ldfld_u2,e->d->f.off);
                break;
            case Type::UINT32:
                emitOp(proc, IL_ldfld_u4,e->d->f.off);
                break;
            case Type::UINT64:
                emitOp(proc, IL_ldfld_u8,e->d->f.off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_ldfld_r4,e->d->f.off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_ldfld_r8,e->d->f.off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(t->typebound)
                    emitOp(proc, IL_ldfld_pp,e->d->f.off);
                else
                    emitOp(proc, IL_ldfld_p,e->d->f.off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_ldfld_vt,e->d->f.off);
                emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_LDFLDA:
            emitOp(proc, IL_ldflda, e->d->f.off);
            break;
        case Tok_LDVAR:
            switch(t->kind)
            {
            case Type::INT8:
                emitOp(proc, IL_ldvar_i1,e->d->off);
                break;
            case Type::INT16:
                emitOp(proc, IL_ldvar_i2,e->d->off);
                break;
            case Type::INT32:
                emitOp(proc, IL_ldvar_i4,e->d->off);
                break;
            case Type::INT64:
                emitOp(proc, IL_ldvar_i8,e->d->off);
                break;
            case Type::UINT8:
            case Type::BOOL:
            case Type::CHAR:
                emitOp(proc, IL_ldvar_u1,e->d->off);
                break;
            case Type::UINT16:
                emitOp(proc, IL_ldvar_u2,e->d->off);
                break;
            case Type::UINT32:
                emitOp(proc, IL_ldvar_u4,e->d->off);
                break;
            case Type::UINT64:
                emitOp(proc, IL_ldvar_u8,e->d->off);
                break;
            case Type::FLOAT32:
                emitOp(proc, IL_ldvar_r4,e->d->off);
                break;
            case Type::FLOAT64:
                emitOp(proc, IL_ldvar_r8,e->d->off);
                break;
            case Type::Pointer:
            case Type::Proc:
                if(t->typebound)
                    emitOp(proc, IL_ldvar_pp,e->d->off);
                else
                    emitOp(proc, IL_ldvar_p,e->d->off);
                break;
            case Type::Struct:
            case Type::Union:
            case Type::Object:
            case Type::Array:
                emitOp(proc, IL_ldvar_vt,e->d->off);
                emitOp(proc, IL_vt_size,t->getByteSize(pointerWidth));
                break;
            default:
                Q_ASSERT(false);
                break;
            }
            break;
        case Tok_LDVARA:
            emitOp(proc, IL_ldvara, e->d->off);
            break;
        case Tok_NEWOBJ:
        case Tok_NEWARR:
        case Tok_INITOBJ: {
                Type* tt = deref(e->d->getType());
                const int len = tt->getByteSize(pointerWidth);
                const IL_op op = e->kind == Tok_NEWOBJ ? IL_alloc1 :
                                 e->kind == Tok_NEWARR ?
                                                       IL_allocN : // N is on stack
                                                             IL_initobj;
                if( tt->objectInit || tt->pointerInit )
                {
                    int id = findTemplate(tt);
                    if( id == -1 )
                    {
                        id = templates.size();
                        templates.push_back(Template());
                        Template& temp = templates.back();
                        temp.type = tt;
                        temp.mem.resize(len);
                        initMemory(temp.mem.data(), tt, true);
                    }
                    emitOp(proc,op, id, true);
                }else
                    emitOp(proc,op, len);
            }
            break;
        case Tok_NOP:
        case Tok_CASTPTR:
            break; // NOP
        case Tok_DUP:
            emitOp(proc,IL_dup, e->getType()->getByteSize(pointerWidth));
            break;
        case Tok_CALL:
        case Tok_CALLVIRT:
            {
                if( !translateProc(e->d) )
                    return false;
                const int id =  findProc(e->d);
                if( id < 0 )
                {
                    qCritical() << "cannot find implementation of" << e->d->toPath();
                    return false;
                }
                if( e->kind == Tok_CALL )
                    emitOp(proc, IL_call, id);
                else
                    emitOp(proc, IL_callvirt, id);
            }
            break;
        case Tok_CALLI:
            emitOp(proc, IL_calli);
            break;
        case Tok_CALLVI:
            emitOp(proc, IL_callvi);
            break;
        case Tok_IIF:
            {
                if( !translateExprSeq(proc, e->e) )
                    return false;
                const int iif = deref(e->lhs->getType())->isInt64() ?
                            emitOp(proc,IL_brfalse_i8) : emitOp(proc,IL_brfalse_i4);
                if( !translateExprSeq(proc, e->next->e) )
                    return false;
                const int end_of_then = emitOp(proc, IL_br);
                branch_here(proc,iif);
                if( !translateExprSeq(proc, e->next->next->e) )
                    return false;
                branch_here(proc,end_of_then);
                e = e->next->next;
            }
            break;
        case Tok_ISINST: {
                const int id = findVtable(t);
                if( id < 0 )
                {
                    qCritical() << "cannot find vtable of" << t->decl->toPath();
                    return false;
                }
                emitOp(proc, IL_isinst, id);
            }
            break;
        case Tok_SIZEOF:
        case Tok_PTROFF:
        case Tok_NEWVLA:
            qCritical() << "ERROR: not yet implemented in interpreter:" << tokenTypeString(e->kind);
            return false;
        default:
            Q_ASSERT(false);
        }
        e = e->next;
    }
    return true;
}

bool Interpreter::Imp::run(quint32 proc)
{
    quint8 locals[PreAllocSize];
    quint8 stack[PreAllocSize];
    return call(0, 0, procs[proc], locals, stack);
}

#define VM_BINARY_OP(type, op) \
    *(type*)frame->slot(-2) = *(type*)frame->slot(-2) op *(type*)frame->slot(-1); \
    frame->pop(Frame::stackAlig); pc++;

#define VM_UNARY_OP(type, op) \
    *(type*)frame->slot(-1) = op *(type*)frame->slot(-1); \
    pc++;

#define VM_ABS_OP(type) \
    *(type*)frame->slot(-1) = qAbs(*(type*)frame->slot(-1)); \
    pc++;

#define VM_CONV_OP(restype, totype,fromtype) \
    *(restype*)frame->slot(-1) = (restype)(totype)(*(fromtype*)frame->slot(-1)); \
    pc++;

#define VM_LOCAL_ADDR (frame->locals.data()+frame->proc->ops[pc].val)

#define VM_VAR_ADDR (moduleData.data()+frame->proc->ops[pc].val)

#define VM_ARG_ADDR (frame->outer->stack.data()+frame->outer->sp-frame->proc->fixArgSize+frame->proc->ops[pc].val)

#define VM_LDELEM(etype, totype) { const quint32 i = frame->popI4(); etype* a = (etype*)frame->popP(); \
    frame->push##totype(a[i]); pc++; }

#define VM_STELEM(etype, fromtype) { etype v = frame->pop##fromtype(); \
    const quint32 i = frame->popI4(); etype* a = (etype*)frame->popP(); \
    a[i] = v; pc++; }

#define VM_LDFLD(ftype, totype) { quint8* obj = (quint8*)frame->popP(); \
    frame->push##totype( *(ftype*)(obj + frame->proc->ops[pc].val) ); pc++; }

#define VM_STFLD(ftype, fromtype) { ftype v = frame->pop##fromtype(); \
    quint8* obj = (quint8*)frame->popP(); \
    *(ftype*)(obj + frame->proc->ops[pc].val) = v; pc++; }

#define VM_STIND(totype, fromtype) { totype v = (totype)frame->pop##fromtype(); \
    totype* p = (totype*)frame->popP(); *p = v; pc++; }

#ifdef _CHECK_HEAP_ADDRESSES
static QSet<void*> dynamics;
#endif

bool Interpreter::Imp::execute(Frame* frame)
{
#ifdef _USE_JUMP_TABLE
#define vmdispatch(x)     goto *disptab[x];
#define vmcase(l)     L_IL_##l:
#define vmbreak		 if( pc >= frame->proc->ops.size() ) return true; vmdispatch(frame->proc->ops[pc].op);

    static const void *const disptab[IL_NUM_OF_OPS] = {
        &&L_IL_invalid,
        #define OPDEF(op, x) &&L_IL_##op
        #include "MilVmOps.h"
        #undef OPDEF
    };

#else
#define vmdispatch(o)	switch(o)
#define vmcase(l)	case IL_##l:
#define vmbreak		break
#endif

    quint8 locals[PreAllocSize];
    quint8 stack[PreAllocSize];

#ifdef _TRACE_CALLS_
    if( frame->proc->decl->kind == Declaration::Module )
        qDebug() << "exec" << frame->proc->decl->name << "(synthetic)";
    else
        qDebug() << "exec" << frame->proc->decl->toPath();
#endif

    int pc = 0;
    while( pc < frame->proc->ops.size() )
    {
        vmdispatch(frame->proc->ops[pc].op)
        {
        vmcase(invalid)
            pc++;
            vmbreak;
        vmcase(add_i4)
                VM_BINARY_OP(qint32,+)
            vmbreak;
        vmcase(add_i8)
                VM_BINARY_OP(qint64,+)
            vmbreak;
        vmcase(add_r4)
                VM_BINARY_OP(float,+)
            vmbreak;
        vmcase(add_r8)
                VM_BINARY_OP(double,+)
            vmbreak;
        vmcase(sub_i4)
                VM_BINARY_OP(qint32,-)
            vmbreak;
        vmcase(sub_i8)
                VM_BINARY_OP(qint64,-)
            vmbreak;
        vmcase(sub_r4)
                VM_BINARY_OP(float,-)
            vmbreak;
        vmcase(sub_r8)
                VM_BINARY_OP(double,-)
            vmbreak;
        vmcase(mul_i4)
                VM_BINARY_OP(qint32,*)
            vmbreak;
        vmcase(mul_i8)
                VM_BINARY_OP(qint64,*)
            vmbreak;
        vmcase(mul_r4)
                VM_BINARY_OP(float,*)
            vmbreak;
        vmcase(mul_r8)
                VM_BINARY_OP(double,*)
            vmbreak;
        vmcase(div_i4)
                VM_BINARY_OP(qint32,/)
            vmbreak;
        vmcase(div_i8)
                VM_BINARY_OP(qint64,/)
            vmbreak;
        vmcase(div_r4)
                VM_BINARY_OP(float,/)
            vmbreak;
        vmcase(div_r8)
                VM_BINARY_OP(double,/)
            vmbreak;
        vmcase(div_un_i4)
                VM_BINARY_OP(quint32,/)
            vmbreak;
        vmcase(div_un_i8)
                VM_BINARY_OP(quint64,/)
            vmbreak;
        vmcase(rem_i4)
                VM_BINARY_OP(qint32,%)
            vmbreak;
        vmcase(rem_i8)
                VM_BINARY_OP(qint64,%)
            vmbreak;
        vmcase(rem_un_i4)
                VM_BINARY_OP(quint32,%)
            vmbreak;
        vmcase(rem_un_i8)
                VM_BINARY_OP(quint64,%)
            vmbreak;
        vmcase(abs_i4)
                VM_ABS_OP(qint32)
            vmbreak;
        vmcase(abs_i8)
                VM_ABS_OP(qint64)
            vmbreak;
        vmcase(abs_r4)
                VM_ABS_OP(float)
            vmbreak;
        vmcase(abs_r8)
                VM_ABS_OP(double)
            vmbreak;
        vmcase(neg_i4)
                VM_UNARY_OP(qint32,-)
            vmbreak;
        vmcase(neg_i8)
                VM_UNARY_OP(qint64,-)
            vmbreak;
        vmcase(neg_r4)
                VM_UNARY_OP(float,-)
            vmbreak;
        vmcase(neg_r8)
                VM_UNARY_OP(double,-)
            vmbreak;
        vmcase(and_i4)
                VM_BINARY_OP(qint32,&)
            vmbreak;
        vmcase(and_i8)
                VM_BINARY_OP(qint64,&)
            vmbreak;
        vmcase(or_i4)
                VM_BINARY_OP(qint32,|)
            vmbreak;
        vmcase(or_i8)
                VM_BINARY_OP(qint64,|)
            vmbreak;
        vmcase(xor_i4)
                VM_BINARY_OP(qint32,^)
            vmbreak;
        vmcase(xor_i8)
                VM_BINARY_OP(qint64,^)
            vmbreak;
        vmcase(not_i4)
                VM_UNARY_OP(qint32,~)
            vmbreak;
        vmcase(not_i8)
                VM_UNARY_OP(qint64,~)
            vmbreak;
        vmcase(shl_i4)
                VM_BINARY_OP(qint32,<<)
            vmbreak;
        vmcase(shl_i8)
                VM_BINARY_OP(qint64,<<)
            vmbreak;
        vmcase(shr_i4)
                VM_BINARY_OP(qint32,>>)
            vmbreak;
        vmcase(shr_i8)
                VM_BINARY_OP(qint64,>>)
            vmbreak;
        vmcase(shr_un_i4)
                VM_BINARY_OP(quint32,>>)
            vmbreak;
        vmcase(shr_un_i8)
                VM_BINARY_OP(quint64,>>)
            vmbreak;
        vmcase(ceq_i4)
                VM_BINARY_OP(qint32,==)
            vmbreak;
        vmcase(ceq_i8)
                VM_BINARY_OP(qint64,==)
            vmbreak;
        vmcase(ceq_r4)
                VM_BINARY_OP(float,==)
            vmbreak;
        vmcase(ceq_r8)
                VM_BINARY_OP(double,==)
            vmbreak;
        vmcase(cgt_i4)
                VM_BINARY_OP(qint32,>)
            vmbreak;
        vmcase(cgt_i8)
                VM_BINARY_OP(qint64,>)
            vmbreak;
        vmcase(cgt_r4)
                VM_BINARY_OP(float,>)
            vmbreak;
        vmcase(cgt_r8)
                VM_BINARY_OP(double,>)
            vmbreak;
        vmcase(cgt_u4)
                VM_BINARY_OP(quint32,>)
            vmbreak;
        vmcase(cgt_u8)
                VM_BINARY_OP(quint64,>)
            vmbreak;
        vmcase(clt_i4)
                VM_BINARY_OP(qint32,<)
            vmbreak;
        vmcase(clt_i8)
                VM_BINARY_OP(qint64,<)
            vmbreak;
        vmcase(clt_r4)
                VM_BINARY_OP(float,<)
            vmbreak;
        vmcase(clt_r8)
                VM_BINARY_OP(double,<)
            vmbreak;
        vmcase(clt_u4)
                VM_BINARY_OP(quint32,<)
            vmbreak;
        vmcase(clt_u8)
                VM_BINARY_OP(quint64,<)
            vmbreak;
        vmcase(conv_i1_i4)
                VM_CONV_OP(qint32, qint8, qint32)
            vmbreak;
        vmcase(conv_i1_i8)
                VM_CONV_OP(qint32, qint8, qint64)
            vmbreak;
        vmcase(conv_i1_r4)
                VM_CONV_OP(qint32, qint8, float)
            vmbreak;
        vmcase(conv_i1_r8)
                VM_CONV_OP(qint32, qint8, double)
            vmbreak;
        vmcase(conv_i2_i4)
                VM_CONV_OP(qint32, qint16, qint32)
            vmbreak;
        vmcase(conv_i2_i8)
                VM_CONV_OP(qint32, qint16, qint64)
            vmbreak;
        vmcase(conv_i2_r4)
                VM_CONV_OP(qint32, qint16, float)
            vmbreak;
        vmcase(conv_i2_r8)
                VM_CONV_OP(qint32, qint16, double)
            vmbreak;
        vmcase(conv_i4_i8)
                VM_CONV_OP(qint32, qint32, qint64)
            vmbreak;
        vmcase(conv_i4_r4)
                VM_CONV_OP(qint32, qint32, float)
            vmbreak;
        vmcase(conv_i4_r8)
                VM_CONV_OP(qint32, qint32, double)
            vmbreak;
        vmcase(conv_i8_i4)
                VM_CONV_OP(qint64, qint64, qint32)
            vmbreak;
        vmcase(conv_i8_r4)
                VM_CONV_OP(qint64, qint64, float)
            vmbreak;
        vmcase(conv_i8_r8)
                VM_CONV_OP(qint64, qint64, double)
            vmbreak;
        vmcase(conv_u1_i4)
                VM_CONV_OP(quint32, quint8, qint32)
            vmbreak;
        vmcase(conv_u1_i8)
                VM_CONV_OP(quint32, quint8, qint64)
            vmbreak;
        vmcase(conv_u1_r4)
                VM_CONV_OP(quint32, quint8, float)
            vmbreak;
        vmcase(conv_u1_r8)
                VM_CONV_OP(quint32, quint8, double)
            vmbreak;
        vmcase(conv_u2_i4)
                VM_CONV_OP(quint32, quint16, qint32)
            vmbreak;
        vmcase(conv_u2_i8)
                VM_CONV_OP(quint32, quint16, qint64)
            vmbreak;
        vmcase(conv_u2_r4)
                VM_CONV_OP(quint32, quint16, float)
            vmbreak;
        vmcase(conv_u2_r8)
                VM_CONV_OP(quint32, quint16, double)
            vmbreak;
        vmcase(conv_u4_i8)
                VM_CONV_OP(quint32, quint32, qint64)
            vmbreak;
        vmcase(conv_u4_r4)
                VM_CONV_OP(quint32, quint32, float)
            vmbreak;
        vmcase(conv_u4_r8)
                VM_CONV_OP(quint32, quint32, double)
            vmbreak;
        vmcase(conv_u8_i4)
                VM_CONV_OP(quint64, quint64, qint32)
            vmbreak;
        vmcase(conv_u8_r4)
                VM_CONV_OP(quint64, quint64, float)
            vmbreak;
        vmcase(conv_u8_r8)
                VM_CONV_OP(quint64, quint64, double)
            vmbreak;
        vmcase(conv_r4_i4)
                VM_CONV_OP(float, float, qint32)
            vmbreak;
        vmcase(conv_r4_i8)
                VM_CONV_OP(float, float, qint64)
            vmbreak;
        vmcase(conv_r4_r8)
                VM_CONV_OP(float, float, double)
            vmbreak;
        vmcase(conv_r8_i4)
                VM_CONV_OP(double, double, qint32)
            vmbreak;
        vmcase(conv_r8_i8)
                VM_CONV_OP(double, double, qint64)
            vmbreak;
        vmcase(conv_r8_r4)
                VM_CONV_OP(double, double, float)
            vmbreak;
        vmcase(ldarg_i1)
                frame->pushI4(*(qint8*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_i2)
                frame->pushI4(*(qint16*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_i4)
                frame->pushI4(*(qint32*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_i8)
                frame->pushI8(*(qint64*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_u1)
                frame->pushI4(*(quint8*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_u2)
                frame->pushI4(*(quint16*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_u4)
                frame->pushI4(*(quint32*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_u8)
                frame->pushI8(*(quint64*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_r4)
                frame->pushR4(*(float*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_r8)
                frame->pushR8(*(double*)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_p)
                frame->pushP(*(void**)VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(ldarg_pp)
                frame->push(VM_ARG_ADDR, stackAligned(sizeof(MethRef)));
                pc++;
                vmbreak;
        vmcase(ldarg_vt)
                frame->push(VM_ARG_ADDR, stackAligned(frame->proc->ops[pc+1].val));
                pc += 2;
            vmbreak;
        vmcase(ldarga)
                frame->pushP(VM_ARG_ADDR);
                pc++;
            vmbreak;
        vmcase(starg_i1)
                *(qint8*)VM_ARG_ADDR = (qint8)frame->popI4();
                pc++;
            vmbreak;
        vmcase(starg_i2)
                *(qint16*)VM_ARG_ADDR = (qint16)frame->popI4();
                pc++;
            vmbreak;
        vmcase(starg_i4)
                *(qint32*)VM_ARG_ADDR = frame->popI4();
                pc++;
            vmbreak;
        vmcase(starg_i8)
                *(qint64*)VM_ARG_ADDR = frame->popI8();
                pc++;
            vmbreak;
        vmcase(starg_r4)
                *(float*)VM_ARG_ADDR = frame->popR4();
                pc++;
            vmbreak;
        vmcase(starg_r8)
                *(double*)VM_ARG_ADDR = frame->popR8();
                pc++;
            vmbreak;
        vmcase(starg_p)
                *(void**)VM_ARG_ADDR = frame->popP();
                pc++;
            vmbreak;
        vmcase(starg_pp)
                frame->copy(VM_ARG_ADDR, // to
                            stackAligned(sizeof(MethRef)), // size on stack
                            sizeof(MethRef)); // true size
                pc++;
                vmbreak;
        vmcase(starg_vt)
                frame->copy(VM_ARG_ADDR, // to
                            stackAligned(frame->proc->ops[pc+1].val), // size on stack
                            frame->proc->ops[pc+1].val); // true size
                pc += 2;
            vmbreak;
        vmcase(ldelem_i1)
                VM_LDELEM(qint8,I4)
            vmbreak;
        vmcase(ldelem_i2)
                VM_LDELEM(qint16,I4)
            vmbreak;
        vmcase(ldelem_i4)
                VM_LDELEM(qint32,I4)
            vmbreak;
        vmcase(ldelem_i8)
                VM_LDELEM(qint64,I8)
            vmbreak;
        vmcase(ldelem_u1)
                VM_LDELEM(quint8,I4)
            vmbreak;
        vmcase(ldelem_u2)
                VM_LDELEM(quint16,I4)
            vmbreak;
        vmcase(ldelem_u4)
                VM_LDELEM(quint32,I4)
            vmbreak;
        vmcase(ldelem_u8)
                VM_LDELEM(quint64,I8)
            vmbreak;
        vmcase(ldelem_r4)
                VM_LDELEM(float,R4)
            vmbreak;
        vmcase(ldelem_r8)
                VM_LDELEM(double,R8)
            vmbreak;
        vmcase(ldelem_p)
                VM_LDELEM(void*,P)
            vmbreak;
        vmcase(ldelema) {
                const quint32 i = frame->popI4(); quint8* a = (quint8*)frame->popP();
                frame->pushP(a + i * frame->proc->ops[pc].val);
                pc++;
            } vmbreak;
        vmcase(ldelem_vt) {
                const quint32 i = frame->popI4(); quint8* a = (quint8*)frame->popP();
                frame->push(a + i * frame->proc->ops[pc].val, stackAligned(frame->proc->ops[pc].val));
                pc++;
            } vmbreak;
        vmcase(stelem_i1)
                VM_STELEM(qint8, I4)
            vmbreak;
        vmcase(stelem_i2)
                VM_STELEM(qint16, I4)
            vmbreak;
        vmcase(stelem_i4)
                VM_STELEM(qint32, I4)
            vmbreak;
        vmcase(stelem_i8)
                VM_STELEM(qint64, I8)
            vmbreak;
        vmcase(stelem_r4)
                VM_STELEM(float, R4)
            vmbreak;
        vmcase(stelem_r8)
                VM_STELEM(double, R8)
            vmbreak;
        vmcase(stelem_p)
                VM_STELEM(void*, P)
            vmbreak;
        vmcase(stelem_vt) {
                const int lenonstack = stackAligned(frame->proc->ops[pc].val);
                const int etlen = frame->proc->ops[pc].val;
                quint8* v = (quint8*)(frame->stack.data() + frame->sp - lenonstack);
                const quint32 i = *(quint32*)(frame->stack.data() + frame->sp - lenonstack - Frame::stackAlig);
                quint8* a = *(quint8**)(frame->stack.data() + frame->sp - lenonstack - Frame::stackAlig - Frame::stackAlig);
                memcpy(a + i * etlen, v, etlen);
                frame->pop(lenonstack + Frame::stackAlig + Frame::stackAlig);
                pc++;
            } vmbreak;
        vmcase(ldfld_i1)
                VM_LDFLD(qint8, I4)
            vmbreak;
        vmcase(ldfld_i2)
                VM_LDFLD(qint16, I4)
            vmbreak;
        vmcase(ldfld_i4)
                VM_LDFLD(qint32, I4)
            vmbreak;
        vmcase(ldfld_i8)
                VM_LDFLD(qint64, I8)
            vmbreak;
        vmcase(ldfld_u1)
                VM_LDFLD(quint8, I4)
            vmbreak;
        vmcase(ldfld_u2)
                VM_LDFLD(quint16, I4)
            vmbreak;
        vmcase(ldfld_u4)
                VM_LDFLD(quint32, I4)
            vmbreak;
        vmcase(ldfld_u8)
                VM_LDFLD(quint64, I8)
            vmbreak;
        vmcase(ldfld_r4)
                VM_LDFLD(float, R4)
            vmbreak;
        vmcase(ldfld_r8)
                VM_LDFLD(double, R8)
            vmbreak;
        vmcase(ldfld_p)
                VM_LDFLD(void*, P)
            vmbreak;
        vmcase(ldfld_pp)
                frame->push( frame->popP() + frame->proc->ops[pc].val, stackAligned(sizeof(MethRef)) );
                pc++;
                vmbreak;
        vmcase(ldfld_vt) {
                quint8* obj = (quint8*)frame->popP();
                frame->push( obj + frame->proc->ops[pc].val, stackAligned(frame->proc->ops[pc+1].val) );
                pc += 2;
            } vmbreak;
        vmcase(ldflda) {
                quint8* obj = (quint8*)frame->popP();
                frame->pushP( obj + frame->proc->ops[pc].val ); pc++;
            } vmbreak;
        vmcase(stfld_i1)
                VM_STFLD(qint8, I4)
            vmbreak;
        vmcase(stfld_i2)
                VM_STFLD(qint16, I4)
            vmbreak;
        vmcase(stfld_i4)
                VM_STFLD(qint32, I4)
            vmbreak;
        vmcase(stfld_i8)
                VM_STFLD(qint64, I8)
            vmbreak;
        vmcase(stfld_r4)
                VM_STFLD(float, R4)
            vmbreak;
        vmcase(stfld_r8)
                VM_STFLD(double, R8)
            vmbreak;
        vmcase(stfld_p)
                VM_STFLD(void*, P)
            vmbreak;
        vmcase(stfld_pp){
            const int lenonstack = stackAligned(sizeof(MethRef));
            const int flen = sizeof(MethRef);
            quint8* v = (quint8*)(frame->stack.data() + frame->sp - lenonstack);
            quint8* obj = *(quint8**)(frame->stack.data() + frame->sp - lenonstack - Frame::stackAlig);
            memcpy(obj + frame->proc->ops[pc].val, v, flen);
            frame->pop(lenonstack + Frame::stackAlig);
            pc++;
        } vmbreak;
        vmcase(stfld_vt) {
                const int lenonstack = stackAligned(frame->proc->ops[pc+1].val);
                const int flen = frame->proc->ops[pc+1].val;
                quint8* v = (quint8*)(frame->stack.data() + frame->sp - lenonstack);
                quint8* obj = *(quint8**)(frame->stack.data() + frame->sp - lenonstack - Frame::stackAlig);
                memcpy(obj + frame->proc->ops[pc].val, v, flen);
                frame->pop(lenonstack + Frame::stackAlig);
                pc += 2;
            } vmbreak;
        vmcase(ldind_i1)
                frame->pushI4(*(qint8*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_i2)
                frame->pushI4(*(qint16*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_i4)
                frame->pushI4(*(qint32*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_i8)
                frame->pushI8(*(qint64*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_u1)
                frame->pushI4(*(quint8*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_u2)
                frame->pushI4(*(quint16*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_u4)
                frame->pushI4(*(quint32*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_u8)
                frame->pushI8(*(quint64*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_r4)
                frame->pushR4(*(float*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_r8)
                frame->pushR8(*(double*)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_p)
                frame->pushP(*(void**)frame->popP()); pc++;
            vmbreak;
        vmcase(ldind_vt){
                void* ptr = frame->popP();
                frame->push(ptr, stackAligned(frame->proc->ops[pc].val)); pc++;
            } vmbreak;
       vmcase(ldind_str) {
                const int n = stackAligned(frame->proc->ops[pc].val);
                const char* from = (const char*)frame->popP();
                char* to = (char*)frame->alloc(n);
                strncpy(to, from, n-1);
                to[n-1] = 0;
                pc++;
            } vmbreak;
        vmcase(stind_i1)
                VM_STIND(qint8, I4)
            vmbreak;
        vmcase(stind_i2)
                VM_STIND(qint16, I4)
            vmbreak;
        vmcase(stind_i4)
                VM_STIND(qint32, I4)
            vmbreak;
        vmcase(stind_i8)
                VM_STIND(qint64, I8)
            vmbreak;
        vmcase(stind_r4)
                VM_STIND(float, R4)
            vmbreak;
        vmcase(stind_r8)
                VM_STIND(double, R8)
            vmbreak;
        vmcase(stind_p)
                VM_STIND(void*, P)
            vmbreak;
        vmcase(stind_vt) {
                const int lenonstack = stackAligned(frame->proc->ops[pc].val);
                const int len = frame->proc->ops[pc].val;
                quint8* v = (quint8*)(frame->stack.data() + frame->sp - lenonstack);
                quint8* ptr = *(quint8**)(frame->stack.data() + frame->sp - lenonstack - Frame::stackAlig);
                memcpy(ptr, v, len);
                frame->pop(lenonstack + Frame::stackAlig);
                pc++;
            } vmbreak;
        vmcase(ldloc_i1)
                frame->pushI4(*(qint8*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_i2)
                frame->pushI4(*(qint16*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_i4)
                frame->pushI4(*(qint32*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_i8)
                frame->pushI8(*(qint64*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_u1)
                frame->pushI4(*(quint8*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_u2)
                frame->pushI4(*(quint16*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_u4)
                frame->pushI4(*(quint32*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_u8)
                frame->pushI8(*(quint64*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_r4)
                frame->pushR4(*(float*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_r8)
                frame->pushR8(*(double*)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_p)
                frame->pushP(*(void**)VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_pp)
                frame->push(VM_LOCAL_ADDR, stackAligned(sizeof(MethRef)));
                pc++;
                vmbreak;
        vmcase(ldloca)
                frame->pushP(VM_LOCAL_ADDR);
                pc++;
            vmbreak;
        vmcase(ldloc_vt)
                frame->push(VM_LOCAL_ADDR, stackAligned(frame->proc->ops[pc+1].val));
                pc += 2;
            vmbreak;
        vmcase(stloc_i1)
                *(qint8*)VM_LOCAL_ADDR = (qint8)frame->popI4();
                pc++;
            vmbreak;
        vmcase(stloc_i2)
                *(qint16*)VM_LOCAL_ADDR = (qint16)frame->popI4();
                pc++;
            vmbreak;
        vmcase(stloc_i4)
                *(qint32*)VM_LOCAL_ADDR = frame->popI4();
                pc++;
            vmbreak;
        vmcase(stloc_i8)
                *(qint64*)VM_LOCAL_ADDR = frame->popI8();
                pc++;
            vmbreak;
        vmcase(stloc_r4)
                *(float*)VM_LOCAL_ADDR = frame->popR4();
                pc++;
            vmbreak;
        vmcase(stloc_r8)
                *(double*)VM_LOCAL_ADDR = frame->popR8();
                pc++;
            vmbreak;
        vmcase(stloc_p)
                *(void**)VM_LOCAL_ADDR = frame->popP();
                pc++;
            vmbreak;
        vmcase(stloc_pp)
                frame->copy(VM_LOCAL_ADDR, // to
                            stackAligned(sizeof(MethRef)), // size on stack
                            sizeof(MethRef)); // true size
                pc++;
                vmbreak;
        vmcase(stloc_vt)
                frame->copy(VM_LOCAL_ADDR, // to
                            stackAligned(frame->proc->ops[pc+1].val), // size on stack
                            frame->proc->ops[pc+1].val); // true size
                pc += 2;
            vmbreak;
        vmcase(ldvar_i1)
                frame->pushI4(*(qint8*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_i2)
                frame->pushI4(*(qint16*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_i4)
                frame->pushI4(*(qint32*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_i8)
                frame->pushI8(*(qint64*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_u1)
                frame->pushI4(*(quint8*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_u2)
                frame->pushI4(*(quint16*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_u4)
                frame->pushI4(*(quint32*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_u8)
                frame->pushI8(*(quint64*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_r4)
                frame->pushR4(*(float*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_r8)
                frame->pushR8(*(double*)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_p)
                frame->pushP(*(void**)VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_pp)
                frame->push(VM_VAR_ADDR, stackAligned(sizeof(MethRef)));
                pc++;
                vmbreak;
        vmcase(ldvara)
                frame->pushP(VM_VAR_ADDR);
                pc++;
            vmbreak;
        vmcase(ldvar_vt)
                frame->push(VM_VAR_ADDR, stackAligned(frame->proc->ops[pc+1].val));
                pc += 2;
            vmbreak;
        vmcase(stvar_i1)
                *(qint8*)VM_VAR_ADDR = (qint8)frame->popI4();
                pc++;
            vmbreak;
        vmcase(stvar_i2)
                *(qint16*)VM_VAR_ADDR = (qint16)frame->popI4();
                pc++;
            vmbreak;
        vmcase(stvar_i4)
                *(qint32*)VM_VAR_ADDR = frame->popI4();
                pc++;
            vmbreak;
        vmcase(stvar_i8)
                *(qint64*)VM_VAR_ADDR = frame->popI8();
                pc++;
            vmbreak;
        vmcase(stvar_r4)
                *(float*)VM_VAR_ADDR = frame->popR4();
                pc++;
            vmbreak;
        vmcase(stvar_r8)
                *(double*)VM_VAR_ADDR = frame->popR8();
                pc++;
            vmbreak;
        vmcase(stvar_p)
                *(void**)VM_VAR_ADDR = frame->popP();
                pc++;
            vmbreak;
        vmcase(stvar_pp)
                frame->copy(VM_VAR_ADDR, // to
                            stackAligned(sizeof(MethRef)), // size on stack
                            sizeof(MethRef)); // true size
                pc++;
                vmbreak;
        vmcase(stvar_vt)
                frame->copy(VM_VAR_ADDR, // to
                            stackAligned(frame->proc->ops[pc+1].val), // size on stack
                            frame->proc->ops[pc+1].val); // true size
                pc += 2;
            vmbreak;
        vmcase(ldc_i4)
                frame->pushI4(ints[frame->proc->ops[pc].val]);
                pc++;
            vmbreak;
        vmcase(ldc_i8)
                frame->pushI8(ints[frame->proc->ops[pc].val]);
                pc++;
            vmbreak;
        vmcase(ldc_r4)
                frame->pushR4(doubles[frame->proc->ops[pc].val]);
                pc++;
            vmbreak;
        vmcase(ldc_r8)
                frame->pushR8(doubles[frame->proc->ops[pc].val]);
                pc++;
            vmbreak;
        vmcase(ldc_i4_m1)
                frame->pushI4(-1);
                pc++;
            vmbreak;
        vmcase(ldc_i4_0)
                frame->pushI4(0);
                pc++;
            vmbreak;
        vmcase(ldc_i4_1)
                frame->pushI4(1);
                pc++;
            vmbreak;
        vmcase(ldc_i4_2)
                frame->pushI4(2);
                pc++;
            vmbreak;
        vmcase(ldc_i4_3)
                frame->pushI4(3);
                pc++;
            vmbreak;
        vmcase(ldc_i4_4)
                frame->pushI4(4);
                pc++;
            vmbreak;
        vmcase(ldc_i4_5)
                frame->pushI4(5);
                pc++;
            vmbreak;
        vmcase(ldc_i4_6)
                frame->pushI4(6);
                pc++;
            vmbreak;
        vmcase(ldc_i4_7)
                frame->pushI4(7);
                pc++;
            vmbreak;
        vmcase(ldc_i4_8)
                frame->pushI4(8);
                pc++;
            vmbreak;
        vmcase(ldnull)
                frame->pushP(0);
                pc++;
            vmbreak;
        vmcase(ldstr)
                frame->pushP((void*)strings[frame->proc->ops[pc].val].c_str());
                pc++;
            vmbreak;
        vmcase(ldobj) {
                QByteArray& obj = objects[frame->proc->ops[pc].val];
                frame->push(obj.data(),obj.size());
                pc++;
            } vmbreak;
        vmcase(br)
                pc = pc + 1 + (frame->proc->ops[pc].minus ? -1 : 1 ) * frame->proc->ops[pc].val;
            vmbreak;
        vmcase(brtrue_i4)
                if( frame->popI4() )
                    pc = pc + 1 + (frame->proc->ops[pc].minus ? -1 : 1 ) * frame->proc->ops[pc].val;
                else
                    pc++;
             vmbreak;
        vmcase(brtrue_i8)
                if( frame->popI8() )
                    pc = pc + 1 + (frame->proc->ops[pc].minus ? -1 : 1 ) * frame->proc->ops[pc].val;
                else
                    pc++;
             vmbreak;
        vmcase(brfalse_i4)
                if( !frame->popI4() )
                    pc = pc + 1 + (frame->proc->ops[pc].minus ? -1 : 1 ) * frame->proc->ops[pc].val;
                else
                    pc++;
             vmbreak;
        vmcase(brfalse_i8)
                if( !frame->popI8() )
                    pc = pc + 1 + (frame->proc->ops[pc].minus ? -1 : 1 ) * frame->proc->ops[pc].val;
                else
                    pc++;
             vmbreak;
        vmcase(ldproc)
                frame->pushP( procs[frame->proc->ops[pc].val] );
                pc++;
             vmbreak;
        vmcase(ldmeth) {
                MethRef m;
                m.obj = frame->popP();
                Vtable* vt = *(Vtable**)m.obj;
                m.proc =vt->methods[frame->proc->ops[pc].val];
                frame->push(&m, stackAligned(sizeof(MethRef)));
                pc++;
            }vmbreak;
        vmcase(sizeof)
        vmcase(ptroff)
                qWarning() << "TODO not yet implemented" << op_names[frame->proc->ops[pc].op];
                pc++;
                vmbreak;
        vmcase(pop)
                frame->pop(stackAligned(frame->proc->ops[pc].val));
                pc++;
             vmbreak;
        vmcase(dup) {
                const int len = stackAligned(frame->proc->ops[pc].val);
                frame->push(frame->stack.data()+frame->sp-len, len);
                pc++;
             }vmbreak;
        vmcase(ret)
            return true; // NOTE: return value is in frame->stack at sp 0
        vmcase(ret_void)
            return true;
        vmcase(call){
                if( !call(frame, pc, procs[frame->proc->ops[pc].val], locals, stack ) )
                    return false;
                pc++;
            } vmbreak;
        vmcase(calli) {
                if( !call(frame, pc, (Procedure*)frame->popP(), locals, stack ) )
                    return false;
                pc++;
            } vmbreak;
        vmcase(callvirt) {
                // dispatch using the SELF pointer on stack; but we can only access the
                // SELF pointer after we have the size of the parameters
                Procedure* proc = procs[frame->proc->ops[pc].val];
                void* obj = *(void**)(frame->stack.data()+frame->sp-proc->fixArgSize);
                Vtable* vtbl = *(Vtable**)(obj);
                proc = vtbl->methods[proc->decl->off];
                if( !call(frame, pc, proc, locals, stack ) )
                    return false;
                pc++;
            } vmbreak;
        vmcase(callvi) {
                MethRef r = frame->popPP();
                // make room for SELF on stack and copy r.obj to the first argument slot
                frame->insert(-r.proc->fixArgSize + StackAlign, r.obj); // fixArgSize already includes SELF
                if( !call(frame, pc, r.proc, locals, stack ) )
                    return false;
                pc++;
            } vmbreak;
        vmcase(initobj) {
                void* ptr = frame->popP();
                if( frame->proc->ops[pc].minus )
                {
                    const Template& tt = templates[frame->proc->ops[pc].val];
                    memcpy(ptr, tt.mem.data(), tt.mem.size());
                }
                pc++;
            } vmbreak;
        vmcase(alloc1) {
                void* ptr;
                if( frame->proc->ops[pc].minus )
                {
                    const Template& tt = templates[frame->proc->ops[pc].val];
                    ptr = malloc(tt.mem.size());
                    memcpy(ptr, tt.mem.data(), tt.mem.size());
                }else
                    ptr = malloc(frame->proc->ops[pc].val);
#ifdef _CHECK_HEAP_ADDRESSES
                dynamics.insert(ptr);
#endif
                frame->pushP(ptr);
                pc++;
            } vmbreak;
        vmcase(allocN) {
            void* ptr;
            const quint32 len = (quint32)frame->popI4();
            if( frame->proc->ops[pc].minus )
            {
                const Template& tt = templates[frame->proc->ops[pc].val];
                ptr = malloc(tt.mem.size() * len);
                for(int i = 0; i < len; i++ )
                {
                    memcpy(ptr + i * tt.mem.size(), tt.mem.data(), tt.mem.size());
                }
            }else
                ptr = malloc(frame->proc->ops[pc].val * len);
#ifdef _CHECK_HEAP_ADDRESSES
                dynamics.insert(ptr);
#endif
                frame->pushP(ptr);
                pc++;
            } vmbreak;
        vmcase(free) {
                void* ptr = frame->popP();
#ifdef _CHECK_HEAP_ADDRESSES
                Q_ASSERT(dynamics.contains(ptr));
                dynamics.remove(ptr);
#endif
                free(ptr);
                pc++;
            } vmbreak;
        vmcase(strcpy) {
                const char* rhs = (const char*)frame->popP();
                char* lhs = (char*)frame->popP();
                strncpy(lhs, rhs, frame->proc->ops[pc].val-1);
                lhs[frame->proc->ops[pc].val-1] = 0;
                pc++;
            } vmbreak;
        vmcase(vt_size)
                Q_ASSERT(false); // instead consumed by other ops
            vmbreak;
        vmcase(isinst) {
                Vtable* obj = (Vtable*)frame->popP();
                Vtable* ref = vtables[frame->proc->ops[pc].val];
                frame->pushI4(Type::isA(obj->type, ref->type));
            } vmbreak;
        vmcase(newvla)
        vmcase(line)
                qWarning() << "TODO not yet implemented" << op_names[frame->proc->ops[pc].op];
                pc++;
            vmbreak;
        }
    }
    return true;
}

bool Interpreter::Imp::call(Frame* frame, int pc, Procedure* proc, void* local, void* stack)
{
    Frame newframe;
    newframe.proc = proc;
    newframe.outer = frame;
    bool res;
    if( proc->ffi )
    {
        newframe.stack.init(stack,PreAllocSize);
        newframe.stack.resize(newframe.proc->returnSize);
        newframe.sp = newframe.proc->returnSize;
        char* retval = 0;
        if( newframe.stack.size() != 0 )
            retval = newframe.stack.data();
        char* args = 0;
        if( frame && proc->fixArgSize )
            args = frame->stack.data() + frame->sp - proc->fixArgSize;
        res = proc->ffi(args, retval);
    }else
    {
        if( proc->init )
        {
            if( proc->called )
                return true; // already called
            proc->called = true;
        }
        newframe.locals.init(local,PreAllocSize);
        DeclList locals = proc->decl->getLocals();
        foreach( Declaration* d, locals )
        {
            Type* t = deref(d->getType());
            initMemory((char*)local+d->off, t,true);
        }

        newframe.stack.init(stack, PreAllocSize);
        res = execute(&newframe);
    }
    if( frame && newframe.proc->fixArgSize )
        frame->pop( newframe.proc->fixArgSize );
    if( frame && newframe.proc->returnSize > 0 )
    {
        if( stackAligned(newframe.proc->returnSize) != newframe.sp)
        {
            qCritical() << "wrong stack size at return of" << proc->decl->toPath();
            if( frame )
                qCritical() << "    called from" << frame->proc->decl->toPath() << "pc" << pc;
            return false;
        }
        frame->push(newframe.stack.data(), newframe.sp);
    }
    return res;
}
