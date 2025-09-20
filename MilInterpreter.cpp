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
#include "MicAtom.h"
#include "MilVmCode.h"
extern "C" {
#include "runtime/MIC+.h"
}
#include <QVector>
#include <QtDebug>
using namespace Mil;
using namespace Vm;

//#define _USE_JUMP_TABLE // instead of a big switch
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
    inline void push(const void* what, int len)
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

#define REGISTER_MICPROC(name) \
    ffiProcs.push_back(MIC_##name); code.addExternal(MIC$, Mic::Atom::getAtom(#name), ffiProcs.size()-1);

struct Interpreter::Imp
{
    Code code;
    AstModel* mdl;

    std::vector<char> moduleData;
    std::vector<FfiProc> ffiProcs;

    Imp(AstModel* mdl):mdl(mdl), code(mdl, sizeof(void*), Frame::stackAlig)
    {
#if 0
        QTextStream out(stdout);
        for(int i = 1; i < LL_NUM_OF_OPS; i++ )
            out << "vmcase(" << VmCode::op_names[i] << ")" << endl;
#endif
        const char* MIC$ = Mic::Atom::getAtom("MIC$").constData();
        REGISTER_MICPROC(relop1);
        REGISTER_MICPROC(relop2);
        REGISTER_MICPROC(relop3);
        REGISTER_MICPROC(relop4);
        REGISTER_MICPROC(SetDiv);
        REGISTER_MICPROC(SetIn);
        REGISTER_MICPROC(printI8);
        REGISTER_MICPROC(printU8);
        REGISTER_MICPROC(printF8);
        REGISTER_MICPROC(printStr);
        REGISTER_MICPROC(printCh);
        REGISTER_MICPROC(printBool);
        REGISTER_MICPROC(printSet);
        REGISTER_MICPROC(assert);
    }

    static inline int stackAligned(int off)
    {
        return AstModel::align(off, Frame::stackAlig );
    }

    bool run(Declaration* proc)
    {
        const int i = code.findProc(proc);
        if( i >= 0 )
            return run(i);
        // proc not found
        return false;
    }

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
    const quint32 id = imp->ffiProcs.size();
    imp->ffiProcs.push_back(proc);
    imp->code.addExternal(Mic::Atom::getAtom(module).constData(),
                          Mic::Atom::getAtom(procName).constData(), id);
}

bool Interpreter::precompile(Declaration* proc)
{
    Q_ASSERT(proc && (proc->kind == Declaration::Procedure || proc->kind == Declaration::Module));

    Declaration* module = proc->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    return imp->code.compile(module);
}

bool Interpreter::dumpProc(QTextStream& out, Declaration* proc)
{
    return imp->code.dumpProc(out, proc);
}

bool Interpreter::dumpModule(QTextStream& out, Declaration* module)
{
    return imp->code.dumpModule(out, module);
}

bool Interpreter::dumpAll(QTextStream& out)
{
    return imp->code.dumpAll(out);
}

bool Interpreter::run(Declaration* proc)
{
    Q_ASSERT(proc && (proc->kind == Declaration::Procedure || proc->kind == Declaration::Module));

    Declaration* module = proc->getModule();
    Q_ASSERT(module);
    if( !module->validated )
        return false;

    if( !module->translated && !precompile(module) )
        return false;

    // init module variables
    imp->moduleData.resize(imp->mdl->getVarMemSize());
    DeclList modules = imp->mdl->getModules();
    foreach(Declaration* module, modules)
    {
        DeclList vars = module->getVars();
        foreach( Declaration* d, vars )
        {
            Type* t = d->getType()->deref();
            imp->code.initMemory(imp->moduleData.data()+d->off, t,true);
        }
    }

    try
    {
        if( proc->kind == Declaration::Module )
            return imp->run(proc);

        if( !proc->entryPoint )
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

bool Interpreter::Imp::run(quint32 proc)
{
    quint8 locals[PreAllocSize];
    quint8 stack[PreAllocSize];
    return call(0, 0, code.getProc(proc), locals, stack);
}

#define VM_BINARY_OP(type, op) \
    *(type*)frame->slot(-2) = *(type*)frame->slot(-2) op *(type*)frame->slot(-1); \
    frame->pop(Frame::stackAlig); pc++;

#define VM_COMPARE_OP(type, op) \
    Q_ASSERT(sizeof(quint64) == Frame::stackAlig); \
    *(quint64*)frame->slot(-2) = *(type*)frame->slot(-2) op *(type*)frame->slot(-1); \
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
#define vmcase(l)     L_LL_##l:
#define vmbreak		 if( pc >= frame->proc->ops.size() ) return true; vmdispatch(frame->proc->ops[pc].op);

    static const void *const disptab[LL_NUM_OF_OPS] = {
        &&L_LL_invalid,
        #define OPDEF(op, x) &&L_LL_##op
        #include "MilVmOps.h"
        #undef OPDEF
    };

#else
#define vmdispatch(o)	switch(o)
#define vmcase(l)	case LL_##l:
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
                VM_COMPARE_OP(qint32,==)
            vmbreak;
        vmcase(ceq_i8)
                VM_COMPARE_OP(qint64,==)
            vmbreak;
        vmcase(ceq_r4)
                VM_COMPARE_OP(float,==)
            vmbreak;
        vmcase(ceq_r8)
                VM_COMPARE_OP(double,==)
            vmbreak;
        vmcase(ceq_p)
                VM_COMPARE_OP(void*,==)
            vmbreak;
        vmcase(ceq_pp)
                qWarning() << "TODO: interpreter ceq_pp not yet implemented";
            vmbreak;
        vmcase(cgt_i4)
                VM_COMPARE_OP(qint32,>)
            vmbreak;
        vmcase(cgt_i8)
                VM_COMPARE_OP(qint64,>)
            vmbreak;
        vmcase(cgt_r4)
                VM_COMPARE_OP(float,>)
            vmbreak;
        vmcase(cgt_r8)
                VM_COMPARE_OP(double,>)
            vmbreak;
        vmcase(cgt_u4)
                VM_COMPARE_OP(quint32,>)
            vmbreak;
        vmcase(cgt_u8)
                VM_COMPARE_OP(quint64,>)
            vmbreak;
        vmcase(cgt_p)
                VM_COMPARE_OP(void*,>)
            vmbreak;
        vmcase(clt_i4)
                VM_COMPARE_OP(qint32,<)
            vmbreak;
        vmcase(clt_i8)
                VM_COMPARE_OP(qint64,<)
            vmbreak;
        vmcase(clt_r4)
                VM_COMPARE_OP(float,<)
            vmbreak;
        vmcase(clt_r8)
                VM_COMPARE_OP(double,<)
            vmbreak;
        vmcase(clt_u4)
                VM_COMPARE_OP(quint32,<)
            vmbreak;
        vmcase(clt_u8)
                VM_COMPARE_OP(quint64,<)
            vmbreak;
        vmcase(clt_p)
                VM_COMPARE_OP(void*,<)
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
                            -stackAligned(sizeof(MethRef)), // size on stack
                            sizeof(MethRef)); // true size
                frame->pop(stackAligned(sizeof(MethRef)));
                pc++;
                vmbreak;
        vmcase(starg_vt)
                frame->copy(VM_ARG_ADDR, // to
                            -stackAligned(frame->proc->ops[pc+1].val), // size on stack
                            frame->proc->ops[pc+1].val); // true size
                frame->pop(stackAligned(frame->proc->ops[pc+1].val));
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
                            -stackAligned(sizeof(MethRef)), // size on stack
                            sizeof(MethRef)); // true size
                frame->pop(stackAligned(sizeof(MethRef)));
                pc++;
                vmbreak;
        vmcase(stloc_vt)
                frame->copy(VM_LOCAL_ADDR, // to
                            -stackAligned(frame->proc->ops[pc+1].val), // size on stack
                            frame->proc->ops[pc+1].val); // true size
                frame->pop(stackAligned(frame->proc->ops[pc+1].val));
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
                            -stackAligned(sizeof(MethRef)), // size on stack
                            sizeof(MethRef)); // true size
                frame->pop(stackAligned(sizeof(MethRef)));
                pc++;
                vmbreak;
        vmcase(stvar_vt)
                frame->copy(VM_VAR_ADDR, // to
                            -stackAligned(frame->proc->ops[pc+1].val), // size on stack
                            frame->proc->ops[pc+1].val); // true size
                frame->pop(stackAligned(frame->proc->ops[pc+1].val));
                pc += 2;
            vmbreak;
        vmcase(ldc_i4)
                frame->pushI4(code.getInt(frame->proc->ops[pc].val));
                pc++;
            vmbreak;
        vmcase(ldc_i8)
                frame->pushI8(code.getInt(frame->proc->ops[pc].val));
                pc++;
            vmbreak;
        vmcase(ldc_r4)
                frame->pushR4(code.getDouble(frame->proc->ops[pc].val));
                pc++;
            vmbreak;
        vmcase(ldc_r8)
                frame->pushR8(code.getDouble(frame->proc->ops[pc].val));
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
                frame->pushP((void*)code.getString(frame->proc->ops[pc].val));
                pc++;
            vmbreak;
        vmcase(ldobj) {
                const std::vector<char>& obj = code.getObject(frame->proc->ops[pc].val);
                frame->push(obj.data(),obj.size());
                pc++;
            } vmbreak;
        vmcase(br)
                pc = pc + 1 + (frame->proc->ops[pc].minus ? -1 : 1 ) * frame->proc->ops[pc].val;
            vmbreak;
        vmcase(brfalse_i4)
                if( !frame->popI4() )
                    pc = pc + 1 + (frame->proc->ops[pc].minus ? -1 : 1 ) * frame->proc->ops[pc].val;
                else
                    pc++;
             vmbreak;
        vmcase(ldproc)
                frame->pushP( code.getProc(frame->proc->ops[pc].val) );
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
                qWarning() << "TODO not yet implemented" << Code::op_names[frame->proc->ops[pc].op];
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
                if( !call(frame, pc, code.getProc(frame->proc->ops[pc].val), locals, stack ) )
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
                Procedure* proc = code.getProc(frame->proc->ops[pc].val);
                void* obj = *(void**)(frame->stack.data()+frame->sp-proc->fixArgSize);
                Vtable* vtbl = *(Vtable**)(obj);
                Q_ASSERT(proc->decl->pd);
                proc = vtbl->methods[proc->decl->pd->slot];
                if( !call(frame, pc, proc, locals, stack ) )
                    return false;
                pc++;
            } vmbreak;
        vmcase(callinst){
                if( !call(frame, pc, code.getProc(frame->proc->ops[pc].val), locals, stack ) )
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
                    const Template& tt = code.getTemplate(frame->proc->ops[pc].val);
                    memcpy(ptr, tt.mem.data(), tt.mem.size());
                }
                pc++;
            } vmbreak;
        vmcase(alloc1) {
                void* ptr;
                if( frame->proc->ops[pc].minus )
                {
                    const Template& tt = code.getTemplate(frame->proc->ops[pc].val);
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
                const Template& tt = code.getTemplate(frame->proc->ops[pc].val);
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
                void* obj = frame->popP();
                Vtable* ref = code.getVtable(frame->proc->ops[pc].val);
                if( obj )
                {
                    Vtable* cls;
                    memcpy(&cls, obj, sizeof(cls));
                    frame->pushI4(Type::isA(cls->type, ref->type));
                }else
                    frame->pushI4(0);
                pc++;
            } vmbreak;
        vmcase(newvla)
        vmcase(line)
                qWarning() << "TODO not yet implemented" << Code::op_names[frame->proc->ops[pc].op];
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
    if( proc->external )
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
        res = ffiProcs[proc->id](args, retval);
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
            Type* t = d->getType()->deref();
            code.initMemory((char*)local+d->off, t,true);
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
