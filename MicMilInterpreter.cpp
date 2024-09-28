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
#include <QtDebug>
using namespace Mic;

//#define _USE_GETTIMEOFDAY
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

// NOTE: replaced original design based on MemSlots(QVector) by MemSlot[] and integrated
// all embedded arrays and structs into the same MemSlot[] so that constructs like
// struct Permute { Benchmark base; int count; } (from Permute.c) can be represented.

struct MemSlot
{
    union {
        qint64 i;
        quint64 u;
        double f;
        MemSlot* sp;
        ProcData* pp;
    };
    enum Type { Invalid, I, U, F,
                SeqVal,  // pointer to sequence of MemSlot, owned
                SeqPtr,  // pointer to sequence of MemSlot, not owned
                SlotPtr, // pointer to MemSlot (variable, parameter, field, element), not owned
                ProcPtr, // pointer to procedure/module, not owned
                Header // the header slot of a record or array; the pointer points to the next slot; u is size
              };
    quint8 t;
    bool hw; // half width for i, u or f

    MemSlot():u(0),t(Invalid),hw(false) {}
    MemSlot(quint64 u, bool h = false):u(u),t(U),hw(h) { if(hw) u &= 0xffffffff; }
    MemSlot(qint64 i, bool h = false):i(i),t(I),hw(h) { if(hw) i = (qint32)i; }
    MemSlot(int i, bool h = true):i(i),t(I),hw(h) {}
    MemSlot(double d,bool h):f(d),t(F),hw(h) {}
    MemSlot(MemSlot* s, bool owned = false):sp(s),t(owned?SeqVal:SeqPtr), hw(false) {}
    MemSlot(ProcData* p):pp(p),t(ProcPtr),hw(false) {}
    MemSlot(const MemSlot& rhs):MemSlot() { *this = rhs; }
    ~MemSlot();
    MemSlot& operator=(const MemSlot& rhs);
    void move( MemSlot& rhs );
    void copy(MemSlot* rhs);
};

typedef QVector<MemSlot> MemSlotList;

static MemSlot* createSequence(int size)
{
    MemSlot* s = new MemSlot[size+1];
    s->t = MemSlot::Header;
    s->u = size;
    s++; // point to the second element which is the actual first element of the sequence
    return s;
}

void MemSlot::copy(MemSlot* rhs)
{
    t = SeqVal;
    sp = 0;
    if( rhs == 0 )
        return;
    MemSlot* header = rhs - 1;
    Q_ASSERT(header->t == MemSlot::Header);
    sp = createSequence(header->u);
    for(int i = 0; i < header->u; i++ )
        sp[i] = rhs[i];
}

MemSlot& MemSlot::operator=(const MemSlot& rhs)
{
    u = rhs.u;
    t = rhs.t;
    hw = rhs.hw;
    if( t == SeqVal )
        copy(rhs.sp);
    return *this;
}

void MemSlot::move( MemSlot& rhs )
{
    u = rhs.u;
    t = rhs.t;
    hw = rhs.hw;
    if( t == SeqVal )
        rhs.sp = 0;
}

MemSlot::~MemSlot()
{
    if( t == SeqVal && sp )
    {
        MemSlot* header = sp - 1;
        Q_ASSERT(header->t == MemSlot::Header);
        delete[] header;
    }
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

    QMap<const char*, ModuleData*> modules; // moduleFullName -> data
    typedef QList< QList<int> > LoopStack;
    struct MilLabel
    {
        quint32 labelPc;
        QList<quint32> gotoPcs;
        MilLabel():labelPc(0){}
    };
    typedef QMap<const char*,MilLabel> Labels;
    QMap<QByteArray,MemSlotList> strings; // internalized strings
    QByteArray intrinsicMod, outMod, inputMod;
    typedef QMap<const char*, MilProcedure> Intrinsics;
    Intrinsics intrinsics;
    QTextStream out;
#ifdef _USE_GETTIMEOFDAY
    struct timeval start; // 57732 us for Bounce 1500, i.e. 23 times worse than Lua 5.4.7 without jump table
#else
    QElapsedTimer timer; // NOTE: QElapsedTimer and gettimeofday are virtually identical
#endif

    MemSlot* internalize(const QByteArray& str)
    {
        MemSlotList& s = strings[str];
        if( s.isEmpty() )
        {
            s.resize(str.size()+1); // string literals are expected to have an explicit terminating zero
            // TODO: this is pretty wasteful; maybe we should support byte strings directly
            s[0].t = MemSlot::Header;
            s[0].u = str.size();
            for( int i = 1; i <= str.size(); i++ )
                s[i] = MemSlot(quint64((quint8)str[i-1]),true);
        }
        return s.data() + 1;
    }

    void initSlot(ModuleData* module, MemSlot& s, const MilQuali& type )
    {
        const MilType* t = getType(module, type);
        if( t == 0 )
            return; // intrinsic type, nothing to initialize
        switch( t->kind )
        {
        // TODO Union, Alias
        case MilEmitter::Struct:
            s.t = MemSlot::SeqVal;
            s.sp = createSequence(t->fields.size()); // TODO: flatten structured types
            initSlots(module, s.sp, t->fields);
            break;
        case MilEmitter::Array:
            s.t = MemSlot::SeqVal;
            s.sp = createSequence(t->len); // TODO: flatten structured types
            initSlots(module, s.sp, t->base);
            break;
        }

    }

    void initSlots(ModuleData* module, MemSlot* ss, const MilQuali& type )
    {
        for( int i = 0; i < (ss-1)->u; i++ )
            initSlot(module,ss[i],type);
    }

    void initSlots(ModuleData* module, MemSlot* ss, const QList<MilVariable>& types )
    {
        // ss.resize(types.size());
        //Q_ASSERT((ss-1)->u == types.size());
        for(int i = 0; i < types.size(); i++ )
            initSlot(module, ss[i], types[i].type);
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
        initSlots(md, md->variables.data(),module->vars);
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
        throw QString("%1!%2 error: %3")
            .arg(module->module->fullName.constData())
            .arg(proc->name.constData()).arg(msg);
    }

    const MilType* getType(ModuleData* module, const MilQuali& q) const
    {
        // this is only for custom types defined with Emitter::addType or begin/endType, not for intrinsic types
        MilModule* m = q.first.isEmpty() ? module->module : loader->getModule(q.first);
        if( m == 0 )
            return 0;
        QPair<MilModule::What,quint32> what = m->symbols.value(q.second.constData());
        if( what.first == MilModule::Type )
            return &m->types[what.second];
        // else
        return 0;
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

    void processBlock(ModuleData* module, MilProcedure* proc, quint32& pc, Labels& labels, LoopStack& loopStack)
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
                    processBlock(module, proc,pc, labels, loopStack);
                assureValid(module,proc,start,pc,IL_do);
                const int then = pc;
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_end )
                    processBlock(module, proc, pc, labels, loopStack); // look for nested statements
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
                    processBlock(module, proc,pc, labels, loopStack);
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
                        processBlock(module, proc, pc, labels, loopStack); // look for nested statements
                    assureValid(module,proc,start,pc,IL_case, IL_else, IL_end);
                }
                if( ops[pc].op == IL_else )
                {
                    ops[prev].index = pc;
                    prev = pc;
                    thenList.append(pc);
                    pc++;
                    while( pc < ops.size() && ops[pc].op != IL_end )
                        processBlock(module, proc, pc, labels, loopStack);
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
                    processBlock(module, proc,pc, labels, loopStack);
                assureValid(module,proc,start,pc, IL_then);
                int then = pc;
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_else && ops[pc].op != IL_end)
                    processBlock(module, proc, pc, labels, loopStack); // then statements
                assureValid(module,proc,start,pc, IL_else, IL_end);
                if( ops[pc].op == IL_else )
                {
                    ops[then].index = pc+1; // then -> else+1
                    const int else_ = pc;
                    pc++;
                    while( pc < ops.size() && ops[pc].op != IL_end )
                        processBlock(module, proc, pc, labels, loopStack); // else statements
                    ops[else_].index = pc; // else -> end
                }else
                    ops[then].index = pc; // then -> end
                assureValid(module,proc,start,pc,IL_end);
                pc++;
            }
            break;
        case IL_repeat:
            {
                // end -> repeat+1
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_until )
                    processBlock(module, proc, pc, labels, loopStack);
                assureValid(module,proc,start,pc, IL_until);
                pc++;
                while( pc < ops.size() && ops[pc].op != IL_end )
                    processBlock(module, proc,pc, labels, loopStack);
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
                    processBlock(module, proc, pc, labels, loopStack);
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
        case IL_ifgoto:
            labels[ops[pc].arg.toByteArray().constData()].gotoPcs.append(pc);
            pc++;
            break;
        case IL_ldfld:
        case IL_ldflda:
        case IL_stfld:
            {
                MilTrident td = ops[pc].arg.value<MilTrident>();
                const MilType* ty = getType(module, td.first);
                if( ty == 0 )
                    execError(module, proc, pc, QString("unknown type '%1'").
                              arg(MilEmitter::toString(td.first).constData()));
                const int idx = ty->indexOf(td.second);
                if( idx < 0 )
                    break; // error?
                ops[pc].index = idx;
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
                ops[pc].index = idx;
            }
            pc++;
            break;
        default:
            pc++;
            break;
        }
    }

    void calcOffsets(ModuleData* module, MilProcedure* proc, quint32& pc)
    {
        Labels labels; // name -> label pos, goto poss
        LoopStack loopStack;
        while( pc < proc->body.size() )
        {
            processBlock(module, proc, pc, labels, loopStack);
        }
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
        case MemSlot::SeqPtr:
        case MemSlot::SlotPtr:
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
        if( data.canConvert<MilRecordLiteral>() )
        {
            // TODO: flatten embedded elements
            MilRecordLiteral m = data.value<MilRecordLiteral>();
            out.sp = createSequence(m.size());
            out.t = MemSlot::SeqVal;
            for( int i = 0; i < m.size(); i++ )
                convert(out.sp[i], m[i].second);
            return;
        }
        switch( data.type() )
        {
        case QVariant::List:
            {
                QVariantList l = data.toList();
                out.sp = createSequence(l.size());
                out.t = MemSlot::SeqVal;
                for( int i = 0; i < l.size(); i++ )
                    convert(out.sp[i], l[i]);
            }
            break;
        case QVariant::ByteArray:
            out.sp = internalize(data.toByteArray());
            out.t = MemSlot::SeqPtr;
            break;
        case QVariant::String:
            out.sp = internalize(data.toString().toLatin1());
            out.t = MemSlot::SeqPtr;
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

    inline void makeCall(QList<MemSlot>& stack, ProcData* proc)
    {
        // TODO: support varargs
        MemSlotList args(proc->proc->params.size());
        for( int i = proc->proc->params.size()-1; i >= 0; i-- )
        {
            if( stack.isEmpty() )
                execError(proc->module,proc->proc,"not enough actual parameters");
            args[i].move(stack.back());
            stack.pop_back();
        }
        MemSlot ret;
        execute(proc->module, proc->proc, args, ret);
        if( !proc->proc->retType.second.isEmpty() )
        {
            stack.push_back(MemSlot());
            stack.back().move(ret);
        }
    }

    static inline QByteArray toStr(const MemSlot& s)
    {
        Q_ASSERT( s.sp );
        MemSlot* header = s.sp - 1;
        Q_ASSERT( header->t == MemSlot::Header );
        QByteArray str(header->u, ' ');
        for( int i = 0; i < str.size(); i++ )
            str[i] = (char)(quint8)s.sp[i].u;
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
        }else
            nyiError(module,proc);
    }

    void callIntrinsic(MilProcedure* proc, MemSlotList& args, MemSlot& ret)
    {
        switch(proc->stackDepth)
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
            Q_ASSERT(args.size()==2 && args.first().sp && args.last().sp );
            for( int i = 0; ; i++ )
            {
                args.first().sp[i] = args.last().sp[i];
                if( args.last().sp[i].u == 0 )
                    break;
            }
            break;
        case 15: // assert
            Q_ASSERT(args.size()==3);
            if( args.first().u == 0 )
                throw QString("assertion failed at %1:%2").arg(toStr(args[2]).constData()).arg(args[1].u);
            break;
        default:
            throw QString("intrinsic proc '$MIC!%1' not yet implemented").arg(proc->name.constData());
        }
    }

    static inline MemSlot addressOf(MemSlot& s)
    {
        if( s.t == MemSlot::SeqVal )
            // in case of a structured value copy the address of the structure, otherwise of the slot
            return MemSlot(s.sp);
        else
            return &s;
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
        quint32 pc = 0;
        if( !proc->compiled )
        {
            calcOffsets(module, proc, pc);
            proc->compiled = true;
        }

// #define _USE_JUMP_TABLE
        // crashes and the debugger becomes veeeery slow
        // likely needs a totally new architecture to make this work; maybe later
#ifdef _USE_JUMP_TABLE
#undef vmdispatch
#undef vmcase
#undef vmbreak

#define vmdispatch(x)     goto *disptab[x];

#define vmcase(l)     L_##l:

#define vmbreak		 if( pc >= proc->body.size() ) return; op = proc->body[pc]; vmdispatch(op.op);

        static const void *const disptab[IL_NUM_OF_OPS] = {
            &&L_IL_invalid,
            &&L_IL_add, &&L_IL_and, &&L_IL_call, &&L_IL_calli, &&L_IL_castptr,
            &&L_IL_ceq, &&L_IL_cgt, &&L_IL_cgt_un, &&L_IL_clt, &&L_IL_clt_un,
            &&L_IL_conv_i1, &&L_IL_conv_i2, &&L_IL_conv_i4, &&L_IL_conv_i8, &&L_IL_conv_r4, &&L_IL_conv_r8,
            &&L_IL_conv_u1, &&L_IL_conv_u2, &&L_IL_conv_u4, &&L_IL_conv_u8, &&L_IL_conv_ip,
            &&L_IL_div, &&L_IL_div_un, &&L_IL_dup, &&L_IL_initobj, &&L_IL_ldarg, &&L_IL_ldarg_s,
            &&L_IL_ldarg_0, &&L_IL_ldarg_1, &&L_IL_ldarg_2, &&L_IL_ldarg_3,
            &&L_IL_ldarga, &&L_IL_ldarga_s,
            &&L_IL_ldc_i4, &&L_IL_ldc_i8, &&L_IL_ldc_i4_s, &&L_IL_ldc_r4, &&L_IL_ldc_r8,
            &&L_IL_ldc_i4_0, &&L_IL_ldc_i4_1, &&L_IL_ldc_i4_2, &&L_IL_ldc_i4_3, &&L_IL_ldc_i4_4, &&L_IL_ldc_i4_5,
            &&L_IL_ldc_i4_6, &&L_IL_ldc_i4_7, &&L_IL_ldc_i4_8, &&L_IL_ldc_i4_m1, &&L_IL_ldc_obj,
            &&L_IL_ldelem, &&L_IL_ldelema, &&L_IL_ldelem_i1, &&L_IL_ldelem_i2,
            &&L_IL_ldelem_i4, &&L_IL_ldelem_i8, &&L_IL_ldelem_u1, &&L_IL_ldelem_u2,
            &&L_IL_ldelem_u4, &&L_IL_ldelem_u8, &&L_IL_ldelem_r4, &&L_IL_ldelem_r8, &&L_IL_ldelem_ip,
            &&L_IL_ldfld, &&L_IL_ldflda,
            &&L_IL_ldind_i1, &&L_IL_ldind_i2, &&L_IL_ldind_i4, &&L_IL_ldind_i8, &&L_IL_ldind_u1, &&L_IL_ldind_u2,
            &&L_IL_ldind_u4, &&L_IL_ldind_r4, &&L_IL_ldind_u8, &&L_IL_ldind_r8, &&L_IL_ldind_ip,
            &&L_IL_ldloc, &&L_IL_ldloc_s, &&L_IL_ldloca, &&L_IL_ldloca_s,
            &&L_IL_ldloc_0, &&L_IL_ldloc_1, &&L_IL_ldloc_2, &&L_IL_ldloc_3, &&L_IL_ldnull,
            &&L_IL_ldobj, &&L_IL_ldproc, &&L_IL_ldstr,
            &&L_IL_ldvar, &&L_IL_ldvara, &&L_IL_mul, &&L_IL_neg,
            &&L_IL_newarr, &&L_IL_newvla, &&L_IL_newobj,
            &&L_IL_not, &&L_IL_or, &&L_IL_rem, &&L_IL_rem_un, &&L_IL_shl, &&L_IL_shr, &&L_IL_shr_un,
            &&L_IL_sizeof, &&L_IL_sub, &&L_IL_xor, &&L_IL_ptroff, &&L_IL_nop,
            &&L_IL_free, &&L_IL_repeat, &&L_IL_until,
            &&L_IL_exit, &&L_IL_goto, &&L_IL_ifgoto, &&L_IL_if, &&L_IL_then, &&L_IL_else, &&L_IL_end,
            &&L_IL_label, &&L_IL_line, &&L_IL_loop, &&L_IL_pop, &&L_IL_ret,
            &&L_IL_starg, &&L_IL_starg_s,
            &&L_IL_stelem, &&L_IL_stelem_i1, &&L_IL_stelem_i2, &&L_IL_stelem_i4, &&L_IL_stelem_i8,
            &&L_IL_stelem_r4, &&L_IL_stelem_r8, &&L_IL_stelem_ip, &&L_IL_stfld,
            &&L_IL_stind_i1, &&L_IL_stind_i2, &&L_IL_stind_i4, &&L_IL_stind_i8, &&L_IL_stind_r4, &&L_IL_stind_r8, &&L_IL_stind_ip,
            &&L_IL_stloc, &&L_IL_stloc_s, &&L_IL_stloc_0, &&L_IL_stloc_1, &&L_IL_stloc_2, &&L_IL_stloc_3,
            &&L_IL_stobj, &&L_IL_stvar, &&L_IL_switch, &&L_IL_case, &&L_IL_while, &&L_IL_do
        };

#endif
        MemSlotList locals(proc->locals.size());
        initSlots(module, locals.data(), proc->locals);
        ret = MemSlot();
        pc = 0;
        QList<MemSlot> stack;
        MemSlot lhs, rhs;
        MilObject mo;
        QList<MemSlot> switchExpr;
        QList<quint8> curStatement;
        while(true)
        {
            if( pc >= proc->body.size() )
                return;
            MilOperation& op = proc->body[pc];

            // NOTE: jump tables (i.e. &&label, computed gotos) makes little sense, since GCC is able
            // to derive it automatically in principle; e.g. Lua 5.4.7 has exactly same
            // performance with or without jump tables; but the VM has a different architecture than
            // the present one.
            vmdispatch(op.op)
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
            vmcase(IL_and)
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
                    ProcData* pd = getProc(module, op.arg.value<MilQuali>());
                    if( pd == 0 )
                        execError(module, proc, pc, QString("cannot resolve procedure %1").
                                  arg(MilEmitter::toString(op.arg.value<MilQuali>()).constData()));
                    makeCall(stack, pd);
                }
                pc++;
                vmbreak;
            vmcase(IL_calli)
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::ProcPtr || lhs.pp == 0 )
                    execError(module, proc, pc, "top of stack is not a procedure");
                makeCall(stack, lhs.pp);
                pc++;
                vmbreak;
            vmcase(IL_castptr)
                // NOP
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
                convertTo(stack, MemSlot::SeqPtr, 0); // TODO
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
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_ldarg)
            vmcase(IL_ldarg_s)
                stack.push_back(args.at(op.arg.toUInt()));
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
                stack.push_back(addressOf(args[op.arg.toUInt()]));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i4)
            vmcase(IL_ldc_i4_s)
                stack.push_back(MemSlot(op.arg.toLongLong(),true));
                pc++;
                vmbreak;
            vmcase(IL_ldc_i8)
                stack.push_back(MemSlot(op.arg.toLongLong()));
                pc++;
                vmbreak;
            vmcase(IL_ldc_r4)
                stack.push_back(MemSlot(op.arg.toDouble(),true));
                pc++;
                vmbreak;
            vmcase(IL_ldc_r8)
                stack.push_back(MemSlot(op.arg.toDouble(),false));
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
            vmcase(IL_ldc_obj)
                mo = op.arg.value<MilObject>();
                convert(lhs, mo.data);
                stack.push_back(MemSlot());
                stack.back().move(lhs);
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
                lhs = stack.takeLast();
                if( (lhs.t != MemSlot::SeqPtr) || lhs.sp == 0 || rhs.u >= (lhs.sp-1)->u )
                    execError(module, proc, pc, "invalid array or index out of bound");
                stack.push_back(lhs.sp[rhs.u]);
                pc++;
                vmbreak;
            vmcase(IL_ldelema)
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::SeqPtr || lhs.sp == 0 || rhs.u >= (lhs.sp-1)->u )
                    execError(module, proc, pc, "invalid array or index out of bound");
                stack.push_back(addressOf(lhs.sp[rhs.u]));
                pc++;
                vmbreak;
            vmcase(IL_ldfld)
                lhs = stack.takeLast();
                if( (lhs.t != MemSlot::SeqPtr) ||
                        lhs.sp == 0 || (lhs.sp-1)->u <= op.index )
                    execError(module, proc, pc, "invalid record or field");
                stack.push_back(lhs.sp[op.index]);
                pc++;
                vmbreak;
            vmcase(IL_ldflda)
                lhs = stack.takeLast();
                if( (lhs.t != MemSlot::SeqPtr) ||
                        lhs.sp == 0 || (lhs.sp-1)->u <= op.index )
                    execError(module, proc, pc, "invalid record or field");
                stack.push_back(addressOf(lhs.sp[op.index]));
                pc++;
                vmbreak;
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
                lhs = stack.takeLast();
                if( lhs.sp == 0 || lhs.t != MemSlot::SeqPtr)
                    execError(module, proc, pc, "invalid pointer on stack");
                stack.push_back(*lhs.sp);
                pc++;
                vmbreak;
            vmcase(IL_ldloc)
            vmcase(IL_ldloc_s)
                stack.push_back(locals.at(op.arg.toUInt()));
                pc++;
                vmbreak;
            vmcase(IL_ldloca)
            vmcase(IL_ldloca_s)
                stack.push_back(addressOf(locals[op.arg.toUInt()]));
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
            vmcase(IL_ldnull)
                stack.push_back(MemSlot((MemSlot*)0));
                pc++;
                vmbreak;
            vmcase(IL_ldobj)
                lhs = stack.takeLast();
                if( (lhs.t != MemSlot::SlotPtr && lhs.t != MemSlot::SeqPtr) || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer on stack");
                if( lhs.t == MemSlot::SlotPtr )
                    stack.push_back(*lhs.sp);
                else
                {
                    //stack.push_back(MemSlot());
                    //stack.back().copy(lhs.ssp);
                    stack.push_back(lhs);
                }
                pc++;
                vmbreak;
            vmcase(IL_ldproc)
                stack.push_back(MemSlot(getProc(module,op.arg.value<MilQuali>())));
                pc++;
                vmbreak;
            vmcase(IL_ldstr)
                stack.push_back(MemSlot(internalize(op.arg.toByteArray())));
                pc++;
                vmbreak;
            vmcase(IL_ldvar)
                if( module->variables.size() <= op.index )
                    execError(module, proc, pc, "invalid variable reference");
                stack.push_back(module->variables.at(op.index));
                pc++;
                vmbreak;
            vmcase(IL_ldvara)
                if( module->variables.size() <= op.index )
                    execError(module, proc, pc, "invalid variable reference");
                stack.push_back(addressOf(module->variables[op.index]));
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
                    MemSlot* array = createSequence(lhs.u);
                    MilQuali ety = op.arg.value<MilQuali>();
                    initSlots(module, array, ety);
                    stack.push_back(MemSlot(array));
                }
                pc++;
                vmbreak;
            vmcase(IL_newobj)
                {
                    const MilQuali q = op.arg.value<MilQuali>();
                    const MilType* ty = getType(module, q);
                    if( ty == 0 )
                        execError(module, proc, pc, QString("unknown type '%1'").
                                  arg(MilEmitter::toString(q).constData()));
                    if( ty->kind != MilEmitter::Struct && ty->kind != MilEmitter::Union )
                        execError(module, proc, pc, "operation not available for given type");
                    MemSlot* record = createSequence(ty->fields.size()); // TODO: flatten
                    initSlots(module, record, ty->fields);
                    stack.push_back(MemSlot( record ) );
                    // TODO: we treat unions as structs here
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
            vmcase(IL_or)
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
                if( (lhs.t != MemSlot::SeqPtr) || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer");
                MemSlot* header = lhs.sp-1;
                Q_ASSERT(header->t == MemSlot::Header);
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
                pc = op.index;
                vmbreak;
            vmcase(IL_goto)
                pc = op.index;
                vmbreak;
            vmcase(IL_ifgoto)
                lhs = stack.takeLast();
                if( lhs.u )
                    pc = op.index;
                else
                    pc++;
                vmbreak;
            vmcase(IL_if) // if(1) then(2) else(2) end(5)
                curStatement.push_back(IL_if);
                pc++;
                vmbreak;
            vmcase(IL_then)
                if( curStatement.isEmpty() )
                    execError(module, proc, pc, "operation not expected here");
                if( curStatement.back() == IL_if )
                {
                    lhs = stack.takeLast();
                    if( lhs.u == 0 )
                        pc = op.index; // jump to else+1 or end
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
                    if( switchExpr.back().t == MemSlot::ProcPtr )
                        pc = op.index; // done, else points to end
                    else
                        pc++; // execute else
                }else if( curStatement.back() == IL_if )
                    pc = op.index; // else points to end, only hit after then is executed
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
                if( op.index )
                    pc = op.index;
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
                if( lhs.t != MemSlot::SlotPtr || rhs.t != MemSlot::I )
                    execError(module, proc, pc, "invalid argument types");
                lhs.sp += rhs.i;
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
                args[op.arg.toUInt()].move(stack.back());
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
            vmcase(IL_stelem_ip)
                {
                    MemSlot val;
                    val.move(stack.back());
                    stack.pop_back();
                    MemSlot index = stack.takeLast();
                    MemSlot array = stack.takeLast();
                    if( array.t != MemSlot::SeqPtr || array.sp == 0 || index.u >= (array.sp-1)->u )
                        execError(module, proc, pc, "invalid array or index out of range");
                    array.sp[index.u].move(val);
                }
                pc++;
                vmbreak;
            vmcase(IL_stfld)
                rhs.move(stack.back());
                stack.pop_back();
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::SeqPtr || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid record");
                lhs.sp[op.index].move(rhs);
                pc++;
                vmbreak;
            vmcase(IL_stind_i1)
            vmcase(IL_stind_i2)
            vmcase(IL_stind_i4)
            vmcase(IL_stind_i8)
            vmcase(IL_stind_r4)
            vmcase(IL_stind_r8)
            vmcase(IL_stind_ip)
                rhs.move(stack.back());
                stack.pop_back();
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::SeqPtr || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer");
                lhs.sp->move(rhs);
                pc++;
                vmbreak;
            vmcase(IL_stloc)
            vmcase(IL_stloc_s)
                locals[op.arg.toUInt()].move(stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_0)
                locals[0].move(stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_1)
                locals[1].move(stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_2)
                locals[2].move(stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stloc_3)
                locals[3].move(stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_stobj)
                rhs.move(stack.back());
                stack.pop_back();
                lhs = stack.takeLast(); // destination
                if( (lhs.t != MemSlot::SlotPtr && lhs.t != MemSlot::SeqPtr) || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer");
                if( lhs.t == MemSlot::SlotPtr )
                {
                    lhs.sp->move(rhs);
                }else
                {
                    if( (rhs.t != MemSlot::SeqPtr && rhs.t != MemSlot::SeqVal) || rhs.sp == 0 )
                        execError(module, proc, pc, "invalid value");
                    if( (lhs.sp-1)->u < (rhs.sp-1)->u )
                        execError(module, proc, pc, "array not big enough for copy");
                    for( int i = 0; i < (rhs.sp-1)->u; i++ )
                        lhs.sp[i] = rhs.sp[i];
                        // copy string literal
                }
                pc++;
                vmbreak;
            vmcase(IL_stvar)
                module->variables[op.index].move(stack.back());
                stack.pop_back();
                pc++;
                vmbreak;
            vmcase(IL_switch) // switch(1) {case(1) then(2)} [else(2)] end(5)
                curStatement.push_back(IL_switch);
                switchExpr.append(MemSlot());
                pc++;
                vmbreak;
            vmcase(IL_case)
                if( switchExpr.back().t == MemSlot::ProcPtr )
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
                if( op.arg.value<CaseLabelList>().contains( switchExpr.back().i ) )
                {
                    switchExpr.back().t = MemSlot::ProcPtr; // mark as done
                    pc += 2; // skip then
                }else
                    pc = op.index;
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
                    pc = op.index; // jump to end+1
                }else
                    pc++;
                vmbreak;
            vmcase(IL_nop)
                pc++;
                vmbreak;
#if 0
            default:
                Q_ASSERT(false);
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
    p.stackDepth = code;
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

    imp->intrinsicMod = Token::getSymbol("$MIC");
    imp->outMod = Token::getSymbol("Out");
    imp->inputMod = Token::getSymbol("Input");
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
        imp->loadModule(module);
    }catch(const QString& str)
    {
        qCritical() << str;
    }
}

