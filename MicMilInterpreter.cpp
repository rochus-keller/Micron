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
#include <QVector>
#include <QtDebug>
using namespace Mic;

struct MemSlots;
struct ModuleData;

struct ProcData
{
    MilProcedure* proc;
    ModuleData* module;
    ProcData(MilProcedure* p, ModuleData* m):proc(p),module(m) {}
};

struct MemSlot
{
    union {
        qint64 i;
        quint64 u;
        double f;
        MemSlot* sp;
        MemSlots* ssp;
        ProcData* pp;
    };
    enum Type { Invalid, I, U, F,
                SS,  // pointer to MemSlots, owned
                SP, // pointer to MemSlot, not owned
                SSP, // pointer to MemSlots, not owned
                PP, // pointer to procedure/module, not owned
              };
    quint8 t;
    bool hw; // half width for i, u or f

    MemSlot():u(0),t(Invalid),hw(false) {}
    MemSlot(quint64 u, bool h = false):u(u),t(U),hw(h) { if(hw) u &= 0xffffffff; }
    MemSlot(qint64 i, bool h = false):i(i),t(I),hw(h) { if(hw) i = (qint32)i; }
    MemSlot(int i, bool h = true):i(i),t(I),hw(h) {}
    MemSlot(double d,bool h):f(d),t(F),hw(h) {}
    MemSlot(MemSlot* s):sp(s),t(SP), hw(false) {}
    MemSlot(MemSlots* ss, bool owned = false):ssp(ss),t(owned?SS:SSP), hw(false) {}
    MemSlot(ProcData* p):pp(p),t(PP),hw(false) {}
    MemSlot(const MemSlot& rhs):MemSlot() { *this = rhs; }
    ~MemSlot();
    MemSlot& operator=(const MemSlot& rhs);
    void move( MemSlot& rhs );
    void copy(MemSlots* rhs);
};

struct MemSlots : public QVector<MemSlot>
{
    MemSlots(int len = 0):QVector<MemSlot>(len) {}
};

void MemSlot::copy(MemSlots* rhs)
{
    t = SS;
    ssp = 0;
    if( rhs == 0 )
        return;
    ssp = new MemSlots(rhs->length());
    for(int i = 0; i < rhs->length(); i++ )
        ssp->operator[](i) = rhs->at(i);
}

MemSlot& MemSlot::operator=(const MemSlot& rhs)
{
    u = rhs.u;
    t = rhs.t;
    hw = rhs.hw;
    if( t == SS )
        copy(rhs.ssp);
    return *this;
}

void MemSlot::move( MemSlot& rhs )
{
    u = rhs.u;
    t = rhs.t;
    hw = rhs.hw;
    if( t == SS )
        rhs.ssp = 0;
}

MemSlot::~MemSlot()
{
    if( t == SS && ssp )
        delete ssp;
}

struct ModuleData
{
    MilModule* module;
    MemSlots variables;
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
    QList< QList<int> > loopStack;
    typedef QMap<const char*, QPair<quint32,QList<quint32> > > Labels;
    Labels labels; // name -> label pos, goto poss
    QMap<QByteArray,MemSlots> strings; // internalized strings
    QByteArray intrinsicMod;
    typedef QMap<const char*, MilProcedure> Intrinsics;
    Intrinsics intrinsics;
    QTextStream out;

    MemSlots* internalize(const QByteArray& str)
    {
        MemSlots& s = strings[str];
        if( s.isEmpty() )
        {
            s.resize(str.size()); // string literals are expected to have an explicit terminating zero
            // TODO: this is pretty wasteful; maybe we should support byte strings directly
            for( int i = 0; i < str.size(); i++ )
                s[i] = MemSlot(quint64((quint8)str[i]),true);
        }
        return &s;
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
            s.t = MemSlot::SS;
            s.ssp = new MemSlots(t->fields.size());
            initSlots(module, *s.ssp, t->fields);
            break;
        case MilEmitter::Array:
            s.t = MemSlot::SS;
            s.ssp = new MemSlots(t->len);
            initSlots(module, *s.ssp, t->base);
            break;
        }

    }

    void initSlots(ModuleData* module, MemSlots& ss, const MilQuali& type )
    {
        for( int i = 0; i < ss.size(); i++ )
            initSlot(module,ss[i],type);
    }

    void initSlots(ModuleData* module, MemSlots& ss, const QList<MilVariable>& types )
    {
        ss.resize(types.size());
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
        initSlots(md, md->variables,module->vars);
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
            MemSlots args;
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

    void processBlock(ModuleData* module, MilProcedure* proc, quint32& pc)
    {
        QList<MilOperation>& op = proc->body;
        const int start = pc;
        switch(op[pc].op)
        {
        case IL_while:
            {
                // end -> while+1, then -> end+1
                pc++;
                while( pc < op.size() && op[pc].op != IL_do )
                    processBlock(module, proc,pc);
                assureValid(module,proc,start,pc,IL_do);
                const int then = pc;
                pc++;
                while( pc < op.size() && op[pc].op != IL_end )
                    processBlock(module, proc, pc); // look for nested statements
                assureValid(module,proc,start,pc,IL_end);
                op[pc].index = start+1; // end jumps to while+1
                pc++;
                op[then].index = pc; // then jumps to end+1
            }
            break;
        case IL_switch:
            {
                // switch -> case -> case -> else -> end
                pc++;
                while( pc < op.size() && op[pc].op != IL_case &&
                       op[pc].op != IL_else && op[pc].op != IL_end )
                    processBlock(module, proc,pc);
                assureValid(module,proc,start,pc,IL_case, IL_else, IL_end);
                int prev = start;
                QList<quint32> thenList;
                while( op[pc].op == IL_case )
                {
                    op[prev].index = pc;
                    prev = pc;
                    pc++;
                    assureValid(module,proc,start,pc,IL_then);
                    thenList.append(pc);
                    pc++;
                    while( pc < op.size() && op[pc].op != IL_case &&
                           op[pc].op != IL_else && op[pc].op != IL_end )
                        processBlock(module, proc, pc); // look for nested statements
                    assureValid(module,proc,start,pc,IL_case, IL_else, IL_end);
                }
                if( op[pc].op == IL_else )
                {
                    op[prev].index = pc;
                    prev = pc;
                    thenList.append(pc);
                    pc++;
                    while( pc < op.size() && op[pc].op != IL_end )
                        processBlock(module, proc, pc);
                }
                assureValid(module,proc,start,pc,IL_end);
                foreach( quint32 off, thenList )
                    op[off].index = pc; // all then and else point to end
                pc++;
            }
            break;
        case IL_if:
            {
                // if -> then -> else -> end
                pc++;
                while( pc < op.size() && op[pc].op != IL_then )
                    processBlock(module, proc,pc);
                assureValid(module,proc,start,pc, IL_then);
                int then = pc;
                pc++;
                while( pc < op.size() && op[pc].op != IL_else && op[pc].op != IL_end)
                    processBlock(module, proc, pc); // then statements
                assureValid(module,proc,start,pc, IL_else, IL_end);
                if( op[pc].op == IL_else )
                {
                    op[then].index = pc+1; // then -> else+1
                    const int else_ = pc;
                    pc++;
                    while( pc < op.size() && op[pc].op != IL_end )
                        processBlock(module, proc, pc); // else statements
                    op[else_].index = pc; // else -> end
                }else
                    op[then].index = pc; // then -> end
                assureValid(module,proc,start,pc,IL_end);
                pc++;
            }
            break;
        case IL_repeat:
            {
                // end -> repeat+1
                pc++;
                while( pc < op.size() && op[pc].op != IL_until )
                    processBlock(module, proc, pc);
                assureValid(module,proc,start,pc, IL_until);
                pc++;
                while( pc < op.size() && op[pc].op != IL_end )
                    processBlock(module, proc,pc);
                assureValid(module,proc,start,pc, IL_end);
                op[pc].index = start+1;
                pc++;
            }
            break;
        case IL_loop:
            {
                // end -> loop
                loopStack.push_back(QList<int>());
                pc++;
                while( pc < op.size() && op[pc].op != IL_end )
                    processBlock(module, proc, pc);
                assureValid(module,proc,start,pc, IL_end);
                op[pc].index = start+1;
                pc++;
                foreach( int exit_, loopStack.back() )
                    op[exit_].index = pc;
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
            labels[op[pc].arg.toByteArray().constData()].first = pc;
            pc++;
            break;
        case IL_goto:
            labels[op[pc].arg.toByteArray().constData()].second.append(pc);
            pc++;
            break;        case IL_ldfld:
        case IL_ldflda:
        case IL_stfld:
            {
                MilTrident td = op[pc].arg.value<MilTrident>();
                const MilType* ty = getType(module, td.first);
                if( ty == 0 )
                    execError(module, proc, pc, QString("unknown type '%1'").
                              arg(MilEmitter::toString(td.first).constData()));
                const int idx = ty->indexOf(td.second);
                if( idx < 0 )
                    break; // error?
                op[pc].index = idx;
            }
            pc++;
            break;
        case IL_ldvar:
        case IL_ldvara:
        case IL_stvar:
            {
                MilQuali q = op[pc].arg.value<MilQuali>();
                MilModule* m = q.first.isEmpty() ? module->module : loader->getModule(q.first);
                if( m == 0 )
                    break; // error?
                const int idx = m->indexOfVar(q.second);
                if( idx < 0 )
                    break; // error?
                op[pc].index = idx;
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
        while( pc < proc->body.size() )
        {
            processBlock(module, proc, pc);
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
        case MemSlot::SP:
        case MemSlot::SSP:
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
            MilRecordLiteral m = data.value<MilRecordLiteral>();
            out.ssp = new MemSlots(m.size());
            out.t = MemSlot::SS;
            for( int i = 0; i < m.size(); i++ )
                convert(out.ssp->operator[](i), m[i].second);
            return;
        }
        switch( data.type() )
        {
        case QVariant::List:
            {
                QVariantList l = data.toList();
                out.ssp = new MemSlots(l.size());
                out.t = MemSlot::SS;
                for( int i = 0; i < l.size(); i++ )
                    convert(out.ssp->operator[](i), l[i]);
            }
            break;
        case QVariant::ByteArray:
            out.ssp = internalize(data.toByteArray());
            out.t = MemSlot::SSP;
            break;
        case QVariant::String:
            out.ssp = internalize(data.toString().toLatin1());
            out.t = MemSlot::SSP;
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
        MemSlots args(proc->proc->params.size());
        for( int i = proc->proc->params.size()-1; i >= 0; i-- )
        {
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
        Q_ASSERT( s.ssp );
        QByteArray str(s.ssp->length(), ' ');
        for( int i = 0; i < str.size(); i++ )
            str[i] = (char)(quint8)s.ssp->at(i).u;
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
    }

    void callIntrinsic(MilProcedure* proc, MemSlots& args, MemSlot& ret)
    {
        switch(proc->stackDepth)
        {
        case 1: // relop1
            Q_ASSERT(args.size()==3);
            ret.t = MemSlot::U;
            ret.u = MIC_relop1(toStr(args[0]).constData(), toStr(args[1]).constData(), args[3].u);
            break;
        case 2: // relop2
            {
                Q_ASSERT(args.size()==3);
                char tmp[2] = ".";
                tmp[0] = (char)(quint8)args[1].u;
                ret.t = MemSlot::U;
                ret.u = MIC_relop1(toStr(args[0]).constData(), tmp, args[3].u);
            }
            break;
        case 3: // relop3
            {
                Q_ASSERT(args.size()==3);
                char tmp[2] = ".";
                tmp[0] = (char)(quint8)args[0].u;
                ret.t = MemSlot::U;
                ret.u = MIC_relop1(tmp, toStr(args[1]).constData(), args[3].u);
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
                ret.u = MIC_relop1(l, r, args[3].u);
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
            Q_ASSERT(args.size()==2 && args.first().ssp && args.last().ssp );
            for( int i = 0; i < args.last().ssp->length(); i++ )
            {
                args.first().ssp->operator[](i) = args.last().ssp->at(i);
                if( args.last().ssp->at(i).u == 0 )
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
        if( s.t == MemSlot::SS )
            // in case of a structured value copy the address of the structure, otherwise of the slot
            return MemSlot(s.ssp);
        else
            return &s;
    }

    void execute(ModuleData* module, MilProcedure* proc, MemSlots& args, MemSlot& ret)
    {
        if( proc->kind == MilProcedure::Intrinsic )
        {
            callIntrinsic(proc,args,ret);
            return;
        }
        quint32 pc = 0;
        if( !proc->compiled )
        {
            calcOffsets(module, proc, pc);
            Labels::const_iterator i;
            for( i = labels.begin(); i != labels.end(); ++i )
            {
                for(int j = 0; j < i.value().second.size(); j++ )
                    proc->body[i.value().second[j]].index = i.value().first;
            }
            proc->compiled = true;
        }
        MemSlots locals(proc->locals.size());
        initSlots(module, locals, proc->locals);
        ret = MemSlot();
        pc = 0;
        QList<MemSlot> stack;
        MemSlot lhs, rhs;
        QList<MemSlot> switchExpr;
        QList<quint8> curStatement;
        while(true)
        {
            if( pc >= proc->body.size() )
                break;
            const MilOperation& op = proc->body[pc];
            // NOTE: jump tables (i.e. &&label) makes no sense, since GCC is able
            // to derive it automatically; e.g. Lua 5.4.7 has exactly same performance w/wo jump tables
            switch(op.op)
            {
            case IL_add:
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
                break;
            case IL_and:
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
                break;
            case IL_call:
                {
                    ProcData* pd = getProc(module, op.arg.value<MilQuali>());
                    if( pd == 0 )
                        execError(module, proc, pc, QString("cannot resolve procedure %1").
                                  arg(MilEmitter::toString(op.arg.value<MilQuali>()).constData()));
                    makeCall(stack,pd);
                }
                pc++;
                break;
            case IL_calli:
                {
                    MemSlot p = stack.takeLast();
                    if( p.t != MemSlot::PP || p.pp == 0 )
                        execError(module, proc, pc, "top of stack is not a procedure");
                    makeCall(stack,p.pp);
                }
                pc++;
                break;
            case IL_castptr:
                // NOP
                pc++;
                break;
            case IL_ceq:
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                stack.push_back(MemSlot(qint64(lhs.u == rhs.u),true));
                pc++;
                break;
            case IL_cgt:
            case IL_cgt_un:
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
                break;
            case IL_clt:
            case IL_clt_un:
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
                break;
            case IL_conv_i1:
                convertTo(stack, MemSlot::I, 1);
                pc++;
                break;
            case IL_conv_i2:
                convertTo(stack, MemSlot::I, 2);
                pc++;
                break;
            case IL_conv_i4:
                convertTo(stack, MemSlot::I, 4);
                pc++;
                break;
            case IL_conv_i8:
                convertTo(stack, MemSlot::I, 0);
                pc++;
                break;
            case IL_conv_r4:
            case IL_conv_r8:
                convertTo(stack, MemSlot::F, 0);
                pc++;
                break;
            case IL_conv_u1:
                convertTo(stack, MemSlot::U, 1);
                pc++;
                break;
            case IL_conv_u2:
                convertTo(stack, MemSlot::U, 2);
                pc++;
                break;
            case IL_conv_u4:
                convertTo(stack, MemSlot::U, 4);
                pc++;
                break;
            case IL_conv_u8:
                convertTo(stack, MemSlot::U, 0);
                pc++;
                break;
            case IL_conv_ip:
                convertTo(stack, MemSlot::SP, 0); // TODO
                pc++;
                break;
            case IL_div:
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
                break;
            case IL_div_un:
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
                break;
            case IL_dup:
                stack.push_back(stack.back());
                pc++;
                break;
            case IL_initobj:
                // NOP
                stack.pop_back();
                pc++;
                break;
            case IL_ldarg:
            case IL_ldarg_s:
                stack.push_back(args.at(op.arg.toUInt()));
                pc++;
                break;
            case IL_ldarg_0:
                stack.push_back(args.at(0));
                pc++;
                break;
            case IL_ldarg_1:
                stack.push_back(args.at(1));
                pc++;
                break;
            case IL_ldarg_2:
                stack.push_back(args.at(2));
                pc++;
                break;
            case IL_ldarg_3:
                stack.push_back(args.at(3));
                pc++;
                break;
            case IL_ldarga:
            case IL_ldarga_s:
                stack.push_back(addressOf(args[op.arg.toUInt()]));
                pc++;
                break;
            case IL_ldc_i4:
            case IL_ldc_i4_s:
                stack.push_back(MemSlot(op.arg.toLongLong(),true));
                pc++;
                break;
            case IL_ldc_i8:
                stack.push_back(MemSlot(op.arg.toLongLong()));
                pc++;
                break;
            case IL_ldc_r4:
                stack.push_back(MemSlot(op.arg.toDouble(),true));
                pc++;
                break;
            case IL_ldc_r8:
                stack.push_back(MemSlot(op.arg.toDouble(),false));
                pc++;
                break;
            case IL_ldc_i4_0:
                stack.push_back(MemSlot(0));
                pc++;
                break;
            case IL_ldc_i4_1:
                stack.push_back(MemSlot(1));
                pc++;
                break;
            case IL_ldc_i4_2:
                stack.push_back(MemSlot(2));
                pc++;
                break;
            case IL_ldc_i4_3:
                stack.push_back(MemSlot(3));
                pc++;
                break;
            case IL_ldc_i4_4:
                stack.push_back(MemSlot(4));
                pc++;
                break;
            case IL_ldc_i4_5:
                stack.push_back(MemSlot(5));
                pc++;
                break;
            case IL_ldc_i4_6:
                stack.push_back(MemSlot(6));
                pc++;
                break;
            case IL_ldc_i4_7:
                stack.push_back(MemSlot(7));
                pc++;
                break;
            case IL_ldc_i4_8:
                stack.push_back(MemSlot(8));
                pc++;
                break;
            case IL_ldc_i4_m1:
                stack.push_back(MemSlot(-1));
                pc++;
                break;
            case IL_ldc_obj:
                {
                    MilObject o = op.arg.value<MilObject>();
                    MemSlot s;
                    convert(s, o.data);
                    stack.push_back(MemSlot());
                    stack.back().move(s);
                }
                pc++;
                break;
            case IL_ldelem:
            case IL_ldelem_i1:
            case IL_ldelem_i2:
            case IL_ldelem_i4:
            case IL_ldelem_i8:
            case IL_ldelem_u1:
            case IL_ldelem_u2:
            case IL_ldelem_u4:
            case IL_ldelem_u8:
            case IL_ldelem_r4:
            case IL_ldelem_r8:
            case IL_ldelem_ip:
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                if( lhs.ssp == 0 || rhs.u >= lhs.ssp->length() )
                    execError(module, proc, pc, "invalid array or index out of bound");
                stack.push_back(lhs.ssp->at(rhs.u));
                pc++;
                break;
            case IL_ldelema:
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::SSP || lhs.ssp == 0 || rhs.u >= lhs.ssp->length() )
                    execError(module, proc, pc, "invalid array or index out of bound");
                stack.push_back(addressOf(lhs.ssp->operator[](rhs.u)));
                pc++;
                break;
            case IL_ldfld:
                {
                    MemSlot s = stack.takeLast();
                    if( (s.t != MemSlot::SS && s.t != MemSlot::SSP) ||
                            s.ssp == 0 || s.ssp->size() <= op.index )
                        execError(module, proc, pc, "invalid record or field");
                    stack.push_back(s.ssp->at(op.index));
                }
                pc++;
                break;
            case IL_ldflda:
                {
                    MemSlot s = stack.takeLast();
                    if( (s.t != MemSlot::SS && s.t != MemSlot::SSP) ||
                            s.ssp == 0 || s.ssp->size() <= op.index )
                        execError(module, proc, pc, "invalid record or field");
                    stack.push_back(addressOf(s.ssp->operator[](op.index)));
                }
                pc++;
                break;
            case IL_ldind_i1:
            case IL_ldind_i2:
            case IL_ldind_i4:
            case IL_ldind_i8:
            case IL_ldind_u1:
            case IL_ldind_u2:
            case IL_ldind_u4:
            case IL_ldind_u8:
            case IL_ldind_r4:
            case IL_ldind_r8:
            case IL_ldind_ip:
                lhs = stack.takeLast();
                if( lhs.sp == 0 || lhs.t != MemSlot::SP)
                    execError(module, proc, pc, "invalid pointer on stack");
                stack.push_back(*lhs.sp);
                pc++;
                break;
            case IL_ldloc:
            case IL_ldloc_s:
                stack.push_back(locals.at(op.arg.toUInt()));
                pc++;
                break;
            case IL_ldloca:
            case IL_ldloca_s:
                stack.push_back(addressOf(locals[op.arg.toUInt()]));
                pc++;
                break;
            case IL_ldloc_0:
                stack.push_back(locals.at(0));
                pc++;
                break;
            case IL_ldloc_1:
                stack.push_back(locals.at(1));
                pc++;
                break;
            case IL_ldloc_2:
                stack.push_back(locals.at(2));
                pc++;
                break;
            case IL_ldloc_3:
                stack.push_back(locals.at(3));
                pc++;
                break;
            case IL_ldnull:
                stack.push_back(MemSlot((MemSlot*)0));
                pc++;
                break;
            case IL_ldobj:
                lhs = stack.takeLast();
                if( (lhs.t != MemSlot::SP && lhs.t != MemSlot::SSP) || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer on stack");
                if( lhs.t == MemSlot::SP )
                    stack.push_back(*lhs.sp);
                else
                {
                    //stack.push_back(MemSlot());
                    //stack.back().copy(lhs.ssp);
                    stack.push_back(lhs);
                }
                pc++;
                break;
            case IL_ldproc:
                stack.push_back(MemSlot(getProc(module,op.arg.value<MilQuali>())));
                pc++;
                break;
            case IL_ldstr:
                stack.push_back(MemSlot(internalize(op.arg.toByteArray())));
                pc++;
                break;
            case IL_ldvar:
                {
                    if( module->variables.size() <= op.index )
                        execError(module, proc, pc, "invalid variable reference");
                    stack.push_back(module->variables.at(op.index));
                }
                pc++;
                break;
            case IL_ldvara:
                {
                    if( module->variables.size() <= op.index )
                        execError(module, proc, pc, "invalid variable reference");
                    stack.push_back(addressOf(module->variables[op.index]));
                }
                pc++;
                break;
            case IL_mul:
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
                break;
            case IL_neg:
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
                break;
            case IL_newarr:
            case IL_newvla:
                {
                    lhs = stack.takeLast();
                    if( lhs.u == 0 )
                        execError(module, proc, pc, "invalid array size");
                    MemSlots* array = new MemSlots(lhs.u);
                    MilQuali ety = op.arg.value<MilQuali>();
                    initSlots(module, *array, ety);
                    stack.push_back(MemSlot(array) );
                }
                pc++;
                break;
            case IL_newobj:
                {
                    const MilQuali q = op.arg.value<MilQuali>();
                    const MilType* ty = getType(module, q);
                    if( ty == 0 )
                        execError(module, proc, pc, QString("unknown type '%1'").
                                  arg(MilEmitter::toString(q).constData()));
                    if( ty->kind != MilEmitter::Struct && ty->kind != MilEmitter::Union )
                        execError(module, proc, pc, "operation not available for given type");
                    MemSlots* record = new MemSlots(ty->fields.size());
                    initSlots(module, *record, ty->fields);
                    stack.push_back(MemSlot( record ) );
                    // TODO: we treat unions as structs here
                }
                pc++;
                break;
            case IL_not:
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
                break;
            case IL_or:
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
                break;
            case IL_rem:
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::I:
                    stack.push_back(MemSlot(lhs.i % rhs.i,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                break;
            case IL_rem_un:
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                switch( lhs.t )
                {
                case MemSlot::U:
                    stack.push_back(MemSlot(lhs.u % rhs.u,lhs.hw));
                    break;
                default:
                    Q_ASSERT(false);
                }
                pc++;
                break;
            case IL_shl:
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
                break;
            case IL_shr:
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
                break;
            case IL_shr_un:
                stack.push_back(MemSlot(lhs.u >> rhs.u,lhs.hw));
                pc++;
                break;
            case IL_sizeof:
                stack.push_back(MemSlot(1)); // RISK
                pc++;
                break;
            case IL_sub:
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
                break;
            case IL_xor:
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
                break;
            case IL_free:
                lhs = stack.takeLast();
                if( (lhs.t != MemSlot::SP && lhs.t != MemSlot::SSP) || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer");
                if( lhs.t == MemSlot::SP )
                    delete lhs.sp;
                else
                    delete lhs.ssp;
                pc++;
                break;
            case IL_repeat: // repeat(1) until(1) end(5)
                curStatement.push_back(IL_repeat);
                pc++;
                break;
            case IL_until:
                pc++;
                break;
            case IL_exit:
                while( !curStatement.isEmpty() && curStatement.back() != IL_loop )
                    curStatement.pop_back(); // exit everything up to the closest loop statement
                if( curStatement.isEmpty() || curStatement.back() != IL_loop )
                    execError(module, proc, pc, "operation not allowed here");
                curStatement.pop_back();
                pc = op.index;
                break;
            case IL_goto:
                pc = op.index;
                break;
            case IL_if: // if(1) then(2) else(2) end(5)
                curStatement.push_back(IL_if);
                pc++;
                break;
            case IL_then:
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
                break;
            case IL_else:
                if( curStatement.isEmpty() )
                    execError(module, proc, pc, "operation not expected here");
                if( curStatement.back() == IL_switch )
                {
                    if( switchExpr.back().t == MemSlot::PP )
                        pc = op.index; // done, else points to end
                    else
                        pc++; // execute else
                }else if( curStatement.back() == IL_if )
                    pc = op.index; // else points to end, only hit after then is executed
                break;
            case IL_end:
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
                break;
            case IL_label:
                // NOP
                pc++;
                break;
            case IL_line:
                // NOP
                pc++;
                break;
            case IL_loop: // loop(1) end(5)
                curStatement.push_back(IL_loop);
                pc++;
                break;
            case IL_pop:
                stack.pop_back();
                pc++;
                break;
            case IL_ptroff:
                rhs = stack.takeLast();
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::SP || rhs.t != MemSlot::I )
                    execError(module, proc, pc, "invalid argument types");
                lhs.sp += rhs.i;
                stack.push_back(lhs);
                pc++;
                break;
            case IL_ret:
                if( !proc->retType.second.isEmpty() )
                {
                    ret.move(stack.back());
                    stack.pop_back();
                }
                if( !stack.isEmpty() )
                    execError(module, proc, pc, "stack must be empty at this place");
                return;

            case IL_starg:
            case IL_starg_s:
                args[op.arg.toUInt()].move(stack.back());
                stack.pop_back();
                pc++;
                break;
            case IL_stelem:
            case IL_stelem_i1:
            case IL_stelem_i2:
            case IL_stelem_i4:
            case IL_stelem_i8:
            case IL_stelem_r4:
            case IL_stelem_r8:
            case IL_stelem_ip:
                {
                    MemSlot val;
                    val.move(stack.back());
                    stack.pop_back();
                    MemSlot index = stack.takeLast();
                    MemSlot array = stack.takeLast();
                    if( array.t != MemSlot::SSP || array.ssp == 0 || index.u >= array.ssp->length() )
                        execError(module, proc, pc, "invalid array or index out of range");
                    array.ssp->operator[](index.u).move(val);
                }
                pc++;
                break;
            case IL_stfld:
                rhs.move(stack.back());
                stack.pop_back();
                lhs = stack.takeLast();
                if( lhs.t != MemSlot::SSP || lhs.ssp == 0 )
                    execError(module, proc, pc, "invalid record");
                lhs.ssp->operator[](op.index).move(rhs);
                pc++;
                break;
            case IL_stind_i1:
            case IL_stind_i2:
            case IL_stind_i4:
            case IL_stind_i8:
            case IL_stind_r4:
            case IL_stind_r8:
            case IL_stind_ip:
                rhs.move(stack.back());
                stack.pop_back();
                lhs = stack.takeLast();
                if( lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer");
                lhs.sp->move(rhs);
                pc++;
                break;
            case IL_stloc:
            case IL_stloc_s:
                locals[op.arg.toUInt()].move(stack.back());
                stack.pop_back();
                pc++;
                break;
            case IL_stloc_0:
                locals[0].move(stack.back());
                stack.pop_back();
                pc++;
                break;
            case IL_stloc_1:
                locals[1].move(stack.back());
                stack.pop_back();
                pc++;
                break;
            case IL_stloc_2:
                locals[2].move(stack.back());
                stack.pop_back();
                pc++;
                break;
            case IL_stloc_3:
                locals[3].move(stack.back());
                stack.pop_back();
                pc++;
                break;
            case IL_stobj:
                rhs.move(stack.back());
                stack.pop_back();
                lhs = stack.takeLast(); // destination
                if( (lhs.t != MemSlot::SP && lhs.t != MemSlot::SSP) || lhs.sp == 0 )
                    execError(module, proc, pc, "invalid pointer");
                if( lhs.t == MemSlot::SP )
                {
                    lhs.sp->move(rhs);
                }else
                {
                    if( (rhs.t != MemSlot::SSP && rhs.t != MemSlot::SS) || rhs.ssp == 0 )
                        execError(module, proc, pc, "invalid value");
                    if( lhs.ssp->length() < rhs.ssp->length() )
                        execError(module, proc, pc, "array not big enough for copy");
                    for( int i = 0; i < rhs.ssp->length(); i++ )
                        lhs.ssp->operator[](i) = rhs.ssp->at(i);
                        // copy string literal
                }
                pc++;
                break;
            case IL_stvar:
                module->variables[op.index].move(stack.back());
                stack.pop_back();
                pc++;
                break;
            case IL_switch: // switch(1) {case(1) then(2)} [else(2)] end(5)
                curStatement.push_back(IL_switch);
                switchExpr.append(MemSlot());
                pc++;
                break;
            case IL_case:
                if( switchExpr.back().t == MemSlot::PP )
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
                    switchExpr.back().t = MemSlot::PP; // mark as done
                    pc += 2; // skip then
                }else
                    pc = op.index;
                break;
            case IL_while: // while(1) do(1) end(5)
                curStatement.push_back(IL_while);
                pc++;
                break;
            case IL_do:
                lhs = stack.takeLast();
                if( lhs.u == 0 )
                {
                    curStatement.pop_back();
                    pc = op.index; // jump to end+1
                }else
                    pc++;
                break;
            case IL_nop:
                break;
            default:
                Q_ASSERT(false);
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

    imp->intrinsicMod = Token::getSymbol("$MIC");
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

