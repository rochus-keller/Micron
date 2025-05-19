#ifndef MILVMCODE_H
#define MILVMCODE_H

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

#include <Micron/MilAst.h>

class QTextStream;

namespace Mil
{

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

enum LL_op
{
    LL_invalid,

#define OPDEF(op, x) LL_##op
#include "MilVmOps.h"
#undef OPDEF

    LL_NUM_OF_OPS
};

struct Operation
{
    uint val : 22;
    uint minus : 1;
    uint op : 9;
    Operation(LL_op op = LL_invalid, quint32 val = 0, bool minus = false):val(val),minus(minus),op(op){}
};

struct Procedure
{
    std::vector<Operation> ops; // std::vector because QVector.detach() is very expensive
    Declaration* decl;
    uint init : 1;
    uint called : 1;
    uint external : 1;
    uint id: 29;
    quint32 localsSize;
    quint32 fixArgSize, returnSize;
    Procedure():decl(0),init(0), called(0), external(0), id(0),
        localsSize(0),fixArgSize(0),returnSize(0),ops(0){}
};

struct Vtable
{
    Vtable* parent;
    Type* type;
    std::vector<Procedure*> methods;
};

struct Template
{
    Type* type;
    std::vector<char> mem; // preinitialized memory for type
    Template():type(0){}
};

struct MethRef
{
    void* obj;
    Procedure* proc;
    MethRef(Vtable* o = 0, Procedure* p = 0):obj(o),proc(p){}
};

class VmCode
{
public:
    VmCode(AstModel*, quint8 pointerWidth, quint8 stackAlignment);
    ~VmCode();

    void addExternal(const char* module, const char* name, quint32 id);

    bool compile(Declaration* procOrModule);

    qint64 getInt(quint32 n) const { return ints[n]; }
    double getDouble(quint32 n) const { return doubles[n]; }
    Procedure* getProc(quint32 n) const { return procs[n]; }
    Vtable* getVtable(quint32 n) const { return vtables[n]; }
    const char* getString(quint32 n) const { return strings[n].c_str(); }
    const std::vector<char>& getObject(quint32 n) const { return objects[n]; }
    const Template& getTemplate(quint32 n) const { return templates[n]; }

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

    inline int stackAligned(int off)
    {
        return AstModel::align(off, stackAlignment );
    }

    void initMemory(char* mem, Type* t, bool doPointerInit );

    bool dumpAll(QTextStream& out);
    bool dumpModule(QTextStream& out, Declaration* module);
    bool dumpProc(QTextStream& out, Declaration* proc);

    static const char* op_names[];

protected:
    bool translateProc(Declaration* proc);
    bool translateModule(Declaration* m);
    bool translateProc(Procedure& proc);
    bool translateStatSeq(Procedure& proc, Statement* s);
    bool translateExprSeq(Procedure& proc, Expression* e);
    bool translateInit(Procedure& proc, quint32 id);
    void render(char* data, quint32 off, Type* t, Constant* c);
    void render(char* data, quint32 start, ComponentList* cl );
    Type* deref(Type* t)
    {
        if( t && t->kind == Type::NameRef )
            return deref(t->getType());
        else if( t )
            return t;
        else
            return mdl->getBasicType(Type::Undefined);
    }
    int emitOp(Procedure& proc, LL_op op, quint32 v = 0, bool minus = false )
    {
        const int res = proc.ops.size();
        proc.ops.push_back(Operation(op, v, minus));
        return res;
    }
    void inline branch_here(Procedure& proc, int pc)
    {
        Q_ASSERT(pc >= 0 && pc < proc.ops.size());
        proc.ops[pc].val = proc.ops.size() - pc - 1;
    }
    void downcopy(Vtable* vt);

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

    quint32 addObject(Constant* c)
    {
        const quint32 id = objects.size();
        objects.push_back(std::vector<char>());
        std::vector<char>& obj = objects.back();
        if( c->kind == Constant::B )
        {
            obj.resize(c->b->len);
            memcpy( obj.data(), (const char*)c->b->b,c->b->len);
        }else
        {
            ComponentList* cl = c->c;
            Q_ASSERT( cl->type );
            obj.resize(deref(cl->type)->getByteSize(sizeof(void*)));
            render(obj.data(), 0, cl);
        }
        return id;
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

    int findTemplate(Type* t) const
    {
        for( int i = 0; i < templates.size(); i++ )
            if( templates[i].type == t )
                return i;
        return -1;
    }

private:
    const quint8 pointerWidth;
    const quint8 stackAlignment;
    AstModel* mdl;

    // accessing std::vector is cheaper than QVector or QByteArray
    std::vector<std::string> strings;
    std::vector< std::vector<char> > objects;
    std::vector<double> doubles;
    std::vector<qint64> ints;
    std::vector<Procedure*> procs;
    std::vector<Vtable*> vtables;
    std::vector<Template> templates;

    // the following are only used during compilation:
    struct Where {
        const char* name;
        int pc;
        Where(const char* name, int pc):name(name),pc(pc){}
    };
    struct Context {
        QList< QList<int> > loopStack;
        QList<Where> gotos, labels;
        Procedure* curProc;

        int findLabel(const char* name) const
        {
            for( int i = 0; i < labels.size(); i++ )
            {
                if( labels[i].name == name )
                    return i;
            }
            return -1;
        }
    };
    QList<Context> ctxStack; // because translateProc is called recursively
    QMap<const char*,QMap<const char*, int> > externals;
};

}

#endif // MILVMCODE_H
