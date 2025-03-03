#ifndef OBXILEMITTER_H
#define OBXILEMITTER_H

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

// Adapted from ObxIlEmitter

#include <QByteArray>
#include <QPair>
#include <QList>
#include <QIODevice>
#include <QTextStream>

namespace Mic
{
    class RowCol;

    typedef QPair<QByteArray,QByteArray> MilQuali; // [module '!'] element
    typedef QPair<MilQuali,QByteArray> MilTrident;

    struct MilVariable
    {
        QByteArray name;
        MilQuali type;
        uint offset : 25;
        uint isPublic : 1;
        uint bits : 6;
        MilVariable():offset(0),isPublic(0),bits(0) {}
        MilVariable(const MilQuali& type, const QByteArray& name):type(type),name(name),offset(0),isPublic(0),bits(0){}
    };

    struct MilOperation
    {
        uint op : 8;
        uint index : 24; // used for jumps, field access
        QVariant arg;
        MilOperation(quint8 ilop = 0):op(ilop),index(0){}
        MilOperation(quint8 ilop, const QVariant& arg, quint32 i = 0 ):op(ilop),index(i),arg(arg){}
    };

    struct MilObject
    {
        MilQuali typeRef;
        QVariant data; // note that array of byte can be either QByteArray or QVariantList
    };
    typedef QPair<QByteArray,QVariant> MilFieldSlot;
    typedef QList<MilFieldSlot> MilRecordLiteral;

    typedef QList<qint64> CaseLabelList;

    struct MilProcedure
    {
        enum Kind { Invalid, Intrinsic, Normal, Extern, Inline, Invar, ModuleInit, ProcType, MethType };
        uint kind : 3;
        uint isPublic : 1;
        uint isVararg : 1;
        uint stackDepth: 16;
        uint compiled: 1;
        QByteArray name;
        QList<MilOperation> body, finally;
        QList<MilVariable> params;
        QList<MilVariable> locals;
        MilQuali retType;
        MilQuali receiver; // first: name, second: local class
        QByteArray origName; // extern
        MilProcedure():kind(Invalid),isPublic(0),isVararg(0),stackDepth(0),compiled(0) {}
    };

    struct MilType
    {
        QByteArray name;
        quint8 kind; // MilEmitter::TypeKind
        bool isPublic;
        MilQuali base; // for arrays, pointers and proctypes
        quint32 len;
        QList<MilVariable> fields; // also proctype params
        QList<MilProcedure> methods;
        int indexOf(const QByteArray& name) const;
        const MilVariable* findField(const QByteArray& name) const;
    };

    struct MilConst
    {
        QByteArray name;
        MilQuali type;
        QVariant val;
    };

    struct MilModule
    {
        QByteArray fullName; // concatenation of path, name and suffix as symbol
        QByteArrayList metaParams; // just names, pointing to const or type decls

        enum What { Invalid, Type, Variable, Proc, Import, Const };
        QMap<const char*,QPair<What,quint32> >  symbols;
        typedef QPair<What,quint32> Order;
        QList<Order> order;
        QList<MilType> types;
        QList<MilProcedure> procs;
        QList<QByteArray> imports; // paths
        QList<MilVariable> vars;
        QList<MilConst> consts;
        int indexOfVar(const QByteArray&) const;
    };

    struct MilMetaParam {
        QByteArray name;
        MilQuali type;
        bool isConst;
        bool isGeneric;
        MilMetaParam():isConst(false),isGeneric(false){}
    };
    typedef QList<MilMetaParam> MilMetaParams;

    class MilRenderer
    {
    public:
        virtual void beginModule( const QByteArray& fullName, const QString& sourceFile, const QByteArrayList& ) {}
        virtual void endModule() {}

        virtual void addImport( const QByteArray& path ) {}

        virtual void addVariable( const MilQuali& typeRef, QByteArray name,  bool isPublic ) {}
        virtual void addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val ) {}
        virtual void addProcedure(const MilProcedure& method ) {} // also ProcType

        virtual void beginType(const QByteArray& name, bool isPublic, quint8 typeKind,
                               const MilQuali& super = MilQuali() ) {} // only Struct, Union and Object
        virtual void endType() {}
        virtual void addType( const QByteArray& name, bool isPublic, const MilQuali& baseType,
                      quint8 typeKind, quint32 len = 0) {}

        virtual void addField( const QByteArray& fieldName, // on top level or in class
                       const MilQuali& typeRef,
                       bool isPublic = true, quint8 bits = 0 ) {}
    };

    class MilEmitter
    {
    public:
        MilEmitter(MilRenderer*);

        void beginModule( const QByteArray& fullName, const QString& sourceFile, const MilMetaParams& = MilMetaParams() );
        void endModule();

        void addImport(const QByteArray& path);

        void addVariable( const MilQuali& typeRef, QByteArray name, bool isPublic = true );

        void addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val ); // always public

        void beginProc(const QByteArray& procName, bool isPublic = true, quint8 kind = MilProcedure::Normal,
                       const MilQuali& receiver = MilQuali() );
        void toFinallySection(bool);
        void endProc();

        enum TypeKind { Invalid, Struct, Union, Object, ProcType, MethType, Alias, Pointer, Array, Generic, MaxType };
        void beginType(const QByteArray& name, bool isPublic = true, quint8 typeKind = Struct,
                       const MilQuali& super = MilQuali() );
            // use for Struct, Union, ProcType, MethType
            // supports addField, addArgument, setReturnType, setVararg depending on typeKind
        void endType();

        void addType( const QByteArray& name, bool isPublic, const MilQuali& baseType,
                      quint8 typeKind = Alias, quint32 len = 0);
            // use for Alias, Pointer, Array

        void addField(const QByteArray& fieldName, // on top level or in struct/union
                       const MilQuali& typeRef, bool isPublic = true, quint8 bits = 0);
        quint32 addLocal( const MilQuali& typeRef, QByteArray name = QByteArray() );
        quint32 addArgument(const MilQuali& typeRef, QByteArray name = QByteArray() );
        void setReturnType(const MilQuali& typeRef);
        void setExtern( const QByteArray& origName = QByteArray() );
        void setVararg();

        enum Type { Unknown, I1, I2, I4, I8, R4, R8, U1, U2, U4, U8, IntPtr };
        static QByteArray typeSymbol1(Type); // MilEmitter::Type
        static QByteArray typeSymbol2(Type); // MilEmitter::Type
        static Type fromSymbol(const QByteArray&);
        static bool equals(const QByteArray&, Type);
        static QByteArray toString(const MilQuali&);
        static QByteArray toString(const MilTrident&);

        void add_();
        void and_();
        void call_( const MilQuali& methodRef, int argCount = 0, bool hasRet = false);
        void calli_( const MilQuali& methodRef, int argCount, bool hasRet = false );
        void callvi_( const MilTrident& methodRef, int argCount, bool hasRet = false );
        void callvirt_( const MilTrident& methodRef, int argCount, bool hasRet = false );
        void case_(const CaseLabelList&);
        void castptr_(const MilQuali& typeRef);
        void ceq_();
        void cgt_(bool withUnsigned = false);
        void clt_(bool withUnsigned = false);
        void conv_(Type);
        void div_(bool withUnsigned = false);
        void free_();
        void do_();
        void dup_();
        void else_();
        void end_();
        void exit_();
        void goto_(const QByteArray& label);
        void ifgoto_(const QByteArray& label);
        void if_();
        void iif_();
        void initobj(const MilQuali& typeRef);
        void isinst_(const MilQuali& typeRef);
        void label_(const QByteArray& name);
        void ldarg_(quint16 arg);
        void ldarga_(quint16 arg);
        void ldc_i4(qint32);
        void ldc_i8(qint64);
        void ldc_r4(double);
        void ldc_r8(double);
        void ldc_obj(const MilObject&);
        void ldelem_(const MilQuali& typeRef);
        void ldelema_(const MilQuali& typeRef);
        void ldfld_(const MilTrident& fieldRef);
        void ldflda_(const MilTrident& fieldRef);
        void ldmeth_(const MilTrident& methodRef); // CIL ldvirtftn, returns methref
        void ldproc_(const MilQuali& methodRef); // CIL ldftn
        void ldind_(Type);
        void ldloc_(quint16);
        void ldloca_(quint16);
        void ldnull_();
        void ldobj_(const MilQuali& typeRef);
        void ldvar_(const MilQuali& memberRef); // CIL ldsfld
        void ldvara_(const MilQuali& memberRef); // CIL ldsflda
        void ldstr_(const QByteArray& str);
        void line_(quint32);
        void loop_();
        void mul_();
        void neg_();
        void newarr_(const MilQuali& typeRef);
        void newvla_(const MilQuali& typeRef); // not in CIL
        void newobj_(const MilQuali& typeRef);
        void nop_();
        void not_();
        void or_();
        void pop_();
        void ptroff_(const MilQuali& typeRef);
        void rem_(bool withUnsigned = false);
        void repeat_();
        void ret_(bool pop);
        void shl_();
        void shr_(bool withUnsigned = false );
        void sizeof_(const MilQuali& typeRef);
        void starg_(quint16);
        void stelem_(const MilQuali& typeRef);
        void stfld_(const MilTrident& fieldRef);
        void stind_( Type ); // without Ux
        void stloc_(quint16);
        void stobj_(const MilQuali& typeRef);
        void stvar_(const MilQuali& memberRef); // CIL stsfld
        void sub_();
        void switch_();
        void then_();
        void until_();
        void while_();
        void xor_();
    protected:
        void delta(int d);
    private:
        quint8 d_typeKind;
        quint16 d_stackDepth;
        quint16 d_maxStackDepth;
        QList<MilProcedure> d_proc; // proc stack
        QList<MilOperation>* ops;
        QByteArray d_library, d_origName;
        MilRenderer* d_out;
    };

    class IlAsmRenderer : public MilRenderer
    {
    public:
        IlAsmRenderer(QIODevice*);

        virtual void beginModule(const QByteArray& moduleName,const QString& sourceFile, const QByteArrayList& mp );
        virtual void endModule();

        virtual void addImport(const QByteArray& path );

        virtual void addVariable( const MilQuali& typeRef, QByteArray name,  bool isPublic );
        virtual void addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val );
        virtual void addProcedure(const MilProcedure& method );

        virtual void beginType(const QByteArray& className, bool isPublic, quint8 classKind,const MilQuali& super);
        virtual void endType();
        virtual void addType( const QByteArray& name, bool isPublic, const MilQuali& baseType,
                              quint8 typeKind, quint32 len = 0);

        virtual void addField( const QByteArray& fieldName,
                       const MilQuali& typeRef,
                       bool isPublic = true, quint8 bits = 0);
    protected:
        inline QByteArray ws() { return QByteArray(level*2,' '); }
        void render(const MilProcedure&);
    private:
        enum State { Idle, Module, Struct, Proc } state;
        QTextStream out;
        int level;
        QString source;
        QByteArray d_moduleName;
        bool sourceRendered;
    };

    class MilSplitter : public MilRenderer
    {
    public:
        MilSplitter(const QList<MilRenderer*>& r):renderer(r) {}

        virtual void beginModule( const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& );
        virtual void endModule();

        virtual void addImport(const QByteArray& path );

        virtual void addVariable( const MilQuali& typeRef, QByteArray name,  bool isPublic );
        virtual void addConst(const MilQuali& typeRef, const QByteArray& name, const QVariant& val );
        virtual void addProcedure(const MilProcedure& method );

        virtual void beginType(const QByteArray& name, bool isPublic, quint8 typeKind,const MilQuali& super);
        virtual void endType();
        virtual void addType( const QByteArray& name, bool isPublic, const MilQuali& baseType,
                      quint8 typeKind, quint32 len = 0);

        virtual void addField( const QByteArray& fieldName, // on top level or in class
                       const MilQuali& typeRef,
                       bool isPublic = true, quint8 bits = 0 );
    private:
        QList<MilRenderer*> renderer;
    };
}

Q_DECLARE_METATYPE(Mic::MilObject)
Q_DECLARE_METATYPE(Mic::CaseLabelList)
Q_DECLARE_METATYPE(Mic::MilTrident)
Q_DECLARE_METATYPE(Mic::MilRecordLiteral)

#endif // OBXILEMITTER_H
