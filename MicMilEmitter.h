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

    struct MilOperation
    {
        uint d_ilop : 8;
        uint d_flags : 24;
        QVariant d_arg;
        MilOperation(quint8 ilop = 0):d_ilop(ilop),d_flags(0){}
        MilOperation(quint8 ilop, const QVariant& arg, quint16 flags = 0 ):d_ilop(ilop),d_flags(flags),d_arg(arg){}
    };

    struct MilObject
    {
        QByteArray typeRef;
        QVariant data; // note that array of byte can be either QByteArray or QVariantList
    };

    typedef QList<qint64> CaseLabelList;

    struct MilProcedure
    {
        enum Kind { Invalid, Normal, Extern, Inline, Invar, ModuleInit, ProcType };
        uint d_kind : 3;
        uint d_isPublic : 1;
        uint d_isVararg : 1;
        uint d_stackDepth: 16;
        QByteArray d_name;
        QList<MilOperation> d_body;
        QList< QPair<QByteArray,QByteArray> > d_args; // idx, type, name
        QList< QPair<QByteArray,QByteArray> > d_locals; // idx, type, name
        QByteArray d_retType;
        QByteArray d_origName; // extern
        MilProcedure():d_kind(Invalid),d_isPublic(0),d_isVararg(0),d_stackDepth(0) {}
    };

    typedef QPair<QByteArray,QByteArray> Qualident;

    struct MilMetaParam {
        QByteArray name;
        Qualident type;
    };
    typedef QList<MilMetaParam> MilMetaParams;

    class MilRenderer
    {
    public:
        virtual void beginModule( const QByteArray& moduleName, const QString& sourceFile, const MilMetaParams& ) {}
        virtual void endModule() {}

        virtual void addImport( const QByteArray& path, const QByteArray& name ) {}

        virtual void addVariable( const QByteArray& typeRef, QByteArray name ) {}
        virtual void addProcedure(const MilProcedure& method ) {}

        virtual void beginType(const QByteArray& name, bool isPublic, quint8 typeKind) {}
        virtual void endType() {}
        virtual void addType( const QByteArray& name, bool isPublic, const QByteArray& baseType,
                      quint8 typeKind, quint32 len = 0) {}

        virtual void addField( const QByteArray& fieldName, // on top level or in class
                       const QByteArray& typeRef,
                       bool isPublic = true ) {}
    };

    class MilEmitter
    {
    public:
        MilEmitter(MilRenderer*);


        void beginModule( const QByteArray& moduleName, const QString& sourceFile, const MilMetaParams& = MilMetaParams() );
        void endModule();

        void addImport( const QByteArray& path, const QByteArray& name = QByteArray() );

        void addVariable( const QByteArray& typeRef, QByteArray name );

        void beginProc(const QByteArray& procName, bool isPublic = true, quint8 kind = MilProcedure::Normal );
        void endProc();

        enum TypeKind { Invalid, Struct, Union, ProcType, Alias, Pointer, Array, MaxType };
        void beginType(const QByteArray& name, bool isPublic = true, quint8 typeKind = Struct ); // Struct, Union, ProcType
            // supports addField, addArgument, setReturnType, setVararg depending on typeKind
        void endType();
        void addType( const QByteArray& name, bool isPublic, const QByteArray& baseType,
                      quint8 typeKind = Alias, quint32 len = 0); // Alias, Pointer, Array

        void addField(const QByteArray& fieldName, // on top level or in struct/union
                       const QByteArray& typeRef, bool isPublic = true);
        quint32 addLocal( const QByteArray& typeRef, QByteArray name = QByteArray() );
        quint32 addArgument(const QByteArray& typeRef, QByteArray name = QByteArray() );
        void setReturnType(const QByteArray& typeRef);
        void setExtern( const QByteArray& origName = QByteArray() );
        void setVararg();

        void add_();
        void and_();
        void call_( const QByteArray& methodRef, int argCount = 0, bool hasRet = false);
        void calli_( const QByteArray& methodRef, int argCount, bool hasRet = false );
        void case_(const CaseLabelList&);
        void castptr_(const QByteArray& typeRef);
        void ceq_();
        void cgt_(bool withUnsigned = false);
        void clt_(bool withUnsigned = false);
        enum ToType { ToI1, ToI2, ToI4, ToI8, ToR4, ToR8, ToU1, ToU2, ToU4, ToU8, ToIp };
        void conv_(ToType);
        void div_(bool withUnsigned = false);
        void disp_();
        void do_();
        void dup_();
        void else_();
        void end_();
        void exit_();
        void goto_(const QByteArray& label);
        void if_();
        void initobj(const QByteArray& typeRef);
        void label_(const QByteArray& name);
        void ldarg_(quint16 arg);
        void ldarga_(quint16 arg);
        void ldc_i4(qint32);
        void ldc_i8(qint64);
        void ldc_r4(double);
        void ldc_r8(double);
        void ldc_obj(const MilObject&);
        void ldelem_(const QByteArray& typeRef);
        void ldelema_(const QByteArray& typeRef);
        void ldfld_(const QByteArray& fieldRef);
        void ldflda_(const QByteArray& fieldRef);
        void ldproc_(const QByteArray& methodRef); // CIL ldftn
        enum IndType { I1, I2, I4, I8, R4, R8, U1, U2, U4, U8, IntPtr };
        void ldind_(IndType);
        void ldloc_(quint16);
        void ldloca_(quint16);
        void ldnull_();
        void ldobj_(const QByteArray& typeRef);
        void ldvar_(const QByteArray& fieldRef); // CIL ldsfld
        void ldvara_(const QByteArray& fieldRef); // CIL ldsflda
        void ldstr_(const QByteArray& str);
        void line_(quint32);
        void loop_();
        void mul_();
        void neg_();
        void newarr_(const QByteArray& typeRef);
        void newvla_(const QByteArray& typeRef); // not in CIL
        void newobj_(const QByteArray& typeRef);
        void not_();
        void or_();
        void pop_();
        void rem_(bool withUnsigned = false);
        void repeat_();
        void ret_(bool hasRetType = false);
        void shl_();
        void shr_(bool withUnsigned = false );
        void sizeof_(const QByteArray& typeRef);
        void starg_(quint16);
        void stelem_(const QByteArray& typeRef);
        void stfld_(const QByteArray& fieldRef);
        void stind_( IndType ); // without Ux
        void stloc_(quint16);
        void stobj_(const QByteArray& typeRef);
        void stvar_(const QByteArray& fieldRef); // CIL stsfld
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
        QList<MilProcedure> d_proc;
        QByteArray d_library, d_origName;
        MilRenderer* d_out;
    };

    class IlAsmRenderer : public MilRenderer
    {
    public:
        IlAsmRenderer(QIODevice*);

        virtual void beginModule(const QByteArray& moduleName,const QString& sourceFile, const MilMetaParams& mp );
        virtual void endModule();

        virtual void addImport( const QByteArray& path, const QByteArray& name );

        virtual void addVariable( const QByteArray& typeRef, QByteArray name );
        virtual void addProcedure(const MilProcedure& method );

        virtual void beginType(const QByteArray& className, bool isPublic, quint8 classKind);
        virtual void endType();
        virtual void addType( const QByteArray& name, bool isPublic, const QByteArray& baseType,
                              quint8 typeKind, quint32 len = 0);

        virtual void addField( const QByteArray& fieldName,
                       const QByteArray& typeRef,
                       bool isPublic = true);
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
}

Q_DECLARE_METATYPE(Mic::MilObject)
Q_DECLARE_METATYPE(Mic::CaseLabelList)

#endif // OBXILEMITTER_H
