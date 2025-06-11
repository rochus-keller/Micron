#ifndef _MILRENDERER_H
#define _MILRENDERER_H

/*
* Copyright 2019-2025 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QByteArray>
#include <QPair>
#include <QList>
#include <QIODevice>
#include <QTextStream>
#include <Micron/MicRowCol.h>

namespace Mil
{
    using Mic::RowCol;

    typedef QPair<QByteArray,QByteArray> Quali; // [module '!'] element
    typedef QPair<Quali,QByteArray> Trident;

    struct EmiTypes // types used in the Emitter interface
    {
        enum Structured { Invalid, Struct, Union, Object, ProcType, MethType, Alias, Pointer, Array, Generic, MaxType };
        enum Basic { Unknown, I1, I2, I4, I8, R4, R8, U1, U2, U4, U8, IntPtr, IPP };
    };

    struct ProcData
    {
        struct Op
        {
            quint8 op;
            QVariant arg;
            Op():op(0){}
            Op(quint8 ilop, const QVariant& arg = QVariant(), quint32 i = 0 ):op(ilop),arg(arg){}
        };

        struct Var
        {
            QByteArray name;
            Quali type;
            quint32 line;
            uint isPublic : 1;
            uint bits : 6; // optional bit width for fields
            Var():isPublic(0),bits(0),line(0) {}
            Var(const Quali& type, const QByteArray& name, quint32 line):
                type(type),name(name),isPublic(0),bits(0),line(line){}
        };

        enum Kind { Invalid, Intrinsic, Normal, Forward, Extern, Inline, Invar, ModuleInit, ProcType, MethType };
        uint kind : 4;
        uint isPublic : 1;
        uint isVararg : 1;
        quint32 endLine;
        QByteArray name;
        QList<Op> body, finally;
        QList<Var> params;
        QList<Var> locals;
        Quali retType;
        QByteArray binding; // if not empty, the first param is receiver
        QByteArray origName; // extern
        ProcData():kind(Invalid),isPublic(0),isVararg(0),endLine(0) {}
    };

    struct MetaParam {
        QByteArray name;
        Quali type;
        bool isConst;
        bool isGeneric;
        MetaParam():isConst(false),isGeneric(false){}
    };
    typedef QList<MetaParam> MetaParams;

    struct ConstrLiteral // the result of a constructor, see ldobj
    {
        Quali typeRef;
        QVariant data; // note that array of byte can be either QByteArray or QVariantList
    };
    typedef QPair<QByteArray,QVariant> FieldData;
    typedef QList<FieldData> RecordLiteral;

    typedef QList<qint64> CaseLabelList;

    class AbstractRenderer
    {
    public:
        virtual void beginModule( const QByteArray& fullName, const QString& sourceFile,
                                  const QByteArrayList& metaParams = QByteArrayList() ) {}
        virtual void endModule() {}

        virtual void addImport( const QByteArray& fullName ) {}

        virtual void addVariable( const Quali& typeRef, QByteArray name,  bool isPublic ) {}
        virtual void addConst(const Quali& typeRef, const QByteArray& name, const QVariant& val ) {}
        virtual void addProcedure(const ProcData& method ) {} // also ProcType

        virtual void beginType(const QByteArray& name, bool isPublic, quint8 typeKind,
                               const Quali& super = Quali() ) {} // only Struct, Union and Object
        virtual void endType() {}
        virtual void addType( const QByteArray& name, bool isPublic, const Quali& baseType,
                              quint8 typeKind, quint32 len = 0) {} // only Alias, Pointer and Array

        virtual void addField( const QByteArray& fieldName, // on top level or in class
                               const Quali& typeRef,
                               bool isPublic = true, quint8 bits = 0 ) {}

        virtual void line(quint32) {}
    };

    class IlAsmRenderer : public AbstractRenderer
    {
    public:
        IlAsmRenderer(QIODevice*, bool renderLineInfo = true);

        virtual void beginModule(const QByteArray& moduleName,const QString& sourceFile, const QByteArrayList& mp );
        virtual void endModule();

        virtual void addImport(const QByteArray& path );

        virtual void addVariable( const Quali& typeRef, QByteArray name,  bool isPublic );
        virtual void addConst(const Quali& typeRef, const QByteArray& name, const QVariant& val );
        virtual void addProcedure(const ProcData& method );

        virtual void beginType(const QByteArray& className, bool isPublic, quint8 classKind,const Quali& super);
        virtual void endType();
        virtual void addType( const QByteArray& name, bool isPublic, const Quali& baseType,
                              quint8 typeKind, quint32 len = 0);

        virtual void addField( const QByteArray& fieldName,
                       const Quali& typeRef,
                       bool isPublic = true, quint8 bits = 0);
        void line(quint32);

        static QString formatDouble(const QVariant& v);

    protected:
        inline QByteArray ws() { return QByteArray(level*2,' '); }
        void render(const ProcData&);
        QByteArray toString(const Quali& q);
        void lineout();

    private:
        enum State { Idle, Module, Struct, Proc } state;
        QTextStream out;
        int level;
        QString source;
        QByteArray d_moduleName;
        quint32 curLine, lastLine;
        bool renderLineInfo;
    };

    class RenderSplitter : public AbstractRenderer
    {
    public:
        RenderSplitter(const QList<AbstractRenderer*>& r):renderer(r) {}

        virtual void beginModule( const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& );
        virtual void endModule();

        virtual void addImport(const QByteArray& path );

        virtual void addVariable( const Quali& typeRef, QByteArray name,  bool isPublic );
        virtual void addConst(const Quali& typeRef, const QByteArray& name, const QVariant& val );
        virtual void addProcedure(const ProcData& method );

        virtual void beginType(const QByteArray& name, bool isPublic, quint8 typeKind,const Quali& super);
        virtual void endType();
        virtual void addType( const QByteArray& name, bool isPublic, const Quali& baseType,
                      quint8 typeKind, quint32 len = 0);

        virtual void addField( const QByteArray& fieldName, // on top level or in class
                       const Quali& typeRef,
                       bool isPublic = true, quint8 bits = 0 );
        void line(quint32);
    private:
        QList<AbstractRenderer*> renderer;
    };

    class AstModel;
    class Declaration;
    class Type;
    class Statement;
    class Expression;

    class IlAstRenderer : public AbstractRenderer
    {
    public:
        IlAstRenderer(AstModel*);
        ~IlAstRenderer();

        bool commit();

        void beginModule( const QByteArray& moduleName, const QString& sourceFile, const QByteArrayList& );
        void endModule();

        void addImport(const QByteArray& path);

        void addVariable(const Quali& typeRef, QByteArray name , bool isPublic);
        void addConst(const Quali& typeRef, const QByteArray& name, const QVariant& val );
        void addProcedure(const ProcData& method );

        void beginType(const QByteArray& name, bool isPublic, quint8 typeKind, const Quali& super);
        void endType();
        void addType( const QByteArray& name, bool isPublic, const Quali& baseType,
                      quint8 typeKind, quint32 len = 0);

        void addField( const QByteArray& fieldName, // on top level or in class
                       const Quali& typeRef,
                       bool isPublic = true, quint8 bits = 0 );

        void line(quint32);

        Declaration* getCurrentModule() const { return module; }
        Type* getCurrentType() const { return type; }

    protected:
        Type* derefType(const Quali& ) const;
        Statement* translateStat(const QList<ProcData::Op>& ops, quint32& pc);
        Expression* translateExpr(const QList<ProcData::Op>& ops, quint32& pc);
        bool expect(const QList<ProcData::Op>& ops, quint32& pc, int op);
        Declaration* derefTrident(const Trident& ) const;
        Declaration* resolve(const Quali&) const;
        void error(const QString&, int pc = -1) const;
        RowCol setline(quint32 line)
        {
            if( line )
                curPos = RowCol(line);
            return curPos;
        }

    private:
        AstModel* mdl;
        Declaration* module;
        Type* type;
        Declaration* curProc;
        RowCol curPos;
        mutable QList<Type*> unresolved;
        mutable bool hasError;
    };
}

Q_DECLARE_METATYPE(Mil::ConstrLiteral)
Q_DECLARE_METATYPE(Mil::CaseLabelList)
Q_DECLARE_METATYPE(Mil::Trident)
Q_DECLARE_METATYPE(Mil::RecordLiteral)


#endif // _MILRENDERER_H
