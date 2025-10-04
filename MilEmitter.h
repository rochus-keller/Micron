#ifndef MILEMITTER_H
#define MILEMITTER_H

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

#include <Micron/MilRenderer.h>

namespace Mil
{
    // this is a simplified backend API for frontend implementation so that the frontend
    // doesn't have to care about the MIL AST
    class Emitter
    {
    public:
        enum DbgInfo { None, RowsOnly, RowsAndCols };
        Emitter(AbstractRenderer*, DbgInfo = None);

        void beginModule( const QByteArray& fullName, const QString& sourceFile, const RowCol&, const QByteArrayList& metaParamNames = QByteArrayList() );
        void endModule(const RowCol&);

        void addImport(const QByteArray& fullName, const RowCol&, bool reverse = false);

        void addVariable( const Quali& typeRef, QByteArray name, const RowCol&, bool isPublic = true );

        void addConst(const Quali& typeRef, const QByteArray& name, const RowCol&, const QVariant& val ); // always public

        void beginProc(const QByteArray& procName, const RowCol&, bool isPublic = true, quint8 kind = ProcData::Normal,
                       const QByteArray& objectType = QByteArray() );
        void toFinallySection(bool, const RowCol&);
        void endProc(const RowCol&);

        void beginType(const QByteArray& name, const RowCol&, bool isPublic = true, quint8 typeKind = EmiTypes::Struct,
                       const Quali& super = Quali() );
            // use for Struct, Union, Object, ProcType, MethType
            // supports addField, addArgument, setReturnType
        void endType();

        void addType( const QByteArray& name, const RowCol&, bool isPublic, const Quali& baseType,
                      quint8 typeKind = EmiTypes::Alias, quint32 len = 0);
            // use for Alias, Pointer, Array, Interfaces

        void addField(const QByteArray& fieldName, const RowCol&, // on top level or in struct/union
                       const Quali& typeRef, bool isPublic = true, quint8 bits = 0);
        quint32 addLocal( const Quali& typeRef, QByteArray name = QByteArray(), const RowCol& = RowCol() );
        quint32 addArgument(const Quali& typeRef, QByteArray name = QByteArray(), const RowCol& = RowCol() ); // SELF is explicit
        void setReturnType(const Quali& typeRef);
        void setOrigName( const QByteArray& origName = QByteArray() ); // in case of foreign

        static QByteArray typeSymbol1(EmiTypes::Basic);
        static QByteArray typeSymbol2(EmiTypes::Basic);
        static EmiTypes::Basic fromSymbol(const QByteArray&);
        static bool equals(const QByteArray&, EmiTypes::Basic);
        static QByteArray toString(const Quali&);
        static QByteArray toString(const Trident&);

        void add_();
        void abs_();
        void and_();
        void call_( const Quali& methodRef, int argCount = 0, bool hasRet = false);
        void calli_( const Quali& methodRef, int argCount, bool hasRet = false );
        void callmi_(const Quali& methodRef, int argCount, bool hasRet = false );
        void callinst_( const Trident& methodRef, int argCount, bool hasRet = false );
        void callvirt_( const Trident& methodRef, int argCount, bool hasRet = false );
        void case_(const CaseLabelList&);
        void castptr_(const Quali& typeRef);
        void ceq_();
        void cgt_(bool withUnsigned = false);
        void clt_(bool withUnsigned = false);
        void conv_(EmiTypes::Basic);
        void div_(bool withUnsigned = false);
        void free_();
        void do_();
        void dup_();
        void else_();
        void end_();
        void exit_();
        void goto_(const QByteArray& label);
        void if_();
        void iif_();
        void initobj(const Quali& typeRef);
        void isinst_(const Quali& typeRef);
        void label_(const QByteArray& name);
        void ldarg_(quint16 arg);
        void ldarga_(quint16 arg);
        void ldc_i4(qint32);
        void ldc_i8(qint64);
        void ldc_r4(double);
        void ldc_r8(double);
        void ldc_obj(const ConstrLiteral&);
        void ldelem_(const Quali& typeRef);
        void ldelema_(const Quali& typeRef);
        void ldfld_(const Trident& fieldRef);
        void ldflda_(const Trident& fieldRef);
        void ldmeth_(const Trident& methodRef); // CIL ldvirtftn, returns methref
        void lditfmeth_(const Trident& methodRef);
        void ldproc_(const Quali& methodRef); // CIL ldftn
        void ldind_(const Quali& typeRef); // was CIL ldobj
        void ldind_(EmiTypes::Basic);
        void ldloc_(quint16);
        void ldloca_(quint16);
        void ldnull_();
        void ldvar_(const Quali& memberRef); // CIL ldsfld
        void ldvara_(const Quali& memberRef); // CIL ldsflda
        void ldstr_(const QByteArray& str);
        void line_(const Mic::RowCol &pos);
        void loop_();
        void mul_();
        void neg_();
        void newarr_(const Quali& typeRef);
        void newvla_(const Quali& typeRef); // not in CIL
        void newobj_(const Quali& typeRef);
        void nop_();
        void not_();
        void or_();
        void pop_();
        void ptroff_(const Quali& typeRef);
        void rem_(bool withUnsigned = false);
        void repeat_();
        void ret_(bool pop);
        void shl_();
        void shr_(bool withUnsigned = false );
        void sizeof_(const Quali& typeRef);
        void starg_(quint16);
        void stelem_(const Quali& typeRef);
        void stfld_(const Trident& fieldRef);
        void stind_( EmiTypes::Basic );
        void stind_(const Quali& typeRef); // was CIL stobj
        void stloc_(quint16);
        void strcpy_();
        void stvar_(const Quali& memberRef); // CIL stsfld
        void sub_();
        void switch_();
        void then_();
        void until_();
        void while_();
        void xor_();
    protected:
        void delta(int d);
        void lineout(const RowCol&);
        quint32 lineset(const RowCol&);
    private:
        quint8 d_typeKind;
        quint16 d_stackDepth;
        quint16 d_maxStackDepth;
        QList<ProcData> d_proc; // proc stack
        QList<ProcData::Op>* ops;
        QByteArray d_library, d_origName;
        AbstractRenderer* d_out;
        quint32 lastLine, firstLine;
        DbgInfo dbgInfo;
    };
}

#endif // MILEMITTER_H
