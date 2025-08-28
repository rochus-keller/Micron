#ifndef MILLVMGEN_H
#define MILLVMGEN_H

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
#include <QTextStream>
#include <QHash>

class QIODevice;

namespace Mil
{
    class LlvmGen
    {
    public:
        LlvmGen(AstModel*);

        bool generate(Declaration* module, QIODevice* out);
        static QString genDedication();

    protected:
        void visitModule();
        void visitProcedure(Declaration*);
        void visitMetaDecl(Declaration*);
        QByteArray typeRef(Type*);
        QByteArray structTypeRef(Type*);
        void procHeader(Declaration* proc);
        void parameter(Declaration* param, bool first = false);
        void typeDecl(Declaration* type);
        void constDecl(Declaration* decl);
        void constValue(Constant* c, Type* t = 0);
        void statementSeq(Statement* s, int level = 0);
        QByteArray expression(Expression* e, int level = 0);
        QByteArray emitBinOp(Expression* e, const char* op, int level);
        QByteArray emitRelOp(Expression* e, const char* op, const char* signedOp = 0, int level = 0);
        QByteArray emitCall(Expression* e, int level);
        void emitSoapInit(const QByteArray& name, Type* t, int level);
        void emitInitializer(Type*);
        Type* deref(Type* t);
        QByteArray nextTemp();
        QByteArray nextLabel();
        void resetCounters();
        QByteArray getLlvmType(Type* t);
        QByteArray getLlvmIntType(Type* t);
        QByteArray getLlvmFloatType(Type* t);
        void declareBuiltins();
        QByteArray mangle(const QByteArray& name);
        QByteArray qualident(Declaration* d);
        void collectStructTypes(Declaration* module);
        void declareStructType(Type* t);
        QByteArray getConstant(Type* t, qint64 val = 0);
        QByteArray emitLoad(Type* t, const QByteArray& ptr);
        void emitStore(const QByteArray& val, const QByteArray& ptr);
        QByteArray emitGetElementPtr(Type* baseType, const QByteArray& ptr, const QByteArray& idx);
        QByteArray emitCast(const QByteArray& val, Type* from, Type* to);
        QByteArray allocaVar(Type* t, const QByteArray& name = QByteArray());
        
    private:
        AstModel* mdl;
        QTextStream out;
        Declaration* curMod;
        Declaration* curProc;
        int tempCount;
        int labelCount;
        QHash<Declaration*, QByteArray> locals;
        QHash<Declaration*, QByteArray> params;
        QHash<Declaration*, QByteArray> structTypes;
        QHash<Type*, QByteArray> anonStructTypes;
        int structTypeCount;
    };
}

#endif // MILLVMGEN_H
