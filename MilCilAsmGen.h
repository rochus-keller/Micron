#ifndef MILCILASMGEN_H
#define MILCILASMGEN_H

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
#include <QStack>

class QIODevice;

namespace Mil
{
    class CilAsmGen
    {
    public:
        CilAsmGen(AstModel*);

        bool generate(Declaration* module, QIODevice* out);
        static QString genDedication();
        
    protected:
        void visitModule();
        void visitProcedure(Declaration*);
        void visitMetaDecl(Declaration*); // For object metadata/vtables
        QByteArray typeRef(Type*);
        QByteArray typeToken(Type*); // For type tokens in instructions
        void procHeader(Declaration* proc);
        void parameter(Declaration* param, int index);
        void field(Declaration* field);
        void typeDecl(Declaration* type);
        void constValue(Constant* c);
        void statementSeq(Statement* s);
        void expression(Expression* e);
        void emitBinOp(Expression* e, const char* op);
        void emitRelOp(Expression* e, const char* op);
        void emitConv(Expression* e);
        void emitCall(Expression* e);
        void emitInitializer(Type*);
        void emitSoapInit(const QByteArray& name, Type* t, bool isStatic);
        Type* deref(Type* t);
        
        // Helper methods for CIL specifics
        void emitLabel(const QByteArray& label);
        void emitLocal(Declaration* local);
        QByteArray mangledName(Declaration* d) { return qualident(d); }
        QByteArray qualident(Declaration* d);
        void emitMaxStack();
        
    private:
        AstModel* mdl;
        QTextStream out;
        Declaration* curMod;
        Declaration* curProc;
        int labelCounter;
        int maxStack;
        int currentStack;
        QStack<int> stackStates;
    };
}

#endif // MILCILASMGEN_H
