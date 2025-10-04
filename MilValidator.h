   #ifndef MILVALIDATOR_H
#define MILVALIDATOR_H

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

namespace Mil
{
    class Validator
    {
    public:
        Validator(AstModel* m);

        bool validate(Declaration* module);

        struct Error {
            QString msg;
            QByteArray where;
            quint32 pc;
            Error():pc(0){}
        };
        QList<Error> errors;

        static Type* tokToBasicType(AstModel* mdl, int t);

    protected:
        Type* deref(Type*);
        Type* deref(Declaration*);
        void visitProcedure(Declaration* proc);
        void visitStatSeq(Statement* stat);
        Statement* visitIfThenElse(Statement* stat);
        void visitLoop(Statement* stat);
        void visitRepeat(Statement* stat);
        Statement* visitSwitch(Statement* stat);
        void visitWhile(Statement* stat);
        Statement* nextStat(Statement* stat);
        Expression* visitExpr(Expression*);
        void error(Declaration *d, const QString&);
        bool expectN(quint32 n, Expression*);
        bool expectN(quint32 n, Statement*);
        Expression* stackAt(int) const;
        Expression* eatStack(quint32 n);
        Type* toType(Constant* c);
        bool equal(Type* lhs, Type* rhs);
        bool assigCompat(Type* lhs, Type* rhs);
        bool assigCompat(Type* lhs, Expression* rhs);
        bool assigCompat(Type* lhs, Declaration* rhs);
        bool checkIfObjectInit(Type* t);
        bool checkIfPointerInit(Type* t);
        int findLabel(const char* name) const;

    private:
        AstModel* mdl;
        Declaration* curMod;
        Declaration* curProc;
        QList<Expression*> stack;
        QList<Expression*> newExprs;
        typedef QList<Statement*> StatList;
        StatList loopStack, blockStack;

        struct NamePos
        {
            Statement* name;
            StatList pos;
            NamePos(Statement* name, const StatList& pos):name(name),pos(pos){}
        };
        QList<NamePos> gotos, labels;

        quint32 pc;
        bool needsPointerInit;
    };
}

#endif // MILVALIDATOR_H
