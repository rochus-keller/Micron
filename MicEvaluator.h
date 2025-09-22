#ifndef MICEVALUATOR_H
#define MICEVALUATOR_H

/*
** Copyright (C) 2024 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the Micron language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include <Micron/MicAst.h>

namespace Mil {
class Emitter;
}
namespace Mic
{
typedef QPair<QByteArray,QByteArray> Qualident;

class Evaluator
{
public:
    Evaluator(AstModel* m, Mil::Emitter* out):mdl(m),out(out) {}

    void pushCurProc(Declaration*d) { curProcs.push_back(d); }
    void popCurProc() { curProcs.pop_back(); }

    bool evaluate(Expression*, bool assureOnMilStack = false);

    bool prepareRhs(Type* lhs, bool assig, const RowCol &pos);
    bool assign(const RowCol &pos); // move rhs (top) to lhs (top-1)
    bool assign(Expression* lhs, Expression* rhs , const RowCol &pos);

    Value pop();
    Value top();

    Type* smallestUIntType(const QVariant& ) const;
    Type* smallestIntType(const QVariant& ) const;

    const QString& getErr() const { return err; }

    static Qualident toQuali(Declaration*, Declaration* module);
    static Qualident toQuali(Type*, Declaration* module);
    Qualident toQuali(Type*);
    Qualident toQuali(Declaration*);
    static QByteArray dequote(const QByteArray& str);

protected:
    bool recursiveRun(Expression*);
    void constructor(Expression*);
    void recurseConstConstructor(Expression*);

    // assign
    bool stind(Expression* lhs, Expression* rhs, const RowCol &pos);
    bool stelem( Expression* lhs, Expression* rhs, const RowCol &pos );
    bool stfld( Expression* lhs, Expression* rhs, const RowCol &pos );

    // expression:
    bool unaryOp(quint8 op, const RowCol& pos); // Tok_Tilde, Tok_NOT, Tok_Plus, Tok_Minus, Tok_At
    bool binaryOp(quint8 op, const RowCol &pos); // push lhs first
    void assureTopOnMilStack(bool pop, const RowCol &pos); // send current top const to mil stack
    void shortCircuitAnd(Expression*);
    void shortCircuitOr(Expression*);

    // designator:
    bool derefPointer(bool byVal); // unary op
    bool desigField(Declaration* field, bool byVal, const RowCol &pos); // binary op
    bool desigIndex(bool byVal, const RowCol &pos); // binary op
    bool call(int nArgs, const RowCol &pos); // n-ary op, callee top on stack
    bool castPtr(Type* to, const RowCol &pos); // unary op
    bool castNum(Type* to, const RowCol &pos); // unary op
    bool desigVar(bool byVal, const RowCol &pos); // unary op
    bool derefValue(); // unary op
    void assureTopIsValue();

    void notOp(Value&, const RowCol &pos);
    Value logicOp(quint8 op, const Value& lhs, const Value& rhs, const RowCol &pos);
    Value arithOp(quint8 op, const Value& lhs, const Value& rhs, const RowCol &pos);
    Value relationOp(quint8 op, const Value& lhs, const Value& rhs, const RowCol &pos);
    Value inOp(const Value& lhs, const Value& rhs, const RowCol &pos);
    Value isOp(const Value& lhs, const Value& rhs, const RowCol &pos);
    void unaryMinusOp(Value&, const RowCol &pos);
    void unaryPlusOp(Value&, const RowCol &pos);
    bool pushMilStack(const Value &, const RowCol &pos);
    void emitRelOp(quint8 op, bool unsig, const RowCol &pos);
    void emitArithOp(quint8 op, bool unsig, bool i64, const RowCol& pos);
    void adjustNumType(Type* me, Type* other);

private:
    AstModel* mdl;
    Mil::Emitter* out;
    QString err;
    QList<Value> stack;
    friend class Builtins;
    QList<Declaration*> curProcs;
};
}

Q_DECLARE_METATYPE(Mic::Qualident)

#endif // MICEVALUATOR_H
