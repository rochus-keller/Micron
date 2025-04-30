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

namespace Mic
{
class MilEmitter;

typedef QPair<QByteArray,QByteArray> Qualident;

class Evaluator
{
public:
    Evaluator(AstModel* m, MilEmitter* out):mdl(m),out(out) {}

    bool evaluate(Expression*, bool assureOnMilStack = false);

    bool prepareRhs(Type* lhs, bool assig = false);
    bool assign(); // move rhs (top) to lhs (top-1)

    Value pop();
    Value top();

    Type* smallestUIntType(const QVariant& ) const;
    Type* smallestIntType(const QVariant& ) const;

    const QString& getErr() const { return err; }

    Qualident toQuali(Declaration*);
    Qualident toQuali(Type*);
    static QByteArray dequote(const QByteArray& str);

protected:
    bool recursiveRun(Expression*);
    void constructor(Expression*);
    void recurseConstConstructor(Expression*);

    // expression:
    bool unaryOp(quint8 op); // Tok_Tilde, Tok_NOT, Tok_Plus, Tok_Minus, Tok_At
    bool binaryOp(quint8 op); // push lhs first
    void assureTopOnMilStack(bool pop = false); // send current top const to mil stack
    void shortCircuitAnd(Expression*);
    void shortCircuitOr(Expression*);

    // designator:
    bool derefPointer(); // unary op
    bool desigField(Declaration* field, bool byVal); // binary op
    bool desigIndex(bool byVal); // binary op
    bool call(int nArgs); // n-ary op, callee top on stack
    bool castPtr(Type* to); // unary op
    bool castNum(Type* to); // unary op
    bool desigVar(bool byVal); // unary op
    bool derefValue(); // unary op
    void assureTopIsValue();

    void notOp(Value&);
    Value logicOp(quint8 op, const Value& lhs, const Value& rhs);
    Value arithOp(quint8 op, const Value& lhs, const Value& rhs);
    Value relationOp(quint8 op, const Value& lhs, const Value& rhs);
    Value inOp(const Value& lhs, const Value& rhs);
    Value isOp(const Value& lhs, const Value& rhs);
    void unaryMinusOp(Value&);
    void unaryPlusOp(Value&);
    bool pushMilStack(const Value&);
    void emitRelOp(quint8 op, bool unsig);
    void emitArithOp(quint8 op, bool unsig = false, bool i64 = false);
    void adjustNumType(Type* me, Type* other);


private:
    AstModel* mdl;
    MilEmitter* out;
    QString err;
    QList<Value> stack;
    friend class Builtins;
};
}

Q_DECLARE_METATYPE(Mic::Qualident)

#endif // MICEVALUATOR_H
