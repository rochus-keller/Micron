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

class MicEvaluator
{
public:
    MicEvaluator(AstModel* m, MilEmitter* out):mdl(m),out(out) {}

    // expression:
    bool unaryOp(quint8 op); // Tok_Tilde, Tok_NOT, Tok_Plus, Tok_Minus, Tok_At
    bool binaryOp(quint8 op); // push lhs first

    bool prepareRhs(Type* lhs);
    bool assign(); // move rhs (top) to lhs (top-1)

    // designator:
    bool derefPointer(); // unary op
    bool derefValue(); // unary op
    bool desigField(); // binary op
    bool desigVar(); // unary op
    bool desigIndex(); // binary op
    bool call(int nArgs); // n-ary op, callee top on stack
    bool cast(); // binary op

    void assureTopOnMilStack(bool pop = false); // send current top const to mil stack
    bool dup();

    bool push(const Value&);
    Value pop();
    Value top();

    Type* smallestUIntType(const QVariant& ) const;
    Type* smallestIntType(const QVariant& ) const;

    const QString& getErr() const { return err; }

    static QByteArray toDesig(Declaration*);
    static QByteArray toDesig(Type*);
    static QByteArray dequote(const QByteArray& str);

protected:
    void notOp(Value&);
    Value logicOp(quint8 op, const Value& lhs, const Value& rhs);
    Value arithOp(quint8 op, const Value& lhs, const Value& rhs);
    Value relationOp(quint8 op, const Value& lhs, const Value& rhs);
    Value inOp(const Value& lhs, const Value& rhs);
    void unaryMinusOp(Value&);
    void unaryPlusOp(Value&);
    bool pushMilStack(const Value&);
    void emitRelOp(quint8 op, bool unsig);
    void emitArithOp(quint8 op, bool unsig = false, bool i64 = false);
    void adjustNumType(Type* me, Type* other);
    void callBuiltin(int builtin, int nArgs);
    int addIncDecTmp();

    // builtin implementations
    void PRINT(int nArgs, bool ln);
    void NEW(int nArgs);
    void DISPOSE(int nArgs);
    void INC(int nArgs);
    void DEC(int nArgs);
    void LEN(int nArgs);
    void incdec(int nArgs, bool inc);
    void ASSERT(int nArgs);
    void BITARITH(int op, int nArgs);

private:
    AstModel* mdl;
    MilEmitter* out;
    QString err;
    QList<Value> stack;
};
}

#endif // MICEVALUATOR_H
