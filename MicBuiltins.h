#ifndef MICBUILTINS_H
#define MICBUILTINS_H

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

#include "MicAst.h"

namespace Mic
{
class Evaluator;

class Builtins
{
public:
    QString checkArgs(quint8 builtin, ExpList& args, Type** ret, AstModel* mdl);
    static bool requiresLvalue(quint8 builtin, quint8 argNr);

    Builtins(Evaluator*);
    void callBuiltin(quint8 builtin, int nArgs, const RowCol& pos);
    // when callBuiltin, either all args are already on MIL stack, or none (if allArgsConst)

protected:
    int addIncDecTmp(bool isInt64);

    // builtin implementations
    void PRINT(int nArgs, bool ln, const RowCol &pos);
    void NEW(int nArgs, const RowCol &pos);
    void DISPOSE(int nArgs, const RowCol &pos);
    void INC(int nArgs, const RowCol &pos);
    void DEC(int nArgs, const RowCol &pos);
    void LEN(int nArgs, const RowCol &pos);
    void incdec(int nArgs, bool inc, const RowCol &pos);
    void ASSERT(int nArgs, const RowCol &pos);
    void bitarith(int op);
    void bitnot();
    void doCast(const RowCol& pos);
    void doVal(const RowCol& pos);
    void doDefault();
    void doAbs();
    void doFlt();
    void doShiftRight(const RowCol& pos);
    void doShiftLeft(const RowCol &pos);
    void doOrd(const RowCol &pos);
    void doSize(const RowCol &pos);
    void doStrlen(const RowCol& pos);
    void doSig(const RowCol &pos);
    void doUsig(const RowCol &pos);

    void checkNumOfActuals(int nArgs, int min, int max = 0);
    void pushActualsToMilStack(int nArgs, const RowCol& pos);
    void pushToMilStack(Value& v, const RowCol& pos);
    bool allArgsConst(int nArgs);
private:
    Evaluator* ev;
};
}

#endif // MICBUILTINS_H
