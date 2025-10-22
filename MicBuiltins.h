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
    void callBuiltin(quint8 builtin, int nArgs);

protected:
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
    void bitarith(int op);
    void bitnot();
    void doSigned();
    void doUnsigned();
    void doDefault();
    void doAbs();
    void doFlt();
    void doShiftRight();
    void doShiftLeft();

    void checkNumOfActuals(int nArgs, int min, int max = 0);
private:
    Evaluator* ev;
};
}

#endif // MICBUILTINS_H
