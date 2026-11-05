#ifndef MICCOMPTIME_H
#define MICCOMPTIME_H

/*
* Copyright (C) 2026 Rochus Keller (me@rochus-keller.ch)
*
* This file is part of the Micron language project.
*
* GNU Lesser General Public License Usage
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include "MicEvaluator.h"
#include <Micron/MilInterpreter.h>

namespace Mic
{
    class Comptime : public Interpreter
    {
    public:
        explicit Comptime(Mil::AstModel* model);
        bool run(Declaration* proc, const QList<Value>& args, Value& result, QString& err);

    private:
        Mil::AstModel* model;
        Mil::Interpreter interp;
    };
}

#endif
