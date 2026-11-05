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

#include "MicComptime.h"
#include <Micron/MilAst.h>

using namespace Mic;

Comptime::Comptime(Mil::AstModel* m):model(m),interp(m)
{
}

bool Comptime::run(Declaration* proc, const QList<Value>& args, Value& result, QString& err)
{
    if( proc == 0 || model == 0 )
    {
        err = "procedure body is not available";
        return false;
    }
    if( proc->kind == Declaration::ForwardDecl )
        proc = proc->deforward();
    if( proc == 0 || proc->kind != Declaration::Procedure )
    {
        err = "procedure body is not available";
        return false;
    }

    Declaration* module = proc->getModule();
    const Qualident q = Evaluator::qualiOfDecl(proc, module);
    Mil::Quali milName = q;
    if( milName.first.isEmpty() )
        milName.first = module->data.value<ModuleData>().fullName;
    Mil::Declaration* milProc = model->resolve(milName);
    if( milProc == 0 )
    {
        err = "procedure body is not available";
        return false;
    }

    QList<QVariant> milArgs;
    foreach( const Value& arg, args )
    {
        if( arg.type && arg.type->kind == Type::StrLit )
            milArgs << Evaluator::dequote(arg.val.toByteArray());
        else
            milArgs << arg.val;
    }

    QVariant value;
    if( !interp.evalComptime(milProc, milArgs, value, err) )
        return false;
    result.mode = Value::Const;
    result.type = proc->getType();
    result.val = value;
    return true;
}
