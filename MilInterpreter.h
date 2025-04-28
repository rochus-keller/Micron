#ifndef MILINTERPRETER_H
#define MILINTERPRETER_H

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

class QTextStream;

namespace Mil
{
    class Interpreter
    {
    public:
        Interpreter(AstModel*);
        ~Interpreter();

        typedef bool (*FfiProc)(void* args, void* ret);
        static qint32 toI4(void* args, int off);
        static qint64 toI8(void* args, int off);
        static float toR4(void* args, int off);
        static double toR8(void* args, int off);
        static void* toP(void* args, int off);
        static void retI4(void* ret, qint32 val);
        static void retI8(void* ret, qint64 val);
        static void retR4(void* ret, float val);
        static void retR8(void* ret, double val);
        static void retP(void* ret, void* val);
        static int stackAligned(int off);

        // proc is either a procedure or a module
        bool precompile(Declaration* proc);
        bool dumpProc(QTextStream& out, Declaration* proc);
        bool dumpModule(QTextStream& out, Declaration* module);
        bool dumpAll(QTextStream& out);
        bool run(Declaration* proc);

    private:
        class Imp;
        Imp* imp;
    };
}

#endif // MILINTERPRETER_H
