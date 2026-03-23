#ifndef MILVMCODE2_H
#define MILVMCODE2_H

/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <Micron/MilVmCode.h>
#include <QTextStream>
#include <QSet>

class QIODevice;

namespace Mil
{
namespace Vm
{
    class Code2 : public Code
    {
    public:
        Code2(AstModel*, quint8 pointerWidth, quint8 stackAlignment);

        void setReverseArguments(bool b) { reverseArguments = b; }
    protected:
        bool translateStatSeq(Procedure& proc, Statement* s);
        bool translateStatExpr(Procedure &proc, Statement *s);
        bool expression(Procedure& proc, Expression* e);
        void pushArgs(Procedure& proc, const QList<Expression*>& args);
    private:
        bool reverseArguments; // only true arguments, not the function or methref in calli and callmi
    };
}
}

#endif // MILVMCODE2_H
