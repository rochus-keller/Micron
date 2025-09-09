#ifndef MILCEEGEN_H
#define MILCEEGEN_H

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
#include <QTextStream>

class QIODevice;

namespace Mil
{
    class CeeGen
    {
    public:
        CeeGen(AstModel*);

        bool generate(Declaration* module, QIODevice* header, QIODevice* body = 0);
        static bool requiresBody(Declaration* module);
        static QString genDedication();
    protected:
        void visitModule();
        void visitProcedure(Declaration*);
        void visitMetaDecl(Declaration*);
        QByteArray typeRef(Type*);
        void procHeader(QTextStream& out, Declaration* proc);
        void parameter(QTextStream& out, Declaration* param);
        void variable(QTextStream& out, Declaration* var);
        void typeDecl(QTextStream& out, Declaration* type);
        void pointerTo(QTextStream& out, Type* type);
        void constValue(QTextStream& out, Constant* c, Type *hint);
        void statementSeq(QTextStream& out, Statement* s, int level = 0);
        void expression(QTextStream& out, Expression* e, Type* hint = 0);
        void emitBinOP(QTextStream& out, Expression* e, const char* op);
        void emitRelOP(QTextStream& out, Expression* e, const char* op);
        void emitSoapInit(QTextStream& out, const QByteArray& name, Type* t, int level);
        void emitSoaInit(QTextStream& out, const QByteArray& name, bool nameIsPtr, Type* t, int level);
        Type* deref(Type* t);
        void emitInitializer(Type*);

        inline QByteArray ws(int level)
        {
            curLevel = level;
            return QByteArray((level+1)*4,' ');
        }
    private:
        AstModel* mdl;
        QTextStream hout;
        QTextStream bout;
        Declaration* curMod;
        Declaration* curProc;
        int curLevel;
    };
}

#endif // MILCEEGEN_H
