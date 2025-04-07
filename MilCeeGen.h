#ifndef MILCEEGEN_H
#define MILCEEGEN_H

#include <Micron/MilAst.h>
#include <QTextStream>

class QIODevice;

namespace Mil
{
    class CeeGen
    {
    public:
        CeeGen(AstModel*);

        bool generate(Declaration* module, QIODevice* header, QIODevice* body);
    protected:
        void visitModule();
        void visitProcedure(Declaration*);
        QByteArray typeRef(Type*) const;
        void procHeader(QTextStream& out, Declaration* proc);
        void parameter(QTextStream& out, Declaration* param);
        void variable(QTextStream& out, Declaration* var);
        void typeDecl(QTextStream& out, Declaration* type);
        void pointerTo(QTextStream& out, Type* type);

    private:
        AstModel* mdl;
        QTextStream hout;
        QTextStream bout;
        Declaration* curMod;
        Declaration* curProc;
    };
}

#endif // MILCEEGEN_H
