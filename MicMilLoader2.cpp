/*
* Copyright 2019-2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MicMilLoader2.h"
#include "MicAtom.h"
#include "MilOps.h"
#include "MilParser2.h"
#include "MilLexer.h"
#include "MilValidator.h"
#include <QFile>
#include <QtDebug>
using namespace Mic;
using namespace Mil;

class Lex : public Scanner2
{
public:
    Lexer lex;
    Token next()
    {
        return lex.nextToken();
    }

    Token peek(int offset)
    {
        return lex.peekToken(offset);
    }

    QString sourcePath() const
    {
        return lex.getSourcePath();
    }
};

MilLoader2::MilLoader2()
{

}

Declaration* MilLoader2::loadFromFile(const QString& file)
{
    QFile f(file);
    if( !f.open(QIODevice::ReadOnly) )
        return 0;
    else
        return loadFromFile(&f, file);
}

Declaration *MilLoader2::loadFromFile(QIODevice * in, const QString &path)
{
    Lex lex;
    lex.lex.setStream(in, path);
    Parser2 p(&mdl, &lex, this);
    // qDebug() << "**** parsing" << path;
    Declaration* module = 0;
    if( p.parseModule() ) // we only parse one module here
    {
        if( !p.errors.isEmpty() )
        {
            foreach( const Parser2::Error& e, p.errors )
                qCritical() << e.path << e.pos.d_row << e.pos.d_col << e.msg;
            p.errors.clear();
        }else
        {
            module = p.takeModule();
            // qDebug() << "module" << module->name;
            Validator v(&mdl);
            if( !v.validate(module) )
            {
                foreach( const Validator::Error& e, v.errors )
                    qCritical() << e.where << e.pc << e.msg;
                v.errors.clear();
                delete module;
                module = 0;
            }
            if( module && !mdl.addModule(module) )
            {
                delete module;
                module = 0;
            }
        }
    }
    return 0;
}

static void visitImports(AstModel* loader, Declaration* top, QList<Declaration*>& res )
{
    Declaration* sub = top->subs;
    while(sub)
    {
        if( sub->kind == Declaration::Import && !res.contains(sub->imported) )
        {
            res.append(sub->imported);
            visitImports(loader, sub->imported, res);
        }
        sub = sub->next;
    }
}

QList<Declaration*> MilLoader2::getModulesInDependencyOrder()
{
    QList<Declaration*> res;
    DeclList modules = mdl.getModules();

    for( int i = 0; i < modules.size(); i++ )
    {
        visitImports(&mdl, modules[i], res);
        if( !res.contains(modules[i]) )
            res << modules[i];
    }
    return res;
}



Declaration*MilLoader2::loadModule(const Import& imp)
{
    // TODO in future parse and load mil file if not already here, using library search paths
    return mdl.findModuleByName(imp.moduleName);
}
