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

#include "MilProject.h"
#include "MilCeeGen.h"
#include <QDir>
#include <QElapsedTimer>
#include <QtDebug>
#include <QFile>
#include "MilParser2.h"
#include "MilLexer.h"
#include "MilValidator.h"
#include "MilInterpreter.h"
#include "MilVmOakwood.h"
using namespace Mil;

Project::Project(AstModel* mdl):mdl(mdl)
{
    Q_ASSERT(mdl);
}

Project::~Project()
{
    clear();
}

void Project::clear()
{

}

void Project::setFiles(const QStringList& files)
{
    clear();
    allMilFiles << ":/runtime/MIC+.mil";
    allMilFiles << files;
}

static QStringList collectFiles( const QDir& dir, const QStringList& suffix )
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ), suffix );

    files = dir.entryList( suffix, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append(dir.absoluteFilePath(f));
    }
    return res;
}

void Project::collectFilesFrom(const QString& rootPath)
{
    clear();
    allMilFiles << ":/runtime/MIC+.mil";
    allMilFiles << collectFiles(rootPath, QStringList() << "*.mil");
}

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

bool Project::parse()
{
    int ok = 0;
    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, allMilFiles )
    {
        Lex lex;
        lex.lex.setStream(file);
        Parser2 p(mdl, &lex, this);
        qDebug() << "**** parsing" << file;
        bool errorsFound = false;
        int parsed = 0;
        while( p.parseModule() ) // TODO: skip already parsed modules
        {
            parsed++;
            if( !p.errors.isEmpty() )
            {
                foreach( const Parser2::Error& e, p.errors )
                    qCritical() << e.path << e.pos.d_row << e.pos.d_col << e.msg;
                p.errors.clear();
                errorsFound = true;
            }else
            {
                Declaration* module = p.takeModule();
                qDebug() << "module" << module->name;
                Validator v(mdl);
                if( !v.validate(module) )
                {
                    foreach( const Validator::Error& e, v.errors )
                        qCritical() << e.where << e.pc << e.msg;
                    v.errors.clear();
                    errorsFound = true;
                    delete module;
                    module = 0;
                }
                if( module && !mdl->addModule(module) )
                {
                    delete module;
                    module = 0;
                }
            }
        }
        if( !p.errors.isEmpty() )
        {
            foreach( const Parser2::Error& e, p.errors )
                qCritical() << e.path << e.pos.d_row << e.pos.d_col << e.msg;
            errorsFound = true;
        }
        if( parsed && !errorsFound )
            ok++;
    }
    qDebug() << "#### finished with" << ok << "files ok of total" << allMilFiles.size() << "files" << "in" <<
                timer.elapsed() << " [ms]";
    return ok == allMilFiles.size();
}

void Project::generateC()
{
    foreach( Declaration* module, mdl->getModules() )
    {
        CeeGen cg(mdl);
        QFile header( escapeFilename(module->name) + ".h");
        header.open(QFile::WriteOnly);
        QFile* body = 0;
        QFile b( escapeFilename(module->name) + ".c");
        module->nobody = !CeeGen::requiresBody(module);
        if( !module->nobody )
        {
            b.open(QFile::WriteOnly);
            body = &b;
        }

        cg.generate(module, &header, body);
    }
}

void Project::interpret(bool dump)
{
    Interpreter r(mdl);

    VmOakwood::addTo(&r);

    mdl->calcMemoryLayouts(sizeof(void*), 8);

    foreach( Declaration* module, mdl->getModules() )
    {
        if( !r.precompile(module) )
            return;
    }
    if( false ) // dump )
    {
        QTextStream out(stdout);
        r.dumpAll(out);
    }
    foreach( Declaration* module, mdl->getModules() )
    {
        if( !r.run(module) )
            return; // TODO: error handling
    }
}

void Project::dumpMil()
{
    foreach( Declaration* module, mdl->getModules() )
    {
        // TODO
    }
}

Declaration*Project::loadModule(const Import& imp)
{
    // we assume here that all modules were already loaded and parsed
    return mdl->findModuleByName(imp.moduleName);
}

