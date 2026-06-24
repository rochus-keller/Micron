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

Project::Project(AstModel* mdl):mdl(mdl),haveOakwood(false)
{
    Q_ASSERT(mdl);
}

Project::~Project()
{
    clear();
}

void Project::clear()
{
    mdl->clear();
    moduleFiles.clear();
}

void Project::setFiles(const QStringList& files)
{
    clear();
    moduleFiles << ModuleFile(":/runtime/MIC+.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("MIC$"),0) );
#if 0
    // no, those are expected to be part of the MIL files set
    if( haveOakwood )
    {
        moduleFiles << ModuleFile(":/oakwood/mil/Args.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("Args"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/Files.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("Files"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/In.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("In"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/Input.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("Input"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/Math.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("Math"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/MathL.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("MathL"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/Out.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("Out"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/Screen.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("Screen"),0) );
        moduleFiles << ModuleFile(":/oakwood/mil/Strings.mil", ModuleFile::Mods() << ModuleFile::Mod(Token::getSymbol("Strings"),0) );
    }
#endif
    foreach( const QString& path, files )
    {
        const QByteArrayList names = Lexer::isMilModule(path);
        if( names.isEmpty() )
            qCritical() << "Invalid MIL file:" << path;
        else // TODO: check for name uniqness
            moduleFiles << ModuleFile(path, names);
    }
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
    setFiles(collectFiles(rootPath, QStringList() << "*.mil"));
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
    int ok = 0, all = 0;;
    QElapsedTimer timer;
    timer.start();
    for( int i = 0; i < moduleFiles.size(); i++ )
    {
        Import imp;
        for( int j = 0; j < moduleFiles[i].mods.size(); j++ )
        {
            all++;
            imp.moduleName = moduleFiles[i].mods[j].first;
            Declaration* d = loadModule(imp);
            if( d && !d->hasErrors )
                ok++;
        }
    }
    qDebug() << "#### finished with" << ok << "files ok of total" << all << "files" << "in" <<
                timer.elapsed() << " [ms]";
    return ok == all;
}

void Project::generateC()
{
    foreach( Declaration* module, mdl->getModules() )
        module->nobody = !CeeGen::requiresBody(module);
    foreach( Declaration* module, mdl->getModules() )
    {
        if( module->generic )
            continue;
        CeeGen cg(mdl);
        QFile header( CeeGen::escapeFilename(module->name) + ".h");
        header.open(QFile::WriteOnly);
        QFile* body = 0;
        QFile b( CeeGen::escapeFilename(module->name) + ".c");
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

    if( haveOakwood )
#ifdef _MIC_HAVE_SCREEN_
        Mil::VmOakwood::addTo(&r, true);
#else
        Mil::VmOakwood::addTo(&r, false);
#endif

    mdl->calcMemoryLayouts(sizeof(void*), 8);

    if( !r.compile() )
        return;

    if( dump )
    {
        QTextStream out(stdout);
        r.dumpAll(out);
    }else
        r.run();
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
    ModuleFile* mf = findByName(imp.moduleName);
    if( mf == 0 )
        return 0;

    ModuleFile::Mod* m = mf->findByName(imp.moduleName);

    if( m->second )
        return m->second;

    Declaration* module = mdl->findModuleByName(imp.moduleName);
    if( module )
        return module; // found in importer list

    Lex lex;
    lex.lex.setStream(mf->path);
    Parser2 p(mdl, &lex, this);
    qDebug() << "**** parsing file" << mf->path;
    int parsed = 0;
    while( p.parseModule() )
    {
        parsed++;
        module = p.takeModule();
        if( !p.errors.isEmpty() )
        {
            foreach( const Parser2::Error& e, p.errors )
                qCritical() << e.path << e.pos.d_row << e.pos.d_col << e.msg;
            p.errors.clear();
            module->hasErrors = true;
        }else
        {
            Validator v(mdl);
            if( !v.validate(module) )
            {
                foreach( const Validator::Error& e, v.errors )
                    qCritical() << e.where << e.pc << e.msg;
                v.errors.clear();
                module->hasErrors = true;
            }
        }
        mdl->popImporter(module);
        if( module && !mdl->addModule(module) )
            delete module;
        else
        {
            m = mf->findByName(module->name);
            if(m)
                m->second = module;
        }
    }
    if( !p.errors.isEmpty() )
    {
        foreach( const Parser2::Error& e, p.errors )
            qCritical() << e.path << e.pos.d_row << e.pos.d_col << e.msg;
    }

    return mdl->findModuleByName(imp.moduleName);
}

Project::ModuleFile *Project::findByName(const QByteArray & name)
{
    for( int i = 0; i < moduleFiles.size(); i++ )
        for( int j = 0; j < moduleFiles[i].mods.size(); j++ )
            if( moduleFiles[i].findByName(name) )
                return &moduleFiles[i];
    return 0;
}

Project::ModuleFile::ModuleFile(const QString &path, const QByteArrayList & names):path(path)
{
    foreach( const QByteArray& name, names )
        mods << Mod(name,0);
}

Project::ModuleFile::Mod *Project::ModuleFile::findByName(const QByteArray &name)
{
    for( int j = 0; j < mods.size(); j++ )
        if( mods[j].first.constData() == name.constData() )
            return &mods[j];
    return 0;
}
