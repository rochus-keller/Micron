/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "MicEiGen.h"
#include "MicMilLoader.h"
#include "MicMilLoader2.h"
#if 0
#include "MicMilInterpreter.h"
#endif
#include "MicAst.h"
#include "MilProject.h"

#include <QCoreApplication>
#include <QFile>
#include <QStringList>
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QElapsedTimer>
#include <MicPpLexer.h>
#include <MicParser2.h>
#include <MicMilEmitter.h>
#include <MilLexer.h>
#include <MilParser.h>
#include <MilToken.h>
#include <QBuffer>
#include <QCommandLineParser>
#include <QTemporaryFile>

#define USE_MILLOADER2

class Lex2 : public Mic::Scanner2
{
public:
    QString sourcePath;
    Mic::PpLexer lex;
    Mic::Token next()
    {
        return lex.nextToken();
    }
    Mic::Token peek(int offset)
    {
        return lex.peekToken(offset);
    }
    QString source() const { return sourcePath; }
};

static QByteArray getModuleName(const QString& file)
{
    Mic::Lexer lex;
    lex.setStream(file);
    Mic::Token t = lex.nextToken();
    while( t.isValid() && t.d_tokenType != Mic::Tok_MODULE )
        t = lex.nextToken();
    if( t.d_tokenType == Mic::Tok_MODULE )
    {
        t = lex.nextToken();
        if( t.d_tokenType == Mic::Tok_ident )
            return t.d_val;
    }
    return QByteArray();
}

struct ModuleSlot
{
    Mic::Import imp;
    QString file;
    Mic::Declaration* decl;
    ModuleSlot():decl(0) {}
    ModuleSlot( const Mic::Import& i, const QString& f, Mic::Declaration* d):imp(i),file(f),decl(d){}
};

static bool operator==(const Mic::Import& lhs, const Mic::Import& rhs)
{
    if( lhs.path != rhs.path )
        return false;
    if( lhs.metaActuals.size() != rhs.metaActuals.size() )
        return false;
    for( int i = 0; i < lhs.metaActuals.size(); i++ )
    {
        if( lhs.metaActuals[i].mode != rhs.metaActuals[i].mode )
            return false;
        if( lhs.metaActuals[i].type != rhs.metaActuals[i].type )
            return false;
        if( lhs.metaActuals[i].val != rhs.metaActuals[i].val )
            return false;
   }
    return true;
}

class Manager : public Mic::Importer {
public:
    typedef QList<ModuleSlot> Modules;
    Modules modules;
    QList<QDir> searchPath;
    QString rootPath;

    Manager() {
#ifdef USE_MILLOADER2
        loader.loadFromFile(":/runtime/MIC+.mil");
#endif
    }
    ~Manager() {
        Modules::const_iterator i;
        for( i = modules.begin(); i != modules.end(); ++i )
            delete (*i).decl;
    }

#ifdef USE_MILLOADER2
    Mic::MilLoader2 loader;
#else
    Mic::MilLoader loader;
#endif

    ModuleSlot* find(const Mic::Import& imp)
    {
        for(int i = 0; i < modules.size(); i++ )
        {
            if( modules[i].imp == imp )
                return &modules[i];
        }
        return 0;
    }

    QByteArray modulePath( const QByteArrayList& path )
    {
        return path.join('$');
    }

    QByteArray moduleSuffix( const Mic::MetaActualList& ma )
    {
        // TODO: this is an intermediate solution assuming everything is built from sources in full everytime.
        return "$" + QByteArray::number(modules.size());
    }

    Mic::Declaration* loadModule( const Mic::Import& imp )
    {
        ModuleSlot* ms = find(imp);
        if( ms != 0 )
            return ms->decl;

        QString file = toFile(imp);
        if( file.isEmpty() )
        {
            qCritical() <<  "cannot find source file of module" << imp.path.join('.');
            modules.append(ModuleSlot(imp,QString(),0));
            return 0;
        }

        // immediately add it so that circular module refs lead to an error
        modules.append(ModuleSlot(imp,file,0));
        ms = &modules.back();

//#define _GEN_OUTPUT_
#ifdef _GEN_OUTPUT_
#if 0
        QFileInfo info(file);
        QFile out(info.dir().absoluteFilePath(info.completeBaseName()+".cod"));
        if( !out.open(QIODevice::WriteOnly) )
        {
            qCritical() << "cannot open file for writing:" << out.fileName();
            return 0;
        }
#else
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
#endif
        Mic::EiGen r(&loader, &out);
#else
#ifdef USE_MILLOADER2
        Mic::InMemRenderer2 r(&loader);
#else
        Mic::InMemRenderer r(&loader);
#endif
#endif
        Lex2 lex;
        lex.sourcePath = file; // to keep file name if invalid
        lex.lex.setStream(file);
        qDebug() << "**** parsing" << QFileInfo(file).fileName();
        Mic::MilEmitter e(&r);
        Mic::AstModel mdl;
        Mic::Parser2 p(&mdl,&lex, &e, this);
        p.RunParser(imp.metaActuals);
        Mic::Declaration* res = 0;
        if( !p.errors.isEmpty() )
        {
            foreach( const Mic::Parser2::Error& e, p.errors )
                qCritical() << QFileInfo(e.path).fileName() << e.row << e.col << e.msg;
        }else
        {
            res = p.takeModule();
        }
        // TODO: uniquely extend the name of generic module instantiations

        ms->decl = res;
        return res;
    }

    QString toFile(const Mic::Import& imp)
    {
        const QString path = imp.path.join('/') + ".mic";
        foreach( const QDir& dir, searchPath )
        {
            const QString tmp = dir.absoluteFilePath(path);
            if( QFile::exists(tmp) )
                return tmp;
        }
        if( !modules.isEmpty() )
        {
            // if the file is not in the search path, look in the directory of the caller assuming
            // that the required module path is relative to the including module
            QFileInfo info( modules.back().file );
            const QString tmp = info.absoluteDir().absoluteFilePath(path);
            if( QFile::exists(tmp) )
                return tmp;
            // TODO: in this case we have to adjust the local path of the imported module to the full path
        }
        return QString();
    }
};

static void process(const QStringList& files, const QStringList& searchPaths, bool run, bool dump)
{
    int ok = 0;
    int all = 0;
    QElapsedTimer timer;
    timer.start();

    QBuffer milout;
    milout.open(QIODevice::WriteOnly);

    foreach( const QString& file, files )
    {
        Manager mgr;
        QFileInfo info(file);
        mgr.rootPath = info.absolutePath();
        mgr.searchPath.append(info.absoluteDir());
        for( int i = 0; i < searchPaths.size(); i++ )
        {
            const QString path = searchPaths[i];
            mgr.searchPath.append(path);
        }

        Mic::Import imp;
        imp.path.append(Mic::Token::getSymbol(info.baseName().toUtf8()));
        Mic::Declaration* module = mgr.loadModule(imp); // recursively compiles all imported files

#ifdef USE_MILLOADER2
        foreach( Mil::Declaration* m, mgr.loader.getModulesInDependencyOrder() )
        {
            if( m->name == "MIC$" )
                continue;
            Mic::IlAsmRenderer r(&milout);
            Mic::MilLoader2::render(&r,m);
            milout.putChar('\n');
        }
#else
        foreach( Mic::MilModule* m, mgr.loader.getModulesInDependencyOrder() )
        {
            Mic::IlAsmRenderer r(&milout);
            Mic::MilLoader::render(&r,m);
            milout.putChar('\n');
        }
#endif

        all += mgr.modules.size();
        foreach( const ModuleSlot& m, mgr.modules )
            ok += m.decl ? 1 : 0;
#if 0
        if( run && module )
        {
            Mic::MilInterpreter intp(&mgr.loader);
            intp.run(imp.path.back());
        }
#endif
    }
    Mic::Expression::killArena();
    Mic::AstModel::cleanupGlobals();
    qDebug() << "#### finished with" << ok << "files ok of total" << all << "files" << "in" << timer.elapsed() << " [ms]";

    milout.close();

    if( dump )
    {
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
        out.write("\n");
        out.write(milout.buffer());
    }
    if( all == ok && run )
    {
        QTemporaryFile out;
        out.open();
        out.write(milout.buffer());
        Mil::AstModel mdl;
        Mil::Project pro(&mdl);
        pro.setFiles(QStringList() << out.fileName());
        out.flush();

        const bool result = pro.parse();
        if( result )
            pro.interpret(true);
    }
}


int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    QCommandLineParser cp;
    cp.setApplicationDescription("Micron compiler");
    cp.addHelpOption();
    cp.addVersionOption();
    cp.addPositionalArgument("main", "the main module of the application");
    QCommandLineOption sp("I", "add a path where to look for modules", "path");
    cp.addOption(sp);
    QCommandLineOption run("r", "run in interpreter");
    cp.addOption(run);
    QCommandLineOption dump("d", "dump MIL code");
    cp.addOption(dump);

    cp.process(a);
    const QStringList args = cp.positionalArguments();
    if( args.isEmpty() )
        return -1;
    const QStringList searchPaths = cp.values(sp);

    process(args, searchPaths, cp.isSet(run), cp.isSet(dump));

    return 0;
}
