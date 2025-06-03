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

#include "MicMilLoader2.h"
#include "MicAst.h"
#include "MilInterpreter.h"
#include "MilVmCode.h"
#include "MilVmOakwood.h"
#include "MilEiGen.h"
#include "MilAstSerializer.h"

#include <QCoreApplication>
#include <QFile>
#include <QStringList>
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QElapsedTimer>
#include "MicPpLexer.h"
#include "MicParser2.h"
#include "MilEmitter.h"
#include "MilLexer.h"
#include "MilParser.h"
#include "MilToken.h"
#include <QCommandLineParser>

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
    while( t.isValid() && t.d_type != Mic::Tok_MODULE )
        t = lex.nextToken();
    if( t.d_type == Mic::Tok_MODULE )
    {
        t = lex.nextToken();
        if( t.d_type == Mic::Tok_ident )
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
        loader.loadFromFile(":/runtime/MIC+.mil");
    }
    ~Manager() {
        Modules::const_iterator i;
        for( i = modules.begin(); i != modules.end(); ++i )
            delete (*i).decl;
    }

    Mic::MilLoader2 loader;

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

        Mil::IlAstRenderer imr(&loader.getModel());

        Lex2 lex;
        lex.sourcePath = file; // to keep file name if invalid
        lex.lex.setStream(file);

//#define _DUMP
#ifdef _DUMP
        QList<Mil::AbstractRenderer*> renderer;
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
        Mil::IlAsmRenderer ilasm(&out,true);
        renderer << &ilasm;
        renderer << &imr;
        Mil::RenderSplitter split(renderer);
        Mil::Emitter e(&split, Mil::Emitter::RowsOnly);
#else
        qDebug() << "**** parsing" << QFileInfo(file).fileName();
        Mil::Emitter e(&imr, Mil::Emitter::RowsOnly);
#endif
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
            if( imr.commit() )
                res = p.takeModule();
#ifdef _DUMP
            out.putChar('\n');
#endif
            // else return 0
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

static void process(const QString& file, const QStringList& searchPaths,
                    bool run, bool dumpIL, bool dumpLL, bool eigen, const QString& arch)
{
    int ok = 0;
    int all = 0;
    QElapsedTimer timer;
    timer.start();

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
    Mic::Declaration* top = mgr.loadModule(imp); // recursively compiles all required files
    if( top )
        mgr.loader.getModel().getModules().last()->entryPoint = true; // top-level module is entry point



    all += mgr.modules.size();
    foreach( const ModuleSlot& m, mgr.modules )
        ok += m.decl ? 1 : 0;

    Mic::Expression::killArena();
    Mic::AstModel::cleanupGlobals();
    qDebug() << "#### finished with" << ok << "modules ok of total" << all << "modules" << "in" << timer.elapsed() << " [ms]";

    if( dumpIL )
    {
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
        out.write("\n");
        foreach( Mil::Declaration* m, mgr.loader.getModulesInDependencyOrder() )
        {
            if( m->name == "MIC$" )
                continue;
            Mil::IlAsmRenderer r(&out, true);
            Mil::AstSerializer::render(&r,m, Mil::AstSerializer::RowsOnly);
            out.putChar('\n');
        }
    }
    if( all == ok && eigen )
    {
        Mil::EiGen::TargetCode target = Mil::EiGen::translate(arch.toUtf8().constData());
        if( target == Mil::EiGen::NoTarget )
            qCritical() << "unknown architecture:" << arch;
        else
        {
            const qint8 alig = Mil::EiGen::stack_align(target);
            mgr.loader.getModel().calcMemoryLayouts(Mil::EiGen::pointer_width(target),
                                                    alig, 2 * alig, true);

            Mil::EiGen gen(&mgr.loader.getModel(), target);

            foreach( Mil::Declaration* module, mgr.loader.getModel().getModules() )
            {
                if( module->name == "MIC$" )
                    continue;
                QFile out(module->name + ".cod");
                if( !out.open(QIODevice::WriteOnly) )
                    qCritical() << "cannot open file for writing:" << out.fileName();
                else if( !gen.generate(module, &out) )
                    qCritical() << "error generating module" << module->name;
            }
        }
    }
    if( all == ok && run )
    {
        Mil::Interpreter r(&mgr.loader.getModel());

        Mil::VmOakwood::addTo(&r);

        mgr.loader.getModel().calcMemoryLayouts(sizeof(void*), 8);

        foreach( Mil::Declaration* module, mgr.loader.getModel().getModules() )
        {
            if( !r.precompile(module) )
                return;
        }
        if( dumpLL )
        {
            QTextStream out(stdout);
            r.dumpAll(out);
        }
        foreach( Mil::Declaration* module, mgr.loader.getModel().getModules() )
        {
            if( !r.run(module) )
                break;
        }
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
    QCommandLineOption dump2("l", "dump low-level bytecode"); // interpreter or eigen
    cp.addOption(dump2);
    QCommandLineOption eigen("c", "generate Eigen bytecode");
    cp.addOption(eigen);
    QCommandLineOption arch("a", "generate code for the given architecture", "arch");
    cp.addOption(arch);

    cp.process(a);
    const QStringList args = cp.positionalArguments();
    if( args.isEmpty() )
        return -1;
    const QStringList searchPaths = cp.values(sp);

    // TODO: what to do with more than one file?
    process(args.first(), searchPaths, cp.isSet(run), cp.isSet(dump), cp.isSet(dump2),
            cp.isSet(eigen) || cp.isSet(arch), cp.value(arch));

    return 0;
}
