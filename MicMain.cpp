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
#include "MicPpLexer.h"
#include "MicParser2.h"
#include "MilEmitter.h"
#include "MilAstSerializer.h"
#include "MilBackend.h"
#include "Version.h"
#ifdef _MIC_HAVE_SCREEN_QT_
#include <QApplication>
#else
#include <QCoreApplication>
#endif
#include <QFile>
#include <QStringList>
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QElapsedTimer>
#include <QTemporaryFile>
#include <QCommandLineParser>
#include <QDirIterator>

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

class Manager : public Mic::Importer {
public:
    typedef QList<ModuleSlot> Modules;
    Modules modules;
    QList<QDir> searchPath;
    QString rootPath;
    Mic::MilLoader2 loader;
    bool dbg;

    Manager():dbg(false) {
        loader.loadFromFile(":/runtime/MIC+.mil");
    }
    ~Manager() {
        Modules::const_iterator i;
        for( i = modules.begin(); i != modules.end(); ++i )
            delete (*i).decl;
    }

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
        Mil::Emitter e(&split, Mil::Emitter::RowsAndCols);
#else
        qDebug() << "**** parsing" << QFileInfo(file).fileName();
        Mil::Emitter e(&imr, dbg ? Mil::Emitter::RowsOnly : Mil::Emitter::None);
#endif
        Mic::AstModel mdl;
        Mic::Parser2 p(&mdl,&lex, &e, this);
        p.RunParser(imp);
        Mic::Declaration* res = 0;
        if( !p.errors.isEmpty() )
        {
            foreach( const Mic::Parser2::Error& e, p.errors )
                qCritical() << QFileInfo(e.path).fileName() << e.row << e.col << e.msg;
        }else if( !imr.errors.isEmpty() )
        {
            foreach( const Mil::AbstractRenderer::Error& e, imr.errors )
                qCritical() << (e.where + ":" + QByteArray::number(e.pc)) << e.msg;
        }else
        {
            res = p.takeModule();
            if( !imr.errors.isEmpty() )
                res->invalid = true;
#ifdef _DUMP
            out.putChar('\n');
#endif
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

extern "C" {
void Args_setArgcArgv(unsigned int c, char** v);
}

int main(int argc, char *argv[])
{
#ifdef _MIC_HAVE_SCREEN_QT_
    QApplication a(argc, argv);
#else
    QCoreApplication a(argc, argv);
#endif
    a.setOrganizationName("Dr. Rochus Keller");
    a.setOrganizationDomain("www.rochus-keller.ch");
    a.setApplicationName("micc");
    a.setApplicationVersion(MICRON_VERSION);

    QCommandLineParser cp;
    cp.setApplicationDescription(QString("Micron compiler, version %1").arg(MICRON_VERSION));
    cp.addHelpOption();
    cp.addVersionOption();
    cp.addPositionalArgument("main", "the main module of the application");
    QCommandLineOption sp("I", "add a path where to look for modules", "path");
    cp.addOption(sp);
    QCommandLineOption op("O", "set the path where compiled modules are stored", "path");
    cp.addOption(op);
    QCommandLineOption run("r", "run in interpreter");
    cp.addOption(run);
    QCommandLineOption dump("d", "dump MIL code");
    cp.addOption(dump);
    QCommandLineOption dump2("l", "dump low-level bytecode");
    cp.addOption(dump2);
    QCommandLineOption arch("a", "generate code for the given architecture", "arch");
    cp.addOption(arch);
    QCommandLineOption dbg("g", "generate debug information");
    cp.addOption(dbg);
    QCommandLineOption cdeclRet("cdecl", "use cdecl-compatible return values (EAX/EAX:EDX for <=8 bytes, x86 only)");
    cp.addOption(cdeclRet);
    QCommandLineOption aapcs("aapcs", "use AAPCS32 calling convention (args in R0-R3, return in R0/R0-R1, ARM only)");
    QCommandLineOption esp32opt("esp32", "generate ESP32-P4 Harvard architecture ELF (split Flash/SRAM memory map)");
    cp.addOption(aapcs);
    cp.addOption(esp32opt);
    QCommandLineOption libs("L", "add a library search directory for the linker", "path");
    cp.addOption(libs);
    QCommandLineOption linkLib("n", "link with archive library lib<name>.a (searched in -L dirs)", "name");
    cp.addOption(linkLib);
    QCommandLineOption linkObj("f", "add an object file (.o) to the linker input", "file");
    cp.addOption(linkObj);

    QStringList allArgs = a.arguments();

    // cut away all arguments starting from "--"; they are sent to the interpreter instead
    const int doubledash = allArgs.indexOf("--");
    if( doubledash != -1 )
        allArgs = allArgs.mid(0, doubledash);

    cp.process(allArgs);
    const QStringList args = cp.positionalArguments();
    if( args.size() != 1 )
    {
        qCritical() << "expecting exactly one source file";
        return -1;
    }

    const QStringList searchPaths = cp.values(sp);
    const QStringList outPaths = cp.values(op);
    if( outPaths.size() > 1 )
    {
        qCritical() << "only one output path can be set";
        return -1;
    }
    QString outPath;
    if( !outPaths.isEmpty() )
        outPath = outPaths.first();

    const QString arch_ = cp.value(arch);
    const QStringList libDirs = cp.values(libs);
    const QStringList linkLibs = cp.values(linkLib);
    const QStringList linkObjs = cp.values(linkObj);
    int ok = 0;
    int all = 0;
    QElapsedTimer timer;
    timer.start();

    Manager mgr;
    mgr.dbg = cp.isSet(dbg);

    QFileInfo info(args.first());
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
        ok += m.decl ? !m.decl->invalid : 0;

    Mic::Expression::killArena();
    Mic::AstModel::cleanupGlobals();
    qDebug() << "#### finished with" << ok << "modules ok of total" << all << "modules" << "in" << timer.elapsed() << " [ms]";

    if( cp.isSet(dump) )
    {
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
        out.write("\n");
        foreach( Mil::Declaration* m, mgr.loader.getModulesInDependencyOrder() )
        {
            if( m->name == "MIC$" )
                continue;
            Mil::IlAsmRenderer r(&out, mgr.dbg);
            Mil::AstSerializer::render(&r,m, Mil::AstSerializer::RowsOnly);
            out.putChar('\n');
        }
    }

    if( outPath.isEmpty() )
       outPath = mgr.rootPath;

    if( all == ok && (arch_ == "arm7" || arch_ == "armv7") )
        Mil::Backend::compileArm(mgr.loader.getModel(), outPath, libDirs, linkLibs, linkObjs, info.baseName(), mgr.dbg, cp.isSet(aapcs));
    if( all == ok && (arch_ == "rv32" || arch_ == "riscv32") )
        Mil::Backend::compileRv32(mgr.loader.getModel(), outPath, libDirs, linkLibs, linkObjs, info.baseName(), mgr.dbg, cp.isSet(aapcs),
                    /*hasFloat*/true, /*hasHwDiv*/true, cp.isSet(esp32opt));
    if( all == ok && (arch_ == "x86" || arch_ == "i386") )
        Mil::Backend::compileX86(mgr.loader.getModel(), outPath, libDirs, linkLibs, linkObjs, info.baseName(), mgr.dbg, cp.isSet(cdeclRet));

    if( all == ok && cp.isSet(run) )
    {
        Mil::Interpreter r(&mgr.loader.getModel());

#ifdef _MIC_HAVE_SCREEN_
        Mil::VmOakwood::addTo(&r, true);
#else
        Mil::VmOakwood::addTo(&r, false);
#endif

        if( !r.compile() )
            return -1;
        if( cp.isSet(dump2) )
        {
            QTextStream out(stdout);
            r.dumpAll(out);
        }

        QByteArrayList args_;
        QVector<char*> argv_;
        argv_.reserve(10);
        argv_.append("Micron Interpreter");

        const int start = qApp->arguments().indexOf("--");
        if( start != -1 )
        {
            for( int i = start+1; i < qApp->arguments().size(); i++ )
                args_.append( qApp->arguments()[i].toUtf8() );

            for( int i = 0; i < args_.size(); i++ )
            {
                if( !args_[i].isEmpty() )
                    argv_.append(args_[i].data());
            }
        }

        Args_setArgcArgv( argv_.size(), argv_.data() );

        r.run();
    }

    return 0;
}
