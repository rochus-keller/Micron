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

#include "MicModuleManager.h"
#include "MicProject2.h"
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

extern "C" {
void Args_setArgcArgv(unsigned int c, char** v);
}

class ModuleLocator : public Mic::ModuleManager::ModuleLocator
{
public:
    QList<QDir> searchPath;

    Mic::ModuleManager::Location locate( const QByteArrayList& path )
    {
        // auto-discovery: a module import path is resolved against the current and include directories,
        // preferring source over prebuilt artifacts
        const QString rel = QString::fromUtf8(path.join('/'));
        static const char* const exts[] = { ".mic", ".mil", ".mob" };
        static const Mic::ModuleManager::ProviderKind kinds[] = {
            Mic::ModuleManager::MicSource,
            Mic::ModuleManager::MilSource,
            Mic::ModuleManager::MilObject
        };
        for( int k = 0; k < 3; k++ )
        {
            foreach( const QDir& dir, searchPath )
            {
                const QString tmp = dir.absoluteFilePath(rel + exts[k]);
                if( QFile::exists(tmp) )
                    return Mic::ModuleManager::Location(kinds[k], tmp);
            }
        }
        return Mic::ModuleManager::Location();
    }
};

struct BuildOpts
{
    bool dbg;
    bool doDump;
    bool dump2;
    bool doRun;
    bool cdeclRet;
    bool aapcs;
    bool esp32;
    QString arch;
    QString outPath;
    QString exeName;
    QStringList libDirs;
    QStringList linkLibs;
    QStringList linkObjs;
    BuildOpts():dbg(false),doDump(false),dump2(false),doRun(false),
        cdeclRet(false),aapcs(false),esp32(false){}
};

static int emitAndRun(Mil::AstModel& model, const BuildOpts& opts, int all, int ok, Mic::Project2* pro = 0)
{
    if( opts.doDump )
    {
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
        out.write("\n");
        foreach( Mil::Declaration* m, model.getModulesInDependencyOrder() )
        {
            if( m->name == "MIC$" )
                continue;
            Mil::IlAsmRenderer r(&out, opts.dbg);
            Mil::AstSerializer::render(&r,m, Mil::AstSerializer::RowsOnly);
            out.putChar('\n');
        }
    }

    QStringList objFiles;
    if( all == ok && (opts.arch == "arm7" || opts.arch == "armv7") )
        objFiles = Mil::Backend::compileArm(model, opts.outPath, opts.dbg, true, opts.aapcs);
    if( all == ok && (opts.arch == "rv32" || opts.arch == "riscv32") )
        objFiles = Mil::Backend::compileRv32(model, opts.outPath, opts.dbg, /*indirectMain*/false, opts.aapcs,
                    /*hasFloat*/true, /*hasHwDiv*/true, opts.esp32);
    if( all == ok && (opts.arch == "x86" || opts.arch == "i386") )
        objFiles = Mil::Backend::compileX86(model, opts.outPath, opts.dbg, true, opts.cdeclRet);

    if( !objFiles.isEmpty() || !opts.libDirs.isEmpty() || !opts.linkLibs.isEmpty() || !opts.linkObjs.isEmpty() )
        Mil::Backend::linkExecutable(objFiles, opts.libDirs, opts.linkLibs, opts.linkObjs, opts.outPath, opts.exeName); // TODO: link errors

    if( all == ok && opts.doRun )
    {
        if( pro )
        {
            pro->interpret();
            return 0;
        }
        Mil::Interpreter r(&model);

#ifdef _MIC_HAVE_SCREEN_
        Mil::VmOakwood::addTo(&r, true);
#else
        Mil::VmOakwood::addTo(&r, false);
#endif

        if( !r.compile() )
            return -1;
        if( opts.dump2 )
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

static int buildSingle(const QFileInfo& info, const QStringList& searchPaths, BuildOpts& opts)
{
    ModuleLocator locator;
    locator.searchPath.append(info.absoluteDir());
    for( int i = 0; i < searchPaths.size(); i++ )
        locator.searchPath.append(QDir(searchPaths[i]));

    Mic::ModuleManager mm(&locator, opts.dbg);

    Mic::Import imp;
    imp.path.append(Mic::Token::getSymbol(info.baseName().toUtf8()));
    Mic::Declaration* top = mm.loadModule(imp); // recursively loads all required modules
    if( top )
    {
        Mil::Declaration* topMil = mm.milModuleFor(imp);
        if( topMil )
            topMil->entryPoint = true; // top-level module is entry point
    }

    const int all = mm.moduleCount();
    int ok = 0;
    foreach( Mic::Declaration* d, mm.getMicModules() )
        ok += d->invalid ? 0 : 1;

    opts.linkObjs += mm.getLinkObjects(); // link the .mob objects of prebuilt providers

    if( opts.outPath.isEmpty() )
        opts.outPath = info.absolutePath();
    if( opts.exeName.isEmpty() )
        opts.exeName = info.baseName();

    // the reconstructed Mic ASTs are no longer needed for code generation
    const int res = emitAndRun(mm.getModel(), opts, all, ok);

    qDebug() << "#### finished with" << ok << "modules ok of total" << all << "modules";
    return res;
}

static int buildProject(const QString& projFile, BuildOpts& opts)
{
    Mic::Project2 prj;
    prj.setDbg(opts.dbg);
    if( !prj.loadFrom(projFile) )
    {
        qCritical() << "cannot load project file" << projFile;
        return -1;
    }

    prj.parse();

    int all = 0, ok = 0;
    foreach( Mic::Declaration* m, prj.getDependencyOrder() )
    {
        all++;
        if( m && !m->invalid )
            ok++;
    }
    foreach( const Mic::Project2::Error& e, prj.errors )
        qCritical() << e.path << e.pos.d_row << e.pos.d_col << e.msg;

    opts.linkObjs += prj.getLinkObjects(); // link the .mob objects of prebuilt providers

    QFileInfo info(projFile);
    if( opts.outPath.isEmpty() )
        opts.outPath = info.absolutePath();
    if( opts.exeName.isEmpty() )
        opts.exeName = prj.getMain().first.isEmpty() ?
                    info.baseName().toUtf8() : QString::fromUtf8(prj.getMain().first);

    const int res = emitAndRun(prj.getMilModel(), opts, all, ok, &prj);

    qDebug() << "#### finished with" << ok << "modules ok of total" << all << "modules";
    return res;
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
    cp.addPositionalArgument("main", "the main module (.mic/.mil/.mob) or a project (.micpro)");
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
        qCritical() << "expecting exactly one source or project file";
        return -1;
    }

    const QStringList searchPaths = cp.values(sp);
    const QStringList outPaths = cp.values(op);
    if( outPaths.size() > 1 )
    {
        qCritical() << "only one output path can be set";
        return -1;
    }

    BuildOpts o;
    o.dbg = cp.isSet(dbg);
    o.doDump = cp.isSet(dump);
    o.dump2 = cp.isSet(dump2);
    o.doRun = cp.isSet(run);
    o.cdeclRet = cp.isSet(cdeclRet);
    o.aapcs = cp.isSet(aapcs);
    o.esp32 = cp.isSet(esp32opt);
    o.arch = cp.value(arch);
    o.libDirs = cp.values(libs);
    o.linkLibs = cp.values(linkLib);
    o.linkObjs = cp.values(linkObj);
    if( !outPaths.isEmpty() )
        o.outPath = outPaths.first();

    QElapsedTimer timer;
    timer.start();

    const QFileInfo info(args.first());
    int res = 0;
    if( info.suffix() == "micpro" )
        res = buildProject(args.first(), o); // .micpro file selects the project build
    else
        res = buildSingle(info, searchPaths, o); // everything else is treated as a single main module with auto-discovery

    Mic::Expression::killArena();
    Mic::AstModel::cleanupGlobals();
    qDebug() << "#### elapsed" << timer.elapsed() << "[ms]";

    return res;
}
