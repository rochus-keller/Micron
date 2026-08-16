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
#include "MilRlCode.h"
#include "MilVmOakwood.h"
#include "MicPpLexer.h"
#include "MicParser2.h"
#include "MilEmitter.h"
#include "MilAstSerializer.h"
#include "MilBackend.h"
#include "MilCeeGen.h"
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

// exit codes, see the user guide
enum ExitCode { Ok = 0, CompileError = 1, LinkError = 2, UsageError = 3 };

static bool s_verbose = false;
static bool s_quiet = false;

static void messageHandler(QtMsgType type, const QMessageLogContext& ctx, const QString& msg)
{
    // -q suppresses the progress notes, but never errors and warnings
    if( s_quiet && type == QtDebugMsg )
        return;
    QByteArray text = msg.toUtf8();
    text += '\n';
    fputs(text.constData(), type == QtDebugMsg || type == QtInfoMsg ? stdout : stderr);
    Q_UNUSED(ctx)
}

class ModuleLocator : public Mic::ModuleManager::ModuleLocator
{
public:
    QList<QDir> searchPath;

    Mic::ModuleManager::Location find( const QByteArrayList& path )
    {
        // source is preferred over a prebuilt artifact
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
                {
                    if( s_verbose )
                        qDebug() << "  provider for" << rel << "is" << tmp;
                    return Mic::ModuleManager::Location(kinds[k], tmp);
                }
            }
        }
        return Mic::ModuleManager::Location();
    }
    Mic::ModuleManager::Location locate( const Mic::Import& imp )
    {
        Mic::ModuleManager::Location res = find(imp.path);
        if( res.kind == Mic::ModuleManager::NotFound && imp.importer )
        {
            QByteArrayList path = imp.importer->data.value<Mic::ModuleData>().path;
            path.pop_back();
            path += imp.path;
            res = find(path);
        }
        return res;
    }
};

struct BuildOpts
{
    // exactly one mode per invocation
    enum Mode { Link, Compile, Check, Run };
    Mode mode;
    bool dbg;
    bool dumpMil, dumpMll, dumpMrl;
    bool cAbi; // --abi c instead of the stack-only default convention
    bool esp32; // --target rv32-esp32
    bool hasFloat, hasHwDiv;
    qint64 base;
    QString target; // empty, rv32, armv7 or x86
    QString outDir;
    QString exeFile;
    QStringList libDirs;
    QStringList linkLibs;
    QStringList linkObjs;
    BuildOpts():mode(Link),dbg(false),dumpMil(false),dumpMll(false),dumpMrl(false),
        cAbi(false),esp32(false),hasFloat(true),hasHwDiv(true),base(-1){}
};

static bool writeDump(const QString& path, Mil::Declaration* module, bool dbg)
{
    QFile out(path);
    if( !out.open(QIODevice::WriteOnly) )
    {
        qCritical() << "cannot write" << path;
        return false;
    }
    Mil::IlAsmRenderer r(&out, dbg);
    Mil::AstSerializer::render(&r, module,
                               dbg ? Mil::AstSerializer::RowsOnly : Mil::AstSerializer::None);
    if( s_verbose )
        qDebug() << "  generated" << path;
    return true;
}

static bool dumpMil(Mil::AstModel& model, const BuildOpts& opts)
{
    // --dump=mil
    foreach( Mil::Declaration* m, model.getModulesInDependencyOrder() )
    {
        if( m->name == "MIC$" || m->extern_ || m->generic )
            continue;
        if( !writeDump(QDir(opts.outDir).absoluteFilePath(
                           QString::fromUtf8(Mil::CeeGen::escapeFilename(m->name)) + ".mil"),
                       m, opts.dbg) )
            return false;
    }
    return true;
}

static bool dumpVmCode(Mil::AstModel& model, Mil::Interpreter& r, const BuildOpts& opts)
{
    // --dump=mll and --dump=mrl
    Mil::Rl::Code mrl(*r.getCode(), sizeof(void*));
    if( opts.dumpMrl && ( !mrl.compile() || !mrl.compactAll() ) )
    {
        qCritical() << "cannot generate register-level code";
        return false;
    }

    foreach( Mil::Declaration* m, model.getModules() )
    {
        if( m->extern_ || m->generic )
            continue;
        const QString base = QDir(opts.outDir).absoluteFilePath(
                    QString::fromUtf8(Mil::CeeGen::escapeFilename(m->name)));
        if( opts.dumpMll )
        {
            QFile f(base + ".mll");
            if( !f.open(QIODevice::WriteOnly) )
            {
                qCritical() << "cannot write" << f.fileName();
                return false;
            }
            QTextStream out(&f);
            r.dumpModule(out, m);
            if( s_verbose )
                qDebug() << "  generated" << f.fileName();
        }
        if( opts.dumpMrl )
        {
            QFile f(base + ".mrl");
            if( !f.open(QIODevice::WriteOnly) )
            {
                qCritical() << "cannot write" << f.fileName();
                return false;
            }
            QTextStream out(&f);
            mrl.dumpModule(out, m);
            if( s_verbose )
                qDebug() << "  generated" << f.fileName();
        }
    }
    return true;
}

static int emitAndRun(Mil::AstModel& model, const BuildOpts& opts, int all, int ok, Mic::Project2* pro = 0)
{
    if( all != ok )
    {
        // a module with errors is never handed to a back end or to the linker
        qCritical() << "errors in" << (all-ok) << "of" << all << "modules; no code generated";
        return CompileError;
    }

    if( opts.dumpMil && !dumpMil(model, opts) )
        return CompileError;

    if( opts.mode == BuildOpts::Run || opts.dumpMll || opts.dumpMrl )
    {
        // the same interpreter dumps and runs the low-level code
        model.calcMemoryLayouts(sizeof(void*), 8);
        Mil::Interpreter r(&model);
        if( pro )
        {
            if( pro->useBuiltInOakwood() )
                Mil::VmOakwood::addTo(&r, pro->oakwoodScreen());
        }else
        {
#ifdef _MIC_HAVE_SCREEN_
            Mil::VmOakwood::addTo(&r, true);
#else
            Mil::VmOakwood::addTo(&r, false);
#endif
        }

        if( !r.compile() )
        {
            qCritical() << "cannot generate low-level code";
            return CompileError;
        }

        if( ( opts.dumpMll || opts.dumpMrl ) && !dumpVmCode(model, r, opts) )
            return CompileError;

        if( opts.mode == BuildOpts::Run )
        {
            QByteArrayList args_;
            QVector<char*> argv_;
            argv_.reserve(10);
            argv_.append("Micron Interpreter");

            if( pro )
                args_ = pro->getArgs().simplified().split(' ');

            const int start = qApp->arguments().indexOf("--");
            if( start != -1 )
            {
                for( int i = start+1; i < qApp->arguments().size(); i++ )
                    args_.append( qApp->arguments()[i].toUtf8() );
            }
            for( int i = 0; i < args_.size(); i++ )
            {
                if( !args_[i].isEmpty() )
                    argv_.append(args_[i].data());
            }

            Args_setArgcArgv( argv_.size(), argv_.data() );

            return r.run() ? Ok : CompileError;
        }
    }

    if( opts.mode == BuildOpts::Check || opts.mode == BuildOpts::Run )
        return Ok;

    QStringList objFiles;
    if( opts.target == "armv7" )
        objFiles = Mil::Backend::compileArm(model, opts.outDir, opts.dbg, true, opts.cAbi, opts.hasHwDiv);
    else if( opts.target == "rv32" )
        objFiles = Mil::Backend::compileRv32(model, opts.outDir, opts.dbg, /*indirectMain*/false, opts.cAbi,
                    opts.hasFloat, opts.hasHwDiv, opts.esp32);
    else if( opts.target == "x86" )
        objFiles = Mil::Backend::compileX86(model, opts.outDir, opts.dbg, true, opts.cAbi);

    if( objFiles.isEmpty() )
        return CompileError; // the back end reported the reason

    if( opts.mode == BuildOpts::Compile )
        return Ok;

    if( s_verbose )
    {
        foreach( const QString& obj, opts.linkObjs )
            qDebug() << "  link input" << obj;
    }

    if( !Mil::Backend::linkExecutable(objFiles, opts.libDirs, opts.linkLibs, opts.linkObjs,
                                      opts.exeFile, opts.esp32, opts.base) )
        return LinkError;

    return Ok;
}

static int buildSingle(const QFileInfo& info, const QStringList& searchPaths, BuildOpts& opts)
{
    ModuleLocator locator;
    locator.searchPath.append(info.absoluteDir());
    for( int i = 0; i < searchPaths.size(); i++ )
        locator.searchPath.append(QDir(searchPaths[i]));

    Mic::ModuleManager mm(&locator, opts.dbg);
    mm.loadRuntime();

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

    if( opts.outDir.isEmpty() )
        opts.outDir = info.absolutePath();
    if( opts.exeFile.isEmpty() )
        opts.exeFile = QDir(opts.outDir).absoluteFilePath(info.baseName());

    // the reconstructed Mic ASTs are no longer needed for code generation
    const int res = emitAndRun(mm.getModel(), opts, all, ok);

    qDebug() << "micc: finished with" << ok << "modules ok of total" << all << "modules";
    return res;
}

static int buildProject(const QString& projFile, const QStringList& searchPaths, BuildOpts& opts)
{
    Mic::Project2 prj;
    prj.setDbg(opts.dbg);
    prj.setSearchPaths(searchPaths);
    if( !prj.loadFrom(projFile) )
    {
        qCritical() << "cannot load project file" << projFile;
        return UsageError;
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
    if( opts.outDir.isEmpty() )
        opts.outDir = info.absolutePath();
    if( opts.exeFile.isEmpty() )
        opts.exeFile = QDir(opts.outDir).absoluteFilePath( prj.getMain().first.isEmpty() ?
                    info.baseName() : QString::fromUtf8(prj.getMain().first) );

    const int res = emitAndRun(prj.getMilModel(), opts, all, ok, &prj);

    qDebug() << "micc: finished with" << ok << "modules ok of total" << all << "modules";
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
    a.setOrganizationDomain("rochus-keller.ch");
    a.setApplicationName("micc");
    a.setApplicationVersion(MICRON_VERSION);

    qInstallMessageHandler(messageHandler);

    QCommandLineParser cp;
    cp.setApplicationDescription(QString("Micron compiler, version %1").arg(MICRON_VERSION));
    cp.addHelpOption();
    cp.addVersionOption();
    cp.addPositionalArgument("main", "the main module (.mic/.mil/.mob) or a project (.micpro)");
    cp.addPositionalArgument("objects", "foreign object files (.o) and archives (.a) for the linker", "[objects...]");

    // modes, mutually exclusive; the default is to compile and link
    QCommandLineOption compileOnly(QStringList() << "c" << "compile", "compile all modules, but don't link");
    cp.addOption(compileOnly);
    QCommandLineOption checkOnly("check", "only parse and check, generate no code");
    cp.addOption(checkOnly);
    QCommandLineOption run("run", "run the program in the interpreter instead of generating code");
    cp.addOption(run);

    // output
    QCommandLineOption exeOpt(QStringList() << "o" << "output", "the executable to be generated (link mode only)", "file");
    cp.addOption(exeOpt);
    QCommandLineOption outDir("out-dir", "the directory for all generated files (default: the main module's)", "path");
    cp.addOption(outDir);
    QCommandLineOption dump("dump", "also generate <Module>.mil, .mll or .mrl in the output directory", "mil|mll|mrl");
    cp.addOption(dump);
    QCommandLineOption dbg("g", "generate debug information");
    cp.addOption(dbg);
    QCommandLineOption verbose("verbose", "report each module provider and each linker input");
    cp.addOption(verbose);
    QCommandLineOption quiet(QStringList() << "q" << "quiet", "report only warnings and errors");
    cp.addOption(quiet);

    // input
    QCommandLineOption sp("I", "add a path where to look for modules (.mic, .mil, .mob)", "path");
    cp.addOption(sp);
    QCommandLineOption rtOpt("runtime", "MIL file implementing MIC$, for freestanding targets", "file");
    cp.addOption(rtOpt);
    QCommandLineOption libs("L", "add a search directory for archives given by -l", "path");
    cp.addOption(libs);
    QCommandLineOption linkLib("l", "link with the archive lib<name>.a, searched in the -L directories", "name");
    cp.addOption(linkLib);

    // target
    QCommandLineOption target("target", "generate code for rv32, rv32-esp32, armv7 or x86", "target");
    cp.addOption(target);
    QCommandLineOption abi("abi", "'default' is the stack-only Micron convention, 'c' the one close to C", "default|c");
    cp.addOption(abi);
    QCommandLineOption noFloat("no-float", "the target has no floating point unit (rv32 only)");
    cp.addOption(noFloat);
    QCommandLineOption noHwDiv("no-hwdiv", "the target has no divide instruction (rv32 and armv7 only)");
    cp.addOption(noHwDiv);
    QCommandLineOption baseOpt("base", "the load address of the executable, hex (default 8048000)", "addr");
    cp.addOption(baseOpt);

    QStringList allArgs = a.arguments();

    // cut away all arguments starting from "--"; they are sent to the interpreted program instead
    const int doubledash = allArgs.indexOf("--");
    if( doubledash != -1 )
        allArgs = allArgs.mid(0, doubledash);

    // parse instead of process, so that a usage error has the documented exit code
    if( !cp.parse(allArgs) )
    {
        qCritical().noquote() << cp.errorText();
        return UsageError;
    }
    if( cp.isSet("help") )
        cp.showHelp(Ok); // exits the app
    if( cp.isSet("version") )
    {
        qDebug().noquote() << a.applicationName() + " " + a.applicationVersion();
        return Ok;
    }

    QStringList args = cp.positionalArguments();
    if( args.isEmpty() )
    {
        qCritical() << "expecting a source or project file";
        return UsageError;
    }

    s_verbose = cp.isSet(verbose);
    s_quiet = cp.isSet(quiet);

    BuildOpts o;
    o.dbg = cp.isSet(dbg);

    // exactly one mode
    const int modes = (cp.isSet(compileOnly)?1:0) + (cp.isSet(checkOnly)?1:0) + (cp.isSet(run)?1:0);
    if( modes > 1 )
    {
        qCritical() << "-c, --check and --run are mutually exclusive";
        return UsageError;
    }
    if( cp.isSet(compileOnly) )
        o.mode = BuildOpts::Compile;
    else if( cp.isSet(checkOnly) )
        o.mode = BuildOpts::Check;
    else if( cp.isSet(run) )
        o.mode = BuildOpts::Run;

    // target
    if( cp.isSet(target) )
    {
        const QString t = cp.value(target);
        if( t == "rv32" || t == "rv32-esp32" )
        {
            o.target = "rv32";
            o.esp32 = t.endsWith("esp32");
        }else if( t == "armv7" || t == "x86" )
            o.target = t;
        else
        {
            qCritical().noquote() << "unknown target '" + t + "'; expecting rv32, rv32-esp32, armv7 or x86";
            return UsageError;
        }
    }
    if( o.mode == BuildOpts::Link || o.mode == BuildOpts::Compile )
    {
        if( o.target.isEmpty() )
        {
            qCritical() << "no --target given; use --check to only parse and check, or --run to interpret";
            return UsageError;
        }
    }else if( !o.target.isEmpty() )
    {
        qCritical() << "--target cannot be combined with --check or --run";
        return UsageError;
    }

    // ABI and target features
    if( cp.isSet(abi) )
    {
        const QString v = cp.value(abi);
        if( v == "c" )
            o.cAbi = true;
        else if( v != "default" )
        {
            qCritical().noquote() << "unknown ABI '" + v + "'; expecting 'default' or 'c'";
            return UsageError;
        }
    }
    if( cp.isSet(noFloat) )
    {
        if( o.target != "rv32" )
        {
            qCritical() << "--no-float is only supported for rv32";
            return UsageError;
        }
        o.hasFloat = false;
    }
    if( cp.isSet(noHwDiv) )
    {
        if( o.target != "rv32" && o.target != "armv7" )
        {
            qCritical() << "--no-hwdiv is only supported for rv32 and armv7";
            return UsageError;
        }
        o.hasHwDiv = false;
    }
    if( cp.isSet(baseOpt) )
    {
        bool ok = false;
        QString str = cp.value(baseOpt);
        if( str.startsWith("0x", Qt::CaseInsensitive) )
            str = str.mid(2);
        o.base = str.toUInt(&ok,16);
        if( !ok )
        {
            qCritical().noquote() << "invalid base address" << cp.value(baseOpt);
            return UsageError;
        }
    }

    // dumps
    foreach( const QString& d, cp.values(dump) )
    {
        if( d == "mil" )
            o.dumpMil = true;
        else if( d == "mll" )
            o.dumpMll = true;
        else if( d == "mrl" )
            o.dumpMrl = true;
        else
        {
            qCritical().noquote() << "unknown dump format '" + d + "'; expecting mil, mll or mrl";
            return UsageError;
        }
    }

    // output
    if( cp.values(outDir).size() > 1 )
    {
        qCritical() << "only one output directory can be set";
        return UsageError;
    }
    if( cp.isSet(outDir) )
    {
        o.outDir = QFileInfo(cp.value(outDir)).absoluteFilePath();
        if( !QDir(o.outDir).exists() )
        {
            qCritical().noquote() << "output directory does not exist" << o.outDir;
            return UsageError;
        }
    }
    if( cp.isSet(exeOpt) )
    {
        if( o.mode != BuildOpts::Link )
        {
            // micc compiles the whole dependency tree, so -o only makes sense for the executable
            qCritical() << "-o names the executable and cannot be combined with -c, --check or --run;"
                        << "use --out-dir instead";
            return UsageError;
        }
        if( cp.values(exeOpt).size() > 1 )
        {
            qCritical() << "only one executable can be generated";
            return UsageError;
        }
        o.exeFile = QFileInfo(cp.value(exeOpt)).absoluteFilePath();
    }

    // input
    const QStringList searchPaths = cp.values(sp);
    if( cp.isSet(rtOpt) )
    {
        // looked up in the -I directories as well
        const QString file = cp.value(rtOpt);
        QString path;
        if( QFile::exists(file) )
            path = file;
        else
            foreach( const QString& dir, searchPaths )
            {
                const QString tmp = QDir(dir).absoluteFilePath(file);
                if( QFile::exists(tmp) )
                {
                    path = tmp;
                    break;
                }
            }
        if( path.isEmpty() )
        {
            qCritical().noquote() << "runtime file not found" << file;
            return UsageError;
        }
        if( s_verbose )
            qDebug() << "  runtime is" << path;
        Mic::ModuleManager::setRuntimePath(path);
    }
    o.libDirs = cp.values(libs);
    o.linkLibs = cp.values(linkLib);

    // the remaining positional arguments are foreign objects and archives
    const QString mainFile = args.takeFirst();
    foreach( const QString& file, args )
    {
        if( !file.endsWith(".o") && !file.endsWith(".a") )
        {
            qCritical().noquote() << "expecting an object file (.o) or an archive (.a), not" << file;
            return UsageError;
        }
        if( !QFile::exists(file) )
        {
            qCritical().noquote() << "file not found" << file;
            return UsageError;
        }
        o.linkObjs.append(file);
    }
    if( o.mode != BuildOpts::Link && ( !o.linkObjs.isEmpty() || !o.libDirs.isEmpty() || !o.linkLibs.isEmpty() ) )
        qWarning() << "no link step, so the given objects, archives and library paths are not used";

    QElapsedTimer timer;
    timer.start();

    const QFileInfo info(mainFile);
    qDebug() << "micc: compiling" << mainFile;
    int res = 0;
    if( info.suffix() == "micpro" )
    {
        res = buildProject(mainFile, searchPaths, o); // .micpro file selects the project build
    }else
        res = buildSingle(info, searchPaths, o); // everything else is treated as a single main module with auto-discovery

    Mic::Expression::killArena();
    Mic::AstModel::cleanupGlobals();
    qDebug() << "micc: elapsed" << timer.elapsed() << "[ms]";

    return res;
}
