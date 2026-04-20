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
#include "MilArmv7Renderer.h"
#include "MilX86Renderer.h"
#include "MilElfLinker.h"
#include <QCoreApplication>
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

static void compileEigen(Manager& mgr, const QString& outPath, bool dbg = false )
{
#if 0
    Mil::EiGen::TargetCode target = Mil::EiGen::translate(arch.toUtf8().constData());
    if( target == Mil::EiGen::NoTarget )
        qCritical() << "unknown architecture:" << arch;
    else
    {
        const qint8 alig = Mil::EiGen::stack_align(target);
        mgr.loader.getModel().calcMemoryLayouts(Mil::EiGen::pointer_width(target), alig, 2 * alig);

        Mil::EiGen gen(&mgr.loader.getModel(), target);
        QStringList objFiles;
        bool hasErrors = false;

        foreach( Mil::Declaration* module, mgr.loader.getModel().getModules() )
        {
            if( module->name == "MIC$" )
                continue;
            QTemporaryFile cod;
            if( !cod.open() )
                qCritical() << "cannot open temporary file for Eigen IR";
            else if( !gen.generate(module, &cod, dbg) )
                qCritical() << "error generating module" << module->name;
            else
            {
                cod.flush();
                cod.close();
                QString objFile;
                if( !outPath.isEmpty() )
                {
                    objFile = QDir(outPath).absoluteFilePath(module->name + ".obj"); // TODO: escape name
                }else if( module->md && !module->md->source.isEmpty() )
                {
                    QFileInfo info(module->md->source);
                    objFile = info.absoluteDir().absoluteFilePath(module->name + ".obj");
                }else
                {
                    qCritical() << "don't know where to store object and symbol files for" << module->name;
                    hasErrors = true;
                    break;
                }
                if( dumpLL )
                {
                    QFileInfo info(objFile);
                    QString path = info.absoluteDir().absoluteFilePath(info.baseName() + ".cod");
                    QFile::remove(path);
                    if( !QFile::copy(cod.fileName(),path) )
                        qWarning() << "failed to copy" << cod.fileName() << "to" << path;
                }
                if( !Mil::Backend::generate(cod.fileName(),objFile, target, dbg) )
                {
                    qCritical() << "cannot generate object file for" << module->name;
                    hasErrors = true;
                    break;
                }else
                    objFiles << objFile;
            }
        }
        if( !hasErrors )
        {
            QString outFile;
            QString suffix;
#ifdef Q_OS_WIN32
            suffis = ".exe";
#endif
            if( !outPath.isEmpty() )
            {
                outFile = QDir(outPath).absoluteFilePath(info.baseName() + suffix);
            }else
            {
                QFileInfo info(file);
                outFile = info.absoluteDir().absoluteFilePath(info.baseName() + suffix);
            }
            Mil::Backend::link(objFiles, rtLibs, outFile, target, false, dbg);
        }
    }
#endif
}

static bool linkExecutable(const QStringList& objFiles, const QStringList& libDirs,
                           const QStringList& linkLibs, const QStringList& linkObjs,
                           const QString& outPath, const QString& exeName,
                           bool esp32 = false);

static void compileArm(Manager& mgr, const QString& outPath, const QStringList& libDirs,
                       const QStringList& linkLibs, const QStringList& linkObjs,
                       const QString& exeName, bool dbg = false, bool useAapcs = true, bool hasHwDiv = true)
{
    // ARMv7 backend: generate ELF relocatable objects for each module
    mgr.loader.getModel().calcMemoryLayouts(4 /*pointerWidth*/, 4 /*stackAlignment*/);

    QStringList objFiles;
    bool hasErrors = false;

    foreach( Mil::Declaration* module, mgr.loader.getModulesInDependencyOrder() )
    {
        if( module->name == "MIC$" )
            continue;

        // Reset all flags for this module's compile pass
        foreach( Mil::Declaration* m, mgr.loader.getModel().getModules() )
        {
            m->translated = false;
            for (Mil::Declaration* sub = m->subs; sub; sub = sub->next) {
                if (sub->kind == Mil::Declaration::Procedure)
                    sub->validated = false;
                if (sub->kind == Mil::Declaration::TypeDecl && sub->getType()) {
                    foreach (Mil::Declaration* msub, sub->getType()->subs) {
                        if (msub->kind == Mil::Declaration::Procedure)
                            msub->validated = false;
                    }
                }
            }
        }

        Mil::Arm::Renderer renderer(&mgr.loader.getModel());
        renderer.setEmitDwarf(dbg);
        renderer.setUseAapcs(useAapcs);
        renderer.setHardwareDivide(hasHwDiv); // set false for Cortex-A8 (BeagleBone)

        if( !renderer.renderModule(module) )
        {
            qCritical() << "error generating ARM code for module" << module->name
                        << ":" << renderer.errorMessage();
            hasErrors = true;
            break;
        }

        const QString objFile = QDir(outPath).absoluteFilePath(module->name + ".o");

        if( !renderer.writeToFile(objFile) )
        {
            qCritical() << "cannot write object file" << objFile;
            hasErrors = true;
            break;
        }
        qDebug() << "  generated" << objFile;
        objFiles << objFile;
    }

    if( !hasErrors )
    {
        qDebug() << "#### generated" << objFiles.size() << "ELF relocatable object files";

        // Generate main.o that calls all module inits in dependency order
        QByteArrayList moduleNames;
        foreach( Mil::Declaration* module, mgr.loader.getModel().getRootModules() )
        {
            if( module->name == "MIC$" )
                continue;
            moduleNames << module->name;
        }
        const QString mainObj = QDir(outPath).absoluteFilePath("main+.o");
        if( Mil::Arm::Renderer::generateMainObject(moduleNames, mainObj, useAapcs) )
        {
            qDebug() << "  generated" << mainObj;
            objFiles << mainObj;
        }
        else
            qCritical() << "cannot generate main+.o";
    }

    if( !hasErrors && (!libDirs.isEmpty() || !linkLibs.isEmpty() || !linkObjs.isEmpty()) )
    {
        if( !linkExecutable(objFiles, libDirs, linkLibs, linkObjs, outPath, exeName) )
            hasErrors = true;
    }
}

static bool linkExecutable(const QStringList& objFiles, const QStringList& libDirs,
                           const QStringList& linkLibs, const QStringList& linkObjs,
                           const QString& outPath, const QString& exeName,
                           bool esp32)
{
    if( libDirs.isEmpty() && linkLibs.isEmpty() && linkObjs.isEmpty() )
    {
        qDebug() << "#### no link options given, skipping link step";
        return true;
    }

    Mil::ElfLinker linker;

    // Add all compiler-generated object files
    for( int i = 0; i < objFiles.size(); i++ )
    {
        if( !linker.addFile(objFiles[i]) )
        {
            qCritical() << "link error:" << linker.errorMessage();
            return false;
        }
    }

    // Add explicitly specified additional object files (-f)
    for( int i = 0; i < linkObjs.size(); i++ )
    {
        qDebug() << "  linking" << linkObjs[i];
        if( !linker.addFile(linkObjs[i]) )
        {
            qCritical() << "link error:" << linker.errorMessage();
            return false;
        }
    }

    // Add archive libraries specified by -l, searching in -L directories
    for( int i = 0; i < linkLibs.size(); i++ )
    {
        const QString libName = "lib" + linkLibs[i] + ".a";
        bool found = false;
        for( int j = 0; j < libDirs.size(); j++ )
        {
            const QString path = QDir(libDirs[j]).absoluteFilePath(libName);
            if( QFile::exists(path) )
            {
                qDebug() << "  linking archive" << path;
                if( !linker.addArchive(path) )
                {
                    qCritical() << "link error:" << linker.errorMessage();
                    return false;
                }
                found = true;
                break;
            }
        }
        if( !found )
        {
            qCritical() << "cannot find library -l" + linkLibs[i]
                        << "(searched:" << libDirs.join(", ") << ")";
            return false;
        }
    }

    const QString exePath = QDir(outPath).absoluteFilePath(exeName);
    qDebug() << "#### linking" << exePath;
    if( !linker.link(exePath) )
    {
        qCritical() << "link error:" << linker.errorMessage();
        return false;
    }
    qDebug() << "#### successfully linked" << exePath;
    return true;
}

static void compileX86(Manager& mgr, const QString& outPath, const QStringList& libDirs,
                       const QStringList& linkLibs, const QStringList& linkObjs,
                       const QString& exeName, bool dbg = false, bool cdeclRet = true)
{
    // x86 backend: generate ELF32 relocatable objects for each module
    mgr.loader.getModel().calcMemoryLayouts(4 /*pointerWidth*/, 4 /*stackAlignment*/);

    QStringList objFiles;
    bool hasErrors = false;

    foreach( Mil::Declaration* module, mgr.loader.getModulesInDependencyOrder() )
    {
        if( module->name == "MIC$" )
            continue;

        // Reset all flags for this module's compile pass
        foreach( Mil::Declaration* m, mgr.loader.getModel().getModules() )
        {
            m->translated = false;
            for (Mil::Declaration* sub = m->subs; sub; sub = sub->next) {
                if (sub->kind == Mil::Declaration::Procedure)
                    sub->validated = false;
                if (sub->kind == Mil::Declaration::TypeDecl && sub->getType()) {
                    foreach (Mil::Declaration* msub, sub->getType()->subs) {
                        if (msub->kind == Mil::Declaration::Procedure)
                            msub->validated = false;
                    }
                }
            }
        }

        Mil::X86::Renderer renderer(&mgr.loader.getModel());
        renderer.setEmitDwarf(dbg);
        renderer.setCdeclReturns(cdeclRet);

        if( !renderer.renderModule(module) )
        {
            qCritical() << "error generating x86 code for module" << module->name
                        << ":" << renderer.errorMessage();
            hasErrors = true;
            break;
        }

        const QString objFile = QDir(outPath).absoluteFilePath(module->name + ".o");

        if( !renderer.writeToFile(objFile) )
        {
            qCritical() << "cannot write object file" << objFile;
            hasErrors = true;
            break;
        }
        qDebug() << "  generated" << objFile;
        objFiles << objFile;
    }

    if( !hasErrors )
    {
        qDebug() << "#### generated" << objFiles.size() << "x86 object files";

        // Generate main.o that calls all module inits in dependency order
        QByteArrayList moduleNames;
        foreach( Mil::Declaration* module, mgr.loader.getModel().getRootModules() )
        {
            if( module->name == "MIC$" )
                continue;
            moduleNames << module->name;
        }
        const QString mainObj = QDir(outPath).absoluteFilePath("main+.o");
        if( Mil::X86::Renderer::generateMainObject(moduleNames, mainObj, cdeclRet) )
        {
            qDebug() << "  generated" << mainObj;
            objFiles << mainObj;
        }
        else
            qCritical() << "cannot generate main+.o";
    }

    if( !hasErrors && (!libDirs.isEmpty() || !linkLibs.isEmpty() || !linkObjs.isEmpty()) )
    {
        if( !linkExecutable(objFiles, libDirs, linkLibs, linkObjs, outPath, exeName) )
            hasErrors = true;
    }
}

static void process(const QString& file, const QStringList& searchPaths,
                    bool run, bool dumpIL, bool dumpLL, bool eigen, const QString& arch, bool dbg, bool cdeclRet, bool useAapcs,
                    QString outPath, const QStringList& libDirs, const QStringList& linkLibs, const QStringList& linkObjs,
                    bool esp32 = false)
{
    int ok = 0;
    int all = 0;
    QElapsedTimer timer;
    timer.start();

    Manager mgr;
    mgr.dbg = dbg;

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
        ok += m.decl ? !m.decl->invalid : 0;

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
            Mil::IlAsmRenderer r(&out, dbg);
            Mil::AstSerializer::render(&r,m, Mil::AstSerializer::RowsOnly);
            out.putChar('\n');
        }
    }

    if( outPath.isEmpty() )
       outPath = mgr.rootPath;
#if 0
    if( all == ok && eigen )
    {
        compileEigen(mgr, outPath, dbg);
    }
#endif
    if( all == ok && (arch == "arm7" || arch == "armv7") )
    {
        compileArm(mgr, outPath, libDirs, linkLibs, linkObjs, info.baseName(), dbg, useAapcs);
    }
    if( all == ok && (arch == "x86" || arch == "i386") )
    {
        compileX86(mgr, outPath, libDirs, linkLibs, linkObjs, info.baseName(), dbg, cdeclRet);
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
    QCommandLineOption op("O", "set the path where compiled modules are stored", "path");
    cp.addOption(op);
    QCommandLineOption run("r", "run in interpreter");
    cp.addOption(run);
    QCommandLineOption dump("d", "dump MIL code");
    cp.addOption(dump);
    QCommandLineOption dump2("l", "dump low-level bytecode"); // interpreter or eigen
    cp.addOption(dump2);
    QCommandLineOption eigen("e", "use Eigen backend");
    cp.addOption(eigen);
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

    cp.process(a);
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

    process(args.first(), searchPaths, cp.isSet(run), cp.isSet(dump), cp.isSet(dump2),
            cp.isSet(eigen) || cp.isSet(arch), cp.value(arch), cp.isSet(dbg), cp.isSet(cdeclRet), cp.isSet(aapcs), outPath,
            cp.values(libs), cp.values(linkLib), cp.values(linkObj), cp.isSet(esp32opt) );

    return 0;
}
