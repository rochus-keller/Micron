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
#include "MilX86Renderer.h"
#include <QCoreApplication>
#include <QFile>
#include <QStringList>
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QElapsedTimer>
#include <QTemporaryFile>
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

static void process(const QString& file, const QStringList& searchPaths,
                    bool run, bool dumpIL, bool dumpLL, const QString& arch, bool dbg, bool cdeclRet, const QString& outPath, const QStringList& rtLibs)
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

    if( all == ok && (arch == "x86" || arch == "i386") )
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
            renderer.setCdeclReturns(cdeclRet);

            if( !renderer.renderModule(module) )
            {
                qCritical() << "error generating x86 code for module" << module->name
                            << ":" << renderer.errorMessage();
                hasErrors = true;
                break;
            }

            QString objFile;
            if( !outPath.isEmpty() )
            {
                objFile = QDir(outPath).absoluteFilePath(module->name + ".o");
            }else if( module->md && !module->md->source.isEmpty() )
            {
                QFileInfo minfo(module->md->source);
                objFile = minfo.absoluteDir().absoluteFilePath(module->name + ".o");
            }else
            {
                objFile = QDir(info.absolutePath()).absoluteFilePath(module->name + ".o");
            }

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
            QString mainObj;
            if( !outPath.isEmpty() )
                mainObj = QDir(outPath).absoluteFilePath("main+.o");
            else
                mainObj = QDir(info.absolutePath()).absoluteFilePath("main+.o");

            if( Mil::X86::Renderer::generateMainObject(moduleNames, mainObj) )
            {
                qDebug() << "  generated" << mainObj;
                objFiles << mainObj;
            }
            else
                qCritical() << "cannot generate main+.o";
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
    QCommandLineOption op("O", "set the path where compiled modules are stored", "path");
    cp.addOption(op);
    QCommandLineOption run("r", "run in interpreter");
    cp.addOption(run);
    QCommandLineOption dump("d", "dump MIL code");
    cp.addOption(dump);
    QCommandLineOption dump2("l", "dump low-level bytecode"); // interpreter or eigen
    cp.addOption(dump2);
    QCommandLineOption arch("a", "generate code for the given architecture", "arch");
    cp.addOption(arch);
    QCommandLineOption dbg("g", "generate debug information");
    cp.addOption(dbg);
    QCommandLineOption cdeclRet("C", "use cdecl-compatible return values (EAX/EAX:EDX for <=8 bytes, x86 only)");
    cp.addOption(cdeclRet);
    QCommandLineOption libs("L", "add a directory where the compiler looks for the runtime libraries", "path");
    cp.addOption(libs);

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
            cp.value(arch), cp.isSet(dbg), cp.isSet(cdeclRet), outPath, cp.values(libs) );

    return 0;
}
