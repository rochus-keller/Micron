/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
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
#include "MicPpLexer.h"
#include "MicParser2.h"
#include "MilEmitter.h"
#include "MilRenderer.h"
#include "MilElfReader.h"
#include "MilParser2.h"
#include "MilLexer.h"
#include "MilValidator.h"
#include "MicAstLoader.h"
#include <QBuffer>
#include <QFile>
#include <QFileInfo>
#include <QtDebug>
using namespace Mic;

// replaces MilLoader2

/*
    This is the single MIC-side driver that locates and loads modules regardless of the format
    makes their public interface available to the Micron compiler as Mic::Declaration modules.

    It makes caching, cycle detection, and format dispatch; the mechanism of where to find a module
    for a given import path is injected through a ModuleLocator.

    For the .mil/.mob cases the reconstructed Mic interface is produced by
    AstLoader. ModuleManager also backs the MIL-level importer:
    MIL resolves imports by name in the model and, on a miss, calls back here to
    locate and load the dependency in whatever format it is available.
*/

ModuleManager::ModuleLocator::~ModuleLocator() {}

ModuleManager::ModuleManager(ModuleLocator* loc, bool d):locator(loc),d_milImporter(0),dbg(d)
{
    // the runtime is loaded on demand, see loadRuntime and clearModel
}

static QString s_runtimePath;

void ModuleManager::setRuntimePath(const QString& path)
{
    // Path of a MIL file implementing MIC$, instead of the built-in declarations.
    s_runtimePath = path;
}

void ModuleManager::loadRuntime()
{
    loadMil(s_runtimePath.isEmpty() ? QString(":/runtime/MIC+.mil") : s_runtimePath);
}

void ModuleManager::clearModel(bool reloadRuntime)
{
    mdl.clear();
    linkObjects.clear();
    invarProcs.clear();
    if( reloadRuntime )
        loadRuntime();
}

Mil::Declaration* ModuleManager::loadMil(const QString& file)
{
    QFile f(file);
    if( !f.open(QIODevice::ReadOnly) )
        return 0;
    return loadMil(&f, file);
}

Mil::Declaration* ModuleManager::loadMil(QIODevice* in, const QString& path)
{
    class Lex : public Mil::Scanner2
    {
    public:
        Mil::Lexer lex;
        Mil::Token next() { return lex.nextToken(); }
        Mil::Token peek(int offset) { return lex.peekToken(offset); }
        QString sourcePath() const { return lex.getSourcePath(); }
    };

    Lex lex;
    lex.lex.setStream(in, path);
    Mil::Parser2 p(&mdl, &lex, this);
    Mil::Declaration* module = 0;
    if( p.parseModule() ) // we only parse one module here
    {
        if( !p.errors.isEmpty() )
        {
            foreach( const Mil::Parser2::Error& e, p.errors )
                qCritical() << e.path << e.pos.d_row << e.pos.d_col << e.msg;
            p.errors.clear();
        }else
        {
            module = p.takeModule();
            Mil::Validator v(&mdl);
            if( !v.validate(module) )
            {
                foreach( const Mil::Validator::Error& e, v.errors )
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
    return module;
}

ModuleManager::~ModuleManager()
{
    foreach( Entry* e, cache )
    {
        delete e->mic;
        delete e;
    }
}

bool ModuleManager::error(const QString& msg)
{
    d_error = msg;
    qCritical() << "ModuleManager:" << msg;
    return false;
}

QByteArray ModuleManager::modulePath(const QByteArrayList& path)
{
    return path.join('$');
}

QByteArray ModuleManager::moduleSuffix(const MetaActualList&)
{
    // TODO: intermediate solution, as in the CLI front-end; generic instantiations
    // will need a suffix which uniquely identifies the actual meta arguments
    return "$" + QByteArray::number(cache.size());
}

ModuleManager::Entry* ModuleManager::entryFor(const Import& imp)
{
    // compare the full import spec including meta actuals, so that each
    // instantiation of a generic module gets its own entry
    foreach( Entry* e, cache )
        if( e->imp == imp )
            return e;
    Entry* e = new Entry();
    e->imp = imp;
    cache.append(e);
    return e;
}

Declaration* ModuleManager::loadModule(const Import& imp)
{
    Location loc = locator ? locator->locate(imp) : Location();
    if( loc.kind == NotFound )
    {
        error(QString("cannot locate module '%1'").arg(QString::fromUtf8(imp.path.join('.'))));
        return 0;
    }

     // as in Project2: make sure each module is registered under full virtual path
    Import fixedImp = imp;
    if( !loc.path.isEmpty() )
        // TODO: check the logic that the new module is in the package where it was actually found
        fixedImp.path = loc.path;

    Entry* e = entryFor(fixedImp);
    if( e->mic )
        return e->mic;
    if( e->loading )
    {
        error(QString("cyclic module dependency involving '%1'").arg(
                  QString::fromUtf8(fixedImp.path.join('.'))));
        return 0;
    }

    e->loading = true; // guard against import cycles during recursive loading
    Declaration* res = 0;
    switch( loc.kind )
    {
    case MicSource:
        res = loadMicSource(fixedImp, loc.file, e);
        break;
    case MilSource:
    case MilObject:
        res = loadMilProvider(fixedImp, loc, e);
        break;
    default:
        break;
    }
    e->loading = false;
    e->mic = res;
    return res;
}

Declaration* ModuleManager::loadMicSource(const Import& imp, const QString& file, Entry* e)
{
    class Scanner : public Mic::Scanner2
    {
    public:
        QString sourcePath;
        Mic::PpLexer lex;
        Mic::Token next() { return lex.nextToken(); }
        Mic::Token peek(int offset) { return lex.peekToken(offset); }
        QString source() const { return sourcePath; }
    };

    Mil::IlAstRenderer imr(&mdl);

    Scanner lex;
    lex.sourcePath = file;
    lex.lex.setStream(file);

    Mil::Emitter em(&imr, dbg ? Mil::Emitter::RowsOnly : Mil::Emitter::None);

    // new per parse; the module declaration is taken out and kept in the cache
    Mic::AstModel local;
    Mic::Parser2 p(&local, &lex, &em, this);
    p.RunParser(imp);

    if( !p.errors.isEmpty() )
    {
        foreach( const Mic::Parser2::Error& err, p.errors )
            qCritical() << QFileInfo(err.path).fileName() << err.row << err.col << err.msg;
        error(QString("errors while parsing '%1'").arg(file));
        return 0;
    }
    if( !imr.errors.isEmpty() )
    {
        foreach( const Mil::AbstractRenderer::Error& err, imr.errors )
            qCritical() << (err.where + ":" + QByteArray::number(err.pc)) << err.msg;
        error(QString("errors while lowering '%1'").arg(file));
        return 0;
    }

    Declaration* res = p.takeModule();
    // used for MIL-level lookups and to relate INVAR procedures to their MIL bodies
    e->mil = imr.getModule();
    if( e->mil == 0 && res )
        e->mil = findMil(res->name);
    return res;
}

Declaration* ModuleManager::loadProvider(const Location& loc, AstModel* into,
                                         ModuleResolver *resolver,
                                         Mil::Declaration** milOut)
{
    Mil::Declaration* milMod = 0;
    if( loc.kind == MilSource )
        milMod = loadMil(loc.file);
    else // MilObject: a .mob ELF object carrying the MIL interface in .micron.mod
    {
        Mil::ElfReader elf;
        if( !elf.loadFromFile(loc.file) )
        {
            error(QString("cannot read object file '%1': %2").arg(loc.file, elf.errorMessage()));
            return 0;
        }
        const QByteArray mil = elf.readMicronModSection();
        if( mil.isEmpty() )
        {
            error(QString("object file '%1' has no .micron.mod module interface").arg(loc.file));
            return 0;
        }
        QByteArray buf(mil);
        QBuffer dev(&buf);
        dev.open(QIODevice::ReadOnly);
        milMod = loadMil(&dev, loc.file);
    }

    if( milMod == 0 )
    {
        error(QString("cannot load MIL module from '%1'").arg(loc.file));
        return 0;
    }
    if( milOut )
        *milOut = milMod;

    // mark the module extern_ to keep the native back ends from regenerating it, and hand its object to the linker
    // A .mil, in contrast, carries the full MIL implementation, so it is compiled
    if( loc.kind == MilObject )
    {
        milMod->extern_ = true;
        linkObjects.append(loc.file);
    }

    // reconstruct the MIC interface from the loaded MIL module
    AstLoader al(into, resolver);
    Declaration* micMod = al.loadModule(milMod);
    if( micMod == 0 )
    {
        error(al.getError());
        return 0;
    }
    recordInvar(al);
    return micMod;
}

Declaration* ModuleManager::loadMilProvider(const Import& imp, const Location& loc, Entry* e)
{
    Mil::Declaration* milMod = 0;
    Declaration* micMod = loadProvider(loc, &mics, this, &milMod);
    e->mil = milMod;
    return micMod;
}

Mil::Declaration* ModuleManager::onMilImport(const Mil::Import& imp)
{
    if( d_milImporter )
        return d_milImporter->loadModule(imp);

    Mil::Declaration* found = findMil(imp.moduleName);
    if( found )
        return found;

    // otherwise locate and load in any format
    Import micImp;
    micImp.path = imp.moduleName.split('$');
    loadModule(micImp);
    return findMil(imp.moduleName);
}

Declaration* ModuleManager::resolveModule(const QByteArray& milModuleName)
{
    Import micImp;
    micImp.path = milModuleName.split('$');
    return loadModule(micImp);
}

Mil::Declaration* ModuleManager::findMil(const QByteArray& milName)
{
    // the name may originate from a split/parse, so compare by value
    foreach( Mil::Declaration* m, mdl.getModules() )
        if( m->name == milName )
            return m;
    return 0;
}

void ModuleManager::recordInvar(const AstLoader& al)
{
    const QHash<Mil::Declaration*, Declaration*>& m = al.declMap();
    QHash<Mil::Declaration*, Declaration*>::const_iterator i;
    for( i = m.begin(); i != m.end(); ++i )
    {
        Mil::Declaration* md = i.key();
        if( md && md->kind == Mil::Declaration::Procedure && md->invar )
            invarProcs.insert(i.value(), md);
    }
}

Mil::Declaration* ModuleManager::invarBody(Declaration* micProc) const
{
    // For an imported INVAR procedure, return its MIL body so the compile-time evaluator can interpret it
    return invarProcs.value(micProc, 0);
}

Mil::Declaration* ModuleManager::milModuleFor(const Import& imp) const
{
    foreach( Entry* e, cache )
        if( e->imp == imp )
            return e->mil;
    return 0;
}

QList<Declaration*> ModuleManager::getMicModules() const
{
    QList<Declaration*> res;
    foreach( Entry* e, cache )
        if( e->mic )
            res.append(e->mic);
    return res;
}
