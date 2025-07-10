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

#include "MicProject2.h"
#include <QBuffer>
#include <QDir>
#include <QtDebug>
#include <QSettings>
#include <QCoreApplication>
#include <qdatetime.h>
#include "MicPpLexer.h"
#include "MicParser2.h"
#include "MilEmitter.h"
#include "MilLexer.h"
#include "MilParser.h"
#include "MilToken.h"
#include "MilCeeGen.h"
using namespace Mic;

struct HitTest
{
    quint32 line, col;
    QList<Declaration*> scopes;
    Declaration* scopeHit;

    HitTest():scopeHit(0){}

    QVariant findHit( Declaration* module, int row, int col )
    {
        this->col = row;
        this->line = line;
        try
        {
            visitDecl(module);
        }catch( Expression* e )
        {
            Q_ASSERT( !scopes.isEmpty() );
            scopeHit = scopes.back();
            return QVariant::fromValue(e);
        }catch( Declaration* d )
        {
            Q_ASSERT( !scopes.isEmpty() );
            scopeHit = scopes.back();
            return QVariant::fromValue(d);
        }catch(...)
        {

        }
        return 0;
    }

    void test( Declaration* d )
    {
        if( line == d->pos.d_row && col >= d->pos.d_col && col <= d->pos.d_col + d->name.size() )
            throw d;
    }

    void test(Expression* e)
    {
        if( e == 0 )
            return;
        if( e->pos.d_row > line )
            return;
        Declaration* n = e->val.value<Declaration*>();
        if( n == 0 )
            return;
        if( line == e->pos.d_row && col >= e->pos.d_col && col <= e->pos.d_col + n->name.size() )
            throw e;
    }

    void visitDecl(Declaration* d)
    {
        test(d);
    }
};

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

Project2::Project2(QObject *parent) : QObject(parent),d_dirty(false),d_useBuiltInOakwood(false),d_level(0)
{
    d_suffixes << ".mic";
}

Project2::~Project2()
{
    clear();
}

void Project2::clear(bool all)
{
    d_mdl.clear();
    modules.clear();
    loader.getModel().clear();
    errors.clear();
    subs.clear();
    if( all )
    {
        d_groups.clear();
        d_filePath.clear();
        d_files.clear();
        foreach(File* f, d_libs)
            delete f;
        d_libs.clear();
    }
    loader.loadFromFile(":/runtime/MIC+.mil");
}

void Project2::createNew()
{
    clear();
    d_filePath.clear();
    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
}

bool Project2::initializeFromDir(const QDir& dir, bool recursive)
{
    clear();
    d_dirty = false;

    QStringList files = findFiles(dir, recursive);
    foreach( const QString& filePath, files )
        addFile(filePath);
    emit sigRenamed();
    return true;
}

bool Project2::initializeFromPackageList(const PackageList& pl)
{
    clear();
    d_dirty = false;
    foreach( const Package& p, pl )
    {
        foreach( const QString& path, p.d_files )
            addFile(path,p.d_path);
    }

    return true;
}

void Project2::setSuffixes(const QStringList& s)
{
    d_suffixes = s;
    touch();
}

void Project2::setMain(const Project2::ModProc& mp)
{
    d_main = mp;
    touch();
}

QString Project2::renderMain() const
{
    QString res = d_main.first;
    if( !d_main.second.isEmpty() )
        res += "." + d_main.second;
    return res;
}

void Project2::setUseBuiltInOakwood(bool on)
{
    d_useBuiltInOakwood = on;
    touch();
}

bool Project2::addFile(const QString& filePath, const VirtualPath& package)
{
    if( d_files.contains(filePath) )
        return false;
    int pos = findPackage(package);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = package;
    }
    FileGroup& fg = d_groups[pos];
    FileRef ref( new File() );
    fg.d_files.append(ref.data());
    ref->d_group = &fg;
    ref->d_filePath = filePath;
    ref->d_name = QFileInfo(filePath).baseName().toUtf8(); // TODO: should we parse the module name instead?
    d_files.insert(filePath,ref);
    touch();
    return true;
}

bool Project2::addPackagePath(const VirtualPath& path)
{
    int pos = findPackage(path);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = path;
        touch();
        return true;
    }else
        return false;
}

bool Project2::removeFile(const QString& filePath)
{
    FileHash::iterator i = d_files.find(filePath);
    if( i == d_files.end() )
        return false;
    const int pos = findPackage( i.value()->d_group->d_package );
    Q_ASSERT( pos != -1 );
    d_groups[pos].d_files.removeAll(i.value().data());
    d_files.erase(i);
    touch();
    return true;
}

bool Project2::removePackagePath(const VirtualPath& path)
{
    if( path.isEmpty() )
        return false;
    int pos = findPackage(path);
    if( pos == -1 )
        return false;
    if( !d_groups[pos].d_files.isEmpty() )
        return false;
    d_groups.removeAt(pos);
    touch();
    return true;
}

const Project2::FileGroup* Project2::getRootFileGroup() const
{
    return findFileGroup(QByteArrayList());
}

const Project2::FileGroup* Project2::findFileGroup(const VirtualPath& package) const
{
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == package )
            return &d_groups[i];
    }
    return 0;
}

Symbol*Project2::getSymbolsOfModule(Declaration* module) const
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].decl == module )
            return modules[i].xref.syms;
    }
    return 0;
}

Symbol* Project2::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Declaration **scopePtr) const
{
    File* f = findFile(file);
    if( f == 0 || f->d_mod == 0 )
        return 0;

    return findSymbolBySourcePos(f->d_mod,line,col, scopePtr);
}

Symbol*Project2::findSymbolBySourcePos(Declaration *module, quint32 line, quint16 col, Declaration **scopePtr) const
{
    Q_ASSERT(module && module->kind == Declaration::Module);
    const ModuleSlot* modslot = find(module);
    if( modslot == 0 || modslot->xref.syms == 0 )
        return 0;
    Symbol* s = modslot->xref.syms;
    do
    {
        if( line == s->pos.d_row && col >= s->pos.d_col && col <= s->pos.d_col + s->len )
            return s;
        s = s->next;
    }while( s && s != modslot->xref.syms );
    return 0;
}

Project2::File* Project2::findFile(const QString& file) const
{
    FileRef f = d_files.value(file);
    if( f == 0 || f->d_mod == 0 ) // || i.value().d_mod->d_hasErrors )
    {
        const ModuleSlot* ms = find(file.toLatin1());
        if( ms )
            return ms->file;
    }
    return f.data();
}

Declaration *Project2::findModule(const QByteArray &name) const
{
    const ModuleSlot* ms = find(name);
    if( ms )
        return ms->decl;
    return 0;
}

Project2::UsageByMod Project2::getUsage(Declaration *n) const
{
    UsageByMod res;
    for( int i = 0; i < modules.size(); i++ )
    {
        const SymList& syms = modules[i].xref.uses.value(n);
        if( !syms.isEmpty() )
            res << qMakePair(modules[i].decl, syms);
    }

    return res;
}

DeclList Project2::getSubs(Declaration* d) const
{
    return subs.value(d);
}

QString Project2::getWorkingDir(bool resolved) const
{
    if( d_workingDir.isEmpty() )
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().path();
        else
            return QCoreApplication::applicationDirPath();
    }
    else if( !resolved )
        return d_workingDir;
    // else
    QString wd = d_workingDir;
    wd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    wd.replace("%APPDIR%", path );
    return wd;
}

void Project2::setWorkingDir(const QString& wd)
{
    d_workingDir = wd;
    touch();
}

QString Project2::getBuildDir(bool resolved) const
{
    if( d_buildDir.isEmpty() )
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().absoluteFilePath("build");
        else
            return QCoreApplication::applicationDirPath() + "/build";
    }
    else if( !resolved )
        return d_buildDir;
    // else
    QString bd = d_buildDir;
    bd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    bd.replace("%APPDIR%", path );
    return bd;
}

void Project2::setBuildDir(const QString& bd)
{
    d_buildDir = bd;
    touch();
}

void Project2::setOptions(const QByteArrayList& o)
{
    d_options = o;
    touch();
}

static inline QByteArray escapeDot(QByteArray name)
{
    return "\"" + name + "\"";
}

quint32 Project2::getSloc() const
{
    return 0; // TODO d_mdl->getSloc();
}

bool Project2::parse()
{
    clear(false);

    if( useBuiltInOakwood() )
    {
        parseLib("In");
        parseLib("Out");
        parseLib("Files");
        parseLib("Input");
        parseLib("Math");
        parseLib("MathL");
        parseLib("Strings");
    }

    bool res = false;
    int all = 0, ok = 0;

    for( int i = 0; i < d_groups.size(); i++ )
    {
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
        {
            QFileInfo info(d_groups[i].d_files[j]->d_filePath);
            Import imp;
            imp.path = d_groups[i].d_package;
            imp.path.append(Token::getSymbol(d_groups[i].d_files[j]->d_name));
            Declaration* module = loadModule(imp); // recursively compiles all imported files
            all++;
            if( module && !module->invalid )
                ok++;
        }
    }

    return all == ok;
    emit sigReparsed();
    return res;
}

bool Project2::generateC(const QString &outDir)
{
    writeC("runtime", "MIC+", outDir);

    if( useBuiltInOakwood() )
    {
        writeC("oakwood", "In", outDir);
        writeC("oakwood", "Out", outDir);
        writeC("oakwood", "Files", outDir);
        writeC("oakwood", "Input", outDir);
        writeC("oakwood", "Math", outDir);
        writeC("oakwood", "MathL", outDir);
        writeC("oakwood", "Strings", outDir);
    }

    QSet<Mil::Declaration*> used;
    QDir dir(outDir);
    // TODO: check if files can be created and written
    foreach( Mil::Declaration* module, loader.getModel().getModules() )
    {
        if( module->generic ) // skip not fully instantiated modules
            continue;
        Mil::Declaration* sub = module->subs;
        while(sub)
        {
            if( sub->kind == Mil::Declaration::Import )
            {
                Mil::Declaration* imported = sub->imported;
                if( imported && !imported->generic )
                    used.insert(imported);
            }
            sub = sub->next;
        }
        Mil::CeeGen cg(&loader.getModel());
        QFile header( dir.absoluteFilePath(escapeFilename(module->name) + ".h"));
        header.open(QFile::WriteOnly);
        QFile* body = 0;
        QFile b( dir.absoluteFilePath(escapeFilename(module->name) + ".c"));
        module->nobody = !Mil::CeeGen::requiresBody(module);
        if( !module->nobody )
        {
            b.open(QFile::WriteOnly);
            body = &b;
        }

        cg.generate(module, &header, body);
    }

    // TODO: check if getMain was set
    QFile main(dir.absoluteFilePath("main+.c"));
    main.open(QFile::WriteOnly);
    QTextStream out(&main);
    out << "// main+.c" << endl;
    out << Mil::CeeGen::genDedication() << endl << endl;
    out << "int main(int argc, char** argv) {" << endl;

    foreach( Mil::Declaration* module, loader.getModel().getModules() )
    {
        // if a module is not in "used", it is never imported and thus a root module
        if( !used.contains(module) && !module->nobody && !module->generic )
            out << "    " <<  module->name << "$begin$();" << endl;
    }
    out << "    return 0;" << endl;
    out << "}" << endl;

    return true;
}

Declaration *Project2::loadModule(const Import &imp)
{
    // toFile also finds modules with incomplete path, such as Interface instead of som.Interface
    // we must thus look for the file first, so that we can complete the Import spec if necessary
    File* file = toFile(imp);
    if( file == 0 )
    {
        QString importer;
        if( imp.importer )
            importer = imp.importer->data.value<ModuleData>().source;
        errors << Error("cannot find source file of imported module", imp.importedAt, importer);
        modules.append(ModuleSlot(imp,0,0));
        return 0;
    }

    Import fixedImp = imp;
    if( file->d_group && !file->d_group->d_package.isEmpty() )
    {
        // Take care that the new module is in the package where it was actually found
        // So that e.g. Interface is in som.Interface, even when imported from Vector
        fixedImp.path = file->d_group->d_package;
        fixedImp.path.append(imp.path.back());
    }

    ModuleSlot* ms = find(fixedImp);
    if( ms != 0 )
        return ms->decl;

    // immediately add it so that circular module refs lead to an error
    modules.append(ModuleSlot(fixedImp,file,0));
    ms = &modules.back();

//#define _NO_MIL_ // TEST
#ifdef _NO_MIL_
    Mil::RenderSplitter imr;
#else
    Mil::IlAstRenderer imr(&loader.getModel());
#endif

    d_level++;
    Lex2 lex;
    lex.sourcePath = file->d_filePath; // to keep file name if invalid
    QBuffer buf; // keep buffer here so it remains valid for the parse if needed
    if( !file->d_cache.isEmpty() )
    {
        buf.setData(file->d_cache);
        buf.open(QIODevice::ReadOnly);
        lex.lex.setStream(&buf, file->d_filePath);
    }else
        lex.lex.setStream(file->d_filePath);

    Mil::Emitter e(&imr, Mil::Emitter::RowsAndCols);
    Mic::Parser2 p(&d_mdl,&lex, &e, this, true);
    p.RunParser(fixedImp);
    if( p.getModule() )
        qDebug() << "*** parsing" << p.getModule()->name << "level" << d_level << (p.getModule()->generic?"generic":""); // TEST
    else
        qDebug() << "*** parse" << file->d_name; // TEST
    Mic::Declaration* res = 0;
    bool hasErrors = false;
    if( !p.errors.isEmpty() )
    {
        foreach( const Mic::Parser2::Error& e, p.errors )
            errors << Error(e.msg, RowCol(e.row, e.col), e.path);
        hasErrors = true;
    }

    if( !imr.errors.isEmpty() )
    {
        foreach( const Mil::AbstractRenderer::Error& e, imr.errors )
            errors << Error(e.msg, RowCol(), e.where + ":" + QByteArray::number(e.pc));
        hasErrors = true;
    }

    res = p.takeModule();
    if( res )
        res->invalid = hasErrors;

    if( fixedImp.metaActuals.isEmpty() )
        file->d_mod = res; // in case of generic modules, file->d_mod points to the non-instantiated version

    if( res && imr.getModule() )
        imr.getModule()->generic = res->generic;

    ms->xref = p.takeXref();
    QHash<Declaration*,DeclList>::const_iterator i;
    for( i = ms->xref.subs.begin(); i != ms->xref.subs.end(); ++i )
        subs[i.key()] += i.value();

    ms->decl = res;
    d_level--;
    return res;
}

QStringList Project2::findFiles(const QDir& dir, bool recursive)
{
    QStringList res;
    QStringList files;

    if( recursive )
    {
        files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

        foreach( const QString& f, files )
            res += findFiles( QDir( dir.absoluteFilePath(f) ), recursive );
    }

    QStringList suff = d_suffixes;
    for(int i = 0; i < suff.size(); i++ )
        suff[i] = "*" + suff[i];
    files = dir.entryList( suff, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append( dir.absoluteFilePath(f) );
    }
    return res;
}

void Project2::touch()
{
    if( !d_dirty )
    {
        d_dirty = true;
        emit sigModified(d_dirty);
    }
}

int Project2::findPackage(const VirtualPath& path) const
{
    int pos = -1;
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == path )
        {
            pos = i;
            break;
        }
    }
    return pos;
}

Project2::ModuleSlot *Project2::find(const Import &imp)
{
    for(int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].imp == imp )
            return &modules[i];
    }
    return 0;
}

const Project2::ModuleSlot *Project2::find(const QByteArray & name) const
{
    for( int i = 0; i < modules.size(); i++ )
        if( modules[i].decl && modules[i].decl->name == name ) // TODO: search by string pointer?
            return &modules[i];
    return 0;
}

const Project2::ModuleSlot *Project2::find(Declaration * m) const
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].decl == m )
            return &modules[i];
    }
    return 0;
}

Project2::File *Project2::toFile(const Import &imp)
{
    QByteArrayList path = imp.path;
    path.pop_back();
    const QByteArray name = imp.path.back();
    const FileGroup* fg = findFileGroup(path);
    if( fg != 0 )
    {
        for( int i = 0; i < fg->d_files.size(); i++ )
        {
            if( fg->d_files[i]->d_name == name )
                return fg->d_files[i];
        }
        if( imp.importer )
        {
            path = imp.importer->data.value<ModuleData>().path;
            path.pop_back();
            path += imp.path;
            path.pop_back();
            fg = findFileGroup(path);
            if( fg != 0 )
            {
                for( int i = 0; i < fg->d_files.size(); i++ )
                {
                    if( fg->d_files[i]->d_name == name )
                        return fg->d_files[i];
                }
            }
        }
    }
    foreach(File* f, d_libs)
    {
        if( (f->d_mod && f->d_mod->name.constData() == name.constData()) || f->d_name == name )
            return f;
    }
    return 0;
}

void Project2::parseLib(const QString & name)
{
    Mil::RenderSplitter imr;
    Lex2 lex;
    QString path = QString(":/oakwood/%1.mic").arg(name);
    lex.sourcePath = path; // to keep file name if invalid
    lex.lex.setStream(path);
    Mil::Emitter e(&imr, Mil::Emitter::RowsAndCols);
    Mic::Parser2 p(&d_mdl,&lex, &e, this, false);
    p.RunParser();
    ModuleSlot ms;
    ms.decl = p.takeModule();
    ms.xref = p.takeXref();
    File* f = new File();
    f->d_filePath = path;
    f->d_mod = ms.decl;
    f->d_name = name.toUtf8();
    d_libs << f;
    ms.file = f;
    modules.append(ms);
}

void Project2::writeC(const QString& where, const QString &what, const QString &out)
{
    QDir dir(out);
    const bool a = QFile::copy(QString(":/%1/%2.h").arg(where).arg(what), dir.absoluteFilePath(what+".h"));
    const bool b = QFile::copy(QString(":/%1/%2+.c").arg(where).arg(what), dir.absoluteFilePath(what+"+.c"));
    qDebug() << what << a << b;
}

bool Project2::save()
{
    if( d_filePath.isEmpty() )
        return false;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings out(d_filePath,QSettings::IniFormat);
    if( !out.isWritable() )
        return false;

    out.setValue("Suffixes", d_suffixes );
    out.setValue("BuiltInOakwood", d_useBuiltInOakwood );
    out.setValue("MainModule", d_main.first );
    out.setValue("MainProc", d_main.second );
    out.setValue("WorkingDir", d_workingDir );
    out.setValue("BuildDir", d_buildDir );
    out.setValue("Options", d_options.join(' ') );

    const FileGroup* root = getRootFileGroup();
    out.beginWriteArray("Modules", root->d_files.size() ); // nested arrays don't work
    for( int i = 0; i < root->d_files.size(); i++ )
    {
        const QString absPath = root->d_files[i]->d_filePath;
        const QString relPath = dir.relativeFilePath( absPath );
        out.setArrayIndex(i);
        out.setValue("AbsPath", absPath );
        out.setValue("RelPath", relPath );
    }
    out.endArray();

    out.beginWriteArray("Packages", d_groups.size() );
    for( int i = 0; i < d_groups.size(); i++ )
    {
        out.setArrayIndex(i);
        out.setValue("Name", d_groups[i].d_package.join('.') ); // '/' in key gives strange effects
    }
    out.endArray();

    for( int i = 0; i < d_groups.size(); i++ )
    {
        if(d_groups[i].d_package.isEmpty())
            continue;
        out.beginWriteArray(QString::fromUtf8("." + d_groups[i].d_package.join('.')), d_groups[i].d_files.size() );
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
        {
            const QString absPath = d_groups[i].d_files[j]->d_filePath;
            const QString relPath = dir.relativeFilePath( absPath );
            out.setArrayIndex(j);
            out.setValue("AbsPath", absPath );
            out.setValue("RelPath", relPath );
        }
        out.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    return true;
}

bool Project2::loadFrom(const QString& filePath)
{
    clear();

    d_filePath = filePath;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings in(d_filePath,QSettings::IniFormat);

    d_suffixes = in.value("Suffixes").toStringList();
    d_useBuiltInOakwood = in.value("BuiltInOakwood").toBool();
    d_main.first = in.value("MainModule").toByteArray();
    d_main.second = in.value("MainProc").toByteArray();
    d_workingDir = in.value("WorkingDir").toString();
    d_buildDir = in.value("BuildDir").toString();
    d_options = in.value("Options").toByteArray().split(' ');

    int count = in.beginReadArray("Modules");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString absPath = in.value("AbsPath").toString();
        const QString relPath = in.value("RelPath").toString();
        if( QFileInfo(absPath).exists() )
            addFile(absPath);
        else
        {
            absPath = dir.absoluteFilePath(relPath);
            if( QFileInfo(absPath).exists() )
                addFile(absPath);
            else
                qCritical() << "Could not open module" << relPath;
        }
    }
    in.endArray();

    QList<QByteArrayList> paths;
    count = in.beginReadArray("Packages");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString name = in.value("Name").toString();
        paths << name.toLatin1().split('.');
        addPackagePath( paths.back() );
    }
    in.endArray();

    for( int j = 0; j < paths.size(); j++ )
    {
        count = in.beginReadArray(QString::fromUtf8("." + paths[j].join('.')));
        for( int i = 0; i < count; i++ )
        {
            in.setArrayIndex(i);
            QString absPath = in.value("AbsPath").toString();
            const QString relPath = in.value("RelPath").toString();
            if( QFileInfo(absPath).exists() )
                addFile(absPath, paths[j]);
            else
            {
                absPath = dir.absoluteFilePath(relPath);
                if( QFileInfo(absPath).exists() )
                    addFile(absPath, paths[j]);
                else
                    qCritical() << "Could not open module" << relPath;
            }
        }
        in.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
    return true;
}

bool Project2::saveTo(const QString& filePath)
{
    d_filePath = filePath;
    const bool res = save();
    emit sigRenamed();
    return res;
}

#ifdef QT_NO_QOBJECT
void Project2::sigModified(bool){}
void Project2::sigRenamed(){}
void Project2::sigReparsed(){}
#endif

