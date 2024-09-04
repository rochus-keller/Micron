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

QStringList collectFiles( const QDir& dir, const QStringList& suffix )
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

static QString root;

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

class Manager : public Mic::Importer {
public:
    typedef QHash<QString,Mic::Declaration*> Modules;

    Manager() {}
    ~Manager() {
        Modules::const_iterator i;
        for( i = modules.begin(); i != modules.end(); ++i )
            delete i.value();
    }

    Mic::MilLoader loader;

    QHash<QByteArray,QString> moduleNameToPath;
    Modules modules;

    Mic::Declaration* loadModule( const QByteArrayList& path )
    {
        const QByteArray name = path.join('.');
        const QString file = moduleNameToPath.value(name);
        if( file.isEmpty() )
        {
            qCritical() << "module" << name << "is not included in the files passed to the compiler";
            return 0;
        }else
            return compile(file);
    }

    Mic::Declaration* compile(const QString& file)
    {
        Modules::const_iterator i = modules.find(file);
        if( i != modules.end() )
            return i.value();

#define _GEN_OUTPUT_
#ifdef _GEN_OUTPUT_
        QFileInfo info(file);
        QFile out(info.dir().absoluteFilePath(info.completeBaseName()+".cod"));
        if( !out.open(QIODevice::WriteOnly) )
        {
            qCritical() << "cannot open file for writing:" << out.fileName();
            return 0;
        }
        qDebug() << "**** generating" << out.fileName().mid(root.size()+1);
        //Mic::EiGen r(&out);
        Mic::IlAsmRenderer r(&out);
#else
        Mic::InMemRenderer r(&loader);
#endif
        Lex2 lex;
        lex.sourcePath = file; // to keep file name if invalid
        lex.lex.setStream(file);
        qDebug() << "**** parsing" << file.mid(root.size()+1);
        Mic::MilEmitter e(&r);
        Mic::AstModel mdl;
        Mic::Parser2 p(&mdl,&lex, &e, this);
        p.RunParser();
        Mic::Declaration* res = 0;
        if( !p.errors.isEmpty() )
        {
            foreach( const Mic::Parser2::Error& e, p.errors )
                qCritical() << e.path.mid(root.size()+1) << e.row << e.col << e.msg;
        }else
        {
            res = p.takeModule();
        }
        modules[file] = res;
        return res;
    }
};

static void compile(const QStringList& files)
{
    int ok = 0;
    QElapsedTimer timer;
    timer.start();
    Manager mgr;
    foreach( const QString& file, files )
    {
        const QByteArray name = getModuleName(file);
        mgr.moduleNameToPath[name] = file;
    }
    foreach( const QString& file, files )
    {
        Mic::Declaration* module = mgr.compile(file);
        if( module )
            ok++;
    }
    foreach( const Mic::MilModule& m, mgr.loader.getModules() )
    {
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
        Mic::IlAsmRenderer r(&out);
        m.render(&r);
    }

    Mic::Expression::killArena();
    Mic::AstModel::cleanupGlobals();
    qDebug() << "#### finished with" << ok << "files ok of total" << files.size() << "files" << "in" << timer.elapsed() << " [ms]";
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    if( a.arguments().size() <= 1 )
        return -1;

    QStringList files;
    QFileInfo info(a.arguments()[1]);
    if( info.isDir() )
    {
        files = collectFiles(info.filePath(), QStringList() << "*.mon" << "*.mic");
        root = info.filePath();
    }else
        files.append(info.filePath());
    compile(files);

    return 0;
}
