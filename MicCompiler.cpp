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

static void compile(const QStringList& files)
{
    int ok = 0;
    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, files )
    {
        Lex2 lex;
        lex.sourcePath = file; // to keep file name if invalid
        lex.lex.setStream(file);
        QFileInfo info(file);
        QFile out(info.dir().absoluteFilePath(info.completeBaseName()+".cod"));
        if( !out.open(QIODevice::WriteOnly) )
        {
            qCritical() << "cannot open file for writing:" << out.fileName();
            continue;
        }
        Mic::EiGen ar(&out);
        Mic::MilEmitter e(&ar);
        Mic::AstModel mdl;
        Mic::Parser2 p(&mdl,&lex, &e);
        qDebug() << "**** parsing" << file.mid(root.size()+1);
        qDebug() << "**** generating" << out.fileName().mid(root.size()+1);
        p.RunParser();
        out.close();
        if( !p.errors.isEmpty() )
        {
            foreach( const Mic::Parser2::Error& e, p.errors )
                qCritical() << e.path.mid(root.size()+1) << e.row << e.col << e.msg;
            // break;
        }else
        {
            ok++;
            qDebug() << "ok";

        }
    }
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
