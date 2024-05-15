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

#include "MilInterpreter.h"

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

class Lex : public Mil::Scanner
{
public:
    Mil::Lexer lex;
    Mil::Token next()
    {
        return lex.nextToken();
    }

    Mil::Token peek(int offset)
    {
        return lex.peekToken(offset);
    }
};

static void dump(QTextStream& out, const Mil::SynTree* node, int level)
{
    QByteArray str;
    if( node->d_tok.d_type == Mil::Tok_Invalid )
        level--;
    else if( node->d_tok.d_type < Mil::SynTree::R_First )
    {
        if( Mil::tokenTypeIsKeyword( node->d_tok.d_type ) )
            str = Mil::tokenTypeString(node->d_tok.d_type);
        else if( node->d_tok.d_type > Mil::TT_Specials )
            str = QByteArray("\"") + node->d_tok.d_val + QByteArray("\"");
        else
            str = QByteArray("\"") + Mil::tokenTypeString(node->d_tok.d_type) + QByteArray("\"");

    }else
        str = Mil::SynTree::rToStr( node->d_tok.d_type );
    if( !str.isEmpty() )
    {
        str += QByteArray("\t") + QFileInfo(node->d_tok.d_sourcePath).baseName().toUtf8() +
                ":" + QByteArray::number(node->d_tok.d_lineNr) +
                ":" + QByteArray::number(node->d_tok.d_colNr);
        QByteArray ws;
        for( int i = 0; i < level; i++ )
            ws += "|  ";
        str = ws + str;
        out << str.data() << endl;
    }
    foreach( Mil::SynTree* sub, node->d_children )
        dump( out, sub, level + 1 );
}

static void runInterpreter(const QStringList& files)
{
    int ok = 0;
    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, files )
    {
        Lex2 lex;
        lex.sourcePath = file; // to keep file name if invalid
        lex.lex.setStream(file);
        QBuffer out;
        out.open(QIODevice::WriteOnly);
        Mic::IlAsmRenderer ar(&out);
        Mic::MilEmitter e(&ar);
        Mic::AstModel mdl;
        Mic::Parser2 p(&mdl,&lex, &e);
        qDebug() << "**** parsing" << file.mid(root.size()+1);
        p.RunParser();
        out.close();
        qDebug() << out.data().constData();
        if( !p.errors.isEmpty() )
        {
            foreach( const Mic::Parser2::Error& e, p.errors )
                qCritical() << e.path.mid(root.size()+1) << e.row << e.col << e.msg;
            // break;
        }else
        {
            ok++;
            qDebug() << "ok";

            Lex l;
            out.open(QIODevice::ReadOnly);
            l.lex.setStream(&out,"MIL");
            Mil::Parser p2(&l);
            p2.RunParser();
            if( !p2.errors.isEmpty() )
            {
                foreach( const Mil::Parser::Error& e, p2.errors )
                    qCritical() << e.path << e.row << e.col << e.msg;
                // break;
            }else
            {
#if 0
                QTextStream s(stdout);
                dump(s,&p2.root,0);
#endif

                Mil::Interpreter ip;
                ip.run(&p2.root);
            }
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
    runInterpreter(files);

    return 0;
}
