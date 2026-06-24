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

#include <QCoreApplication>
#include <QFileInfo>
#include "MilProject.h"
#include "Version.h"
#include <QCommandLineParser>
#include <QtDebug>

extern "C" {
void Args_setArgcArgv(unsigned int c, char** v);
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    a.setOrganizationName("Dr. Rochus Keller");
    a.setOrganizationDomain("www.rochus-keller.ch");
    a.setApplicationName("milc");
    a.setApplicationVersion(MICRON_VERSION);


    QCommandLineParser cp;
    cp.setApplicationDescription(QString("Micron Intermediate Language (MIL) compiler, version %1").arg(MICRON_VERSION));
    cp.addHelpOption();
    cp.addVersionOption();
    cp.addPositionalArgument("file", "a single mil file, or the directory searched for *.mil files");
    QCommandLineOption cgen("cgen", "generate C code");
    cp.addOption(cgen);
    QCommandLineOption run("r", "run in interpreter");
    cp.addOption(run);
    QCommandLineOption dump("d", "dump MIL code");
    cp.addOption(dump);
    QCommandLineOption dump2("l", "dump low-level bytecode");
    cp.addOption(dump2);
    QCommandLineOption oak("oakwood", "add oakwood modules");
    cp.addOption(oak);

    QStringList allArgs = a.arguments();

    // cut away all arguments starting from "--"; they are sent to the interpreter instead
    const int doubledash = allArgs.indexOf("--");
    if( doubledash != -1 )
        allArgs = allArgs.mid(0, doubledash);

    cp.process(allArgs);
    const QStringList args = cp.positionalArguments();
    if( args.isEmpty() )
        cp.showHelp(-1);

    Mil::AstModel mdl;
    Mil::Project pro(&mdl);
    pro.setOakwood(cp.isSet(oak));
    QFileInfo info(args.first());
    if( info.isDir() )
        pro.collectFilesFrom(info.filePath());
    else
        pro.setFiles(QStringList() << info.filePath());

    const bool result = pro.parse();

    if( !result )
        return 1;

    QByteArrayList argc_;
    QVector<char*> argv_;
    argv_.reserve(10);
    argv_.append("Micron Interpreter");

    if( doubledash != -1 )
    {
        for( int i = doubledash+1; i < a.arguments().size(); i++ )
            argc_.append( a.arguments()[i].toUtf8() );

        for( int i = 0; i < argc_.size(); i++ )
        {
            if( !argc_[i].isEmpty() )
                argv_.append(argc_[i].data());
        }
    }

    Args_setArgcArgv( argv_.size(), argv_.data() );

    if( cp.isSet(cgen) )
        pro.generateC();
    if( cp.isSet(dump2) )
        pro.interpret(true);
    if( cp.isSet(run) )
        pro.interpret();

    return 0;
}
