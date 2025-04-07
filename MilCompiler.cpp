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
#include <QCommandLineParser>


int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    QCommandLineParser cp;
    cp.setApplicationDescription("Micron Intermediate Language (MIL) compiler");
    cp.addHelpOption();
    cp.addVersionOption();
    cp.addPositionalArgument("file", "a single mil file, or the directory searched for *.mil files");
    QCommandLineOption cgen("cgen", "generate C code");
    cp.addOption(cgen);

    cp.process(a);
    const QStringList args = cp.positionalArguments();
    if( args.isEmpty() )
        return -1;

    Mil::AstModel mdl;
    Mil::Project pro(&mdl);
    QFileInfo info(args.first());
    if( info.isDir() )
        pro.collectFilesFrom(info.filePath());
    else
        pro.setFiles(QStringList() << info.filePath());

    const bool result = pro.parse();

    if( result && cp.isSet(cgen) )
        pro.generateC();

    return 0;
}
