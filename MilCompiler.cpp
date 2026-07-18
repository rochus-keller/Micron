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
#include "MilBackend.h"
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
    QCommandLineOption op("O", "set the path where compiled modules are stored", "path");
    cp.addOption(op);

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

    const QString arch_ = cp.value(arch);
    const QStringList libDirs = cp.values(libs);
    const QStringList linkLibs = cp.values(linkLib);
    const QStringList linkObjs = cp.values(linkObj);
    const bool dbg_ = cp.isSet(dbg);
    const QStringList outPaths = cp.values(op);
    if( outPaths.size() > 1 )
    {
        qCritical() << "only one output path can be set";
        return -1;
    }
    QString outPath;
    if( !outPaths.isEmpty() )
        outPath = outPaths.first();
    else
        outPath = QFileInfo(args.first()).absolutePath();

    if( arch_ == "arm7" || arch_ == "armv7" )
        Mil::Backend::compileArm(mdl, outPath, libDirs, linkLibs, linkObjs, info.baseName(), dbg_, cp.isSet(aapcs));
    if( arch_ == "rv32" || arch_ == "riscv32" )
        Mil::Backend::compileRv32(mdl, outPath, libDirs, linkLibs, linkObjs, info.baseName(), dbg_, cp.isSet(aapcs),
                    /*hasFloat*/true, /*hasHwDiv*/true, cp.isSet(esp32opt));
    if( arch_ == "x86" || arch_ == "i386" )
        Mil::Backend::compileX86(mdl, outPath, libDirs, linkLibs, linkObjs, info.baseName(), dbg_, cp.isSet(cdeclRet));

    if( cp.isSet(cgen) )
        pro.generateC();
    if( cp.isSet(dump2) )
        pro.interpret(true);
    if( cp.isSet(run) )
        pro.interpret();

    return 0;
}
