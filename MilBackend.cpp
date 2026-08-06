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

#include "MilBackend.h"
#include "MicAst.h"
#include "MilArmv7Renderer.h"
#include "MilRv32Renderer.h"
#include "MilX86Renderer.h"
#include "MilElfLinker.h"
#include "MilCeeGen.h"
#include <QtDebug>
#include <QDir>

using namespace Mil;

static bool isInterfaceOnly(Mil::Declaration* module)
{
    // TODO: this needs a proper solution; the goal is to keep custom MIC$ impls
    for( Mil::Declaration* sub = module->subs; sub; sub = sub->next )
        if( sub->kind == Mil::Declaration::Procedure && !sub->extern_ )
            return false;
    return true;
}

static bool skipModule(Mil::Declaration* module)
{
    return module->generic || module->extern_ ||
            ( module->name == "MIC$" && isInterfaceOnly(module) );
}


QStringList Backend::compileX86(Mil::AstModel& mdl, const QString& outPath, bool dbg, bool indirectMain, bool cdeclRet)
{
    // x86 backend: generate ELF32 relocatable objects for each module
    mdl.calcMemoryLayouts(4 /*pointerWidth*/, 4 /*stackAlignment*/);

    QStringList objFiles;
    bool hasErrors = false;

    foreach( Mil::Declaration* module, mdl.getModulesInDependencyOrder() )
    {
        if( skipModule(module) )
            continue;

        // Reset all flags for this module's compile pass
        foreach( Mil::Declaration* m, mdl.getModules() )
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

        Mil::X86::Renderer renderer(&mdl);
        renderer.setCdeclReturns(cdeclRet);
        renderer.setEmitDwarf(dbg);

        if( !renderer.renderModule(module) )
        {
            qCritical() << "error generating x86 code for module" << module->name
                        << ":" << renderer.errorMessage();
            hasErrors = true;
            break;
        }

        const QString objFile = QDir(outPath).absoluteFilePath(Mil::CeeGen::escapeFilename(module->name) + ".o");

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
        foreach( Mil::Declaration* module, mdl.getRootModules() )
        {
            if( module->name == "MIC$" || module->extern_ )
                continue;
            moduleNames << module->name;
        }
        const QString mainObj = QDir(outPath).absoluteFilePath("main+.o");
        if( Mil::X86::Renderer::generateMainObject(moduleNames, mainObj, indirectMain) )
        {
            qDebug() << "  generated" << mainObj;
            objFiles << mainObj;
        }
        else
            qCritical() << "cannot generate main+.o";
    }else
        objFiles.clear();
    return objFiles;
}

QStringList Backend::compileRv32(Mil::AstModel& mdl, const QString& outPath, bool dbg, bool indirectMain, bool useRvAbi,
                       bool hasFloat, bool hasHwDiv, bool esp32)
{
    // RV32 backend: generate ELF relocatable objects for each module
    mdl.calcMemoryLayouts(4 /*pointerWidth*/, 4 /*stackAlignment*/);

    QStringList objFiles;
    bool hasErrors = false;

    foreach( Mil::Declaration* module, mdl.getModulesInDependencyOrder() )
    {
        if( skipModule(module) )
            continue;

        // Reset all flags for this module's compile pass
        foreach( Mil::Declaration* m, mdl.getModules() )
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

        Mil::Rv32::Renderer renderer(&mdl);
        renderer.setEmitDwarf(dbg);
        renderer.setUseRvAbi(useRvAbi);
        renderer.setHasFloat(hasFloat);
        renderer.setHardwareDivide(hasHwDiv);

        if( !renderer.renderModule(module) )
        {
            qCritical() << "error generating RV32 code for module" << module->name
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
        qDebug() << "#### generated" << objFiles.size() << "RV32 ELF relocatable object files";

        // Generate main.o that calls all module inits in dependency order
        QByteArrayList moduleNames;
        foreach( Mil::Declaration* module, mdl.getRootModules() )
        {
            if( module->name == "MIC$" || module->extern_ )
                continue;
            moduleNames << module->name;
        }
        const QString mainObj = QDir(outPath).absoluteFilePath("main+.o");
        if( Mil::Rv32::Renderer::generateMainObject(moduleNames, mainObj, indirectMain ))
        {
            qDebug() << "  generated" << mainObj;
            objFiles << mainObj;
        }
        else
            qCritical() << "cannot generate main+.o";
    }else
        objFiles.clear();
    return objFiles;
}

QStringList Backend::compileArm(Mil::AstModel& mdl, const QString& outPath, bool dbg, bool indirectMain, bool useAapcs, bool hasHwDiv)
{
    // ARMv7 backend: generate ELF relocatable objects for each module
    mdl.calcMemoryLayouts(4 /*pointerWidth*/, 4 /*stackAlignment*/);

    QStringList objFiles;
    bool hasErrors = false;

    foreach( Mil::Declaration* module, mdl.getModulesInDependencyOrder() )
    {
        if( skipModule(module) )
            continue;

        // Reset all flags for this module's compile pass
        foreach( Mil::Declaration* m, mdl.getModules() )
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

        Mil::Arm::Renderer renderer(&mdl);
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
        foreach( Mil::Declaration* module, mdl.getRootModules() )
        {
            if( module->name == "MIC$" || module->extern_ )
                continue;
            moduleNames << module->name;
        }
        const QString mainObj = QDir(outPath).absoluteFilePath("main+.o");
        if( Mil::Arm::Renderer::generateMainObject(moduleNames, mainObj, indirectMain) )
        {
            qDebug() << "  generated" << mainObj;
            objFiles << mainObj;
        }
        else
            qCritical() << "cannot generate main+.o";
    }else
        objFiles.clear();
    return objFiles;
}

bool Backend::linkExecutable(const QStringList& objFiles, const QStringList& libDirs,
                           const QStringList& linkLibs, const QStringList& linkObjs,
                           const QString& outPath, const QString& exeName,
                           bool esp32, qint64 baseAddress)
{
    if( libDirs.isEmpty() && linkLibs.isEmpty() && linkObjs.isEmpty() )
    {
        qDebug() << "#### no link options given, skipping link step";
        return true;
    }

    Mil::ElfLinker linker;
    if( baseAddress >= 0 )
        linker.setBaseAddress((quint32)baseAddress); // e.g. bare metal, instead of the default OS load address
    if( esp32 )
        linker.setEsp32MemoryMap(0x40020000, 0x40010000, 0x4FF20000); // intentionally using 0x40020000 and not 0x40000000
            // we simply move .rodata higher up in the cache window, out of the Mask ROM's shadow.
            // 0x40020000 is safely inside the Flash Cache MMU window. The bootloader will correctly map it.
            // It is far above the Internal ROM. When the CPU reads 0x40020194, it will fetch the actual 0x50118000 literal from the Flash.

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
