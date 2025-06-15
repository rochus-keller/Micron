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

#include "MilBackend.h"
#include "EiGen/debugging.hpp"
#include "EiGen/driver.hpp"
#include "EiGen/stdcharset.hpp"
#include "EiGen/stringpool.hpp"
#include "EiGen/cdgenerator.hpp"
#include "EiGen/amd64.hpp"
#include "EiGen/amd64generator.hpp"
#include "EiGen/arma64generator.hpp"
#include "EiGen/armt32generator.hpp"
#include "EiGen/arma32generator.hpp"
#include "EiGen/asmparser.hpp"
#include "EiGen/assembly.hpp"
#include "EiGen/cdchecker.hpp"
#include "EiGen/objlinker.hpp"
#include "EiGen/dbgdwarfconverter.hpp"
#include <QtDebug>

using namespace Mil;
using namespace ECS;

static ASCIICharset charset;
static StringPool stringPool;
static StreamDiagnostics diagnostics {std::cerr};

static std::string extensionOf(const std::string& path)
{
    const size_t pos = path.find_last_of(".");
    std::string extension;
    if( pos != std::string::npos )
        extension = path.substr(pos);
    return extension;
}

static void generate(const Assembly::Program& program, Assembly::Generator& generator, const char* output, bool debug)
{
    Code::Sections sections;
    Code::Checker checker {diagnostics, charset, generator.platform};
    checker.Check (program, sections);

    Object::Binaries binaries;
    Debugging::Information information;
    std::ostream listing {nullptr};
    generator.Generate (sections, program.source, binaries, information, listing);

    std::string path(output);

    if( debug )
    {
        Debugging::DWARFConverter converter(diagnostics, charset);
        Object::Binaries dbfobj;
        converter.Convert (information, program.source, dbfobj);
        ECS::File dbf_file {path, ".dbf"};
        dbf_file << dbfobj;
        // TODO register_for_cleanup(dbf_file.getPath().c_str(),1);
    }

    ECS::File object( path, extensionOf(path) ); // File constructor wants to replace the extension of path in any case, even if empty
    object << binaries;
}

static void generate_amd16(const Assembly::Program& program, const char* output, bool debug)
{
    AMD64::Generator generator {diagnostics, stringPool, charset, AMD64::RealMode, false, true};
    generate(program, generator, output, debug);
}

static void generate_amd32(const Assembly::Program& program, const char* output, bool debug)
{
    AMD64::Generator generator {diagnostics, stringPool, charset, AMD64::ProtectedMode, true, true}; // RK: was false, true
    // use media instead of legacy float instructions, see https://software.openbrace.org/boards/3/topics/44?r=60#message-60
    // speed-up is factor 10
    generate(program, generator, output, debug);
}

static void generate_amd64(const Assembly::Program& program, const char* output, bool debug)
{
    AMD64::Generator generator {diagnostics, stringPool, charset, AMD64::LongMode, true, true};
    generate(program, generator, output, debug);
}

static void generate_arma32(const Assembly::Program& program, const char* output, bool debug)
{
    ARM::A32::Generator generator {diagnostics, stringPool, charset, true};
    generate(program, generator, output, debug);
}

static void generate_arma64(const Assembly::Program& program, const char* output, bool debug)
{
    ARM::A64::Generator generator {diagnostics, stringPool, charset, false};
    generate(program, generator, output, debug);
}

static void generate_armt32(const Assembly::Program& program, const char* output, bool debug)
{
    ARM::T32::Generator generator {diagnostics, stringPool, charset, false};
    generate(program, generator, output, debug);
}

static void generate_armt32fpe(const Assembly::Program& program, const char* output, bool debug)
{
    ARM::T32::Generator generator {diagnostics, stringPool, charset, true};
    generate(program, generator, output,debug);
}

bool Backend::generate(const QString &inFile, const QString &outFile, EiGen::TargetCode target, bool debug)
{
    try
    {
        const std::string input = inFile.toUtf8().toStdString();
        Assembly::Program program{input};
        Assembly::Parser parser {diagnostics, stringPool, true};
        std::ifstream file;
        file.open (input, file.binary);
        if (!file.is_open ())
            qCritical() << "failed to open input file " << inFile;
        parser.Parse (file, GetLine (Position(file, input, 1, 1)), program);

        const QByteArray output = outFile.toUtf8();

        const QByteArray backend = EiGen::backend(target);
        if( backend == "amd16" )
            generate_amd16(program,output.constData(),debug);
        else if( backend == "amd32" )
            generate_amd32(program,output.constData(),debug);
        else if( backend == "amd64" )
            generate_amd64(program,output.constData(),debug);
        else if( backend == "arma32" )
            generate_arma32(program,output.constData(),debug);
        else if( backend == "arma64" )
            generate_arma64(program,output.constData(),debug);
        else if( backend == "armt32" )
            generate_armt32(program,output.constData(),debug);
        else if( backend == "armt32fpe" )
            generate_armt32fpe(program,output.constData(),debug);
        else
            qCritical() << "no generator available for " << backend;
    }catch(...)
    {
        // already reported
        return false;
    }
    return true;
}

#if defined Q_OS_LINUX || defined Q_OS_UNIX
#include <sys/stat.h>
#endif
#include <deque>

bool Backend::link(const QStringList &inFiles, const QStringList &searchDirs, const QString &outFile, EiGen::TargetCode target, bool linkLib, bool debug)
{
    try
    {
        Object::Linker linker(diagnostics);
        Object::Binaries binaries;

        std::deque<std::string> paths;
        std::deque<std::string> libs;
        for (int i = 0; i < searchDirs.size(); i++)
            paths.push_back(searchDirs[i].toUtf8().constData());

        // add internal libs
        if( !linkLib && target > EiGen::NoTarget && target <= EiGen::Win64 )
        {
            std::string lib = EiGen::name(target);
            lib += "run.obf";
            libs.push_back(lib);
            lib = EiGen::name(target);
            lib += "mic.lib";
            libs.push_back(lib);
        }

        for (int i = 0; i < inFiles.size(); i++)
        {
            std::ifstream file;
            file.open (inFiles[i].toUtf8().constData(), file.binary);
            if (!file.is_open ())
                qCritical() << "failed to open input file" << inFiles[i];
            file >> binaries;
            if (!file)
                qCritical() << "invalid object file" << inFiles[i];
        }

        if( debug )
        {
            for (int i = 0; i < inFiles.size(); i++)
            {
                std::ifstream dbfin;
                const std::string dbf = ECS::File::replace_extension(inFiles[i].toUtf8().constData(),".dbf");
                dbfin.open (dbf, dbfin.binary);
                if (!dbfin.is_open ())
                    qCritical() << "failed to open input file" <<  dbf.c_str();
                dbfin >> binaries;
                const int s = binaries.size();
                if (!dbfin)
                    qCritical() << "invalid object file" << dbf.c_str();
            }
        }

        for( int i = 0; i < libs.size(); i++ )
        {
            bool found = false;
            for( int j = 0; j < paths.size(); j++ )
            {
                std::string path = paths[j];
                if( path.empty() )
                    continue;
                const char last = path[path.size()-1];
                if( last != '/' && last != '\\' )
                    path += '/';
                path += libs[i];
                std::ifstream file;
                file.open(path, file.binary);
                if (!file.is_open ())
                    continue;
                file >> binaries;
                if (!file)
                    qCritical() << "invalid object file" << path.c_str();
                else
                    found = true;
                break;
            }
            if( !found )
                qCritical() << "could not find library" << libs[i].c_str();
        }

        std::string path = outFile.toUtf8().constData();
        if( target >= EiGen::BareAmd16 && target <= EiGen::BareArmA64)
        {
            Object::MappedByteArrangement ram, rom;
            linker.Link (binaries, rom, ram, rom);
            ECS::File ramfile {path, ".ram", ramfile.binary};
            Object::WriteBinary (ramfile, ram.bytes);
            ECS::File romfile {path, ".rom", romfile.binary};
            Object::WriteBinary (romfile, rom.bytes);
            ECS::File map {path, ".map"};
            map << ram.map << rom.map;
        }else if( linkLib )
        {
            linker.Combine (binaries);
            ECS::File library {path, ".lib"};
            library << binaries;
        }else
        {
            Object::MappedByteArrangement arrangement;
            linker.Link (binaries, arrangement);
            std::string ext = GetContents ("_extension", binaries, charset, ".bin");
            if( ext.empty() )
                ext = extensionOf(path);
            ECS::File file(path, ext, ECS::File::binary);
            Object::WriteBinary (file, arrangement.bytes);
#if defined Q_OS_LINUX || defined Q_OS_UNIX
            chmod(file.getPath().c_str (), S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
#endif
        }
    }catch(...)
    {
        // already reported
        return false;
    }
    return true;
}

