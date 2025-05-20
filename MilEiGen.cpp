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

#include "MilEiGen.h"
#include "EiGen/layout.hpp"
#include "EiGen/cdemitter.hpp"
#include "EiGen/cdemittercontext.hpp"
#include "EiGen/stdcharset.hpp"
#include "EiGen/stringpool.hpp"
#include "EiGen/strdiagnostics.hpp"
#include "EiGen/cdgenerator.hpp"
#include <iostream>
using namespace Mil;
using namespace ECS;

class MyEmitter : public Code::Emitter
{
public:
    MyEmitter(Diagnostics& d, StringPool& sp, Charset& c, Code::Platform& p):
        Code::Emitter(d,sp,c,p),ctx(*this,s) {}

    using Smop = Emitter::Context::SmartOperand;
    using Label = Emitter::Context::Label;
    using Context = Code::Emitter::Context;
    using RestoreRegisterState = Code::Emitter::Context::RestoreRegisterState;

    Code::Sections s;
    Context ctx;
};

class EiGen::Imp
{
public:
    enum { target_stack_align = 4, target_pointer_width = 4, target_has_linkregister = 0, int_width = 4 };

    AstModel* mdl;
    Declaration* curMod;
    Declaration* curProc;
    ASCIICharset charset;
    StringPool stringPool;
    StreamDiagnostics diagnostics;
    Layout layout;
    Code::Platform platform;
    MyEmitter emitter;

    Imp(AstModel* mdl):mdl(mdl),curMod(0), curProc(0),
        diagnostics(std::cerr),
        layout(
            {int_width, 1, 8},
            {4, 4, 8},
            target_pointer_width,
            target_pointer_width,
            {0, target_stack_align, target_stack_align}, true),
        platform(layout, target_has_linkregister),
        emitter(diagnostics,stringPool,charset, platform)
    {

    }

};

EiGen::EiGen(AstModel* mdl)
{
    imp = new Imp(mdl);
}

EiGen::~EiGen()
{
    delete imp;
}

bool EiGen::generate(Declaration* module, QIODevice* out)
{

}

