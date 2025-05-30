#ifndef MILEIGEN_H
#define MILEIGEN_H

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

#include <Micron/MilAst.h>

class QIODevice;

namespace Mil
{
    class EiGen
    {
    public:
        typedef enum {NoTarget, AMD32Linux, AMD64Linux, ARMA32Linux, ARMA64Linux, ARMT32Linux, ARMT32FPELinux,
                      BIOS16, BIOS32, BIOS64, DOS, EFI32, EFI64, OSX32, OSX64, RPi2B, Win32, Win64,
                      BareAmd16, BareAmd32, BareAmd64, BareArmA32, BareArmT32, BareArmA64,
                      MaxTarget} TargetCode;
        typedef enum { NoProcessor, Amd16, Amd32, Amd64, Arma32, Armt32, Arma64} ProcessorCode;

        EiGen(AstModel*, TargetCode);
        ~EiGen();

        bool generate(Declaration* module, QIODevice* out);

        static TargetCode translate(const char*);
        static quint8 pointer_width(TargetCode);
        static quint8 stack_align(TargetCode);

    private:
        class Imp;
        Imp* imp;
    };
}

#endif // MILEIGEN_H
