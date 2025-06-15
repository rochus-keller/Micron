#ifndef _MILBACKEND_H
#define _MILBACKEND_H

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

#include <QStringList>
#include <Micron/MilEiGen.h>

namespace Mil
{
    class Backend {
    public:
        static bool generate(const QString& inFile, const QString& outFile, EiGen::TargetCode, bool debug = false);
        static bool link(const QStringList& inFiles, const QStringList& searchDirs, const QString& outFile, EiGen::TargetCode target, bool linkLib = false, bool debug = false);
    };
}

#endif // _MILBACKEND_H
