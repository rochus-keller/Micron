#ifndef MILBACKEND_H
#define MILBACKEND_H

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

#include <QStringList>

namespace Mil
{
class AstModel;

class Backend
{
public:
    Backend();

    static QStringList compileArm(Mil::AstModel& mdl, const QString& outPath, bool dbg = false, bool indirectMain = true, bool useAapcs = true, bool hasHwDiv = true);

    static QStringList compileRv32(Mil::AstModel& mdl, const QString& outPath, bool dbg = false, bool indirectMain = false, bool useRvAbi = true,
                           bool hasFloat = true, bool hasHwDiv = true, bool esp32 = false);

    static QStringList compileX86(Mil::AstModel& mdl, const QString& outPath, bool dbg = false, bool indirectMain = true, bool cdeclRet = true);

    static bool linkExecutable(const QStringList& objFiles, const QStringList& libDirs,
                                    const QStringList& linkLibs, const QStringList& linkObjs,
                                    const QString& exePath,
                                    bool esp32 = false, qint64 baseAddress = -1);
};
}

#endif // MILBACKEND_H
