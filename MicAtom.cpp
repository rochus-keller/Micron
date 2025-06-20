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

#include "MicAtom.h"
#include <QHash>
using namespace Mic;

static QHash<QByteArray,QByteArray> d_atoms;


QByteArray Atom::getAtom(const QByteArray& str)
{
    if( str.isEmpty() )
        return str;
    QByteArray& sym = d_atoms[str];
    if( sym.isEmpty() )
        sym = str;
    return sym;
}
