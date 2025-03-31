#ifndef MILPROJECT_H
#define MILPROJECT_H

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

namespace Mil
{
    // This is a file-based project, a collection of mil-files which can
    // be parsed and fed to AstModel
    class Project : public Importer
    {
    public:
        Project(AstModel*);

        void clear(); // delete all Ast objects and files

        void setFiles(const QStringList&);
        void collectFilesFrom( const QString& rootPath);
        void parse();

    protected:
        Declaration* loadModule( const Import& imp );

    private:
        AstModel* mdl;
        QStringList allMilFiles;
    };
}

#endif // MILPROJECT_H
