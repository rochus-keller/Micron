#ifndef MILPROJECT_H
#define MILPROJECT_H

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
