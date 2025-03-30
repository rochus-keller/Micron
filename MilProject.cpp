#include "MilProject.h"
#include <QDir>
#include <QElapsedTimer>
#include <QtDebug>
#include "MilParser2.h"
#include "MilLexer.h"
using namespace Mil;

Project::Project(AstModel* mdl):mdl(mdl)
{
    Q_ASSERT(mdl);
}

void Project::clear()
{

}

void Project::setFiles(const QStringList& files)
{
    clear();
    allMilFiles << ":/runtime/MIC.mil";
    allMilFiles << files;
}

static QStringList collectFiles( const QDir& dir, const QStringList& suffix )
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ), suffix );

    files = dir.entryList( suffix, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append(dir.absoluteFilePath(f));
    }
    return res;
}

void Project::collectFilesFrom(const QString& rootPath)
{
    clear();
    allMilFiles << ":/runtime/MIC.mil";
    allMilFiles << collectFiles(rootPath, QStringList() << "*.mil");
}

class Lex : public Scanner2
{
public:
    Lexer lex;
    Token next()
    {
        return lex.nextToken();
    }

    Token peek(int offset)
    {
        return lex.peekToken(offset);
    }

    QString sourcePath() const
    {
        return lex.getSourcePath();
    }
};

void Project::parse()
{
    int ok = 0;
    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, allMilFiles )
    {
        Lex lex;
        lex.lex.setStream(file);
        Parser2 p(mdl, &lex, this);
        qDebug() << "**** parsing" << file;
        bool errorsFound = false;
        while( p.parseModule() )
        {
            if( !p.errors.isEmpty() )
            {
                foreach( const Parser2::Error& e, p.errors )
                    qCritical() << e.path << e.row << e.col << e.msg;
                p.errors.clear();
                errorsFound = true;
            }else
            {
                Declaration* module = p.takeModule();
                qDebug() << "module" << module->name;
                if( !mdl->addModule(module) )
                    delete module;
            }
        }
        if( !errorsFound )
            ok++;
    }
    qDebug() << "#### finished with" << ok << "files ok of total" << allMilFiles.size() << "files" << "in" <<
                timer.elapsed() << " [ms]";
}

Declaration*Project::loadModule(const Import& imp)
{
    // we assume here that all modules were already loaded and parsed
    return mdl->findModuleByName(imp.moduleName);
}

