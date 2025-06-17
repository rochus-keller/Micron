#ifndef OBXPROJECT_H
#define OBXPROJECT_H

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

// adopted from Oberon project

#include <QObject>
#include <QStringList>
#include <QExplicitlySharedDataPointer>
#include <Micron/MicAst.h>
#include <Micron/MicMilLoader2.h>

class QDir;

namespace Mic
{
    class Project2 : public QObject, public Mic::Importer
    {
#ifndef QT_NO_QOBJECT
        Q_OBJECT
#endif
    public:
        typedef QByteArrayList VirtualPath;
        // A virtual path is a hierarchical name to identify a Package or a Module
        // either the complete path of a virtual dir or a module in it

        // A Package is just a bunch of physical module files which are associated with the same VirtualPath
        struct Package
        {
            VirtualPath d_path;
            QStringList d_files; // list of absolute module file paths belonging to the package
        };
        typedef QList<Package> PackageList;

        struct FileGroup;

        struct File : public QSharedData
        {
            QString d_filePath;
            FileGroup* d_group;
            QByteArray d_name;
            QByteArray d_cache; // unsafed file contents
            Declaration* d_mod;
            File():d_group(0){}
        };
        typedef QExplicitlySharedDataPointer<File> FileRef;

        struct FileGroup
        {
            VirtualPath d_package;
            QList<File*> d_files;
        };

        struct ModuleSlot
        {
            Mic::Import imp;
            File* file;
            Mic::Declaration* decl;
            Xref xref;
            ModuleSlot():decl(0) {}
            ModuleSlot( const Mic::Import& i, File* f, Mic::Declaration* d):imp(i),file(f),decl(d){}
        };

        typedef QList<FileGroup> FileGroups;
        typedef QHash<QString,FileRef> FileHash; // FilePath -> File
        typedef QList<FileRef> FileList;
        typedef QPair<QByteArray,QByteArray> ModProc; // module.procedure or just module

        explicit Project2(QObject *parent = 0);

        void clear();

        void createNew();
        bool initializeFromDir( const QDir&, bool recursive = false );
        bool initializeFromPackageList( const PackageList& );
        bool loadFrom( const QString& filePath );
        bool save();
        bool saveTo(const QString& filePath );
        void setSuffixes( const QStringList& ); // Form: ".suffix"
        const QStringList& getSuffixes() const { return d_suffixes; }
        const QString& getProjectPath() const { return d_filePath; }
        bool isDirty() const { return d_dirty; }

        void setMain( const ModProc& );
        const ModProc& getMain() const { return d_main; }
        QString renderMain() const;
        void setUseBuiltInOakwood(bool);
        bool useBuiltInOakwood() const { return d_useBuiltInOakwood; }
        QString getWorkingDir(bool resolved = false) const;
        void setWorkingDir( const QString& );
        QString getBuildDir(bool resolved = false) const;
        void setBuildDir( const QString& );
        QByteArrayList getOptions() const { return d_options; }
        void setOptions( const QByteArrayList& );

        bool addFile(const QString& filePath, const VirtualPath& package = QByteArrayList() );
        bool removeFile( const QString& filePath );
        bool addPackagePath(const VirtualPath& path );
        bool removePackagePath( const VirtualPath& path );

        bool parse();

        const FileHash& getFiles() const { return d_files; }
        const FileGroups& getFileGroups() const { return d_groups; }
        FileGroup getRootFileGroup() const;
        FileGroup findFileGroup(const VirtualPath& package ) const;
        QList<Declaration*> getModulesToGenerate(bool includeTemplates=false) const; // in exec/depencency order
        File* findFile( const QString& file ) const; // arg can be a path or module name

        Symbol *findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Declaration ** = 0 ) const;
        Symbol *findSymbolBySourcePos(Declaration* module, quint32 line, quint16 col, Declaration** scopePtr) const;
        typedef QList<QPair<Declaration*, SymList> > UsageByMod;
        UsageByMod getUsage( Declaration* ) const;

        quint32 getSloc() const;

        struct Error {
            QString msg;
            RowCol pos;
            QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
        };
        QList<Error> errors;

        // Mic::Import implementation
        QByteArray modulePath( const QByteArrayList& path )
        {
            return path.join('$');
        }

        QByteArray moduleSuffix( const Mic::MetaActualList& ma )
        {
            // TODO: this is an intermediate solution assuming everything is built from sources in full everytime.
            return "$" + QByteArray::number(modules.size());
        }

        Declaration* loadModule( const Import& imp );

    signals:
        void sigModified(bool);
        void sigRenamed();
        void sigReparsed();

    protected:
        QStringList findFiles(const QDir& , bool recursive = false);
        void touch();
        int findPackage(const VirtualPath& path ) const;
        ModuleSlot* find(const Mic::Import& imp);
        const ModuleSlot* find(const QByteArray& ) const;
        const ModuleSlot* find(Declaration*) const;
        File* toFile(const Mic::Import& imp);

    private:
        AstModel d_mdl;
        Mic::MilLoader2 loader;
        FileHash d_files;
        typedef QList<ModuleSlot> Modules;
        Modules modules;
        FileGroups d_groups;
        QString d_filePath; // path where the project file was loaded from or saved to
        QStringList d_suffixes;
        QByteArrayList d_options;
        QString d_workingDir, d_buildDir;
        ModProc d_main;
        bool d_dirty;
        bool d_useBuiltInOakwood;
    };
}

#endif // OBXPROJECT_H
