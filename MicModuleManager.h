#ifndef MICMODULEMANAGER_H
#define MICMODULEMANAGER_H

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

#include "MicAst.h"
#include "MilAst.h"
#include <QHash>
#include <QString>
#include <QStringList>

class QIODevice;

namespace Mic
{
    class AstLoader;

    class ModuleManager : public Importer, public Mil::Importer, public ModuleResolver
    {
    public:
        enum ProviderKind { NotFound, MicSource, MilSource, MilObject };

        struct Location {
            ProviderKind kind;
            QString file;
            Location():kind(NotFound){}
            Location( ProviderKind k, const QString& f ):kind(k),file(f){}
        };

        class ModuleLocator
        {
        public:
            virtual ~ModuleLocator();
            virtual Location locate( const QByteArrayList& path ) = 0;
        };

        ModuleManager( ModuleLocator* locator, bool dbg = false );
        ~ModuleManager();

        // Mic::Importer
        Declaration* loadModule( const Import& imp );
        QByteArray moduleSuffix( const MetaActualList& ma );
        QByteArray modulePath( const QByteArrayList& path );

        // ModuleResolver
        Declaration* resolveModule( const QByteArray& milModuleName );

        Mil::Declaration* onMilImport( const Mil::Import& imp );

        // Mil::Importer
        Mil::Declaration* loadModule( const Mil::Import& imp ) { return onMilImport(imp); }

        Mil::Declaration* loadMil( const QString& path );
        Mil::Declaration* loadMil( QIODevice*, const QString& path );

        void setMilImporter( Mil::Importer* i ) { d_milImporter = i; }

        Declaration* loadProvider( const Location& loc, AstModel* into,
                                   ModuleResolver* resolver,
                                   Mil::Declaration** milOut = 0 );

        void loadRuntime();
        static void setRuntimePath( const QString& );
        void clearModel( bool reloadRuntime = true );

        Mic::AstModel& getMics() { return mics; }
        Mil::AstModel& getModel() { return mdl; }

        Mil::Declaration* milModuleFor( const Import& imp ) const;
        int moduleCount() const { return cache.size(); }
        QList<Declaration*> getMicModules() const;

        Mil::Declaration* invarBody( Declaration* micProc ) const;

        QStringList getLinkObjects() const { return linkObjects; }

        QString getError() const { return d_error; }
        bool hasError() const { return !d_error.isEmpty(); }

    private:
        struct Entry {
            Declaration* mic; // reconstructed or parsed Mic module
            Mil::Declaration* mil; // the module as present in the Mil::AstModel
            bool loading; // in-progress marker for cycle detection
            Entry():mic(0),mil(0),loading(false){}
        };

        Entry* entryFor( const QByteArrayList& path );
        Declaration* loadMicSource( const Import& imp, const QString& file, Entry* e );
        Declaration* loadMilProvider( const Import& imp, const Location& loc, Entry* e );
        Mil::Declaration* findMil( const QByteArray& milName );
        void recordInvar( const AstLoader& al );
        bool error( const QString& );

        ModuleLocator* locator;
        Mil::Importer* d_milImporter;
        Mic::AstModel mics;
        Mil::AstModel mdl;
        QHash<QByteArray, Entry*> cache;   // key: import path joined by '/'
        QHash<Declaration*, Mil::Declaration*> invarProcs;
        QStringList linkObjects;     // .mob paths to add to the linker input
        QString d_error;
        bool dbg;
    };
}

#endif // MICMODULEMANAGER_H
