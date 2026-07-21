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

        // Policy seam: map an import path to a concrete provider file. Front-ends
        // supply the policy (search/include paths, or a manifest lookup).
        class ModuleLocator
        {
        public:
            virtual ~ModuleLocator();
            virtual Location locate( const QByteArrayList& path ) = 0;
        };

        ModuleManager( ModuleLocator* locator, bool dbg = false );
        ~ModuleManager();

        // Mic::Importer: entry point used by the Micron parser to resolve imports.
        Declaration* loadModule( const Import& imp );
        QByteArray moduleSuffix( const MetaActualList& ma );
        QByteArray modulePath( const QByteArrayList& path );

        // AstLoader::ModuleResolver: resolve a cross-module reference from a
        // reconstructed MIL module to the corresponding Mic module.
        Declaration* resolveModule( const QByteArray& milModuleName );

        // MIL-level importer callback: resolve a MIL import in any format.
        Mil::Declaration* onMilImport( const Mil::Import& imp );

        // Mil::Importer: the seam the MIL parser calls to resolve an import while a
        // loaded .mil/.mob pulls its own dependencies. Forwards to onMilImport.
        Mil::Declaration* loadModule( const Mil::Import& imp ) { return onMilImport(imp); }

        // Parse a MIL module from a .mil text file (or a device holding MIL text,
        // e.g. an ELF ".micron.mod" section) into the shared Mil model.
        Mil::Declaration* loadMil( const QString& path );
        Mil::Declaration* loadMil( QIODevice*, const QString& path );

        // Install an external MIL-import resolver. When set, MIL-level imports
        // (arising while a loaded .mil/.mob pulls its own dependencies) are routed
        // to it instead of this manager's own locator-driven resolution. This lets
        // an orchestrator that owns the import policy (e.g. Project2) compose the
        // manager while keeping its package-aware, source-parsing import path.
        void setMilImporter( Mil::Importer* i ) { d_milImporter = i; }

        // Load a .mil/.mob provider into the shared Mil model and reconstruct its
        // Mic interface into 'into', resolving cross-module references through
        // 'resolver'. Marks a .mob module extern_ and collects its link object, and
        // records INVAR bodies. This is the shared provider mechanism; an external
        // orchestrator (Project2) calls it directly so the loading is not
        // duplicated. Returns the reconstructed Mic module (owned by 'into') or 0.
        Declaration* loadProvider( const Location& loc, AstModel* into,
                                   ModuleResolver* resolver,
                                   Mil::Declaration** milOut = 0 );

        // Load the runtime interface (MIC+) into the shared Mil model.
        void loadRuntime();
        // Clear the shared Mil model and the collected link objects/INVAR bodies,
        // optionally reloading the runtime interface afterwards.
        void clearModel( bool reloadRuntime = true );

        Mic::AstModel& getMics() { return mics; }
        Mil::AstModel& getModel() { return mdl; }

        // The MIL module produced/loaded for a given (already loaded) import, or 0.
        Mil::Declaration* milModuleFor( const Import& imp ) const;
        // Number of modules the manager attempted to load (including failures).
        int moduleCount() const { return cache.size(); }
        // Successfully loaded Mic module interfaces.
        QList<Declaration*> getMicModules() const;

        // For a reconstructed (imported) INVAR Mic procedure, return its MIL body so
        // the compile-time evaluator can interpret it; 0 if unknown. Bodies of
        // INVAR procedures of the module currently being compiled from source are
        // found directly in getMil() by their qualident.
        Mil::Declaration* invarBody( Declaration* micProc ) const;

        // ELF object files (.mob) of loaded providers that must be handed to the
        // linker (they contain the real implementation of the provided modules).
        QStringList getLinkObjects() const { return linkObjects; }

        QString getError() const { return d_error; }
        bool hasError() const { return !d_error.isEmpty(); }

    private:
        struct Entry {
            Declaration* mic;        // reconstructed or parsed Mic module interface
            Mil::Declaration* mil;   // the module as present in the Mil::AstModel
            bool loading;            // in-progress marker for cycle detection
            Entry():mic(0),mil(0),loading(false){}
        };

        Entry* entryFor( const QByteArrayList& path );
        Declaration* loadMicSource( const Import& imp, const QString& file, Entry* e );
        Declaration* loadMilProvider( const Import& imp, const Location& loc, Entry* e );
        Mil::Declaration* findMil( const QByteArray& milName );
        void recordInvar( const AstLoader& al );
        bool error( const QString& );

        ModuleLocator* locator;
        Mil::Importer* d_milImporter; // external MIL-import resolver (optional)
        Mic::AstModel mics;          // used by AstLoader for reconstructed interfaces
        Mil::AstModel mdl;           // the shared Mil model (owned here)
        QHash<QByteArray, Entry*> cache;   // key: import path joined by '/'
        QHash<Declaration*, Mil::Declaration*> invarProcs;
        QStringList linkObjects;     // .mob paths to add to the linker input
        QString d_error;
        bool dbg;
    };
}

#endif // MICMODULEMANAGER_H
