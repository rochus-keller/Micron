#ifndef MICASTLOADER_H
#define MICASTLOADER_H

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

#include <QByteArray>
#include <QHash>
#include <QString>

namespace Mil
{
    class Declaration;
    class Type;
    struct Constant;
}

namespace Mic
{
    class AstModel;
    class Declaration;
    class Type;
    class ModuleResolver;

    class AstLoader
    {
    public:
        AstLoader(AstModel* mdl, ModuleResolver* resolver = 0);

        Declaration* loadModule( Mil::Declaration* milModule );

        QString getError() const { return d_error; }
        bool hasError() const { return !d_error.isEmpty(); }

        // this lets a driver relate a reconstructed INVAR MIC procedure back to its MIL body
        const QHash<Mil::Declaration*, Declaration*>& declMap() const { return d_map; }

    private:
        void createShell( Mil::Declaration* milDecl );
        void buildHierarchy();
        void attachToParent( Mil::Declaration* milDecl, Declaration* micDecl );

        void fillType( Mil::Declaration* milDecl, Declaration* micDecl );
        void fillProc( Mil::Declaration* milDecl, Declaration* micDecl );
        void fillConst( Mil::Declaration* milDecl, Declaration* micDecl );
        void fillVar( Mil::Declaration* milDecl, Declaration* micDecl );

        Type* mapType( Mil::Type* );
        Type* mapNamedType( Mil::Type* ); // returns 0 if not a resolvable named reference
        Type* buildInlineType( Mil::Type* );
        Declaration* makeField( Mil::Declaration* milField );
        Declaration* makeParam( Mil::Declaration* milParam, int index );

        void appendMember( Declaration* scope, Declaration* member );
        bool error( const QString& );
        static QByteArray sym( const QByteArray& );

        AstModel* mdl;
        ModuleResolver* resolver;
        QString d_error;
        Mil::Declaration* d_milModule;
        Declaration* d_micModule;
        QHash<Mil::Declaration*, Declaration*> d_map; // MIL decl -> MIC decl, produced by the last loadModule call
        QHash<QByteArray, Declaration*> d_byName; // MIL flattened name -> MIC decl
    };
}

#endif // MICASTLOADER_H
