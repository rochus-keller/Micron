#ifndef MICEIGEN_H
#define MICEIGEN_H

/*
** Copyright (C) 2024 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the Micron language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include <Micron/MicMilEmitter.h>

namespace Mic
{
    class EiGen : public MilRenderer
    {
    public:
        EiGen(QIODevice* out);
        ~EiGen();

        void beginModule( const QByteArray& moduleName, const QString& sourceFile, const MilMetaParams& );
        void endModule();

        void addImport( const QByteArray& path, const QByteArray& name );

        void addVariable( const QByteArray& typeRef, QByteArray name );
        void addProcedure(const MilProcedure& method );

        void beginType(const QByteArray& name, bool isPublic, quint8 typeKind);
        void endType();
        void addType( const QByteArray& name, bool isPublic, const QByteArray& baseType,
                      quint8 typeKind, quint32 len = 0);

        void addField( const QByteArray& fieldName,
                       const QByteArray& typeRef,
                       bool isPublic = true );
    private:
        class Imp;
        Imp* imp;
    };
}

#endif // MICEIGEN_H
