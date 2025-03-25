#ifndef MIL_TOKEN_H
#define MIL_TOKEN_H

/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QString>
#include <MilTokenType.h>
#include <MicRowCol.h>

namespace Mil
{
    struct Token
    {
#ifdef _DEBUG
        union
        {
            quint16 d_type; // TokenType
            TokenType d_tokenType;
        };
        union
        {
            quint16 d_code; // TokenType, the pseudo keyword which an ident represents, or invalid
            TokenType d_codeType;
        };
#else
        quint16 d_type; // TokenType
        quint8 d_code; // TokenType, signifies a pseudo keyword
#endif
        quint16 d_len;

        uint d_lineNr : Mic::RowCol::ROW_BIT_LEN; // supports 524k lines
        uint d_colNr : Mic::RowCol::COL_BIT_LEN; // supports 4k chars per line
        uint d_double : 1;     // originally unused, now set if floating point mantissa or exponent require double precision

        QByteArray d_val;
        QString d_sourcePath;
        Token(quint16 t = Tok_Invalid, quint32 line = 0, quint16 col = 0, quint16 len = 0, const QByteArray& val = QByteArray() ):
            d_type(t),d_lineNr(line),d_colNr(col),d_len(len),d_val(val),d_double(0),d_code(0){}
        bool isValid() const;
        bool isEof() const;
        const char* getName() const;
        const char* getString() const;
        Mic::RowCol toRowCol() const { return Mic::RowCol(d_lineNr,d_colNr); }
        Mic::Loc toLoc() const { return Mic::Loc(d_lineNr,d_colNr,d_sourcePath); }
        static QByteArray getSymbol( const QByteArray& );
    };

    typedef QList<Token> TokenList;
}

#endif // MIL_TOKEN_H
