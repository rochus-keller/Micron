/*
* Copyright 2019-2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

// Adopted from Oberon+

#include "MicLexer.h"
#include <QBuffer>
#include <QFile>
#include <QIODevice>
#include <QtDebug>
#include <ctype.h>
using namespace Mic;

QHash<QByteArray,QByteArray> Lexer::d_symbols;

Lexer::Lexer(QObject *parent) : QObject(parent),
    d_lastToken(Tok_Invalid),d_lineNr(0),d_colNr(0),d_in(0),
    d_ignoreComments(true), d_packComments(true),d_enableExt(true), d_sensExt(true),
    d_sensed(false), d_sloc(0), d_lineCounted(false)
{

}

void Lexer::setStream(QIODevice* in, const QString& sourcePath, const QDateTime& ts)
{
    if( in == 0 )
        setStream( sourcePath );
    else
    {
        d_in = in;
        d_lineNr = 0;
        d_colNr = 0;
        d_sourcePath = sourcePath;
        d_lastToken = Tok_Invalid;
    }
}

bool Lexer::setStream(const QString& sourcePath)
{
    QIODevice* in = 0;

    QFile* file = new QFile(sourcePath, this);
    if( !file->open(QIODevice::ReadOnly) )
    {
        delete file;
        return false;
    }
    in = file;

     // else
    setStream( in, sourcePath );
    return true;
}

Token Lexer::nextToken()
{
    Token t;
    if( !d_buffer.isEmpty() )
    {
        t = d_buffer.first();
        d_buffer.pop_front();
    }else
        t = nextTokenImp();
    while( t.d_type == Tok_Comment && d_ignoreComments )
        t = nextToken();
    return t;
}

Token Lexer::peekToken(quint8 lookAhead)
{
    Q_ASSERT( lookAhead > 0 );
    while( d_buffer.size() < lookAhead )
    {
        Token t = nextTokenImp();
        while( t.d_type == Tok_Comment && d_ignoreComments )
            t = nextTokenImp();
        d_buffer.push_back( t );
    }
    return d_buffer[ lookAhead - 1 ];
}

QList<Token> Lexer::tokens(const QString& code)
{
    return tokens( code.toLatin1() );
}

QList<Token> Lexer::tokens(const QByteArray& code, const QString& path)
{
    QBuffer in;
    in.setData( code );
    in.open(QIODevice::ReadOnly);
    setStream( &in, path );

    QList<Token> res;
    Token t = nextToken();
    while( t.isValid() )
    {
        res << t;
        t = nextToken();
    }
    return res;
}

QByteArray Lexer::getSymbol(const QByteArray& str)
{
    if( str.isEmpty() )
        return str;
    QByteArray& sym = d_symbols[str];
    if( sym.isEmpty() )
        sym = str;
    return sym;
}

static inline bool isHexDigit( char c )
{
    return ::isdigit(c) || c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F'
            || c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e' || c == 'f';
}

Token Lexer::nextTokenImp()
{
    if( d_in == 0 )
        return token(Tok_Eof);
    skipWhiteSpace();

    while( d_colNr >= d_line.size() )
    {
        if( d_in->atEnd() )
        {
            Token t = token( Tok_Eof, 0 );
            if( d_in->parent() == this )
                d_in->deleteLater();
            return t;
        }
        nextLine();
        skipWhiteSpace();
    }
    Q_ASSERT( d_colNr < d_line.size() );
    while( d_colNr < d_line.size() )
    {
        const char ch = quint8(d_line[d_colNr]);

        if( ch == '"' || ch == '\'' )
            return string();
        else if( ch == '$' )
                return hexstring();
        else if( ::isalpha(ch) || ( ch == '_' ) )
            return ident();
        else if( ::isdigit(ch) )
            return number();
        // else
        int pos = d_colNr;
        TokenType tt = tokenTypeFromString(d_line,&pos);

        if( tt == Tok_Latt )
            return comment();
        else if( tt == Tok_2Slash )
        {
            const int len = d_line.size() - d_colNr;
            return token( Tok_Comment, len, d_line.mid(d_colNr,len) );
        }else if( tt == Tok_Invalid || pos == d_colNr )
            return token( Tok_Invalid, 1, QString("unexpected character '%1' %2").arg(char(ch)).arg(int(ch)).toUtf8() );
        else {
            const int len = pos - d_colNr;
            return token( tt, len, d_line.mid(d_colNr,len) );
        }
    }
    Q_ASSERT(false);
    return token(Tok_Invalid);
}

int Lexer::skipWhiteSpace()
{
    const int colNr = d_colNr;
    while( d_colNr < d_line.size() && ( ( ::isspace( d_line[d_colNr] ) || d_line[d_colNr] == char(28) ) ) )
        d_colNr++;
    return d_colNr - colNr;
}

void Lexer::nextLine()
{
    d_colNr = 0;
    d_lineNr++;
    d_line = d_in->readLine();
    d_lineCounted = false;

    if( d_line.endsWith("\r\n") )
        d_line.chop(2);
    else if( d_line.endsWith('\n') || d_line.endsWith('\r') || d_line.endsWith('\025') )
        d_line.chop(1);
}

int Lexer::lookAhead(int off) const
{
    if( int( d_colNr + off ) < d_line.size() )
    {
        return d_line[ d_colNr + off ];
    }else
        return 0;
}

Token Lexer::token(TokenType tt, int len, const QByteArray& val)
{
    if( tt != Tok_Invalid && tt != Tok_Comment && tt != Tok_Eof )
        countLine();

    QByteArray v = val;
    if( tt != Tok_Comment && tt != Tok_Invalid )
        v = getSymbol(v);
    Token t( tt, d_lineNr, d_colNr + 1, len, v );
    d_lastToken = t;
    d_colNr += len;
    t.d_sourcePath = d_sourcePath;
    return t;
}

static inline bool isAllLowerCase( const QByteArray& str )
{
    for( int i = 0; i < str.size(); i++ )
    {
        if( !::islower(str[i] ) )
                return false;
    }
    return true;
}

Token Lexer::ident()
{
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        if( !::isalnum(c) && c != '_' )
            break;
        else
            off++;
    }
    const QByteArray str = d_line.mid(d_colNr, off );
    if( !isAscii(str) )
        return token( Tok_Invalid, off, "invalid characters in identifier" );
    Q_ASSERT( !str.isEmpty() );
    int pos = 0;
    QByteArray keyword = str;
    if( d_sensExt && !d_sensed )
    {
        d_sensed = true;
        if( isAllLowerCase(keyword) )
        {
            keyword = keyword.toUpper();
            TokenType t = tokenTypeFromString( keyword, &pos );
            if( t != Tok_Invalid && pos == keyword.size() )
            {
                d_enableExt = true;
                return token( t, off );
            }
        }
    }else if( d_enableExt && isAllLowerCase(keyword) )
        keyword = keyword.toUpper();
    TokenType t = tokenTypeFromString( keyword, &pos );
    if( t != Tok_Invalid && pos != keyword.size() )
        t = Tok_Invalid;
    if( t != Tok_Invalid )
        return token( t, off );
    else
        return token( Tok_ident, off, str );
}

static inline bool checkHexNumber( QByteArray str )
{
    const int pos = str.indexOf('\'');
    if( pos != -1 )
        str = str.left(pos);
    if( str.size() < 2 || ( !str.endsWith('H') && !str.endsWith('h')
                            && !str.endsWith('L') && !str.endsWith('l')
                            && !str.endsWith('I') && !str.endsWith('i')
                            && !str.endsWith('X') && !str.endsWith('x') ) )
        return false;
    else
        return true;
}

static inline bool checkDecNumber( QByteArray str, bool oneOff = false )
{
    for( int i = 0; i < (str.size() - (oneOff ? 1 : 0)); i++ )
    {
        if( !::isdigit(str[i]) )
            return false;
    }
    return true;
}

Token Lexer::number()
{
    // integer      ::=  digit {digit} ['I' | 'L'] | digit {hexDigit} 'H' ['I' | 'L']
    // real         ::=  digit {digit} '.' {digit} [ScaleFactor]
    // ScaleFactor  ::=  ('E'|'D'|'S') ['+' | '-'] digit {digit}
    int lhsPlaces = 0, rhsPlaces = 0, expPlaces = 0;
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        if( !isHexDigit(c) ) // also accepts d and e!
            break;
        else
            off++;
    }
    lhsPlaces = off;
    bool isHex = false;
    bool is64bit = false;
    bool is32bit = false;
    bool isChar = false;
    bool isReal = false;
    int commaPos = -1, ePos = -1;
    const char o1 = lookAhead(off);
    if( o1 == 'L' || o1 == 'l' )
    {
        is64bit = true;
        off++;
    }else if( o1 == 'I' || o1 == 'i' )
    {
        is32bit = true;
        off++;
    }else if( o1 == 'H' || o1 == 'h' )
    {
        isHex = true;
        off++;
        const char o2 = lookAhead(off);
        if( o2 == 'L' || o2 == 'l' )
        {
            is64bit = true;
            off++;
        }else if( o2 == 'I' || o2 == 'i' )
        {
            is32bit = true;
            off++;
        }
    }else if( o1 == 'X' || o1 == 'x' )
    {
        isChar = true;
        off++;
    }else if( o1 == '.' && lookAhead(off+1) == '.' )
    {
        ; // look for decimal point but not for range
    }else if( o1 == '.'  )
    {
        if( !checkDecNumber(d_line.mid(d_colNr, off) ) )
                return token( Tok_Invalid, off, "invalid mantissa" );
        commaPos = off;
        off++;
        isReal = true;
        while( true )
        {
            const char c = lookAhead(off);
            if( !::isdigit(c) )
                break;
            else
                off++;
            rhsPlaces++;
        }
        const char de = lookAhead(off);
        if( de == 'E' || de == 'D' || de == 'S' || de == 'e' || de == 'd' || de == 's' )
        {
            is64bit = ( de == 'D' || de == 'd' );
            is32bit = ( de == 'S' || de == 's' );

            ePos = off;
            off++;
            char o = lookAhead(off);
            if( o == '+' || o == '-' )
            {
                off++;
                o = lookAhead(off);
            }
            if( !::isdigit(o) )
                return token( Tok_Invalid, off, "invalid real" );
            while( true )
            {
                const char c = lookAhead(off);
                if( !::isdigit(c) )
                    break;
                else
                    off++;
                expPlaces++;
            }
        }
    }
    QByteArray str = d_line.mid(d_colNr, off );
    Q_ASSERT( !str.isEmpty() );
    if( isHex && !checkHexNumber(str) )
        return token( Tok_Invalid, off, "invalid hexadecimal integer" );
    else if( isChar && !checkHexNumber(str) )
        return token( Tok_Invalid, off, "invalid hexadecimal string" );

    if( isChar )
    {
        return token( Tok_hexchar, off, str );
    }
    else if( isReal)
    {
        Token tok = token( Tok_real, off, str );
        QByteArray mantissa = ePos != -1 ? str.left(ePos) : str;
        QByteArray lhs = mantissa;
        if( commaPos != -1 )
        {
            lhs = lhs.left(commaPos);
            mantissa.remove(commaPos,1);
        }
        bool mOk, lOk;
        const quint64 l = lhs.toULongLong(&lOk);
        const quint64 m = mantissa.toULongLong(&mOk); // !ok if mantissa is too large
        const int e = ePos != -1 ? str.mid(ePos+1).toInt() : 0;
        tok.d_double = !is32bit && ( !mOk || is64bit || e > 127 || e < -126 || m > 8388607 );
        if( is32bit && ( !lOk || e > 127 || e < -126 || l > 8388607 ) )
            return token( Tok_Invalid, off, "literal too large for REAL" );
        if( tok.d_double && ( e > 1023 || e < -1022 || l > 9007199254740991L ) )
            return token( Tok_Invalid, off, "literal too large for LONGREAL" );
        return tok;
    }else if( !isHex && !checkDecNumber(str, is32bit || is64bit) )
        return token( Tok_Invalid, off, "invalid decimal integer" );
    else
        // NOTE: we dont have to deal with is32bit and is64bit here because the string includes the suffices
        return token( Tok_integer, off, str );
}

void Lexer::parseComment( const QByteArray& str, int& pos, int& level )
{
    enum State { Idle, Lb, Star } state = Idle;
    while( pos < str.size() )
    {
        const char* tmp = str.constData() + pos;
        const char c = str[pos++];
        switch( state )
        {
        case Idle:
            if( c == '(')
                state = Lb;
            else if( c == '*' )
                state = Star;
            break;
        case Lb:
            if( c == '*' )
            {
                level++;
                state = Idle;
            }else if( c != '(')
                state = Idle;
            break;
        case Star:
            if( c == ')')
            {
                level--;
                state = Idle;
            }else if( c != '*' )
                state = Idle;
            if( level <= 0 )
                return;
            break;
        }
    }
}

static quint32 readUInt32(QIODevice* in)
{
    const QByteArray buf = in->read(4);
    if( buf.size() != 4 )
    {
        qWarning() << "cannot read 4 bytes from stream";
        return 0;
    }
    const quint8* raw = (const quint8*)buf.constData();
    const quint32 len = raw[0] + ( raw[1] << 8 ) + ( raw[2] << 16 ) + ( raw[3] << 24 );
    return len;
}

static quint8 readUInt8(QIODevice* in)
{
    char ch;
    in->getChar(&ch);
    return ch;
}

static QByteArray readString(QIODevice* in)
{
    QByteArray res;
    while( !in->atEnd() )
    {
        char ch;
        in->getChar(&ch);
        if( ch == 0 )
            break;
        res += ch;
    }
    return res;
}

QByteArray Lexer::extractText(QIODevice* in)
{
    if( in == 0 )
        return QByteArray();
    QByteArray text;
    in->reset();
    skipBom(in);
    text = in->readAll();

    return text;
}

bool Lexer::skipBom(QIODevice* in)
{
    const QByteArray buf = in->peek(3);
    if( buf.size() == 3 && buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf )
    {
        in->read(3);
        return true;
    }else
        return false;
}

Token Lexer::comment()
{
    const int startLine = d_lineNr;
    const int startCol = d_colNr;


    int level = 0;
    int pos = d_colNr;
    parseComment( d_line, pos, level );
    QByteArray str = d_line.mid(d_colNr,pos-d_colNr);
    while( level > 0 && !d_in->atEnd() )
    {
        nextLine();
        pos = 0;
        parseComment( d_line, pos, level );
        if( !str.isEmpty() )
            str += '\n';
        str += d_line.mid(d_colNr,pos-d_colNr);
    }
    if( d_packComments && level > 0 && d_in->atEnd() )
    {
        d_colNr = d_line.size();
        Token t( Tok_Invalid, startLine, startCol + 1, str.size(), tr("non-terminated comment").toLatin1() );
        t.d_sourcePath = d_sourcePath;
        return t;
    }
    // Col + 1 weil wir immer bei Spalte 1 beginnen, nicht bei Spalte 0
    Token t( ( d_packComments ? Tok_Comment : Tok_Latt ), startLine, startCol + 1, str.size(), str );
    t.d_sourcePath = d_sourcePath;
    d_lastToken = t;
    d_colNr = pos;
    if( !d_packComments && level == 0 )
    {
        Token t(Tok_Ratt,d_lineNr, pos - 2 + 1, 2 );
        t.d_sourcePath = d_sourcePath;
        d_lastToken = t;
        d_buffer.append( t );
    }
    return t;
}

static bool isPotentialIdent( const QByteArray& str )
{
    bool firstAlphaFound = false;
    for( int i = 0; i < str.size(); i++ )
    {
        const char ch = str[i];
        if( firstAlphaFound )
        {

        }else if( ::isdigit(ch) )
            return false;
        else if( ::isalpha(ch) )
            firstAlphaFound = true;
    }
    return firstAlphaFound;
}

Token Lexer::string()
{
    const char quote = lookAhead(0);
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        off++;
        if( c == quote )
            break;
        if( c == 0 )
            return token( Tok_Invalid, off, "non-terminated string" );
    }
    const QByteArray str = d_line.mid(d_colNr, off );
    return token( Tok_string, off, str );
}

enum { HEX_PENDING, HEX_END, HEX_INVALID };
static int readHex( QByteArray& to, const QByteArray& from, int& pos )
{
    int res = HEX_PENDING;
    for( ; pos < from.size(); pos++ )
    {
        const char ch = from[pos];
        if( ch == '$' )
        {
            res = HEX_END;
            pos++;
            break;
        }else if( ::isspace(ch) )
            ; // ignore space
        else if( isHexDigit(ch) )
            to.append(ch);
        else
        {
            res = HEX_INVALID;
            break;
        }
    }
    return res;
}

Token Lexer::hexstring()
{
    countLine();
    // inofficial extension of Oberon found in ProjectOberon,
    // e.g. arrow := SYSTEM.ADR($0F0F 0060 0070 0038 001C 000E 0007 8003 C101 E300 7700 3F00 1F00 3F00 7F00 FF00$);
    // in Display.Mod; sogar über mehrere Zeilen zulässig

    const int startLine = d_lineNr;
    const int startCol = d_colNr;

    QByteArray str;
    int pos = d_colNr + 1;
    int res = readHex(str, d_line, pos);

    while( res == HEX_PENDING && !d_in->atEnd() )
    {
        nextLine();
        countLine();
        pos = d_colNr;
        res = readHex(str, d_line, pos);
    }
    if( d_packComments && res != HEX_END )
    {
        d_colNr = pos;
        Token t( Tok_Invalid, startLine, startCol + 1, str.size(), tr("non-terminated hexadecimal string").toLatin1() );
        t.d_sourcePath = d_sourcePath;
        return t;
    }
    // else

    if( d_packComments || ( res == HEX_END && startLine == d_lineNr ) )
    {
        Token t( Tok_hexstring, startLine, startCol + 1,
                 startLine == d_lineNr ? pos - startCol : str.size(), str );
        t.d_sourcePath = d_sourcePath;
        d_lastToken = t;
        d_colNr = pos;
        return t;
    }else
    {
        Token t1( Tok_Dlr, startLine, startCol + 1,
                 startLine == d_lineNr ? pos - startCol : str.size(), str );
        t1.d_sourcePath = d_sourcePath;
        d_lastToken = t1;
        d_colNr = pos;
        if( res == HEX_END )
        {
            Token t2(Tok_Dlr,d_lineNr, pos - 1, 2 );
            t2.d_sourcePath = d_sourcePath;
            d_lastToken = t2;
            d_buffer.append( t2 );
        }
        return t1;
    }
}

bool Lexer::isHexstring(int off) const
{
    for( int i = d_colNr + off; i < d_line.size(); i++ )
    {
        const char ch = d_line[i];
        if( ch == '$' )
            return true;
        if( !isHexDigit(ch) && !::isspace(ch) )
            return false;
    }
    const QByteArray buf = d_in->peek(1000); // RISK
    for( int i = 0; i < buf.size(); i++ )
    {
        const char ch = buf[i];
        if( ch == '$' )
            return true;
        if( !isHexDigit(ch) && !::isspace(ch) )
            return false;
    }
    return false;
}

void Lexer::countLine()
{
    if( !d_lineCounted )
        d_sloc++;
    d_lineCounted = true;
}

bool Lexer::isUtf8(const QByteArray& str)
{
    if( isAscii(str) )
        return false;
    // Quelle: https://stackoverflow.com/questions/1031645/how-to-detect-utf-8-in-plain-c
    // see also http://www.perlmonks.org/?node=910102
    const unsigned char * bytes = (const unsigned char *)str.constData();
    while(*bytes)
    {
        if( (// ASCII
             // use bytes[0] <= 0x7F to allow ASCII control characters
                bytes[0] == 0x09 ||
                bytes[0] == 0x0A ||
                bytes[0] == 0x0D ||
                (0x20 <= bytes[0] && bytes[0] <= 0x7E)
            )
        ) {
            bytes += 1;
            continue;
        }

        if( (// non-overlong 2-byte
                (0xC2 <= bytes[0] && bytes[0] <= 0xDF) &&
                (0x80 <= bytes[1] && bytes[1] <= 0xBF)
            )
        ) {
            bytes += 2;
            continue;
        }

        if( (// excluding overlongs
                bytes[0] == 0xE0 &&
                (0xA0 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF)
            ) ||
            (// straight 3-byte
                ((0xE1 <= bytes[0] && bytes[0] <= 0xEC) ||
                    bytes[0] == 0xEE ||
                    bytes[0] == 0xEF) &&
                (0x80 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF)
            ) ||
            (// excluding surrogates
                bytes[0] == 0xED &&
                (0x80 <= bytes[1] && bytes[1] <= 0x9F) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF)
            )
        ) {
            bytes += 3;
            continue;
        }

        if( (// planes 1-3
                bytes[0] == 0xF0 &&
                (0x90 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF) &&
                (0x80 <= bytes[3] && bytes[3] <= 0xBF)
            ) ||
            (// planes 4-15
                (0xF1 <= bytes[0] && bytes[0] <= 0xF3) &&
                (0x80 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF) &&
                (0x80 <= bytes[3] && bytes[3] <= 0xBF)
            ) ||
            (// plane 16
                bytes[0] == 0xF4 &&
                (0x80 <= bytes[1] && bytes[1] <= 0x8F) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF) &&
                (0x80 <= bytes[3] && bytes[3] <= 0xBF)
            )
        ) {
            bytes += 4;
            continue;
        }

        return false;
    }

    return true;
}

bool Lexer::isAscii(const QByteArray& str)
{
    const quint8* bytes = (const quint8*)str.constData();
    for( int i = 0; i < str.size(); i++ )
    {
        if( bytes[i] >= 0x80 )
            return false;
    }
    return true;
}

bool Lexer::isValidIdent(const QByteArray& str)
{
    if( str.isEmpty() || !isAscii(str) )
        return false;

    if( !(::isalpha(str[0]) || str[0] == '_') )
        return false;
    for( int i = 1; i < str.size(); i++ )
    {
        if( !::isalnum(str[i]) && str[i] != '_')
            return false;
    }
    if( isAllLowerCase(str) && tokenTypeFromString( str.toUpper() ) != Tok_Invalid )
        return false;
    if( tokenTypeFromString( str ) != Tok_Invalid )
        return false;
    return true;
}
