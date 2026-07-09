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

#include "MilLexer.h"
#include <QBuffer>
#include <QFile>
#include <QIODevice>
#include <QtDebug>
#include <ctype.h>
using namespace Mil;

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
            {
                d_in->deleteLater();
                d_in = 0;
            }
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
        else if( ch == '#' )
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

static inline bool isKeyword(TokenType tt)
{
    switch( tt )
    {
    case Tok_TYPE:
    case Tok_VAR:
    case Tok_END:
    case Tok_PROCEDURE:
    case Tok_PROC:
    case Tok_BEGIN:
    case Tok_IMPORT:
    case Tok_CONST:
        return true;
    default:
        return false;
    }
}

Token Lexer::token(TokenType tt, int len, const QByteArray& val)
{
    if( tt != Tok_Invalid && tt != Tok_Comment && tt != Tok_Eof )
        countLine();

    QByteArray v = val;
    if( tt != Tok_Comment && tt != Tok_Invalid )
        v = Token::getSymbol(v);
    Token t( tt, d_lineNr, d_colNr + 1, len, v );
    d_lastToken = t;
    d_colNr += len;
    t.d_sourcePath = d_sourcePath;
    if( tt > TT_Keywords && tt < TT_Specials)
    {
        t.d_code = tt;
        if( isKeyword(tt) )
            t.d_type = tt;
        else
            t.d_type = Tok_ident;
        // there are no keywords; whenever one is found it is
        // acutally an ident which can also be used as a keyword
    }

    return t;
}

static inline bool isAllLowerCase( const QByteArray& str )
{
    for( int i = 0; i < str.size(); i++ )
    {
        if( str[i] == '_' || ::isdigit(str[i]) )
            continue;
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
        if( !::isalnum(c) && c != '_' && c != '$' )
            break;
        else
            off++;
    }
    const QByteArray str = d_line.mid(d_colNr, off );
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
        return token( t, off, str );
    else
        return token( Tok_ident, off, str );
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

static inline bool checkOctalNumber( QByteArray str, bool oneOff = false )
{
    for( int i = 0; i < (str.size() - (oneOff ? 1 : 0)); i++ )
    {
        if( !(str[i] >= '0' && str[i] <= '7') )
            return false;
    }
    return true;
}

static inline bool checkBinaryNumber( QByteArray str, bool oneOff = false )
{
    for( int i = 0; i < (str.size() - (oneOff ? 1 : 0)); i++ )
    {
        if( !(str[i] >= '0' && str[i] <= '1') )
            return false;
    }
    return true;
}

Token Lexer::number()
{
    // unsigned     ::=  digit {digit} ['O' | 'Z'] | digit {hexDigit} 'H'
    // float        ::=  digit {digit} '.' {digit} [Exponent]
    // Exponent     ::= 'E' ['+' | '-'] digit {digit}
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
    bool isOctal = false;
    bool isBinary = false;
    bool isReal = false;
    int commaPos = -1, ePos = -1;
    const char o1 = lookAhead(off);
    if( o1 == 'O' || o1 == 'o' )
    {
        isOctal = true;
        off++;
    }else if( o1 == 'Z' || o1 == 'z' )
    {
        isBinary = true;
        off++;
    }else if( o1 == 'H' || o1 == 'h' )
    {
        isHex = true;
        off++;
    }else if( o1 == '.' && lookAhead(off+1) == '.' )
    {
        ; // look for decimal point but not for '..'
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
        if( de == 'E' || de == 'e' )
        {
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

    if( isReal)
        return token( Tok_float, off, str );
    else
    {
        if( isOctal && !checkOctalNumber(str,true) )
            return token( Tok_Invalid, off, "invalid octal integer" );
        if( isBinary && !checkBinaryNumber(str,true) )
            return token( Tok_Invalid, off, "invalid binary integer" );
        if( !(isHex || isOctal || isBinary) && !checkDecNumber(str, false) )
            return token( Tok_Invalid, off, "invalid decimal integer" );
        return token( Tok_unsigned, off, str );
    }
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

QByteArrayList Lexer::isMilModule(const QString &sourcePath)
{
    Lexer lex;
    lex.setStream(sourcePath);
    lex.setIgnoreComments(true);
    lex.setEnableExt(true);
    Token t = lex.nextToken();
    QByteArrayList res;
    while( t.isValid() )
    {
        if( t.d_code == Tok_MODULE )
        {
            t = lex.nextToken();
            if( t.d_code == Tok_LINE )
            {
                t = lex.nextToken();
                if( t.d_code != Tok_unsigned )
                    return QByteArrayList();
                t = lex.nextToken();
            }
            if( t.d_type != Tok_ident )
                return QByteArrayList();
            else
                res << t.d_val;
        }
        t = lex.nextToken();
    }
    return res;
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

Token Lexer::comment()
{
    const int startLine = d_lineNr;
    const int startCol = d_colNr;


    int level = 0;
    int pos = d_colNr;
    parseComment( d_line, pos, level );
    QByteArray str = d_line.mid(d_colNr,pos-d_colNr);
    while( level > 0 && d_in && !d_in->atEnd() )
    {
        nextLine();
        pos = 0;
        parseComment( d_line, pos, level );
        if( !str.isEmpty() )
            str += '\n';
        str += d_line.mid(d_colNr,pos-d_colNr);
    }
    if( d_packComments && level > 0 && d_in && d_in->atEnd() )
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
        if( ch == '#' )
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

    const int startLine = d_lineNr;
    const int startCol = d_colNr;

    QByteArray str;
    int pos = d_colNr + 1;
    int res = readHex(str, d_line, pos);

    while( res == HEX_PENDING && d_in && !d_in->atEnd() )
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
        Token t1( Tok_Hash, startLine, startCol + 1,
                 startLine == d_lineNr ? pos - startCol : str.size(), str );
        t1.d_sourcePath = d_sourcePath;
        d_lastToken = t1;
        d_colNr = pos;
        if( res == HEX_END )
        {
            Token t2(Tok_Hash,d_lineNr, pos - 1, 2 );
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
        if( ch == '#' )
            return true;
        if( !isHexDigit(ch) && !::isspace(ch) )
            return false;
    }
    if( d_in == 0 )
        return false;
    const QByteArray buf = d_in->peek(1000); // RISK
    for( int i = 0; i < buf.size(); i++ )
    {
        const char ch = buf[i];
        if( ch == '#' )
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
