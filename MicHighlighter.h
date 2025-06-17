#ifndef MIC_NHIGHLIGHTER_H
#define MIC_NHIGHLIGHTER_H

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

// adopted from the Oberon project

#include <QSyntaxHighlighter>
#include <QSet>

namespace Mic
{
    class Highlighter : public QSyntaxHighlighter
    {
    public:
        enum { TokenProp = QTextFormat::UserProperty };
        explicit Highlighter(QTextDocument *parent = 0);
        void addBuiltIn(const QByteArray& bi);

    protected:
        QTextCharFormat formatForCategory(int) const;

        void highlightBlock(const QString &text);

    private:
        enum Category { C_Num, C_Str, C_Kw, C_Type, C_Ident, C_Op, C_Pp, C_Cmt, C_Section, C_Brack, C_Max };
        QTextCharFormat d_format[C_Max];
        QSet<QByteArray> d_builtins;
     };

    class LogPainter : public QSyntaxHighlighter
    {
    public:
        explicit LogPainter(QTextDocument *parent = 0);
    protected:
        // overrides
        void highlightBlock(const QString &text);
    };
}

#endif // MIC_NHIGHLIGHTER_H
