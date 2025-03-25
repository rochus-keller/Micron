#ifndef __MIL_PARSER__
#define __MIL_PARSER__

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

#include <Micron/MilToken.h>
#include <QList>

namespace Mil {

	class Scanner2 {
	public:
		virtual Token next() = 0;
		virtual Token peek(int offset) = 0;
	};

	class Parser2 {
	public:
		Parser2(Scanner2* s):scanner(s) {}
		void RunParser();
		struct Error {
		    QString msg;
		    int row, col;
		    QString path;
		    Error( const QString& m, int r, int c, const QString& p):msg(m),row(r),col(c),path(p){}
		};
		QList<Error> errors;
	protected:
		void Mil();
		void integer();
		void number();
		void qualident();
		void trident();
		void identdef();
		void ConstDeclaration();
		void TypeDeclaration();
		void type();
		void NamedType();
		void ArrayType();
		void length();
		void StructUnionType();
		void FieldList();
		void IdentList();
		void ObjectType();
		void MemberList();
		void PointerType();
		void ProcedureType();
		void VariableDeclaration();
		void ProcedureDeclaration();
		void Binding();
		void ProcedureBody();
		void LocalDeclaration();
		void FormalParameters();
		void ReturnType();
		void FPSection();
		void module();
		void ImportList();
		void import();
		void DeclarationSequence();
		void Expression();
		void ExpInstr();
		void CondOp();
		void StatementSequence();
		void Statement();
		void IfThenElse();
		void Loop();
		void Switch();
		void RepeatUntil();
		void WhileDo();
		void MetaActuals();
		void MetaParams();
		void ConstExpression();
		void ConstExpression2();
		void constructor();
		void component_list();
		void component();
	protected:
		Token cur;
		Token la;
		Scanner2* scanner;
		void next();
		Token peek(int off);
		void invalid(const char* what);
		bool expect(int tt, bool pkw, const char* where);
	};
}
#endif // include
