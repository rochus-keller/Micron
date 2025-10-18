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
#include <Micron/MilAst.h>
#include <QList>

namespace Mil {

	class Scanner2 {
	public:
		virtual Token next() = 0;
		virtual Token peek(int offset) = 0;
        virtual QString sourcePath() const = 0;
	};

	class Parser2 {
	public:
        Parser2(AstModel* m, Scanner2* s, Importer* imp);
        ~Parser2();

        bool parseModule(); // true: module found, false: no module found
        Declaration* takeModule(); // get module declaration and take ownership (otherwise deleted by parser)

		struct Error {
		    QString msg;
            RowCol pos;
		    QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
		};
		QList<Error> errors;
	protected:
        qint64 integer();
        void number(Constant* c);
        Declaration* qualident(Quali* = 0);
        Declaration* trident(bool maybeQuali = false);
        Declaration* identdef(Declaration::Kind k);
		void ConstDeclaration();
		void TypeDeclaration();
        Type* type();
        Type* NamedType(bool allowAny = false);
        Type* ArrayType();
        quint32 length();
        Type* StructUnionType();
        void FieldList();
        DeclList IdentList(Declaration::Kind k);
        Type* ObjectType();
		void MemberList();
        Type* PointerType();
        Type* ProcedureType();
		void VariableDeclaration();
		void ProcedureDeclaration();
        QByteArray Binding();
        void ProcedureBody(Declaration* proc);
		void LocalDeclaration();
        Type* FormalParameters();
        Type* ReturnType();
		void FPSection();
		void module();
		void ImportList();
        void ImporterList();
        void import();
        void importer();
		void DeclarationSequence();
        void Line();
        Expression* Expression_();
        Expression* ExpInstr();
        Statement* StatementSequence();
        Statement* Statement_();
        Statement* IfThenElse();
        Statement* Loop();
        Statement* Switch();
        Statement* RepeatUntil();
        Statement* WhileDo();
		void MetaActuals();
		void MetaParams();
        Constant* ConstExpression();
        Constant* ConstExpression2();
        Constant* constructor();
        ComponentList* component_list();
        void component(Component &cp);
        quint32 numberOrIdent(bool param); // param or local var

	protected:
		void next();
		Token peek(int off);
		void invalid(const char* what);
		bool expect(int tt, bool pkw, const char* where);
        void error( const Token&, const QString& msg);
        void error( const RowCol&, const QString& msg );
        Declaration* addDecl(const Token& id, Declaration::Kind, bool public_ = false);
        Declaration* addDecl(const QByteArray& name, const RowCol& pos, Declaration::Kind, bool public_ = false);
        Declaration* qualident2(const Quali& q, const RowCol& pos);
        RowCol select(const RowCol& pos) const
        {
            if( source.isEmpty() )
                return pos;
            else
                return curPos;
        }
        bool nextIsLine();

    private:
        AstModel* mdl;
        Importer* imp;
        Token cur;
        Token la;
        Scanner2* scanner;
        QList<Declaration*> scopeStack;
        QList<Type*> unresolved;
        Declaration* curMod;
        Declaration* curDecl;
        QString source;
        RowCol curPos;
        bool firstModule;
        bool lineFound;
	};
}
#endif // include
