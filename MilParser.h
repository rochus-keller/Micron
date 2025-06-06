#ifndef __MIL_PARSER__
#define __MIL_PARSER__
// This file was automatically generated by EbnfStudio; don't modify it!

#include <Micron/MilSynTree.h>

namespace Mil {

	class Scanner {
	public:
		virtual Token next() = 0;
		virtual Token peek(int offset) = 0;
	};

	class Parser {
	public:
		Parser(Scanner* s):scanner(s) {}
		void RunParser();
		SynTree root;
		struct Error {
		    QString msg;
		    int row, col;
		    QString path;
		    Error( const QString& m, int r, int c, const QString& p):msg(m),row(r),col(c),path(p){}
		};
		QList<Error> errors;
	protected:
		void Mil(SynTree*);
		void integer(SynTree*);
		void number(SynTree*);
		void qualident(SynTree*);
		void trident(SynTree*);
		void identdef(SynTree*);
		void Line(SynTree*);
		void ConstDeclaration(SynTree*);
		void TypeDeclaration(SynTree*);
		void type(SynTree*);
		void NamedType(SynTree*);
		void ArrayType(SynTree*);
		void length(SynTree*);
		void StructUnionType(SynTree*);
		void FieldList(SynTree*);
		void IdentList(SynTree*);
		void ObjectType(SynTree*);
		void MemberList(SynTree*);
		void PointerType(SynTree*);
		void ProcedureType(SynTree*);
		void VariableDeclaration(SynTree*);
		void ProcedureDeclaration(SynTree*);
		void Binding(SynTree*);
		void ProcedureBody(SynTree*);
		void LocalDeclaration(SynTree*);
		void FormalParameters(SynTree*);
		void ReturnType(SynTree*);
		void FPSection(SynTree*);
		void module(SynTree*);
		void ImportList(SynTree*);
		void import(SynTree*);
		void DeclarationSequence(SynTree*);
		void Expression(SynTree*);
		void ExpInstr(SynTree*);
		void CondOp(SynTree*);
		void StatementSequence(SynTree*);
		void Statement(SynTree*);
		void IfThenElse(SynTree*);
		void Loop(SynTree*);
		void Switch(SynTree*);
		void RepeatUntil(SynTree*);
		void WhileDo(SynTree*);
		void MetaActuals(SynTree*);
		void MetaParams(SynTree*);
		void ConstExpression(SynTree*);
		void ConstExpression2(SynTree*);
		void constructor(SynTree*);
		void component_list(SynTree*);
		void component(SynTree*);
	protected:
		Token cur;
		Token la;
		Scanner* scanner;
		void next();
		Token peek(int off);
		void invalid(const char* what);
		bool expect(int tt, bool pkw, const char* where);
		void addTerminal(SynTree* st);
	};
}
#endif // include
