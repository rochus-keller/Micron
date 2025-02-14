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

#include "MicParser2.h"
#include "MicMilEmitter.h"
#include "MicEvaluator.h"
#include "MicBuiltins.h"
#include <QtDebug>
using namespace Mic;

static inline bool FIRST_Micron(int tt) {
	return tt == Tok_MODULE;
}

static inline bool FIRST_number(int tt) {
	return tt == Tok_integer || tt == Tok_real;
}

static inline bool FIRST_qualident(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_identdef(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ConstDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ConstExpression(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_TypeDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_type(int tt) {
	switch(tt){
	case Tok_Hat:
	case Tok_Lpar:
	case Tok_POINTER:
	case Tok_Lbrack:
	case Tok_ARRAY:
	case Tok_PROCEDURE:
	case Tok_ident:
	case Tok_RECORD:
	case Tok_PROC:
		return true;
	default: return false;
	}
}

static inline bool FIRST_NamedType(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ArrayType(int tt) {
	return tt == Tok_Lbrack || tt == Tok_ARRAY;
}

static inline bool FIRST_length(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_VAR:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_RecordType(int tt) {
	return tt == Tok_RECORD;
}

static inline bool FIRST_inline_(int tt) {
	return tt == Tok_IN || tt == Tok_INLINE;
}

static inline bool FIRST_VariantPart(int tt) {
	return tt == Tok_CASE;
}

static inline bool FIRST_FixedPart(int tt) {
	return tt == Tok_IN || tt == Tok_INLINE || tt == Tok_ident;
}

static inline bool FIRST_FieldList(int tt) {
	return tt == Tok_IN || tt == Tok_INLINE || tt == Tok_ident;
}

static inline bool FIRST_IdentList(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_PointerType(int tt) {
	return tt == Tok_Hat || tt == Tok_POINTER;
}

static inline bool FIRST_enumeration(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_constEnum(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_VariableDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_designator(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_selector(int tt) {
	return tt == Tok_Lpar || tt == Tok_Hat || tt == Tok_Lbrack || tt == Tok_Dot;
}

static inline bool FIRST_expression(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_relation(int tt) {
	switch(tt){
	case Tok_Geq:
	case Tok_Gt:
	case Tok_IN:
	case Tok_Eq:
	case Tok_Leq:
    //case Tok_IS:
    case Tok_Hash:
	case Tok_Lt:
		return true;
	default: return false;
	}
}

static inline bool FIRST_SimpleExpression(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_AddOperator(int tt) {
	return tt == Tok_Plus || tt == Tok_OR || tt == Tok_Minus;
}

static inline bool FIRST_term(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_MulOperator(int tt) {
    return tt == Tok_Amp || tt == Tok_DIV || tt == Tok_Star || tt == Tok_MOD || tt == Tok_Slash || tt == Tok_AND;
}

static inline bool FIRST_literal(int tt) {
	switch(tt){
    case Tok_FALSE:
    case Tok_hexchar:
    case Tok_hexstring:
    case Tok_integer:
    case Tok_NIL:
    case Tok_real:
    case Tok_string:
    case Tok_TRUE:
       return true;
	default: return false;
	}
}

static inline bool FIRST_constructor(int tt) {
    return tt == Tok_Lbrace || tt == Tok_ident;
}

static inline bool FIRST_component(int tt) {
    switch(tt){
    case Tok_At:
    case Tok_FALSE:
    case Tok_hexchar:
    case Tok_hexstring:
    case Tok_ident:
    case Tok_integer:
    case Tok_Lbrace:
    case Tok_Lbrack:
    case Tok_Lpar:
    case Tok_Minus:
    case Tok_NIL:
    case Tok_NOT:
    case Tok_Plus:
    case Tok_real:
    case Tok_string:
    case Tok_Tilde:
    case Tok_TRUE:
        return true;
    default: return false;
    }
}

static inline bool FIRST_factor(int tt) {
	switch(tt){
	case Tok_Tilde:
    case Tok_NOT:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_variableOrFunctionCall(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_element(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_statement(int tt) {
	switch(tt){
	case Tok_CASE:
	case Tok_REPEAT:
	case Tok_GOTO:
	case Tok_IF:
	case Tok_LOOP:
	case Tok_WHILE:
	case Tok_FOR:
	case Tok_EXIT:
	case Tok_RETURN:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_assignmentOrProcedureCall(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_StatementSequence(int tt) {
	switch(tt){
	case Tok_Semi:
	case Tok_CASE:
	case Tok_REPEAT:
	case Tok_GOTO:
	case Tok_IF:
	case Tok_LOOP:
	case Tok_WHILE:
	case Tok_FOR:
	case Tok_EXIT:
	case Tok_RETURN:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_gotoLabel(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_GotoStatement(int tt) {
	return tt == Tok_GOTO;
}

static inline bool FIRST_IfStatement(int tt) {
	return tt == Tok_IF;
}

static inline bool FIRST_ElsifStatement(int tt) {
	return tt == Tok_ELSIF;
}

static inline bool FIRST_ElseStatement(int tt) {
	return tt == Tok_ELSE;
}

static inline bool FIRST_CaseStatement(int tt) {
	return tt == Tok_CASE;
}

static inline bool FIRST_Case(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_CaseLabelList(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_LabelRange(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_label(int tt) {
	switch(tt){
	case Tok_Tilde:
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_Lpar:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Minus:
	case Tok_Lbrace:
	case Tok_real:
	case Tok_ident:
	case Tok_At:
		return true;
	default: return false;
	}
}

static inline bool FIRST_WhileStatement(int tt) {
	return tt == Tok_WHILE;
}

static inline bool FIRST_ElsifStatement2(int tt) {
	return tt == Tok_ELSIF;
}

static inline bool FIRST_RepeatStatement(int tt) {
	return tt == Tok_REPEAT;
}

static inline bool FIRST_ForStatement(int tt) {
	return tt == Tok_FOR;
}

static inline bool FIRST_LoopStatement(int tt) {
	return tt == Tok_LOOP;
}

static inline bool FIRST_ExitStatement(int tt) {
	return tt == Tok_EXIT;
}

static inline bool FIRST_procedure(int tt) {
	return tt == Tok_PROCEDURE || tt == Tok_PROC;
}

static inline bool FIRST_ProcedureType(int tt) {
	return tt == Tok_PROCEDURE || tt == Tok_PROC;
}

static inline bool FIRST_ProcedureDeclaration(int tt) {
	return tt == Tok_PROCEDURE || tt == Tok_PROC;
}

static inline bool FIRST_ProcedureHeading(int tt) {
	return tt == Tok_PROCEDURE || tt == Tok_PROC;
}

static inline bool FIRST_block(int tt) {
	return tt == Tok_BEGIN;
}

static inline bool FIRST_ProcedureBody(int tt) {
	switch(tt){
	case Tok_VAR:
	case Tok_PROCEDURE:
	case Tok_TYPE:
	case Tok_CONST:
	case Tok_BEGIN:
	case Tok_PROC:
		return true;
	default: return false;
	}
}

static inline bool FIRST_DeclarationSequence(int tt) {
	return tt == Tok_VAR || tt == Tok_PROCEDURE || tt == Tok_TYPE || tt == Tok_CONST || tt == Tok_PROC;
}

static inline bool FIRST_ReturnStatement(int tt) {
	return tt == Tok_RETURN;
}

static inline bool FIRST_FormalParameters(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_ReturnType(int tt) {
	return tt == Tok_Hat || tt == Tok_POINTER || tt == Tok_ident;
}

static inline bool FIRST_FPSection(int tt) {
    return tt == Tok_ident || tt == Tok_CONST;
}

static inline bool FIRST_ImportList(int tt) {
	return tt == Tok_IMPORT;
}

static inline bool FIRST_import(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_MetaActuals(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_MetaParams(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_MetaSection(int tt) {
	return tt == Tok_TYPE || tt == Tok_CONST || tt == Tok_ident;
}

Parser2::Parser2(AstModel* m, Scanner2* s, MilEmitter* out, Importer* i):
    mdl(m),scanner(s),out(out),imp(i),thisMod(0), thisDecl(0)
{
    ev = new Evaluator(m,out);
}

Parser2::~Parser2()
{
    if( thisMod )
        delete thisMod;
    delete ev;
}

void Parser2::RunParser(const MetaActualList& ma) {
	errors.clear();
	next();
    metaActuals = ma;
    module();
}

Declaration*Parser2::takeModule()
{
    Declaration* res = thisMod;
    thisMod = 0;
    return res;
}

void Parser2::next() {
	cur = la;
	la = scanner->next();
	while( la.d_type == Tok_Invalid ) {
		errors << Error(la.d_val, la.d_lineNr, la.d_colNr, la.d_sourcePath);
		la = scanner->next();
	}
}

Token Parser2::peek(int off) {
	if( off == 1 )
		return la;
	else if( off == 0 )
		return cur;
	else
		return scanner->peek(off-1);
}

void Parser2::invalid(const char* what) {
	errors << Error(QString("invalid %1").arg(what),la.d_lineNr, la.d_colNr, la.d_sourcePath);
    next();
}

bool Parser2::expect(int tt, bool pkw, const char* where) {
	if( la.d_type == tt) { next(); return true; }
    else {
        errors << Error(QString("'%1' expected in %2").arg(tokenTypeString(tt)).arg(where),
                           la.d_lineNr, la.d_colNr, la.d_sourcePath); return false;
    }
}

void Parser2::error(const Token& t, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg,t.d_lineNr, t.d_colNr, t.d_sourcePath);
}

void Parser2::error(int row, int col, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg, row, col, scanner->source());
}

void Parser2::error(const RowCol& pos, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg, pos.d_row, pos.d_col, scanner->source());
}

Declaration* Parser2::findDecl(const Token& id)
{
    Declaration* x = mdl->findDecl(id.d_val);
    if( x == 0 )
        error(id, QString("cannot find declaration of '%1'").arg(id.d_val.constData()));
    return x;
}

bool Parser2::assigCompat(Type* lhs, Type* rhs) const
{
    if( lhs == 0 || rhs == 0 )
        return false;

    if( sameType(lhs,rhs) )
        return true;

    // Te and Tv are numeric types and Tv includes Te
    if( (lhs->isInt() && rhs->isInt()) ||
            (lhs->isUInt() && rhs->isUInt()) ||
            (lhs->isReal() && rhs->isReal()) )
        return lhs->form >= rhs->form;

    // Te and Tv are pointer types and the pointers have equal base types;
    if( lhs->form == Type::Pointer && rhs->form == Type::Pointer &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    // Te and Tv are non-open array types with the same length and have equal base types;
    if( lhs->form == Type::Array && rhs->form == Type::Array && lhs->len != 0 && lhs->len == rhs->len &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    // Tv is a pointer or a procedure type and e is NIL;
    if( ( lhs->form == Type::Pointer || lhs->form == Type::Proc ) && rhs->form == BasicType::Nil )
        return true;

    // Te and Tv are pointer types and Tv is a POINTER TO ANY;
    if( lhs->form == Type::Pointer && rhs->form == Type::Pointer && lhs->base->form == BasicType::Any )
        return true;

    // Tv is a pointer to one dimensional open array, Te is a pointer to any one or more dimensional array,
    // and their element types are equal, or
    if( lhs->form == Type::Pointer && lhs->base->form == Type::Array &&
            lhs->base->len == 0 && lhs->base->base->form != Type::Array &&
            rhs->form == Type::Pointer && rhs->base->form == Type::Array )
    {
        Type* base = rhs->base->base;
        while( base && base->form == Type::Array )
            base = base->base;
        return equalTypes( lhs->base->base, base ) ;
    }

    // Tv is a pointer to a record TR and Te is a pointer to a record the first field of which is of type TR, or
    // is again a record the first field of which is of type TR.
    if( lhs->form == Type::Pointer && lhs->base->form == Type::Record &&
            rhs->form == Type::Pointer && rhs->base->form == Type::Record )
    {
        Declaration* first = !rhs->base->subs.isEmpty() ? rhs->base->subs.first() : 0;
        while(first && first->type->form == Type::Record)
        {
            if( sameType(lhs->base, first->type) )
                return true;
            first = !first->type->subs.isEmpty() ? first->type->subs.first() : 0;
        }
    }

    if( lhs->form == Type::Proc && rhs->form == Type::Proc )
        return matchFormals(lhs->subs, rhs->subs) && matchResultType(lhs->base,rhs->base);

    return false;
}

bool Parser2::assigCompat(Type* lhs, Declaration* rhs) const
{
    // Tv is a procedure type and e is the name of a procedure whose formal parameters match those of Tv.
    if( rhs->mode == Declaration::Procedure )
    {
        if( lhs->form == Type::Proc )
            return matchFormals(lhs->subs, rhs->getParams()) && matchResultType(lhs->base,rhs->type);
        else
            return false;
    }

    // Tv is an enumeration type and e is a valid element of the enumeration;
    if( lhs->form == Type::ConstEnum )
        return lhs->subs.contains(rhs);

    if( lhs->form == Type::Array && lhs->base->form == BasicType::CHAR && lhs->len > 0 &&
            rhs->mode == Declaration::ConstDecl && rhs->type->form == BasicType::String )
        return strlen(rhs->data.toByteArray().constData()) < lhs->len;

    return assigCompat(lhs, rhs->type);
}

bool Parser2::assigCompat(Type* lhs, const Expression* rhs) const
{
    if( lhs == 0 || rhs == 0 )
        return false;

    if( rhs->kind == Expression::TypeDecl )
        return false;

    // Tv is a signed integer and e is an unsigned integer constant, and Tv includes the
    // smallest integer type necessary to represent e.
    if( lhs->isInt() && rhs->isConst() && rhs->type->isUInt() )
        return assigCompat(lhs, ev->smallestIntType(rhs->val));

    if( rhs->kind == Expression::ConstDecl || rhs->kind == Expression::ProcDecl )
        return assigCompat(lhs, rhs->val.value<Declaration*>() );

    if( lhs->form == Type::Array && lhs->base->form == BasicType::CHAR && lhs->len > 0 &&
            rhs->isLiteral() && rhs->type->form == BasicType::String)
        return strlen(rhs->getLiteralValue().toByteArray().constData()) < lhs->len;

    // A string of length 1 can be used wherever a character constant is allowed and vice versa.
    if( lhs->form == BasicType::CHAR && rhs->type->form == BasicType::String )
        return strlen(rhs->val.toByteArray().constData()) == 1;

    return assigCompat(lhs, rhs->type);
}

bool Parser2::paramCompat(Declaration* lhs, const Expression* rhs) const
{
    Q_ASSERT(lhs->mode == Declaration::ParamDecl);

    // Tf is a pointer to an open array of CHAR, f is CONST, and a is string literal
    if( lhs->visi == Declaration::ReadOnly && lhs->type->form == Type::Pointer &&
            lhs->type->base->form == Type::Array && lhs->type->base->base->form == BasicType::CHAR &&
            lhs->type->base->len == 0 && rhs->type->form == BasicType::String )
        return true;

    if( rhs->kind == Expression::TypeDecl )
        return false;
    if( rhs->kind == Expression::ProcDecl )
        return assigCompat(lhs->type,rhs);

    // Tf and Ta are equal types, or Ta is assignment compatible with Tf
    return equalTypes(lhs->type,rhs->type) || assigCompat(lhs->type,rhs);
}

bool Parser2::matchFormals(const QList<Declaration*>& a, const QList<Declaration*>& b) const
{
    if( a.size() != b.size() )
        return false;
    for( int i = 0; i < a.size(); i++ )
    {
        if( i == a.size() - 1 && a[i]->name == ".." && a[i]->name != b[i]->name )
            return false;
        if( a[i]->visi != b[i]->visi )
            return false;
        if( !equalTypes(a[i]->type, b[i]->type) )
            return false;
    }
    return true;
}

bool Parser2::matchResultType(Type* lhs, Type* rhs) const
{
    if( lhs == 0 || rhs == 0 ) // TODO: is this a valid state?
        return false;
    return equalTypes(lhs,rhs) || (lhs->form == BasicType::NoType && rhs->form == BasicType::NoType);
}

bool Parser2::sameType(Type* lhs, Type* rhs) const
{
    return lhs == rhs;
}

bool Parser2::equalTypes(Type* lhs, Type* rhs) const
{
    // Ta and Tb are the same type,
    if(sameType(lhs,rhs))
        return true;

    if( lhs == 0 || rhs == 0 )
        return false;

    // Ta and Tb are pointer types with equal base types
    if( lhs->form == Type::Pointer && rhs->form == Type::Pointer &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    // Ta and Tb are open array types with equal element types, or
    // Ta and Tb are non-open array types with same length and equal element types, or
    if( lhs->form == Type::Array && rhs->form == Type::Array && lhs->len == rhs->len &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    // Ta and Tb are procedure types whose formal parameters match,
    if( lhs->form == Type::Proc && rhs->form == Type::Proc && matchFormals(lhs->subs,rhs->subs) &&
            matchResultType(lhs->base, rhs->base))
        return true;

    return false;
}

void Parser2::ForwardDeclaration()
{
    procedure();
    expect(Tok_Hat, false, "ProcedureDeclaration");
    const IdentDef id = identdef();
    Declaration* forwardDecl = addDecl(id,Declaration::ForwardDecl);
    mdl->openScope(forwardDecl);
    if( FIRST_FormalParameters(la.d_type) ) {
        forwardDecl->type = FormalParameters();
    }

    mdl->closeScope();

    if( la.d_type == Tok_INLINE || la.d_type == Tok_INVAR || la.d_type == Tok_EXTERN ) {
        if( la.d_type == Tok_INLINE ) {
            expect(Tok_INLINE, true, "ProcedureDeclaration");
            forwardDecl->inline_ = true;
        } else if( la.d_type == Tok_INVAR ) {
            expect(Tok_INVAR, true, "ProcedureDeclaration");
            forwardDecl->invar = true;
        } else if( la.d_type == Tok_EXTERN ) {
            expect(Tok_EXTERN, true, "ProcedureDeclaration");
            forwardDecl->extern_ = true;
        } else
            invalid("ProcedureDeclaration");
    }
}

Expression* Parser2::number() {
    Expression* res = Expression::create(Expression::Literal,la.toRowCol());
	if( la.d_type == Tok_integer ) {
		expect(Tok_integer, false, "number");
        QByteArray number = cur.d_val.toLower();
        number.replace('_',"");
        Type* type = 0;
        bool signed_ = false;
        if( number.size() > 1 && number[number.size()-1] == 'u' )
            number.chop(1);
        else if( number.size() > 1 && number[number.size()-1] == 'i' )
        {
            signed_ = true;
            number.chop(1);
        }else if( number.size() > 2 && number[number.size()-2] == 'u' )
        {
            switch(number[number.size()-1])
            {
            case '1':
                type = mdl->getType(BasicType::UINT8);
                break;
            case '2':
                type = mdl->getType(BasicType::UINT16);
                break;
            case '4':
                type = mdl->getType(BasicType::UINT32);
                break;
            case '8':
                type = mdl->getType(BasicType::UINT64);
                break;
            }
            number.chop(2);
        }else if( number.size() > 2 && number[number.size()-2] == 'i' )
        {
            signed_ = true;
            switch(number[number.size()-1])
            {
            case '1':
                type = mdl->getType(BasicType::INT8);
                break;
            case '2':
                type = mdl->getType(BasicType::INT16);
                break;
            case '4':
                type = mdl->getType(BasicType::INT32);
                break;
            case '8':
                type = mdl->getType(BasicType::INT64);
                break;
            }
            number.chop(2);
        }
        const char suffix = ( number.size() > 1 ? number[number.size()-1] : '0' );
        if( !::isdigit(suffix) )
            number.chop(1);
        switch(suffix)
        {
        case 'o':
            res->val = signed_ ? number.toLongLong(0,8) : number.toULongLong(0,8);
            break;
        case 'b':
            res->val = signed_ ? number.toLongLong(0,2) : number.toULongLong(0,2);
            break;
        case 'h':
            res->val = signed_ ? number.toLongLong(0,16) : number.toULongLong(0,16);
            break;
        default:
            res->val = signed_ ? number.toLongLong() : number.toULongLong();
            break;
        }
        Type* derived = signed_ ? ev->smallestIntType(res->val) : ev->smallestUIntType(res->val);
        if( type == 0 )
            type = derived;
        else if( derived->form > type->form )
            error(cur,"the given constant value cannot be represented by the given type");
        res->type = type;
	} else if( la.d_type == Tok_real ) {
		expect(Tok_real, false, "number");
        QByteArray str = cur.d_val;
        str.replace('_',"");
        if( str.contains('d') || str.contains('D') )
        {
            str.replace('d','e');
            str.replace('D','E');
            res->type = mdl->getType(BasicType::LONGREAL);
        }else if( str.contains('s') || str.contains('S') )
        {
            str.replace('f','e');
            str.replace('F','E');
            res->type = mdl->getType(BasicType::REAL);
        }else
        {
            if( cur.d_double )
                res->type = mdl->getType(BasicType::LONGREAL);
            else
                res->type = mdl->getType(BasicType::REAL);
       }
        res->val = str.toDouble(); // we save double in any case
    } else
		invalid("number");
    return res;
}

Parser2::Quali Parser2::qualident() {
    Quali res;
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Dot )  ) {
		expect(Tok_ident, false, "qualident");
        res.first = cur.d_val;
		expect(Tok_Dot, false, "qualident");
	}
	expect(Tok_ident, false, "qualident");
    res.second = cur.d_val;
    return res;
}

Parser2::IdentDef Parser2::identdef() {
    IdentDef res;
    res.visi = IdentDef::Private;
	expect(Tok_ident, false, "identdef");
    res.name = cur;
	if( la.d_type == Tok_Star || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Star ) {
			expect(Tok_Star, false, "identdef");
            res.visi = IdentDef::Public;
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "identdef");
            res.visi = IdentDef::ReadOnly;
		} else
			invalid("identdef");
	}
    return res;
}

void Parser2::ConstDeclaration() {
    const IdentDef id = identdef();
	expect(Tok_Eq, false, "ConstDeclaration");
    Declaration* d = addDecl(id, Declaration::ConstDecl);
    Token t = la;
    Expression* e = ConstExpression(0);
    if( e && !ev->evaluate(e) )
        error(t, ev->getErr());
    Value v = ev->pop();
    d->data = v.val;
    d->type = v.type;
    Expression::deleteAllExpressions();
}

Expression* Parser2::ConstExpression(Type* hint) {
    const Token tok = la;
    Expression* res = expression(hint);
    if( res == 0 )
        return 0;
    if( !res->isConst() )
        error(tok, QString("expression is not constant"));
    return res;
}

void Parser2::TypeDeclaration() {
    const IdentDef id = identdef();
	expect(Tok_Eq, false, "TypeDeclaration");
    Type* t = 0;

    // declare the type immediately so it is known in the forthcoming declaration
    Declaration* d = addDecl(id,Declaration::TypeDecl);
    thisDecl = d;
    Quali q;
    // Token tok = la;
    if( FIRST_NamedType(la.d_type) ) {
        t = NamedType(&q);
    }else
        t = type(false);
    // No, it is allowed here: openArrayError(tok,t);
    if( t && t->decl == 0 )
    {
        t->decl = d;
        d->ownstype = true;
    }
    d->type = t;
    resolveAndCheckType(d);
    thisDecl = 0;
    emitType(t, q);
}

Type* Parser2::type(bool deanonymize) {
    Type* res = mdl->getType(BasicType::Undefined);
    bool isNewType = true;
	if( FIRST_NamedType(la.d_type) ) {
        res = NamedType();
        isNewType = false;
	} else if( FIRST_ArrayType(la.d_type) ) {
        res = ArrayType();
	} else if( FIRST_RecordType(la.d_type) ) {
        res = RecordType();
	} else if( FIRST_PointerType(la.d_type) ) {
        res = PointerType();
	} else if( FIRST_ProcedureType(la.d_type) ) {
        res = ProcedureType();
	} else if( FIRST_enumeration(la.d_type) ) {
        res = enumeration();
	} else
		invalid("type");
    if( res && res->form != BasicType::Undefined && isNewType && deanonymize )
        addHelper(res);
    return res;
}

Type* Parser2::NamedType(Quali* qout,bool allowUnresovedLocal) {
    // allowUnresovedLocal==true...allow pointer base to reference not yet declared record
    const Token tok = la;
    Declaration* d = resolveQualident(qout, allowUnresovedLocal);
    if( d == 0 )
        return 0;
    Type* t = d->type;
    if( thisDecl != 0 )
    {
        // we are in a type declaration; mark each named type by this intermediate object
        // which is then removed and replaced later
        t = new Type();
        t->form = Type::NameRef;
        t->subs.append(d);
        t->base = d->type;
    }
    if( d->mode != Declaration::TypeDecl )
        error(tok, QString("invalid type: %1").arg(d->name.constData()) );
    else if( d == thisDecl )
    {
        // we are in a type declaration using itself;
        // the type is not yet known and has to be resolved later
        t->selfref = true;
    }
    return t;
}

Type* Parser2::ArrayType() {
    Type* etype = 0;
    quint32 len = 0;
    bool vla = false;
    Token tok,tok2;
	if( la.d_type == Tok_ARRAY ) {
		expect(Tok_ARRAY, true, "ArrayType");
		if( FIRST_length(la.d_type) ) {
            tok = la;
            length(len);
            if( len == 0 )
                vla = true;
		}
		expect(Tok_OF, true, "ArrayType");
        tok2 = la;
        etype = type();
	} else if( la.d_type == Tok_Lbrack ) {
		expect(Tok_Lbrack, false, "ArrayType");
		if( FIRST_length(la.d_type) ) {
            tok = la;
            length(len);
            if( len == 0 )
                vla = true;
        }
		expect(Tok_Rbrack, false, "ArrayType");
        tok2 = la;
        etype = type();
	} else
		invalid("ArrayType");

    if(vla)
        error(tok,"VLA not yet supported"); // TODO

    openArrayError(tok2,etype);
    invalidTypeError(tok2,etype);
    Type* arr = new Type();
    arr->form = Type::Array;
    arr->len = len;
    arr->base = etype;
    return arr;
}

void Parser2::length(quint32& len) {
	if( FIRST_ConstExpression(la.d_type) ) {
        const Token tok = la;
        Expression* lenExpr = ConstExpression(0);
        if( !ev->evaluate(lenExpr) )
            error(tok, ev->getErr());
        Value v = ev->pop();
        if( !v.type->isUInt() )
        {
            error(tok,QString("an array length must be empty or a positive integer"));
            len = 1;
        }else
        {
            len = v.val.toUInt();
            if( len == 0 )
            {
                error(tok,QString("expecting at least length 1"));
                len = 1;
            }
        }
	} else if( la.d_type == Tok_VAR ) {
		expect(Tok_VAR, true, "length");
        Token t = la;
        if( !ev->evaluate(expression(0)) )
            error(t, ev->getErr());
        Value v = ev->pop();
        // TODO
        len = 0;
        Expression::deleteAllExpressions();
    } else
		invalid("length");
}

Type* Parser2::RecordType() {
	expect(Tok_RECORD, true, "RecordType");
    Type* rec = new Type();
    rec->form = Type::Record;
    mdl->openScope(0);
	if( FIRST_FixedPart(la.d_type) ) {
        FixedPart();
	}
	if( FIRST_VariantPart(la.d_type) ) {
        VariantPart();
	}
	expect(Tok_END, true, "RecordType");
    rec->subs = toList(mdl->closeScope(true));
    return rec;
}

bool Parser2::inline_() {
    bool res = false;
	if( la.d_type == Tok_INLINE ) {
		expect(Tok_INLINE, true, "inline_");
        res = true;
	} else if( la.d_type == Tok_IN ) {
		expect(Tok_IN, true, "inline_");
        res = true;
	} else
		invalid("inline_");
    return res;
}

void Parser2::VariantPart() {
	expect(Tok_CASE, true, "VariantPart");
	while( la.d_type == Tok_Bar || FIRST_inline_(la.d_type) || FIRST_identdef(la.d_type) ) {
		if( la.d_type == Tok_Bar ) {
			expect(Tok_Bar, false, "VariantPart");
		}
        bool isInl = false;
		if( FIRST_inline_(la.d_type) ) {
			inline_();
            isInl = true;
		}
        const IdentDef id = identdef();
		expect(Tok_Colon, false, "VariantPart");
        const Token tok = la;
        Type* t = type();
        openArrayError(tok,t);
        invalidTypeError(tok,t);
        Declaration* d = addDecl(id,Declaration::Variant);
        d->inline_ = isInl;
        d->type = t;
	}
}

void Parser2::FixedPart() {
    FieldList();
	if( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "FixedPart");
	}
	while( FIRST_FieldList(la.d_type) ) {
        FieldList();
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "FixedPart");
		}
	}
}

void Parser2::FieldList() {
	if( FIRST_IdentList(la.d_type) ) {
        const IdentDefList l = IdentList();
		expect(Tok_Colon, false, "FieldList");
        const Token tok = la;
        Type* t = type();
        openArrayError(tok,t);
        invalidTypeError(tok,t);
        for(int i = 0; i < l.size(); i++ )
        {
            Declaration* d = addDecl(l[i],Declaration::Field);
            d->type = t;
        }
	} else if( FIRST_inline_(la.d_type) ) {
		inline_();
        const IdentDef id = identdef();
		expect(Tok_Colon, false, "FieldList");
        const Token tok = la;
        Type* t = type();
        openArrayError(tok,t);
        invalidTypeError(tok,t);
        Declaration* d = addDecl(id,Declaration::Field);
        d->inline_ = true;
        d->type = t;
    } else
		invalid("FieldList");
}

Parser2::IdentDefList Parser2::IdentList() {
    IdentDefList res;
    res << identdef();
	while( la.d_type == Tok_Comma || FIRST_identdef(la.d_type) ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "IdentList");
		}
        res << identdef();
	}
    return res;
}

Type* Parser2::PointerType() {
	if( la.d_type == Tok_POINTER ) {
		expect(Tok_POINTER, true, "PointerType");
		expect(Tok_TO, true, "PointerType");
	} else if( la.d_type == Tok_Hat ) {
		expect(Tok_Hat, false, "PointerType");
	} else
		invalid("PointerType");

    Type* res = new Type();
    res->form = Type::Pointer;

    if( FIRST_NamedType(la.d_type) ) {
        const Token tok = la;
        Quali q;
        Type* t = NamedType(&q, true);
        if( t == 0 )
        {
            res->base = mdl->getType(BasicType::Undefined);
            if( q.first.isEmpty() )
            {
                // we're looking for a local declaration
                res->deferred = true;
                deferred << qMakePair(res,tok);
            } // else import error already reported
        }else
            res->base = t;
    }else
        res->base = type();

    return res;
}

Type* Parser2::enumeration() {
	expect(Tok_Lpar, false, "enumeration");
    Type* res = new Type();
    if( FIRST_constEnum(la.d_type) ) {
        res->subs = constEnum();
        foreach( Declaration* d, res->subs )
            d->type = res;
        res->form = Type::ConstEnum;
    } else
		invalid("enumeration");
	expect(Tok_Rpar, false, "enumeration");

    return res;
}

DeclList Parser2::constEnum() {
	expect(Tok_ident, false, "constEnum");

    DeclList res;

    qint32 index = 0;

    Declaration* d = addDecl(cur, 0, Declaration::ConstDecl);

	if( la.d_type == Tok_Eq ) {
		expect(Tok_Eq, false, "constEnum");
        const Token tok = la;
        if( !ev->evaluate(ConstExpression(0)) )
            error(tok, ev->getErr());
        Value v = ev->pop();
        if( !v.type->isInteger() )
            error(tok,"expecting an integer");
        else
            index = v.val.toInt();
	}

    d->data = index;
    res << d;

	while( la.d_type == Tok_Comma || la.d_type == Tok_ident ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "constEnum");
		}
		expect(Tok_ident, false, "constEnum");
        d = addDecl(cur, 0, Declaration::ConstDecl);
        d->data = ++index;
        res << d;
    }
    return res;
}

void Parser2::VariableDeclaration() {
    const IdentDefList ids = IdentList();
	expect(Tok_Colon, false, "VariableDeclaration");
    const Token tok = la;
    Type* t = type();
    if( t == 0 )
        return;
    openArrayError(tok,t);
    invalidTypeError(tok,t);
    Declaration* outer = mdl->getTopScope();
    foreach( const IdentDef& id, ids )
    {
        Declaration* d = addDecl(id,outer->mode == Declaration::Module ?
                                     Declaration::VarDecl : Declaration::LocalDecl);
        d->outer = outer;
        d->type = t;
        if( d->mode == Declaration::VarDecl )
            out->addVariable(ev->toQuali(d->type),d->name);
        else
            out->addLocal(ev->toQuali(d->type),d->name);
    }
}

Expression*Parser2::toExpr(Declaration* d, const RowCol& rc)
{
    Expression::Kind k = Expression::Invalid;
    QVariant val;

    switch( d->mode )
    {
    case Declaration::Builtin:
        k = Expression::Builtin;
        val = d->id;
        break;
    case Declaration::Procedure:
    case Declaration::ForwardDecl:
        k = Expression::ProcDecl;
        val = QVariant::fromValue(d->alias ? d->link : d);
        break;
    case Declaration::ConstDecl:
        k = Expression::ConstDecl;
        val = QVariant::fromValue(d);
        break;
    case Declaration::VarDecl:
        k = Expression::ModuleVar;
        val = QVariant::fromValue(ev->toQuali(d));
        break;
    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
        Q_ASSERT(d->outer);
        val = d->outer->getIndexOf(d);
        Q_ASSERT(val.toInt() != -1 );
        k = d->mode == Declaration::LocalDecl ? Expression::LocalVar : Expression::Param;
        break;
    case Declaration::TypeDecl:
        k = Expression::TypeDecl;
        val = QVariant::fromValue(ev->toQuali(d));
        break;
    default:
        error(rc.d_row, rc.d_col, "invalid designator");
        return 0;
    }
    Expression* res = Expression::create(k,rc);
    res->val = val;
    res->type = d->type;
    res->visi = d->visi;
    return res;
}

void Parser2::emitType(Type* t, const Quali& q)
{
    if( t == 0 || t->decl == 0 )
        return;
    Q_ASSERT( t && t->decl );
    if( !q.second.isEmpty() )
    {
       out->addType(ev->toQuali(t).second,t->decl->isPublic(),q, MilEmitter::Alias);
    }else if( t->form == Type::Record || t->form == Type::Proc )
    {
        if( t->form == Type::Record )
        {
            bool hasFixed = false;
            bool hasVariant = false;
            foreach( Declaration* field, t->subs )
            {
                if( field->mode == Declaration::Field )
                    hasFixed = true;
                else if( field->mode == Declaration::Variant )
                    hasVariant = true;
            }

            out->beginType(ev->toQuali(t).second,t->decl->isPublic(), !hasFixed ? MilEmitter::Union : MilEmitter::Struct );
            // TODO: record can have fixed and variable part which go to separate struct and union or embedded union
            foreach( Declaration* field, t->subs )
                out->addField(field->name,ev->toQuali(field->type),field->isPublic());
        }else
        {
            out->beginType(ev->toQuali(t).second,t->decl->isPublic(), MilEmitter::ProcType );
            foreach( Declaration* param, t->subs )
                out->addArgument(ev->toQuali(param->type), param->name);
            if( t->base && t->base->form != BasicType::NoType )
                out->setReturnType(ev->toQuali(t->base));
        }
        out->endType();
    }else if( t->form == Type::Pointer || t->form == Type::Array )
    {
        Qualident base;
        if( t->deferred )
        {
            for( int i = 0; i < deferred.size(); i++ )
            {
                if( deferred[i].first == t )
                {
                    base = qMakePair(QByteArray(),deferred[i].second.d_val);
                    break;
                }
            }
        }else
            base = t && t->base ? ev->toQuali(t->base) : Qualident();
        out->addType(ev->toQuali(t).second,t->decl->isPublic(),base,
                     t->form == Type::Pointer ? MilEmitter::Pointer : MilEmitter::Array, t->len );
    }else if( t->form == Type::ConstEnum )
    {
        out->addType(ev->toQuali(t).second,t->decl->isPublic(),qMakePair(QByteArray(),Token::getSymbol("int32")), MilEmitter::Alias);
    }else
        Q_ASSERT(false);
}

Declaration*Parser2::addHelper(Type* t)
{
    Declaration* decl = mdl->addHelper();
    // we need these syntetic declarations because emitter doesn't support anonymous types
    decl->mode = Declaration::TypeDecl;
    decl->type = t;
    decl->ownstype = true;
    decl->outer = thisMod;
    t->decl = decl;
    t->anonymous = true;
    emitType(t);
    return decl;
}

Declaration*Parser2::addTemp(Type* t)
{
    Declaration* decl = mdl->addDecl(mdl->getTempName());
    decl->mode = Declaration::LocalDecl;
    decl->type = t;
    decl->outer = mdl->getTopScope();
    decl->id = out->addLocal(ev->toQuali(t),decl->name);
    return decl;
}

void Parser2::openArrayError(const Token& tok, Type* t)
{
    if( t && t->form == Type::Array && t->len == 0)
        error(tok,"open array cannot be used here");
}

void Parser2::invalidTypeError(const Token& tok, Type* t)
{
    if( t && t->form == BasicType::Any )
        error(tok,"this type cannot be used here");
}

void Parser2::prepareParam(const DeclList& formals, const ExpList& actuals)
{
    if( actuals.size() > formals.size() )
        return; // error reported elsewhere
    Declaration* formal = formals[actuals.size()-1];
    Expression* actual = actuals.last();
    if( !paramCompat(formal,actual) ) {
        // TEST paramCompat(formal,actual);
        error(actual->pos.d_row, actual->pos.d_col, "actual argument not compatible with formal parameter");
    }
}

void Parser2::resolveAndCheckType(Declaration* d)
{
    if( d == 0 || d->type == 0 )
        return; // already reported
    if( d->type->selfref )
        error(d->row, d->col, "circular type declaration");
    d->type = resolveAndCheckType(d->type, false);
    foreach( Type* t, deferDeleteNamedType )
    {
        t->subs.clear();
        t->base = 0;
        delete t;
    }
    deferDeleteNamedType.clear();
}

Type*Parser2::resolveAndCheckType(Type* t, bool selfRefBroken)
{
    if( t == 0 || t->form < BasicType::Max || t->form == Type::ConstEnum )
        return t;
    Type* res = t;
    switch( t->form )
    {
    case Type::Pointer:
        t->base = resolveAndCheckType(t->base, true);
        break;
    case Type::Proc:
        t->base = resolveAndCheckType(t->base, true);
        for( int i = 0; i < t->subs.size(); i++ )
            t->subs[i]->type = resolveAndCheckType(t->subs[i]->type, true);
        break;
    case Type::Array:
        if( !selfRefBroken && t->base->selfref )
            error(t->base->subs.first()->row, t->base->subs.first()->col,"a structured type cannot contain itself");
        t->base = resolveAndCheckType(t->base, selfRefBroken);
        break;
    case Type::Record:
        for( int i = 0; i < t->subs.size(); i++ )
        {
            if( !selfRefBroken && t->subs[i]->type && t->subs[i]->type->selfref )
                error(t->subs[i]->row, t->subs[i]->col,"a structured type cannot contain itself");
            t->subs[i]->type = resolveAndCheckType(t->subs[i]->type, selfRefBroken);
        }
        break;
    case Type::NameRef:
        res = t->base;
        if( t->selfref )
            res = t->subs.first()->type;
        deferDeleteNamedType.insert(t); // because more than one field can point to same type
        // stop at named refs
        break;
    }
    return res;
}

static inline Type* maxType(Type* lhs, Type* rhs)
{
    if( lhs->form >= rhs->form )
        return lhs;
    else
        return rhs;
}

static Expression* createAutoCast(Expression* e, Type* t)
{
    Expression* tmp = Expression::create(Expression::AutoCast,e->pos);
    tmp->type = t;
    tmp->lhs = e;
    return tmp;
}

static void castArithOp(Expression* e)
{
    e->type = maxType(e->lhs->type,e->rhs->type);
    if( e->type != e->lhs->type )
        e->lhs = createAutoCast(e->lhs,e->type);
    if( e->type != e->rhs->type )
        e->rhs = createAutoCast(e->rhs,e->type);
}

static void castUintToInt(Expression* e, Expression* other, Evaluator* ev)
{
    // If one of the operands is an unsigned integer constant and the other operand is of type integer,
    // the unsigned integer constant is converted to the smallest integer type which includes the constant value.
    if( e->type->isUInt() && other->type->isInt() && e->isLiteral() )
    {
        QVariant val = e->getLiteralValue();
        e->type = ev->smallestIntType(val);
    }
}

void Parser2::checkArithOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 || e->rhs->type == 0 )
        return; // already reported?
    if( e->lhs->type->isNumber() && e->rhs->type->isNumber() )
    {
        castUintToInt(e->lhs, e->rhs, ev);
        castUintToInt(e->rhs, e->lhs, ev);
        if( e->lhs->type->isInt() && e->rhs->type->isInt() ||
                e->lhs->type->isUInt() && e->rhs->type->isUInt() )
            switch(e->kind)
            {
            case Expression::Mul:
            case Expression::Div:
            case Expression::Mod:
            case Expression::Add:
            case Expression::Sub:
                castArithOp(e);
                break;
            default:
                error(e->pos.d_row, e->pos.d_col,"operator not supported for integer operands");
                break;
            }
        else if( e->lhs->type->isReal() && e->rhs->type->isReal() )
            switch(e->kind)
            {
            case Expression::Mul:
            case Expression::Fdiv:
            case Expression::Add:
            case Expression::Sub:
                castArithOp(e);
                break;
            default:
                error(e->pos.d_row, e->pos.d_col,"operator not supported for real operands");
                break;
            }
        else
            error(e->pos.d_row, e->pos.d_col,"operands are not of the same type");
    }else if( e->lhs->type->isSet() && e->rhs->type->isSet() )
    {
        switch(e->kind)
        {
        case Expression::Mul:
        case Expression::Div:
        case Expression::Add:
        case Expression::Sub:
            e->type = e->lhs->type;
            break;
        default:
            error(e->pos.d_row, e->pos.d_col,"operator not supported for set operands");
            break;
        }
    }else if(e->lhs->type->isBoolean() && e->rhs->type->isBoolean())
    {
        if( e->kind == Expression::And || e->kind == Expression::Or )
            e->type = e->lhs->type;
        else
            error(e->pos.d_row, e->pos.d_col,"operator not supported for boolean operands");
    }else if((e->lhs->type->form == BasicType::String || e->lhs->type->form == BasicType::CHAR) &&
             (e->rhs->type->form == BasicType::String || e->rhs->type->form == BasicType::CHAR))
    {
        if( e->kind != Expression::Add )
            error(e->pos.d_row, e->pos.d_col,"only the '+' operator can be applied to string and char literals");
        else if( !e->isConst() )
            error(e->pos.d_row, e->pos.d_col,"operation is only available for string and char literals");
        else
            e->type = mdl->getType(BasicType::String);
    }else
        error(e->pos.d_row, e->pos.d_col,"operands not compatible with operator");
}

void Parser2::checkUnaryOp(Expression* e)
{
    if( e->kind == Expression::Plus || e->kind == Expression::Minus )
    {
        if( e->lhs->type->isNumber() )
        {
            switch(e->type->form)
            {
            case BasicType::UINT8:
                e->lhs = createAutoCast(e->lhs, mdl->getType(BasicType::INT16));
                break;
            case BasicType::UINT16:
                e->lhs = createAutoCast(e->lhs, mdl->getType(BasicType::INT32));
                break;
            case BasicType::UINT32:
                e->lhs = createAutoCast(e->lhs, mdl->getType(BasicType::INT64));
                break;
            case BasicType::UINT64:
                error(e->pos.d_row, e->pos.d_col, "unary + operator is not applicable to operands of UINT64 type");
                break;
            }
            e->type = e->lhs->type;
        }else if ( e->kind == Expression::Minus && e->type->isSet() )
        {
            // NOP
        }else
            error(e->pos.d_row, e->pos.d_col, "unary operator not applicable to this type");

    }else if( e->kind == Expression::Not )
    {
        if( !e->type->isBoolean() )
            error(e->pos.d_row, e->pos.d_col, "unary '~' or 'NOT' not applicable to this type");
    }
}

void Parser2::checkRelOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 || e->rhs->type == 0 )
        return; // already reported

    if( e->lhs->type->isNumber() && e->rhs->type->isNumber() )
    {
        castUintToInt(e->lhs, e->rhs, ev);
        castUintToInt(e->rhs, e->lhs, ev);
        if( e->lhs->type->isInt() && e->rhs->type->isInt() ||
                e->lhs->type->isUInt() && e->rhs->type->isUInt() ||
                e->lhs->type->isReal() && e->rhs->type->isReal() )
        {
            Type* mt = maxType(e->lhs->type,e->rhs->type);
            if( mt != e->lhs->type )
                e->lhs = createAutoCast(e->lhs,mt);
            if( mt != e->rhs->type )
                e->rhs = createAutoCast(e->rhs,mt);
        }else
            error(e->pos.d_row, e->pos.d_col, "operands are not of the same type");
    }else if( e->lhs->type->isText() && e->rhs->type->isText() ||
              e->lhs->type->form == Type::Pointer && e->rhs->type->form == Type::Pointer ||
              e->lhs->type->form == Type::Pointer && e->rhs->type->form == BasicType::Nil ||
              e->lhs->type->form == BasicType::Nil && e->rhs->type->form == Type::Pointer ||
              e->lhs->type->form == BasicType::Nil && e->rhs->type->form == BasicType::Nil ||
              e->lhs->type->form == Type::ConstEnum  && e->rhs->type->form == Type::ConstEnum ||
              e->lhs->type->form == Type::Proc && e->rhs->type->form == Type::Proc ||
              e->lhs->type->form == Type::Proc && e->rhs->type->form == BasicType::Nil ||
              e->lhs->type->form == BasicType::Nil && e->rhs->type->form == Type::Proc )
    {
        if( e->lhs->type->form == Type::ConstEnum  && e->rhs->type->form == Type::ConstEnum &&
                e->lhs->type != e->rhs->type )
            error(e->pos.d_row, e->pos.d_col, "cannot compare the elements of different enumeration types");
    }else if( ( e->lhs->type->isSet() && e->rhs->type->isSet() ) ||
              (e->lhs->type->isBoolean() && e->rhs->type->isBoolean()) )
    {
        if( e->kind != Expression::Eq && e->kind != Expression::Neq )
            error(e->pos.d_row, e->pos.d_col, "operation not supported for given operands");
    }else if( e->lhs->type->isInteger() && e->rhs->type->isSet() )
    {
        if( e->kind != Expression::In )
            error(e->pos.d_row, e->pos.d_col, "operation not supported for given operands");
    }else
        error(e->pos.d_row, e->pos.d_col, "operands not compatible with operator");
}

static bool renderLvalue( Expression* proc, int arg )
{
    if( proc->kind != Expression::Builtin )
        return false;

    return Builtins::requiresLvalue(proc->val.toInt(), arg);
}

// designator results in an lvalue if possible, unless needsLvalue is false
Expression* Parser2::designator(bool needsLvalue) {
    // original: qualident();

    Token tok = la;

    bool isLvalue = true;

    Expression* res = maybeQualident();
    if( !res )
        return 0;

    // TODO: check res is a variable
    if( !res->isLvalue() )
        isLvalue = false;

    // TODO: track read-only

    while( FIRST_selector(la.d_type) ) {
        // inlined selector

        if( res->type == 0 )
            break; // error already reported

        if( la.d_type == Tok_Dot ) {
            tok = la;
            expect(Tok_Dot, false, "selector");
            expect(Tok_ident, false, "selector");
            if( res->type->form == Type::Pointer )
            {
                res->setByVal();
                Expression* tmp = Expression::create(Expression::Deref, tok.toRowCol() );
                tmp->lhs = res;
                tmp->type = res->type->base;
                res = tmp;
            }
            if( res->type->form == Type::Record )
            {
                Declaration* field = res->type->findField(cur.d_val);
                if( field == 0 ) {
                    error(cur,QString("the record doesn't have a field named '%1'").
                          arg(cur.d_val.constData()) );
                    return 0;
                }else
                {    
                    Expression* tmp = Expression::create(Expression::Select, tok.toRowCol() );
                    tmp->val = QVariant::fromValue(field);
                    tmp->lhs = res;
                    tmp->type = field->type;
                    res = tmp;
                }
            }else
            {
                error(cur,QString("cannot select a field in given type") );
                return 0;
            }
        } else if( la.d_type == Tok_Lbrack ) {
            expect(Tok_Lbrack, false, "selector");
            if( res->type->form == Type::Pointer )
            {
                res->setByVal();
                Expression* tmp = Expression::create(Expression::Deref, tok.toRowCol() );
                tmp->lhs = res;
                tmp->type = res->type->base;
                res = tmp;
            }
            if( res->type->form != Type::Array )
            {
                error(cur,QString("cannot index an element in given type") );
                return 0;
            }
            Expression* tmp = Expression::create(Expression::Index, cur.toRowCol() );
            tmp->lhs = res;
            tmp->type = res->type->base;
            res = tmp;
            tok = la;
            res->rhs = expression(0);
            expect(Tok_Rbrack, false, "selector");
            if( res->rhs && !res->rhs->type->isInteger())
            {
                error(cur,QString("expecting an index of integer type") );
                return 0;
            }
        } else if( la.d_type == Tok_Hat ) {
            tok = la;
            expect(Tok_Hat, false, "selector");
            if( res->type->form != Type::Pointer )
            {
                error(tok,"only a pointer type can be dereferenced");
                return 0;
            }
            res->setByVal();
            Expression* tmp = Expression::create(Expression::Deref, tok.toRowCol() );
            tmp->lhs = res;
            tmp->type = res->type->base;
            res = tmp;
        } else if( la.d_type == Tok_Lpar ) {
            expect(Tok_Lpar, false, "selector");
            const Token lpar = cur;
            Expression* proc = res;

            proc->setByVal(); // we need the value of the procedure type variable, if any

            const DeclList formals = proc->getFormals();
            ExpList args;
            bool isTypeCast = false;
            if( FIRST_expression(la.d_type) ) {
                // inlined ExpList
                tok = la;
                Type* pt = formals.isEmpty() ? 0 : formals.first()->type;

                Expression* arg = expression(pt, renderLvalue(proc,args.size()));
                if( arg == 0 )
                    return 0;
                args.append(arg);
                isTypeCast = arg->kind == Expression::TypeDecl;
                if( !isTypeCast && proc->kind != Expression::Builtin )
                    prepareParam(formals,args);
                while( la.d_type == Tok_Comma || FIRST_expression(la.d_type) ) {
                    if( la.d_type == Tok_Comma )
                        expect(Tok_Comma, false, "ExpList");
                    tok = la;
                    Type* pt = args.size() < formals.size() ? formals[args.size()]->type : 0;

                    Expression* arg = expression(pt, renderLvalue(proc,args.size()));
                    if( arg == 0 )
                        return 0;
                    args.append(arg);
                    if( !isTypeCast && proc->kind != Expression::Builtin )
                        prepareParam(formals,args);
                }
                if( !isTypeCast && proc->kind != Expression::Builtin && args.size() < formals.size() )
                    error(tok,"not enough actual arguments");
            }
            expect(Tok_Rpar, false, "selector");

            Type* retType;
            if( isTypeCast )
                retType = args.first()->type;
            else if( proc->kind == Expression::ProcDecl )
                retType = proc->type;
            else if( proc->kind == Expression::Builtin )
            {
                Builtins bi(ev);
                const QString err = bi.checkArgs(proc->val.toInt(), args, &retType, mdl);
                if( !err.isEmpty() )
                    error(lpar,err);
            }else if( proc->type )
                retType = proc->type->base;


            if( proc->kind == Expression::Builtin && proc->val.toInt() == Builtin::ASSERT )
            {
                Expression* e = Expression::create(Expression::Literal,lpar.toRowCol());
                e->type = mdl->getType(BasicType::UINT32);
                e->val = lpar.d_lineNr;
                args << e;
                e = Expression::create(Expression::Literal,lpar.toRowCol());
                e->type = mdl->getType(BasicType::String);
                e->val = lpar.d_sourcePath.toUtf8();
                args << e;
            }

            bool hasError = false;
            if( isTypeCast )
            {
                if( args.size() != 1 )
                {
                    error(lpar,"type guard requires a single argument");
                    hasError = true;
                }else if( proc->type->form != Type::Pointer )
                {
                    error(lpar,"type guard only applicable to pointer types");
                    // TODO: avoid casting records on the stack by value
                    hasError = true;
                }

                Expression* tmp = Expression::create(Expression::Cast, lpar.toRowCol() );
                tmp->lhs = proc;
                tmp->type = new Type();
                tmp->type->form = Type::Pointer;
                tmp->type->base = retType;
                addHelper(tmp->type);
                res = tmp;
#if 0
                else if( !ev->cast() )
                {
                    error(tok, ev->getErr() );
                    hasError = true;
                }
#endif
            }else
            {
                Expression* tmp = Expression::create(Expression::Call, lpar.toRowCol() );
                tmp->lhs = proc;
                tmp->val = QVariant::fromValue(args);
                tmp->type = retType;
                res = tmp;
            }
        } else
            invalid("selector");
    }

    if( !needsLvalue && isLvalue )
    {
        res->setByVal();
    }
    return res;
}

Expression* Parser2::maybeQualident()
{
    expect(Tok_ident, false, "designator");
    Token tok = cur;
    Declaration* d = mdl->findDecl(cur.d_val);
    if( d )
    {
        quint8 visi = Declaration::NA; // symbol is local, no read/write restriction
        if( la.d_type == Tok_Dot && d->mode == Declaration::Import )
        {
            // this is the one and only qualident case
            expect(Tok_Dot, false, "selector");
            expect(Tok_ident, false, "selector");
            tok = cur;
            Declaration* d2 = mdl->findDecl(d,cur.d_val);
            if( d2 == 0 )
                error(cur,QString("declaration '%1' not found in imported module '%2'").
                      arg(cur.d_val.constData()).arg(d->name.constData()) );
            else
            {
                if( d2->visi == Declaration::Private )
                    error(cur,QString("cannot access private declaration '%1' from module '%2'").
                          arg(cur.d_val.constData()).arg(d->name.constData()) );
                d = d2;
                visi = d->visi;
            }
        }
        Expression* res = toExpr(d, tok.toRowCol());
        if( res )
            res->visi = visi;
        return res;
    }else
    {
        error(cur,QString("cannot find declaration of '%1'").arg(cur.d_val.constData()));
        return 0;
    }
}

Declaration* Parser2::resolveQualident(Parser2::Quali* qq, bool allowUnresovedLocal)
{
    const Token tok = la;
    Quali q = qualident();
    if( qq )
        *qq = q;
    Declaration* d = 0;
    if( !q.first.isEmpty() )
    {
        d = mdl->findDecl(q.first);
        if( d == 0 )
        {
            error(tok, QString("cannot find import declaration: %1").arg(q.first.constData()) );
            return 0;
        }else if( d->mode != Declaration::Import )
        {
            error(tok, QString("identifier doesn't refer to an import declaration: %1").arg(q.first.constData()) );
            return 0;
        }
    }
    if( d != 0 )
    {
        d = mdl->findDecl(d, q.second);
        if( d == 0 )
        {
            error(tok, QString("cannot find declaration '%1' in module '%2'").
                  arg(q.second.constData()).arg(q.first.constData()) );
            return 0;
        }
        if( d->visi == Declaration::Private )
        {
            error(cur,QString("cannot access private declaration '%1' from module '%2'").
                  arg(q.second.constData()).arg(q.first.constData()) );
            return 0;
        }
    }else
    {
       d = mdl->findDecl(q.second);
       if( d == 0 && !allowUnresovedLocal )
       {
           error(tok, QString("cannot find declaration '%1'").arg(q.second.constData()) );
           return 0;
       }

    }
    return d;
}

void Parser2::ProcAlias()
{
    procedure();
    const IdentDef id = identdef();
    Declaration* procDecl = addDecl(id,Declaration::Procedure);
    procDecl->alias = true;
    expect(Tok_Eq, false, "ProcedureDeclaration");
    const Token tok2 = la;
    Declaration* otherProc = resolveQualident();
    if( otherProc && otherProc->mode != Declaration::Procedure )
        error(tok2, QString("qualident must reference a procedure declaration"));
    else if( otherProc && otherProc->alias )
        otherProc = otherProc->link; // always point to the original procedure
    procDecl->link = otherProc;
}

DeclList Parser2::toList(Declaration* d)
{
    DeclList res;
    while( d )
    {
        res << d;
        Declaration* old = d;
        d = d->next;
        old->next = 0; // the next based list is converted to DeclList, avoid two redundant lists
    }
    return res;
}

Declaration*Parser2::addDecl(const Token& id, quint8 visi, quint8 mode, bool* doublette)
{
    bool collision;
    Declaration* d = mdl->addDecl(id.d_val, &collision);
    if( doublette )
        *doublette = collision;
    if(!collision)
    {
        d->mode = mode;
        d->visi = visi;
        d->row = id.d_lineNr;
        d->col = id.d_colNr;
    }else
        error(id, QString("name is not unique: %1").arg(id.d_val.constData()));
    return d;
}

Declaration*Parser2::addDecl(const Parser2::IdentDef& id, quint8 mode, bool* doublette)
{
    return addDecl(id.name, id.visi, mode, doublette);
}

void Parser2::resolveDeferreds()
{
    if( deferred.isEmpty() )
        return;
    for(int i = 0; i < deferred.size(); i++ )
    {
        Declaration* d = mdl->findDecl(deferred[i].second.d_val);
        if( d == 0 || d->type == 0 || d->mode != Declaration::TypeDecl )
        {
            error(deferred[i].second, QString("invalid type: %1").arg(deferred[i].second.d_val.constData()) );
        }else
            deferred[i].first->base = d->type;
    }
    deferred.clear();
}

Expression* Parser2::expression(Type* hint, bool lvalue) {
    Expression* res = SimpleExpression(hint, lvalue);
    if( res == 0 )
        return 0;
	if( FIRST_relation(la.d_type) ) {
        const Token tok = la;
        Expression* tmp = Expression::createFromToken(relation(), tok.toRowCol());
        tmp->lhs = res;
        tmp->type = mdl->getType(BasicType::BOOLEAN);
        res = tmp;
        res->rhs = SimpleExpression(0);
        if( res->rhs == 0 )
            return 0;
        checkRelOp(res);
    }
    return res;
}

quint8 Parser2::relation() {
	if( la.d_type == Tok_Eq ) {
		expect(Tok_Eq, false, "relation");
	} else if( la.d_type == Tok_Hash ) {
		expect(Tok_Hash, false, "relation");
	} else if( la.d_type == Tok_Lt ) {
		expect(Tok_Lt, false, "relation");
	} else if( la.d_type == Tok_Leq ) {
		expect(Tok_Leq, false, "relation");
	} else if( la.d_type == Tok_Gt ) {
		expect(Tok_Gt, false, "relation");
	} else if( la.d_type == Tok_Geq ) {
		expect(Tok_Geq, false, "relation");
	} else if( la.d_type == Tok_IN ) {
		expect(Tok_IN, true, "relation");
	} else
		invalid("relation");
    return cur.d_type;
}

Expression* Parser2::SimpleExpression(Type* hint, bool lvalue) {
    quint8 op = 0;
    Token tok = la;
    if( la.d_type == Tok_Plus || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Plus ) {
			expect(Tok_Plus, false, "SimpleExpression");
            op = Tok_Plus;
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "SimpleExpression");
            op = Tok_Minus;
		} else
			invalid("SimpleExpression");
	}
    Expression* res = term(hint, lvalue);
    if( res == 0 )
        return 0;
    if( op != 0 ) {
        Expression* tmp = Expression::create(op == Tok_Plus ? Expression::Plus : Expression::Minus, tok.toRowCol());
        tmp->lhs = res;
        tmp->type = res->type;
        res = tmp;
        checkUnaryOp(res);
    }
	while( FIRST_AddOperator(la.d_type) ) {
        Token tok = la;
        Expression* tmp = Expression::createFromToken(AddOperator(), tok.toRowCol());
        tmp->lhs = res;
        res = tmp;
        res->rhs = term(0);
        if( res->rhs == 0 )
            return 0;
        checkArithOp(res);
    }
    return res;
}

quint8 Parser2::AddOperator() {
	if( la.d_type == Tok_Plus ) {
		expect(Tok_Plus, false, "AddOperator");
	} else if( la.d_type == Tok_Minus ) {
		expect(Tok_Minus, false, "AddOperator");
	} else if( la.d_type == Tok_OR ) {
		expect(Tok_OR, true, "AddOperator");
	} else
		invalid("AddOperator");
    return cur.d_type;
}

Expression* Parser2::term(Type* hint, bool lvalue) {
    Expression* res = factor(hint, lvalue);
    if( res == 0 )
        return 0;
	while( FIRST_MulOperator(la.d_type) ) {
        Token tok = la;
        Expression* tmp = Expression::createFromToken(MulOperator(),tok.toRowCol());
        tmp->lhs = res;
        res = tmp;
        res->rhs = factor(0);
        if( res->rhs == 0 )
            return 0;
        checkArithOp(res);
	}
    return res;
}

quint8 Parser2::MulOperator() {
	if( la.d_type == Tok_Star ) {
		expect(Tok_Star, false, "MulOperator");
	} else if( la.d_type == Tok_Slash ) {
		expect(Tok_Slash, false, "MulOperator");
	} else if( la.d_type == Tok_DIV ) {
		expect(Tok_DIV, true, "MulOperator");
	} else if( la.d_type == Tok_MOD ) {
		expect(Tok_MOD, true, "MulOperator");
	} else if( la.d_type == Tok_Amp ) {
		expect(Tok_Amp, false, "MulOperator");
    } else if( la.d_type == Tok_AND ) {
        expect(Tok_AND, false, "MulOperator");
    } else
		invalid("MulOperator");
    return cur.d_type;
}

Expression* Parser2::literal() {
    Expression* res;
	if( FIRST_number(la.d_type) ) {
        res = number();
	} else if( la.d_type == Tok_string ) {
		expect(Tok_string, false, "literal");
        res = Expression::create(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::String);
        res->val = ev->dequote(cur.d_val);
        // string literal: byte array latin-1 with type BasicType::String
    }else if( la.d_type == Tok_hexstring ) {
        expect(Tok_hexstring, false, "literal");
        // alternative syntax for A{ x x x } with A = array of byte
        res = Expression::create(Expression::Literal,cur.toRowCol());
        const QByteArray bytes = QByteArray::fromHex(cur.d_val); // already comes without quotes
        Type* arr = new Type();
        arr->base = mdl->getType(BasicType::UINT8);
        arr->form = Type::Array;
        arr->len = bytes.size();
        addHelper(arr);
        res->type = arr;
        res->val = bytes;
        // byte array literal: byte array with type array of uint8
    } else if( la.d_type == Tok_hexchar ) {
		expect(Tok_hexchar, false, "literal");
        res = Expression::create(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::CHAR);
        QByteArray tmp = cur.d_val;
        tmp.chop(1); // remove X postfix
        res->val = QVariant::fromValue((char)(quint8)tmp.toUInt(0,16));
    } else if( la.d_type == Tok_NIL ) {
		expect(Tok_NIL, true, "literal");
        res = Expression::create(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::Nil);
        res->val = QVariant();
    } else if( la.d_type == Tok_TRUE ) {
		expect(Tok_TRUE, true, "literal");
        res = Expression::create(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::BOOLEAN);
        res->val = true;
    } else if( la.d_type == Tok_FALSE ) {
		expect(Tok_FALSE, true, "literal");
        res = Expression::create(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::BOOLEAN);
        res->val = false;
	} else
		invalid("literal");
    return res;
}

Expression* Parser2::constructor(Type* hint) {
    const Token t = la;
    Expression* res = Expression::create(Expression::Constructor, t.toRowCol());
    if( FIRST_NamedType(t.d_type) ) {
        res->type = NamedType();
    }else if( hint )
        res->type = hint;
    else
        res->type = mdl->getType(BasicType::SET);

    if( res->type == 0 )
    {
        error(t,"constructor type cannot be inferred");
        return 0;
    }else if( res->type->form != Type::Record && res->type->form != Type::Array &&
              res->type->form != BasicType::SET && res->type->form != Type::Pointer )
    {
        error(t,"constructors only supported for record, array, set and pointer types");
        return 0;
    }

    expect(Tok_Lbrace, false, "constructor");
    int index = 0;
    if( FIRST_component(la.d_type) ) {
        Expression* e = component(res->type, index);
        if( e == 0 )
            return 0;
        res->appendRhs(e);
        while( la.d_type == Tok_Comma || FIRST_component(la.d_type) ) {
            if( la.d_type == Tok_Comma ) {
                expect(Tok_Comma, false, "constructor");
            }
            Expression* e = component(res->type, index);
            if( e == 0 )
                return 0;
            res->appendRhs(e);
        }
    }
    expect(Tok_Rbrace, false, "constructor");

    if( res->type->form == Type::Record )
    {
        QSet<Declaration*> test;
        Expression* c = res->rhs;
        while(c)
        {
            Q_ASSERT( c->kind == Expression::NameValue);
            Declaration* name = c->val.value<Declaration*>();
            if( test.contains(name) )
                error(c->pos, "value for this field was already defined");
            test.insert(name);
            c = c->next;
        }
        // TODO: If the record type has a variant part, only named component can be used, and
        // only one option of the variant part can be initialized in the constructor.
    }else if( res->type->form == Type::Array && res->type->len == 0 )
    {
        QSet<qint64> test; qint64 maxIndex = 0;
        Expression* c = res->rhs;
        while(c)
        {
            Q_ASSERT( c->kind == Expression::IndexValue);
            const qint64 index = c->val.toLongLong();
            if( test.contains(index) )
                error(c->pos, "value at array index was already defined");
            test.insert(index);
            if( index > maxIndex )
                maxIndex = index;
            c = c->next;
        }
        if( test.isEmpty() )
        {
            error(res->pos, "cannot determine length of array");
            return 0;
        }
        Type* a = new Type();
        a->form = Type::Array;
        a->len = maxIndex + 1;
        a->base = res->type->base;
        addHelper(a);
        res->type = a;
    }else if( res->type->form == Type::Pointer )
    {
        if( res->rhs == 0 || res->rhs->next != 0 )
            error(res->pos, "pointer constructor requires exactly one component");
    }
    return res;
}

Expression* Parser2::component(Type* constrType, int& index) {
    Expression* res;
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Colon )  ) {
        expect(Tok_ident, false, "component");
        if( constrType->form != Type::Record )
        {
            error(cur, "named components only supported in record constructors");
            return 0;
        }
        Declaration* field = constrType->findField(cur.d_val);
        if( field == 0 )
        {
            error(cur, "field not known in record");
            return 0;
        }
        expect(Tok_Colon, false, "component");
        const Token colon = cur;
        Expression* rhs = expression(0);
        if( rhs == 0 )
            return 0;
        res = Expression::create(Expression::NameValue, colon.toRowCol());
        res->val = QVariant::fromValue(field);
        res->rhs = rhs;
        if( !assigCompat(field->type, rhs ) )
            error(rhs->pos, "incompatible value");
        index = constrType->subs.indexOf(field);
    } else if( la.d_type == Tok_Lbrack ) {
        expect(Tok_Lbrack, false, "component");
        if( constrType->form != Type::Array )
        {
            error(cur, "indexed components only supported in array constructors");
            return 0;
        }
        Expression* lhs = ConstExpression(0);
        if( lhs == 0 )
            return 0;
        if( !lhs->type->isInteger() || lhs->val.toLongLong() < 0 )
            error(lhs->pos, "expecting positive integer type index");
        expect(Tok_Rbrack, false, "component");
        expect(Tok_Colon, false, "component");
        const Token colon = cur;
        Expression* rhs = expression(0);
        if( rhs == 0 )
            return 0;
        res = Expression::create(Expression::IndexValue, colon.toRowCol());
        if( !ev->evaluate(lhs) )
            error(lhs->pos, ev->getErr());
        res->val = ev->pop().val;
        index = res->val.toLongLong();
        res->rhs = rhs;
        if( !assigCompat(constrType->base, rhs ) )
            error(rhs->pos, "incompatible value");
    } else if( FIRST_expression(la.d_type) ) {
        if( constrType->form == Type::Pointer )
        {
            res = ConstExpression(0);
            if( res )
            {
                if( !ev->evaluate(res) )
                    error(res->pos, ev->getErr() );
                else
                    res->val = ev->pop().val;
            }
        }else
            res = expression(0);
        if( res == 0 )
            return 0;
        if( la.d_type == Tok_2Dot ) {
            expect(Tok_2Dot, false, "component");
            const Token t = cur;
            if( constrType->form != BasicType::SET )
            {
                error(cur, "range components only supported in set constructors");
                return 0;
            }
            Expression* rhs = expression(0);
            if( rhs == 0 )
                return 0;
            if( !res->type->isInteger() || !rhs->type->isInteger() )
            {
                error(cur, "range expects integer boundaries");
                return 0;
            }
            Expression* range = Expression::create(Expression::Range, t.toRowCol());
            range->lhs = res;
            range->rhs = rhs;
            res = range;
        }else if( constrType->form == Type::Record)
        {
            if( index < 0 || index >= constrType->subs.size() )
            {
                error(res->pos, "component cannot be associated with record field");
                return 0;
            }
            Expression* res2 = Expression::create(Expression::NameValue, res->pos);
            res2->val = QVariant::fromValue(constrType->subs[index]);
            res2->rhs = res;
            if( !assigCompat(constrType->subs[index]->type, res ) )
                error(res->pos, "incompatible value");
            res = res2;
        }else if( constrType->form == Type::Array)
        {
            if( constrType->len && index >= constrType->len || index < 0 )
                error(res->pos, "component is out of range of the array");
            Expression* res2 = Expression::create(Expression::IndexValue, res->pos);
            res2->val = index;
            res2->rhs = res;
            if( !assigCompat(constrType->base, res ) )
                error(res->pos, "incompatible value");
            res = res2;
        }else if( constrType->form == Type::Pointer)
        {
            if( !res->type->isUInt() )
                error(res->pos, "expecting unsigned integer to initialize pointer");
        }
        index++;
    } else
        invalid("component");
    // TODO: for arrays change the array type to correct len
    return res;
}

Expression* Parser2::factor(Type* hint, bool lvalue) {
    Expression* res = 0;
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Lbrace )  ) {
        res = constructor(hint);
	} else if( FIRST_literal(la.d_type) ) {
        res = literal();
	} else if( FIRST_variableOrFunctionCall(la.d_type) ) {
        res = variableOrFunctionCall(lvalue);
	} else if( la.d_type == Tok_Lpar ) {
		expect(Tok_Lpar, false, "factor");
        res = expression(0);
		expect(Tok_Rpar, false, "factor");
    } else if( la.d_type == Tok_Tilde || la.d_type == Tok_NOT ) {
        if( la.d_type == Tok_NOT )
            expect(Tok_NOT, false, "factor");
        else
            expect(Tok_Tilde, false, "factor");
        Expression* tmp = factor(0);
        if( tmp == 0 )
            return 0;

        res = Expression::create(Expression::Not, cur.toRowCol());
        res->lhs = tmp;
        res->type = res->lhs->type;

        checkUnaryOp(res);

    } else if( la.d_type == Tok_At ) {
        const Token tok = la;
        expect(Tok_At, false, "factor");
        Expression* tmp = designator(true);
        if( tmp == 0 )
            return 0; // reported elsewhere

        if( !tmp->isLvalue() )
            error(tok,"cannot take address of this object");

        res = Expression::create(Expression::Addr, cur.toRowCol());
        res->lhs = tmp;
        Type* ptr = new Type();
        ptr->base = res->lhs->type;
        ptr->form = Type::Pointer;
        addHelper(ptr);
        res->type = ptr;
    } else
        invalid("factor");
    return res;
}

Expression* Parser2::variableOrFunctionCall(bool lvalue) {
    return designator(lvalue);
}

Expression* Parser2::element() {
    Expression* res = expression(0);
	if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "element");
        Expression* tmp = Expression::create(Expression::Range, cur.toRowCol());
        tmp->lhs = res;
        res = tmp;
        res->rhs = expression(0);
	}
    return res;
}

void Parser2::statement() {
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Colon )  ) {
        gotoLabel();
    }else if( FIRST_assignmentOrProcedureCall(la.d_type) ) {
        assignmentOrProcedureCall();
	} else if( FIRST_IfStatement(la.d_type) ) {
		IfStatement();
	} else if( FIRST_CaseStatement(la.d_type) ) {
		CaseStatement();
	} else if( FIRST_LoopStatement(la.d_type) ) {
		LoopStatement();
	} else if( FIRST_ExitStatement(la.d_type) ) {
		ExitStatement();
	} else if( FIRST_GotoStatement(la.d_type) ) {
		GotoStatement();
	} else if( FIRST_ReturnStatement(la.d_type) ) {
		ReturnStatement();
	} else if( FIRST_WhileStatement(la.d_type) ) {
		WhileStatement();
	} else if( FIRST_RepeatStatement(la.d_type) ) {
		RepeatStatement();
	} else if( FIRST_ForStatement(la.d_type) ) {
		ForStatement();
	} else
		invalid("statement");
}

void Parser2::assignmentOrProcedureCall() {
    Token t = la;
    Expression* lhs = designator(true);
    if( lhs == 0 )
        return;
    if( la.d_type == Tok_ColonEq ) {
        const Token tok = la;
		expect(Tok_ColonEq, false, "assignmentOrProcedureCall");
        if( !ev->evaluate(lhs) )
            error(t, ev->getErr());
        Expression* rhs = expression(lhs->type);
        if( rhs && !ev->evaluate(rhs) )
            error(tok, ev->getErr());         // value is pushed in ev->assign
        // TODO: avoid assigning to structured return values of functions on left side
        if( rhs && !assigCompat( lhs->type, rhs ) )
            error(tok, "right side is not assignment compatible with left side");
        else if( rhs && !ev->assign() )
            error(tok, ev->getErr() );
        Expression::deleteAllExpressions();
    }else
    {
        if( lhs->kind == Expression::ProcDecl || lhs->type && lhs->type->form == Type::Proc )
        {
            // call procedure without ()
            const DeclList formals = lhs->getFormals();
            if( !formals.isEmpty() )
                error(t,"expecting actual parameters to call this procedure");
            Expression* tmp = Expression::create(Expression::Call, lhs->pos);
            tmp->lhs = lhs;
            if( lhs->kind == Expression::ProcDecl )
                tmp->type = lhs->type;
            else
                tmp->type = lhs->type->base;
            lhs = tmp;
        }
        if( !ev->evaluate(lhs) )
            error(t, ev->getErr());
    }
    Expression::deleteAllExpressions();
}

void Parser2::StatementSequence() {
    blockDepth.push_back(RowCol(la.d_lineNr,la.d_colNr));
	while( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "StatementSequence");
	}
    while( FIRST_statement(la.d_type) ) {
		statement();
		while( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "StatementSequence");
		}
	}
    blockDepth.pop_back();
}

void Parser2::gotoLabel() {
	expect(Tok_ident, false, "gotoLabel");
    if( labels.contains(cur.d_val.constData()) )
        error(cur,"goto label is not unique within block");
    else
    {
        labels.insert(cur.d_val.constData(), Label(blockDepth,cur));
        out->label_(cur.d_val);
    }
	expect(Tok_Colon, false, "gotoLabel");
}

void Parser2::GotoStatement() {
	expect(Tok_GOTO, true, "GotoStatement");
	expect(Tok_ident, false, "GotoStatement");
    out->goto_(cur.d_val);
    gotos.push_back( qMakePair(blockDepth,cur));
}

void Parser2::IfStatement() {
	expect(Tok_IF, true, "IfStatement");
    out->if_();
    Token t = la;
    Expression* cond = expression(0);
    if( cond != 0 && !ev->evaluate(cond, true) )
        error(t, ev->getErr());
    Expression::deleteAllExpressions();
    ev->pop();
	expect(Tok_THEN, true, "IfStatement");
    out->then_();
    StatementSequence();
    int level = 0;
	while( FIRST_ElsifStatement(la.d_type) ) {
        // inlined ElsifStatement();
        out->else_();
        expect(Tok_ELSIF, true, "ElsifStatement");
        out->if_();
        Token t = la;
        if( !ev->evaluate(expression(0), true) )
            error(t, ev->getErr());
        Expression::deleteAllExpressions();
        ev->pop();
        expect(Tok_THEN, true, "ElsifStatement");
        out->then_();
        StatementSequence();
        level++;
	}
	if( FIRST_ElseStatement(la.d_type) ) {
        // inlined ElseStatement();
        expect(Tok_ELSE, true, "ElseStatement");
        out->else_();
        StatementSequence();
    }
    for( int i = 0; i < level; i++ )
        out->end_();
	expect(Tok_END, true, "IfStatement");
    out->end_();
}

void Parser2::CaseStatement() {
	expect(Tok_CASE, true, "CaseStatement");
    out->switch_();
    Token tok = la;
    if( !ev->evaluate(expression(0), true) )
        error(tok, ev->getErr());
    Type* t = ev->top().type;
    Expression::deleteAllExpressions();
    ev->pop();
    if( !t->isInteger() && t->form != BasicType::CHAR && t->form != Type::ConstEnum )
        error(tok, "case expression must be of integer, char or constant enumeration type");
    expect(Tok_OF, true, "CaseStatement");
    CaseLabels l;
	if( FIRST_Case(la.d_type) ) {
        Case(t, l);
	}
	while( la.d_type == Tok_Bar ) {
		expect(Tok_Bar, false, "CaseStatement");
        Case(t, l);
	}
	if( la.d_type == Tok_ELSE ) {
		expect(Tok_ELSE, true, "CaseStatement");
        out->else_();
		StatementSequence();
	}
	expect(Tok_END, true, "CaseStatement");
    out->end_();
}

void Parser2::Case(Type* t, CaseLabels& ll) {
    CaseLabels l;
    Token tok = la;
    CaseLabelList(t,l);
    if( ll.contains(l) )
        error(tok,"label list overlaps with other cases");
    else
        ll += l;
    expect(Tok_Colon, false, "Case");
    out->case_(l.toList());
    out->then_();
    StatementSequence();
}

void Parser2::CaseLabelList(Type* t, CaseLabels& l) {
    LabelRange(t,l);
    while( la.d_type == Tok_Comma || FIRST_LabelRange(la.d_type) ) {
        if( la.d_type == Tok_Comma )
            expect(Tok_Comma, false, "CaseLabelList");
        LabelRange(t,l);
	}
}

void Parser2::LabelRange(Type* t, CaseLabels& l) {
    Token tok = la;
    qint64 lhs = label(t).val.toLongLong();
    if( l.contains(lhs) )
        error(tok,"label not unique in list");
	if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "LabelRange");
        Token tok = la;
        const qint64 rhs = label(t).val.toLongLong();
        if( l.contains(rhs) )
            error(tok,"label not unique in list");
        else
            l.insert(rhs);
        if( lhs >= rhs )
            do {
                if( l.contains(lhs) )
                    error(tok,"label range not unique in list");
                else
                    l.insert(lhs);
            }while(--lhs > rhs);
        else
            do {
                if( l.contains(lhs) )
                    error(tok,"label range not unique in list");
                else
                    l.insert(lhs);
            }while(++lhs < rhs);
    }else
        l.insert(lhs);
}

Value Parser2::label(Type* t) {
    Token tok = la;
    Expression* e = ConstExpression(0);
    if( !ev->evaluate(e) )
        error(tok, ev->getErr());
    Value res = ev->pop();
    if( !assigCompat(t, e) )
        error(tok,"label has incompatible type");
    Expression::deleteAllExpressions();
    return res;
}

void Parser2::WhileStatement() {
	expect(Tok_WHILE, true, "WhileStatement");
    out->while_();
    const Token t = la;
    Expression* res = expression(0);
    if( res && !ev->evaluate(res, true) )
        error(t, ev->getErr());
    if( ev->top().type->form != BasicType::BOOLEAN )
        error(t,"expecting boolean expression");
    Expression::deleteAllExpressions();
    ev->pop();
    expect(Tok_DO, true, "WhileStatement");
    out->do_();
    StatementSequence();
	expect(Tok_END, true, "WhileStatement");
    out->end_();
}

void Parser2::RepeatStatement() {
	expect(Tok_REPEAT, true, "RepeatStatement");
    out->repeat_();
	StatementSequence();
	expect(Tok_UNTIL, true, "RepeatStatement");
    out->until_();
    const Token t = la;
    if( !ev->evaluate(expression(0), true) )
        error(t, ev->getErr());
    if( ev->top().type->form != BasicType::BOOLEAN )
        error(t,"expecting boolean expression");
    Expression::deleteAllExpressions();
    ev->pop();
    out->end_();
}

void Parser2::ForStatement() {
	expect(Tok_FOR, true, "ForStatement");
	expect(Tok_ident, false, "ForStatement");
    Declaration* idxvar = mdl->findDecl(cur.d_val);
    if( idxvar == 0 || !idxvar->isLvalue() )
    {
        error(cur,"identifier must reference a variable or parameter");
        return;
    }
    if( idxvar->type == 0 || !idxvar->type->isInteger() )
    {
        error(cur,"control variable must be of integer type");
        return;
    }
    // TODO: support enums as well
    expect(Tok_ColonEq, false, "ForStatement");
    Token tok = cur;

    {
        // i := start
        Expression* lhs = toExpr(idxvar, tok.toRowCol());
        if( !ev->evaluate(lhs) )
            error(tok, ev->getErr());
        if( !ev->evaluate(expression(0)) )
            error(tok, ev->getErr());         // rhs
        ev->assign();
        Expression::deleteAllExpressions();
    }

    expect(Tok_TO, true, "ForStatement");
    tok = cur;
    Declaration* to = addTemp(idxvar->type);
    {
        // to := end
        Expression* lhs = toExpr(to, tok.toRowCol());
        if( lhs && !ev->evaluate(lhs) )
            error(tok, ev->getErr());
        Expression* rhs = expression(0);
        if( rhs && !ev->evaluate(rhs) )
            error(tok, ev->getErr());         // rhs
        ev->assign();
        Expression::deleteAllExpressions();
    }

    int by = 1;
	if( la.d_type == Tok_BY ) {
		expect(Tok_BY, true, "ForStatement");
        tok = la;
        Expression* e = ConstExpression(0);
        if( !ev->evaluate(e) )
            error(tok, ev->getErr());
        Value v = ev->pop();
        if( idxvar && !assigCompat(idxvar->type, e) )
            error(tok,"constant expression is not compatible with control variable");
        else
            by = v.val.toInt();
        if( by == 0 )
            error(tok,"BY expression cannot be zero");
        Expression::deleteAllExpressions();
    }
	expect(Tok_DO, true, "ForStatement");
    tok = cur;

    out->while_();
    {
        // while i <= to
        Expression* lhs = toExpr(idxvar, tok.toRowCol());
        lhs->byVal = true;
        Expression* rhs = toExpr(to, tok.toRowCol());
        rhs->byVal = true;
        Expression* op = Expression::create(by > 0 ? Expression::Leq : Expression::Geq, tok.toRowCol());
        op->lhs = lhs;
        op->rhs = rhs;
        op->type = lhs->type;
        if( !ev->evaluate(op) )
            error(tok, ev->getErr());
        ev->pop();
        Expression::deleteAllExpressions();
    }
    out->do_();
    StatementSequence();

    // i := i + 1
    {
        Expression* lhs = toExpr(idxvar, tok.toRowCol());
        if( !ev->evaluate(lhs) )
            error(tok, ev->getErr());

        Expression* l = toExpr(idxvar, tok.toRowCol());
        l->byVal = true;

        Expression* r = Expression::create(Expression::Literal, tok.toRowCol());
        r->type = idxvar->type;
        r->val = by;

        Expression* rhs = Expression::create(Expression::Add, tok.toRowCol());
        rhs->lhs = l;
        rhs->rhs = r;
        rhs->type = idxvar->type;
        if( !ev->evaluate(rhs) )
            error(tok, ev->getErr());

        ev->assign();
        Expression::deleteAllExpressions();
    }

    out->end_();
	expect(Tok_END, true, "ForStatement");
}

void Parser2::LoopStatement() {
	expect(Tok_LOOP, true, "LoopStatement");
    out->loop_();
    loopStack.push_back(RowCol(cur.d_lineNr,cur.d_colNr));
	StatementSequence();
	expect(Tok_END, true, "LoopStatement");
    loopStack.pop_back();
    out->end_();
}

void Parser2::ExitStatement() {
	expect(Tok_EXIT, true, "ExitStatement");
    out->exit_();
    if( loopStack.isEmpty() )
        error(cur,"cannot call exit when not in a loop");
}

void Parser2::procedure() {
	if( la.d_type == Tok_PROCEDURE ) {
		expect(Tok_PROCEDURE, true, "procedure");
	} else if( la.d_type == Tok_PROC ) {
		expect(Tok_PROC, true, "procedure");
	} else
		invalid("procedure");
}

Type* Parser2::ProcedureType() {
	procedure();
    mdl->openScope(0);
    Type* ret = 0;
	if( FIRST_FormalParameters(la.d_type) ) {
        ret = FormalParameters();
	}
    Type* p = new Type();
    p->form = Type::Proc;
    p->subs = toList(mdl->closeScope(true));
    p->base = ret;
    return p;
}

void Parser2::ProcedureDeclaration() {
	if( ( ( peek(1).d_type == Tok_PROC || peek(1).d_type == Tok_PROCEDURE ) && peek(2).d_type == Tok_Hat )  ) {
        ForwardDeclaration();
    } else if( ( ( peek(1).d_type == Tok_PROCEDURE || peek(1).d_type == Tok_PROC ) &&
                 ( peek(3).d_type == Tok_Eq || peek(4).d_type == Tok_Eq ) )  ) {
        ProcAlias();
	} else if( FIRST_ProcedureHeading(la.d_type) ) {
        // inlined ProcedureHeading();
        procedure();

        const IdentDef id = identdef();
        if( !id.isValid() )
            return; // invalid syntax

        Declaration* forward = mdl->findDecl(id.name.d_val,false);
        if( forward && forward->mode != Declaration::ForwardDecl )
        {
            error(id.name, QString("procedure name is not unique: %1").arg(id.name.d_val.constData()) );
            return;
        }
        if( forward )
            forward->name.clear(); // so it doesn't intervene name lookup

        Declaration* procDecl = addDecl(id, Declaration::Procedure);

        mdl->openScope(procDecl);
        if( FIRST_FormalParameters(la.d_type) ) {
            procDecl->type = FormalParameters();
        }

        if( forward )
        {
            if( !matchFormals(forward->getParams(), procDecl->getParams() ) ||
                    !matchResultType(forward->type, procDecl->type) ||
                    forward->extern_ != procDecl->extern_ ||
                    forward->inline_ != procDecl->inline_ ||
                    forward->invar != procDecl->invar ||
                    forward->visi != procDecl->visi )
                error(id.name,"procedure declaration is not compatible with preceding forward declaration");
        }

        if( peek(1).d_type == Tok_EXTERN ||
                ( peek(1).d_type == Tok_Semi && peek(2).d_type == Tok_EXTERN ) ) {
            if( la.d_type == Tok_Semi ) {
                expect(Tok_Semi, false, "ProcedureDeclaration");
            }
            expect(Tok_EXTERN, true, "ProcedureDeclaration");
            procDecl->extern_ = true;
            out->beginProc(ev->toQuali(procDecl).second,mdl->getTopScope()->mode == Declaration::Module &&
                           id.visi > 0, MilProcedure::Extern);

            const QList<Declaration*> params = procDecl->getParams();
            foreach( Declaration* p, params )
                out->addArgument(ev->toQuali(p->type),p->name);
            if( procDecl->type && procDecl->type->form != BasicType::NoType )
                out->setReturnType(ev->toQuali(procDecl->type));

            if( la.d_type == Tok_ident ) {
                expect(Tok_ident, false, "ProcedureDeclaration");
                procDecl->data = cur.d_val;
            }
            out->endProc();
        } else if( la.d_type == Tok_INLINE || la.d_type == Tok_INVAR ||
                   la.d_type == Tok_Semi || FIRST_ProcedureBody(la.d_type) ) {
            quint8 kind = MilProcedure::Normal;
			if( la.d_type == Tok_INLINE || la.d_type == Tok_INVAR ) {
				if( la.d_type == Tok_INLINE ) {
					expect(Tok_INLINE, true, "ProcedureDeclaration");
                    procDecl->inline_ = true;
                    kind = MilProcedure::Inline;
				} else if( la.d_type == Tok_INVAR ) {
					expect(Tok_INVAR, true, "ProcedureDeclaration");
                    procDecl->invar = true;
                    kind = MilProcedure::Invar;
				} else
					invalid("ProcedureDeclaration");
			}
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "ProcedureDeclaration");
			}
            out->beginProc(ev->toQuali(procDecl).second,mdl->getTopScope()->mode == Declaration::Module &&
                           id.visi > 0, kind);

            const QList<Declaration*> params = procDecl->getParams();
            foreach( Declaration* p, params )
                out->addArgument(ev->toQuali(p->type),p->name);
            if( procDecl->type && procDecl->type->form != BasicType::NoType )
                out->setReturnType(ev->toQuali(procDecl->type));

            // inlined ProcedureBody();
            DeclarationSequence();
            block();
            expect(Tok_END, true, "ProcedureBody");
            expect(Tok_ident, false, "ProcedureBody");
            if( procDecl->name.constData() != cur.d_val.constData() )
                error(cur, QString("name after END differs from procedure name") );
            out->endProc();
        }  else
			invalid("ProcedureDeclaration");
        mdl->closeScope();
        if(forward)
            mdl->removeDecl(forward);
	} else
		invalid("ProcedureDeclaration");
}

void Parser2::block() {
	expect(Tok_BEGIN, true, "block");
    labels.clear();
    gotos.clear();
	StatementSequence();
    Gotos::const_iterator i;
    for( i = gotos.begin(); i != gotos.end(); ++i )
    {
        Labels::iterator j = labels.find((*i).second.d_val.constData() );
        if( j == labels.end() )
            error((*i).second,"goto label not defined in this block");
        else
        {
            j.value().used = true;
            if( (*i).first.size() < j.value().depth.size() )
                error((*i).second,"goto cannot jump into a structured statement");
            else if( !(*i).first.isEmpty() && (*i).first.size() == j.value().depth.size() &&
                     !((*i).first.last() == j.value().depth.last()) )
                error((*i).second,"goto cannot jump into another structured statement");
        }
    }
    Labels::const_iterator j;
    for( j = labels.begin(); j != labels.end(); ++j )
    {
        if( !j.value().used )
        {
            error(j.value().tok,"goto label declared but not used");
        }
    }
}

void Parser2::DeclarationSequence() {
    while( la.d_type == Tok_CONST || la.d_type == Tok_TYPE || la.d_type == Tok_VAR ||
           FIRST_ProcedureDeclaration(la.d_type) ) {
		if( la.d_type == Tok_CONST ) {
			expect(Tok_CONST, true, "DeclarationSequence");
			while( FIRST_ConstDeclaration(la.d_type) ) {
				ConstDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
		} else if( la.d_type == Tok_TYPE ) {
			expect(Tok_TYPE, true, "DeclarationSequence");
			while( FIRST_TypeDeclaration(la.d_type) ) {
				TypeDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
            resolveDeferreds();
		} else if( la.d_type == Tok_VAR ) {
			expect(Tok_VAR, true, "DeclarationSequence");
			while( FIRST_VariableDeclaration(la.d_type) ) {
				VariableDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
		} else if( FIRST_ProcedureDeclaration(la.d_type) ) {
			ProcedureDeclaration();
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "DeclarationSequence");
			}
		} else
			invalid("DeclarationSequence");
	}
}

void Parser2::ReturnStatement() {
	expect(Tok_RETURN, true, "ReturnStatement");
    if( mdl->getTopScope()->mode != Declaration::Procedure )
        error(cur,"return statement only supported in a procedure declaration");
	if( FIRST_expression(la.d_type) ) {
        if( mdl->getTopScope()->type == 0 || mdl->getTopScope()->type->form == BasicType::NoType )
                error(cur,"this return statement doesn't expect an expression");
        const Token tok = la;
        Expression* e = expression(mdl->getTopScope()->type);
        if( e && !ev->evaluate(e) )
            error(tok, ev->getErr()); // value is pushed on stack by prepareRhs
        if( !assigCompat( mdl->getTopScope()->type, e ) )
            error(tok,"expression is not compatible with the return type");
        if( !ev->prepareRhs(mdl->getTopScope()->type) )
            error(tok, ev->getErr());
        out->ret_(true);
        Expression::deleteAllExpressions();
    }else if( mdl->getTopScope()->type != 0 && mdl->getTopScope()->type->form != BasicType::NoType )
        error(cur,"this return statement requires an expression");
    else
        out->ret_(false);
}

Type* Parser2::FormalParameters() {
	expect(Tok_Lpar, false, "FormalParameters");
	if( FIRST_FPSection(la.d_type) ) {
		FPSection();
        while( FIRST_FPSection(la.d_type) ||
               ( peek(1).d_type == Tok_Semi && ( peek(2).d_type == Tok_ident ||peek(2).d_type == Tok_CONST ) ) )
        {
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "FormalParameters");
			}
			FPSection();
		}
		if( la.d_type == Tok_Semi || la.d_type == Tok_2Dot ) {
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "FormalParameters");
			}
			expect(Tok_2Dot, false, "FormalParameters");
            cur.d_val = Token::getSymbol("..");
            Declaration* d = addDecl(cur,0,Declaration::ParamDecl);
            d->outer = mdl->getTopScope();
            d->type = mdl->getType(BasicType::NoType);
        }
	}
	expect(Tok_Rpar, false, "FormalParameters");
    Type* res = 0;
	if( la.d_type == Tok_Colon ) {
		expect(Tok_Colon, false, "FormalParameters");
        res = ReturnType();
	}
    if(res == 0)
        res = mdl->getType(BasicType::NoType);
    return res;
}

Type* Parser2::ReturnType() {
    bool ptr = false;
	if( la.d_type == Tok_POINTER || la.d_type == Tok_Hat ) {
		if( la.d_type == Tok_POINTER ) {
			expect(Tok_POINTER, false, "ReturnType");
			expect(Tok_TO, false, "ReturnType");
            ptr = true;
		} else if( la.d_type == Tok_Hat ) {
			expect(Tok_Hat, false, "ReturnType");
            ptr = true;
		} else
			invalid("ReturnType");
	}
    Type* t = NamedType();
    if( ptr )
    {
        Type* res = new Type();
        res->base = t;
        res->form = Type::Pointer;
        t = res;

        addHelper(res);
    }
    return t;
}

void Parser2::FPSection() {
    bool isConst = false;
    if( la.d_type == Tok_CONST )
    {
        expect(Tok_CONST, false, "FPSection");
        isConst = true;
    }
	expect(Tok_ident, false, "FPSection");
    TokenList l;
    l << cur;
	while( la.d_type == Tok_Comma || la.d_type == Tok_ident ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "FPSection");
		}
		expect(Tok_ident, false, "FPSection");
        l << cur;
    }
	expect(Tok_Colon, false, "FPSection");
    Type* t = FormalType();
    for(int i = 0; i < l.size(); i++ )
    {
        Declaration* d = addDecl(l[i], 0, Declaration::ParamDecl);
        if( isConst )
            d->visi = Declaration::ReadOnly;
        d->type = t;
    }
}

Type* Parser2::FormalType() {
    const Token tok = la;
    Type* t = type();
    openArrayError(tok,t);
    invalidTypeError(tok,t);
    return t;
}

void Parser2::module() {
    if( la.d_type != Tok_MODULE )
    {
        la.d_sourcePath = scanner->source();
        la.d_lineNr = 1;
        error(la,"not a Micron module");
        return;
    }
    Declaration* m = new Declaration();
    m->mode = Declaration::Module;
    if( thisMod )
        delete thisMod;
    thisMod = m;
    mdl->openScope(m);

    expect(Tok_MODULE, true, "module");
	expect(Tok_ident, false, "module");
    m->name = cur.d_val;

    ModuleData md;
    md.path = scanner->path();
    md.path += cur.d_val;
    if( imp )
        md.fullName = Token::getSymbol(imp->modulePath(md.path));
    else
        md.fullName = Token::getSymbol(md.path.join('/'));
    md.metaActuals = metaActuals;

    const QString source = cur.d_sourcePath;
    MilMetaParams mps;
    if( FIRST_MetaParams(la.d_type) ) {
        MetaParamList mp = MetaParams();
        md.metaParams = mp;

        if( !metaActuals.isEmpty() )
        {
            if( metaActuals.size() != mp.size() )
                error(cur,"number of actual doesn't meet numer of formal meta parameters");
            else
                for( int i = 0; i < mp.size(); i++ )
                {
                    MilMetaParam m;
                    m.name = mp[i]->name;
                    if( metaActuals[i].isConst() && mp[i]->mode != Declaration::ConstDecl ||
                            !metaActuals[i].isConst() && mp[i]->mode == Declaration::ConstDecl )
                        error(cur,QString("formal and actual meta parameter %1 not compatible").arg(i+1));
                    if( metaActuals[i].type->form == Type::Generic )
                    {
                        m.isGeneric = true;
                        m.isConst = mp[i]->mode == Declaration::ConstDecl;
                    }else
                    {
                        m.type = ev->toQuali(mp[i]->type);
                        delete mp[i]->type;
                        mp[i]->ownstype = false;
                        if( mp[i]->mode == Declaration::TypeDecl )
                        {
                            mp[i]->type = metaActuals[i].type;
                        }else if( mp[i]->mode == Declaration::ConstDecl )
                        {
                            mp[i]->type = metaActuals[i].type;
                            mp[i]->data = metaActuals[i].val;
                            m.isConst = true;
                            // TODO: check declaration type as soon as corresponding CONST appears
                        }
                    }
                    mps << m;
                }
            if( imp )
            {
                Import i;
                i.metaActuals = metaActuals;
                i.path = md.path;
                md.suffix = imp->moduleSuffix(i.metaActuals);
                if( !md.suffix.isEmpty() )
                {
                    md.fullName = Token::getSymbol(md.fullName + md.suffix);
                    m->name = Token::getSymbol(m->name + md.suffix);
                }
            }
        }else
        {
            for( int i = 0; i < mp.size(); i++ )
            {
                MilMetaParam m;
                m.name = mp[i]->name;
                m.isGeneric = true;
                m.isConst = mp[i]->mode == Declaration::ConstDecl;
                mps << m;
            }
        }
    }else if( !metaActuals.isEmpty() )
        error(cur,"cannot instantiate a non generic module");

    m->data = QVariant::fromValue(md);

    if( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "module");
	}
    out->beginModule(md.fullName,source,mps);

    // call "out" here for all non-generic type and const
    for( int i = 0; i < metaActuals.size(); i++ )
    {
        const Value& ma = metaActuals[i];
        if( ma.type->form == Type::Generic )
            continue;
        if( ma.mode == Value::Const )
        {
            if( i < md.metaParams.size() )
                out->addConst(ev->toQuali(ma.type), md.metaParams[i]->name, ma.val );
        }else if( ma.type->isSimple() )
        {
            if( i < md.metaParams.size() )
                out->addType(md.metaParams[i]->name,false,ev->toQuali(ma.type),MilEmitter::Alias);
        }else
            emitType(ma.type);  // TODO: this doesn't look right; what name should we use?
    }

	while( FIRST_ImportList(la.d_type) || FIRST_DeclarationSequence(la.d_type) ) {
		if( FIRST_ImportList(la.d_type) ) {
			ImportList();
        } else if( FIRST_DeclarationSequence(la.d_type) || la.d_type == Tok_BEGIN ||
                   la.d_type == Tok_TYPE || la.d_type == Tok_VAR || la.d_type == Tok_CONST ||
                   la.d_type == Tok_IMPORT || la.d_type == Tok_END || la.d_type == Tok_PROCEDURE ||
                   la.d_type == Tok_PROC ) {
			DeclarationSequence();
		} else
			invalid("module");
	}
	if( FIRST_block(la.d_type) ) {
        IdentDef id;
        id.name= la;
        id.name.d_val = "$begin";
        id.visi = IdentDef::Private;
        Declaration* procDecl = addDecl(id, Declaration::Procedure);
        mdl->openScope(procDecl);
        out->beginProc("$begin",0, MilProcedure::ModuleInit);
        block();
        out->endProc();
        mdl->closeScope();
    }
	expect(Tok_END, true, "module");
	expect(Tok_ident, false, "module");
	if( la.d_type == Tok_Dot ) {
		expect(Tok_Dot, false, "module");
	}
    out->endModule();
    mdl->closeScope();
}

void Parser2::ImportList() {
	expect(Tok_IMPORT, true, "ImportList");
	import();
	while( la.d_type == Tok_Comma || FIRST_import(la.d_type) ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "ImportList");
		}
		import();
	}
	if( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "ImportList");
	}
}

void Parser2::import() {
    Token localName;
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_ColonEq )  ) {
		expect(Tok_ident, false, "import");
        localName = cur;
		expect(Tok_ColonEq, false, "import");
	}
    TokenList path;
	expect(Tok_ident, false, "import");
    path << cur;
	while( la.d_type == Tok_Dot ) {
		expect(Tok_Dot, false, "import");
		expect(Tok_ident, false, "import");
        path << cur;
    }
    if( localName.d_tokenType == 0 )
        localName = path.last();

    bool doublette;
    Declaration* importDecl = addDecl(localName, 0, Declaration::Import,&doublette);

    Import import;
    foreach( const Token& t, path)
        import.path << t.d_val;

	if( FIRST_MetaActuals(la.d_type) ) {
        // inlined MetaActuals();
        expect(Tok_Lpar, false, "MetaActuals");
        if( !ev->evaluate(ConstExpression(0)) )
            error(cur, ev->getErr());
        Value v = ev->pop();
        import.metaActuals << v;
        Expression::deleteAllExpressions();
        while( la.d_type == Tok_Comma || FIRST_ConstExpression(la.d_type) ) {
            if( la.d_type == Tok_Comma ) {
                expect(Tok_Comma, false, "MetaActuals");
            }
            if( !ev->evaluate(ConstExpression(0)) )
                error(cur, ev->getErr());
            v = ev->pop();
            import.metaActuals << v;
            Expression::deleteAllExpressions();
        }
        expect(Tok_Rpar, false, "MetaActuals");
    }

    if( !doublette )
        importDecl->data = QVariant::fromValue(import);

    if( imp )
    {
        Declaration* mod = imp->loadModule(import);
        if( mod )
        {
            // loadModule returns the module decl; we just need the list of module elements:
            importDecl->link = mod->link;
            ModuleData md = mod->data.value<ModuleData>();
            out->addImport(md.fullName);
        }
    }else
        out->addImport(Token::getSymbol(import.path.join('/')));
}

bool Parser2::isUnique( const MetaParamList& l, const Declaration* m)
{
    foreach( const Declaration* tmp, l )
    {
        if( tmp->name.constData() == m->name.constData() )
            return false;
    }
    return true;
}

MetaParamList Parser2::MetaParams() {
	expect(Tok_Lpar, false, "MetaParams");
    MetaParamList res;
    bool isType = true;
    res << MetaSection(isType);
    while( la.d_type == Tok_Semi || FIRST_MetaSection(la.d_type) ) {
        if( la.d_type == Tok_Semi ) {
            expect(Tok_Comma, false, "MetaParams");
		}
        res << MetaSection(isType);
	}
	expect(Tok_Rpar, false, "MetaParams");
    return res;
}

MetaParamList Parser2::MetaSection(bool& isType) {
    if( la.d_type == Tok_CONST )
    {
        expect(Tok_CONST, true, "MetaSection");
        isType = false;
    }else if( la.d_type == Tok_TYPE )
    {
        expect(Tok_TYPE, true, "MetaSection");
        isType = true;
    }

    MetaParamList res;

    expect(Tok_ident, false, "MetaSection");

    Declaration* decl = addDecl(cur, 0, isType ? Declaration::TypeDecl : Declaration::ConstDecl);
    decl->meta = true;
    res << decl;

    while( ( ( peek(1).d_type == Tok_Comma && peek(2).d_type == Tok_ident ) || peek(1).d_type == Tok_ident )  ) {
        if( la.d_type == Tok_Comma ) {
            expect(Tok_Comma, false, "MetaSection");
        }
        expect(Tok_ident, false, "MetaSection");

        Declaration* decl = addDecl(cur, 0, isType ? Declaration::TypeDecl : Declaration::ConstDecl);
        decl->meta = true;
        res << decl;
    }
    Type* t = 0;
    if( la.d_type == Tok_Colon ) {
        expect(Tok_Colon, false, "MetaSection");
        t = NamedType();
    }

    for( int i = 0; i < res.size(); i++ )
    {
        if( t )
            res[i]->type = t;
        else
        {
            // don't share the type between the section items because it's not a variable
            // but a type declaration
            res[i]->type = new Type();
            res[i]->type->form = Type::Generic;
            res[i]->ownstype = true;
        }
    }

    return res;
}

