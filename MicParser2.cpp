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

static inline bool FIRST_symbolEnum(int tt) {
	return tt == Tok_Colon;
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
	case Tok_TRUE:
	case Tok_NIL:
	case Tok_FALSE:
	case Tok_hexchar:
	case Tok_hexstring:
	case Tok_string:
	case Tok_integer:
	case Tok_Lbrace:
	case Tok_real:
		return true;
	default: return false;
	}
}

static inline bool FIRST_constructor(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_component(int tt) {
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

static inline bool FIRST_set(int tt) {
	return tt == Tok_Lbrace;
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
    mdl(m),scanner(s),out(out),imp(i),thisMod(0)
{
    ev = new MicEvaluator(m,out);
}

Parser2::~Parser2()
{
    delete ev;
}

void Parser2::RunParser() {
	errors.clear();
	next();
    module();
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
    errors << Error(msg,t.d_lineNr, t.d_colNr, t.d_sourcePath);
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

    return false;
}

bool Parser2::assigCompat(Type* lhs, Declaration* rhs) const
{
    // Tv is a procedure type and e is the name of a procedure whose formal parameters match those of Tv.
    if( lhs->form == Type::Proc && rhs->mode == Declaration::Procedure )
        return matchFormals(lhs->subs, rhs->getParams()) && matchResultType(lhs->base,rhs->type);

    // Tv is an enumeration type and e is a valid element of the enumeration;
    if( lhs->form == Type::ConstEnum || lhs->form == Type::SymEnum )
        return lhs->subs.contains(rhs);

    if( lhs->form == Type::Array && lhs->base->form == BasicType::CHAR && lhs->len > 0 &&
            rhs->mode == Declaration::ConstDecl && rhs->type->form == BasicType::String )
        return rhs->data.toByteArray().size() < lhs->len;

    return assigCompat(lhs, rhs->type);
}

bool Parser2::assigCompat(Type* lhs, const Value& rhs) const
{
    if( lhs == 0 )
        return false;

    // Tv is a signed integer and e is an unsigned integer constant, and Tv includes the
    // smallest integer type necessary to represent e.
    if( lhs->isInt() && rhs.isConst() && rhs.type->isUInt() )
        return assigCompat(lhs, ev->smallestIntType(rhs.val));

    if( rhs.mode == Declaration::ConstDecl || rhs.mode == Declaration::Procedure )
        return assigCompat(lhs, rhs.val.value<Declaration*>() );

    if( lhs->form == Type::Array && lhs->base->form == BasicType::CHAR && lhs->len > 0 &&
            rhs.mode == Value::Const && rhs.type->form == BasicType::String)
        return rhs.val.toByteArray().size() < lhs->len;

    // A string of length 1 can be used wherever a character constant is allowed and vice versa.
    if( lhs->form == BasicType::CHAR && rhs.type->form == BasicType::String )
        return rhs.val.toByteArray().size() == 1;

    return assigCompat(lhs, rhs.type);
}

bool Parser2::paramCompat(Declaration* lhs, const Value& rhs) const
{
    Q_ASSERT(lhs->mode == Declaration::ParamDecl);

    // Tf is a pointer to an open array of CHAR, f is CONST, and a is string literal
    if( lhs->visi == Declaration::ReadOnly && lhs->type->form == Type::Pointer &&
            lhs->type->base->form == Type::Array && lhs->type->base->base->form == BasicType::CHAR &&
            lhs->type->base->len == 0 && rhs.type->form == BasicType::String )
        return true;

    // Tf and Ta are equal types, or Ta is assignment compatible with Tf
    return equalTypes(lhs->type,rhs.type) || assigCompat(lhs->type,rhs);
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
    return sameType(lhs,rhs) || (lhs->form == BasicType::NoType && rhs->form == BasicType::NoType);
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

void Parser2::number(Value& v) {
    v.mode = Value::Const;
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
            v.val = signed_ ? number.toLongLong(0,8) : number.toULongLong(0,8);
            break;
        case 'b':
            v.val = signed_ ? number.toLongLong(0,2) : number.toULongLong(0,2);
            break;
        case 'h':
            v.val = signed_ ? number.toLongLong(0,16) : number.toULongLong(0,16);
            break;
        default:
            v.val = signed_ ? number.toLongLong() : number.toULongLong();
            break;
        }
        Type* derived = signed_ ? ev->smallestIntType(v.val) : ev->smallestUIntType(v.val);
        if( type == 0 )
            type = derived;
        else if( derived->form > type->form )
            error(cur,"the given constant value cannot be represented by the given type");
        v.type = type;
	} else if( la.d_type == Tok_real ) {
		expect(Tok_real, false, "number");
        QByteArray str = cur.d_val;
        str.replace('_',"");
        if( str.contains('d') || str.contains('D') )
        {
            str.replace('d','e');
            str.replace('D','E');
            v.type = mdl->getType(BasicType::LONGREAL);
        }else if( str.contains('s') || str.contains('S') )
        {
            str.replace('f','e');
            str.replace('F','E');
            v.type = mdl->getType(BasicType::REAL);
        }else
        {
            if( cur.d_double )
                v.type = mdl->getType(BasicType::LONGREAL);
            else
                v.type = mdl->getType(BasicType::REAL);
       }
        v.val = str.toDouble(); // we save double in any case
    } else
		invalid("number");
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
    ConstExpression();
    Value v = ev->pop();
    d->data = v.val;
    d->type = v.type;
}

void Parser2::ConstExpression() {
    const Token tok = la;
    expression();
    if( !ev->top().isConst() )
        error(tok, QString("expression is not constant"));
}

void Parser2::TypeDeclaration() {
    const IdentDef id = identdef();
	expect(Tok_Eq, false, "TypeDeclaration");
    Type* t = 0;
    // first read type before adding the declaration so it is not known in the type(),
    // and also allow to declare anonymous types used by this one before this one
    Quali q;
    // Token tok = la;
    if( FIRST_NamedType(la.d_type) ) {
        t = NamedType(&q);
    }else
        t = type(false);
    // No, it is allowed here: openArrayError(tok,t);
    Declaration* d = addDecl(id,Declaration::TypeDecl);
    if( t->decl == 0 )
    {
        t->decl = d;
        d->ownstype = true;
    }
    d->type = t;
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
    if( isNewType && deanonymize )
        addHelper(res);
    return res;
}

Type* Parser2::NamedType(Quali* qout) {
    const Token tok = la;
    Declaration* d = resolveQualident(qout);
    if( d == 0 || d->type == 0 || d->mode != Declaration::TypeDecl )
    {
        if( d )
            error(tok, QString("invalid type: %1").arg(d->name.constData()) );
        return mdl->getType(BasicType::Undefined);
    }else
        return d->type;
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
    Type* arr = new Type();
    arr->form = Type::Array;
    arr->len = len;
    arr->base = etype;
    return arr;
}

void Parser2::length(quint32& len) {
	if( FIRST_ConstExpression(la.d_type) ) {
        const Token tok = la;
        ConstExpression();
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
        expression();
        Value v = ev->pop();
        // TODO
        len = 0;
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
        Quali q;
        const Token tok = la;
        Declaration* d = resolveQualident(&q, true);
        if( d == 0 || d->type == 0 || d->mode != Declaration::TypeDecl )
        {
            res->base = mdl->getType(BasicType::Undefined);
            if( q.first.isEmpty() )
            {
                if( d )
                    error(tok, QString("invalid type: %1").arg(tok.d_val.constData()) );
                else
                {
                    res->deferred = true;
                    deferred << qMakePair(res,tok);
                }
            } // else import error already reported
        }else
            res->base = d->type;
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
    } else if( FIRST_symbolEnum(la.d_type) ) {
        res->subs = symbolEnum();
        res->form = Type::SymEnum;
    } else
		invalid("enumeration");
	expect(Tok_Rpar, false, "enumeration");

    return res;
}

Parser2::DeclList Parser2::constEnum() {
	expect(Tok_ident, false, "constEnum");

    DeclList res;

    qint32 index = 0;

    Declaration* d = addDecl(cur, 0, Declaration::ConstDecl);

	if( la.d_type == Tok_Eq ) {
		expect(Tok_Eq, false, "constEnum");
        const Token tok = la;
        ConstExpression();
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

Parser2::DeclList Parser2::symbolEnum() {
    DeclList res;

	expect(Tok_Colon, false, "symbolEnum");
	expect(Tok_ident, false, "symbolEnum");

    bool doublette;
    Declaration* d = mdl->addDecl(cur.d_val, &doublette);
    if( !doublette )
    {
        d->mode = Declaration::ConstDecl;
        d->type = mdl->getType(BasicType::String);
        d->symbol = true;
        d->data = thisMod->data.value<QByteArrayList>().join('.') + "." + d->name;
    }else if( d->mode != Declaration::ConstDecl && !d->symbol )
        error(cur, QString("symbol name collides with existing declaration: %1").arg(cur.d_val.constData()) );
    res << d;

	while( la.d_type == Tok_Comma || la.d_type == Tok_Colon ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "symbolEnum");
		}
		expect(Tok_Colon, false, "symbolEnum");
		expect(Tok_ident, false, "symbolEnum");
        d = mdl->addDecl(cur.d_val, &doublette);
        if( !doublette )
        {
            d->mode = Declaration::ConstDecl;
            d->type = mdl->getType(BasicType::SYMBOL);
            d->symbol = true;
            d->data = thisMod->name + "." + d->name;
        }else if( d->mode != Declaration::ConstDecl && !d->symbol )
            error(cur, QString("symbol name collides with existing declaration: %1").arg(cur.d_val.constData()) );
        res << d;
    }
    return res;
}

void Parser2::VariableDeclaration() {
    const IdentDefList ids = IdentList();
	expect(Tok_Colon, false, "VariableDeclaration");
    const Token tok = la;
    Type* t = type();
    openArrayError(tok,t);
    Declaration* outer = mdl->getTopScope();
    foreach( const IdentDef& id, ids )
    {
        Declaration* d = addDecl(id,outer->mode == Declaration::Module ?
                                     Declaration::VarDecl : Declaration::LocalDecl);
        d->outer = outer;
        d->type = t;
        if( d->mode == Declaration::VarDecl )
            out->addVariable(ev->toDesig(d->type),d->name);
        else
            out->addLocal(ev->toDesig(d->type),d->name);
    }
}

void Parser2::toValue(Value& v, Declaration* d )
{
    v.mode = d->mode;
    v.visi = d->visi;
    switch( v.mode )
    {
    case Declaration::ConstDecl:
        v.val = QVariant::fromValue(d);
        break;
    case Declaration::Builtin:
        v.val = d->level;
        break;
    case Declaration::Procedure:
        v.val = QVariant::fromValue(d);
        break;
    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
        Q_ASSERT(d->outer);
        v.val = d->outer->getIndexOf(d);
        Q_ASSERT(v.val.toInt() != -1 );
        break;
    default:
        v.val = MicEvaluator::toDesig(d);
        break;
    }
    v.type = d->type;
    if( v.type == 0 )
        v.type = mdl->getType(BasicType::NoType);
}

void Parser2::emitType(Type* t, const Quali& q)
{
    Q_ASSERT( t && t->decl );
    if( !q.second.isEmpty() )
    {
       out->addType(MicEvaluator::toDesig(t),t->decl->isPublic(),toDesig(q), MilEmitter::Alias);
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

            out->beginType(MicEvaluator::toDesig(t),t->decl->isPublic(), !hasFixed ? MilEmitter::Union : MilEmitter::Struct );
            // TODO: record can have fixed and variable part which go to separate struct and union or embedded union
            foreach( Declaration* field, t->subs )
                out->addField(field->name,MicEvaluator::toDesig(field->type),field->isPublic());
        }else
        {
            out->beginType(MicEvaluator::toDesig(t),t->decl->isPublic(), MilEmitter::ProcType );
            foreach( Declaration* param, t->subs )
                out->addArgument(param->name,MicEvaluator::toDesig(param->type));
            if( t->base && t->base->form != BasicType::NoType )
                out->setReturnType(MicEvaluator::toDesig(t->base));
        }
        out->endType();
    }else if( t->form == Type::Pointer || t->form == Type::Array )
    {
        QByteArray base;
        if( t->deferred )
        {
            for( int i = 0; i < deferred.size(); i++ )
            {
                if( deferred[i].first == t )
                {
                    base = deferred[i].second.d_val;
                    break;
                }
            }
        }else
            base = t && t->base ? MicEvaluator::toDesig(t->base) : QByteArray();
        out->addType(MicEvaluator::toDesig(t),t->decl->isPublic(),base,
                     t->form == Type::Pointer ? MilEmitter::Pointer : MilEmitter::Array, t->len );
    }else if( t->form == Type::ConstEnum )
    {
        out->addType(MicEvaluator::toDesig(t),t->decl->isPublic(),"int32", MilEmitter::Alias);
    }else if( t->form == Type::SymEnum )
    {
        out->addType(MicEvaluator::toDesig(t),t->decl->isPublic(),"char", MilEmitter::Pointer);
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
    decl->level = out->addLocal(ev->toDesig(t),decl->name);
    return decl;
}

void Parser2::openArrayError(const Token& tok, Type* t)
{
    if( t->form == Type::Array && t->len == 0)
        error(tok,"open array cannot be used here");
}

void Parser2::prepareParam(const Parser2::DeclList& formals, const Parser2::Args& actuals)
{
    if( actuals.size() > formals.size() )
        return; // error reported elsewhere
    Declaration* formal = formals[actuals.size()-1];
    QPair<Token,Value> actual = actuals.last();
    if( !paramCompat(formal,actual.second) )
        error(actual.first, "actual argument not compatible with formal parameter");
    else
        ev->prepareRhs(formal->type); // actual.second is ev->top()
}

QByteArray Parser2::toDesig(const Parser2::Quali& q)
{
    QByteArray res;
    if( q.first.isEmpty() )
        res = "." + q.first;
    res += q.second;
    return res;
}

static bool renderLvalue( const Value& proc, int arg )
{
    if( proc.mode != Declaration::Builtin )
        return false;

    switch( proc.val.toInt() )
    {
    case Builtin::NEW:
    case Builtin::INC:
    case Builtin::DEC:
        if( arg == 0 )
            return true;
        break;
    }
    return false;
}

// designator results in an lvalue if possible
void Parser2::designator(bool lvalue) {
    // original: qualident();

    Value v;
    Token tok = la;
    maybeQualident(v);
    ev->push(v);

    if( v.isLvalue() )
    {
        if( !ev->desigVar() )
            error(tok, ev->getErr() );
        v = ev->top();
    }

    // TODO: track read-only

    while( FIRST_selector(la.d_type) ) {
        // inlined selector

        // TODO avoid unnecessary ld*a ldind pairs if possible
        if( v.ref && !v.type->isStructured() )
        {
            ev->derefValue();
            v = ev->top();
        }

        if( la.d_type == Tok_Dot ) {
            tok = la;
            expect(Tok_Dot, false, "selector");
            expect(Tok_ident, false, "selector");
            if( v.type->form == Type::Pointer )
            {
                if( !ev->derefPointer() )
                    error(tok, ev->getErr() );
                v = ev->top();
            }
            if( v.type->form == Type::Record )
            {
                Declaration* field = v.type->findField(cur.d_val);
                if( field == 0 )
                    error(cur,QString("the record doesn't have a field named '%1'").
                          arg(cur.d_val.constData()) );
                else
                {
                    v.mode = field->mode;
                    v.type = field->type;
                    v.ref = false;
                    v.val = QVariant::fromValue(field);
                    ev->push(v);
                    if( !ev->desigField() )
                        error(tok, ev->getErr() );
                    v = ev->top();
                }
            }else
                error(cur,QString("cannot select a field in given type") );
        } else if( la.d_type == Tok_Lbrack ) {
            expect(Tok_Lbrack, false, "selector");
            if( v.type->form == Type::Pointer )
            {
                if( !ev->derefPointer() )
                    error(tok, ev->getErr() );
                v = ev->top();
            }
            tok = la;
            expression();
            expect(Tok_Rbrack, false, "selector");
            if( v.type->form == Type::Array )
            {
                if( !ev->top().type->isInteger() )
                    error(tok,"expecting an integer to index an array");
                else if( !ev->desigIndex() )
                    error(tok, ev->getErr() );
                v = ev->top();
            }else
                error(cur,QString("cannot index an element in given type") );
        } else if( la.d_type == Tok_Hat ) {
            tok = la;
            expect(Tok_Hat, false, "selector");
            if( ev->top().type->form != Type::Pointer )
                error(tok,"only a pointer type can be dereferenced");
            else if( !ev->derefPointer() )
                error(tok, ev->getErr() );
            v = ev->top();
        } else if( la.d_type == Tok_Lpar ) {
            expect(Tok_Lpar, false, "selector");
            const Token lpar = cur;
            const Value proc = v;
            DeclList formals;
            if( proc.mode == Declaration::Procedure )
                formals = proc.val.value<Declaration*>()->getParams();
            else if( proc.type->form == Type::Proc )
                formals = proc.type->subs;
            Args args;
            bool isTypeCast = false;
            if( FIRST_expression(la.d_type) ) {
                // inlined ExpList
                tok = la;
                expression(renderLvalue(proc,args.size()));
                isTypeCast = ev->top().mode == Declaration::TypeDecl;
                args << qMakePair(tok,ev->top());
                if( !isTypeCast && proc.mode != Declaration::Builtin )
                    prepareParam(formals,args);
                while( la.d_type == Tok_Comma || FIRST_expression(la.d_type) ) {
                    if( la.d_type == Tok_Comma )
                        expect(Tok_Comma, false, "ExpList");
                    tok = la;
                    expression(renderLvalue(proc,args.size()));
                    args << qMakePair(tok,ev->top());
                    if( !isTypeCast && proc.mode != Declaration::Builtin )
                        prepareParam(formals,args);
                }
            }
            expect(Tok_Rpar, false, "selector");

            if( proc.mode == Declaration::Builtin && proc.val.toInt() == Builtin::ASSERT )
            {
                Value tmp;
                tmp.mode = Value::Const;
                tmp.type = mdl->getType(BasicType::UINT32);
                tmp.val = lpar.d_lineNr;
                args << qMakePair(lpar,tmp);
                ev->push(tmp);
                tmp.type = mdl->getType(BasicType::String);
                tmp.val = "\"" + lpar.d_sourcePath.toUtf8() + "\"";
                args << qMakePair(lpar,tmp);
                ev->push(tmp);
            }

            bool hasError = false;
            if( isTypeCast )
            {
                if( args.size() != 1 )
                {
                    error(lpar,"type guard requires a single argument");
                    hasError = true;
                }else if( args.first().second.type->form != Type::Record )
                {
                    error(lpar,"type guard only applicable to record types");
                    // TODO: avoid casting records on the stack by value
                    hasError = true;
                }else if( !ev->cast() )
                {
                    error(tok, ev->getErr() );
                    hasError = true;
                }
            }else
            {
                // NOTE: built-in args are checked in ev->call instead
                if( proc.mode != Declaration::Builtin && args.size() != formals.size() )
                {
                    error(lpar,"number of formal and actual arguments don't match");
                    hasError = true;
                }else if( !ev->call(args.size()) )
                {
                    error(lpar, ev->getErr() );
                    hasError = true;
                }
            }
            if( hasError )
            {
                for( int i = 0; i < args.size(); i++ )
                    ev->pop();
                if( !isTypeCast )
                    ev->pop(); // callee
            }else
                v = ev->top();

        } else
            invalid("selector");
    }

    if( !lvalue && v.ref )
        ev->derefValue();
}

void Parser2::maybeQualident(Value& v)
{
    expect(Tok_ident, false, "designator");
    Declaration* d = mdl->findDecl(cur.d_val);
    if( d )
    {
        toValue(v,d);
        if( la.d_type == Tok_Dot && d->mode == Declaration::Import )
        {
            // this is the one and only qualident case
            expect(Tok_Dot, false, "selector");
            expect(Tok_ident, false, "selector");
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
                toValue(v,d);
            }
        }else
            v.visi = Declaration::NA; // symbol is local, no read/write restriction
    }else
    {
        error(cur,QString("cannot find declaration of '%1'").arg(cur.d_val.constData()));
        v.type = mdl->getType(BasicType::NoType);
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

Parser2::DeclList Parser2::toList(Declaration* d)
{
    DeclList res;
    while( d )
    {
        res << d;
        d = d->next;
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

void Parser2::expression(bool lvalue) {
    SimpleExpression(lvalue);
	if( FIRST_relation(la.d_type) ) {
        const Token tok = la;
        quint8 op = relation();
        SimpleExpression();
        if( !ev->binaryOp(op) )
            error(tok,ev->getErr());
    }
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

void Parser2::SimpleExpression(bool lvalue) {
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
    term(lvalue);
    if( op != 0 ) {
        if( !ev->unaryOp(op) )
            error(tok,ev->getErr());
    }
	while( FIRST_AddOperator(la.d_type) ) {
        Token tok = la;
        op = AddOperator();
        term();
        if( !ev->binaryOp(op) )
            error(tok,ev->getErr());
    }
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

void Parser2::term(bool lvalue) {
    factor(lvalue);
	while( FIRST_MulOperator(la.d_type) ) {
        const quint8 op = MulOperator();
        factor();
        ev->binaryOp(op);
	}
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

void Parser2::literal() {
    Value v;
    v.mode = Value::Const;
	if( FIRST_number(la.d_type) ) {
        number(v);
	} else if( la.d_type == Tok_string ) {
		expect(Tok_string, false, "literal");
        v.type = mdl->getType(BasicType::String);
        v.val = ev->dequote(cur.d_val);
    }else if( la.d_type == Tok_hexstring ) {
        expect(Tok_hexstring, false, "literal");
        // alternative syntax for A{ x x x } with A = array of byte
        const QByteArray bytes = QByteArray::fromHex(cur.d_val); // already comes without quotes
        Type* arr = new Type();
        arr->base = mdl->getType(BasicType::UINT8);
        arr->form = Type::Array;
        arr->len = bytes.size();
        addHelper(arr);
        v.type = arr;
        v.val = bytes;
    } else if( la.d_type == Tok_hexchar ) {
		expect(Tok_hexchar, false, "literal");
        v.type = mdl->getType(BasicType::CHAR);
        QByteArray tmp = cur.d_val;
        tmp.chop(1); // remove X postfix
        v.val = QVariant::fromValue((char)tmp.toUInt(0,16));
    } else if( la.d_type == Tok_NIL ) {
		expect(Tok_NIL, true, "literal");
        v.type = mdl->getType(BasicType::Nil);
        v.val.clear();
    } else if( FIRST_set(la.d_type) ) {
        set();
        return; // set() has already pushed its value
	} else if( la.d_type == Tok_TRUE ) {
		expect(Tok_TRUE, true, "literal");
        v.type = mdl->getType(BasicType::BOOLEAN);
        v.val = true;
    } else if( la.d_type == Tok_FALSE ) {
		expect(Tok_FALSE, true, "literal");
        v.type = mdl->getType(BasicType::BOOLEAN);
        v.val = false;
	} else
		invalid("literal");
    ev->push(v);
}

void Parser2::constructor() {
    const Token ttok = la;
    Quali q;
    Type* t = NamedType(&q);
    Q_ASSERT( t != 0 );
    Value v;
    v.mode = Value::Const;
    v.type = t;
    expect(Tok_Lbrace, false, "constructor");
	if( FIRST_component(la.d_type) ) {
        QList<Value> vals;
        Value c;
        DeclList dl;
        quint8 state = 0;
        component(t,c,state,dl);
        vals << c;

        while( la.d_type == Tok_Comma || FIRST_component(la.d_type) ) {
            if( la.d_type == Tok_Comma )
                expect(Tok_Comma, false, "constructor");
            component(t, c, state, dl);
            vals << c;
		}

        if( t->form == Type::Record )
        {
            Q_ASSERT( vals.size() == dl.size() );
            QPair<int, int> count = t->getFieldCount();
            if( count.second )
                count.first++;
            if( count.first != dl.size() )
                error(ttok,QString("the number of components (%1) doesn't fit the number of fields "
                                   "(%2, variants count as one)").arg(dl.size()).arg(count.first));
            if( state == Named )
            {
                QVariantMap m;
                for( int i = 0; i < vals.size(); i++ )
                    m.insert(dl[i]->name, vals[i].val);
                v.val = m;
            }else
            {
                QVariantList l;
                for( int i = 0; i < vals.size(); i++ )
                    l.append(vals[i].val);
                v.val = l;
            }
        }else if( t->form == Type::Array )
        {
            QVariantList l;
            if( t->len && t->len != vals.size() )
                error(ttok,QString("the number of components doesn't fit the array length"));
            else if( t->len == 0 )
            {
                Type* arr = new Type();
                arr->base = t->base;
                arr->form = Type::Array;
                arr->len = vals.size();
                addHelper(arr);
                v.type = arr;
            }
            for( int i = 0; i < vals.size(); i++ )
                l << vals[i].val;
            v.val = l;
        }else if( t->form == Type::Pointer )
        {
            if( vals.size() != 1 )
                error(ttok,QString("a pointer constructor expects exactly one component"));
            v.val = vals.first().val;
        }else
            error(ttok,QString("constructor for type '%1'' not supported").arg(q.second.constData()));
	}

	expect(Tok_Rbrace, false, "constructor");
    ev->push(v);
}

void Parser2::component(Type* constrType, Value& v, quint8& state,  DeclList& dl) {
    QByteArray id;
    const Token tok = la;
    if( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Eq )
    {
        expect(Tok_ident, false, "component");
        id = cur.d_val;
        expect(Tok_Eq, false, "component");
        if( constrType->form != Type::Record )
            error(tok,QString("constructor for this type expects anonymous components"));
    }

    if( state == Anonymous && !id.isEmpty() || state == Named && id.isEmpty() )
        error(tok,QString("named and anonymous components cannot be mixed"));

    if( state == FirstComponent )
    {
        if( !id.isEmpty() )
            state = Named;
        else
            state = Anonymous;
    }

    if( constrType->form == Type::Record )
    {
        if( !id.isEmpty() )
        {
            Declaration* d = constrType->findField(id);
            if( d == 0 )
                error(tok,QString("the constructor type has no field '%1'").arg(id.constData()));
            if( dl.contains(d) )
                error(tok,QString("duplicate initializer for '%1'").arg(id.constData()));
        }else if(dl.isEmpty())
        {
            dl.append(constrType->subs);
        }else
        {
            if( dl.last()->next == 0 )
                error(tok,QString("more components than fields"));
            else
                dl.append(dl.last()->next);
        }
        if( dl.back()->mode == Declaration::Variant && state != Named )
            error(tok,QString("records with variant parts can only be constructed with named components"));
        componentTypeStack.push_back(dl.back()->type);
        int variantCount = 0;
        for( int i = 0; i < dl.size(); i++ )
            if( dl[i]->mode == Declaration::Variant )
                variantCount++;
        if(variantCount > 1)
            error(tok,QString("records with variant parts can only initialize one variant (found %1)").arg(variantCount));
    }else if( constrType->form == Type::Array )
        componentTypeStack.push_back(constrType->base);
    else if( constrType->form == Type::Pointer )
        componentTypeStack.push_back(constrType);
    else
        componentTypeStack.push_back(mdl->getType(BasicType::Undefined));

    if( ( constrType->form == Type::Array || constrType->form == Type::Pointer ) && state == Named )
        error(tok,QString("only record constructors support named components"));

    ConstExpression();
    v = ev->pop();
    if( v.type->form == BasicType::String )
        v.val = QString::fromLatin1(v.val.toByteArray());

    if( componentTypeStack.back()->form == Type::Pointer && !v.type->isUInt() )
        error(tok,QString("pointer constructor expects one anonymous unsigned integer component"));
    if( !assigCompat(componentTypeStack.back(), v) )
        error(tok,QString("the component is not compatible with the given field or element type"));

    componentTypeStack.pop_back();
}

void Parser2::factor(bool lvalue) {
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Lbrace )  ) {
        constructor();
	} else if( FIRST_literal(la.d_type) ) {
        literal();
	} else if( FIRST_variableOrFunctionCall(la.d_type) ) {
        variableOrFunctionCall(lvalue);
	} else if( la.d_type == Tok_Lpar ) {
		expect(Tok_Lpar, false, "factor");
        expression();
		expect(Tok_Rpar, false, "factor");
    } else if( la.d_type == Tok_Tilde || la.d_type == Tok_NOT ) {
        const Token tok = la;
        if( la.d_type == Tok_NOT )
            expect(Tok_NOT, false, "factor");
        else
            expect(Tok_Tilde, false, "factor");
        factor();
        if( !ev->unaryOp(Tok_NOT) )
            error(tok,ev->getErr());
    } else if( la.d_type == Tok_At ) {
        expect(Tok_At, false, "factor");
        designator(true);

        Value v = ev->pop();
        Q_ASSERT(v.ref);
        Type* ptr = new Type();
        ptr->base = v.type;
        ptr->form = Type::Pointer;
        addHelper(ptr);

        v.mode = Value::Val;
        v.type = ptr;
        v.ref = true;
        ev->push(v);
    } else
		invalid("factor");
}

void Parser2::variableOrFunctionCall(bool lvalue) {
    designator(lvalue);
}

void Parser2::set() {
    // TODO
	expect(Tok_Lbrace, false, "set");
	if( FIRST_element(la.d_type) ) {
		element();
        while( la.d_type == Tok_Comma || FIRST_element(la.d_type) ) {
            if( la.d_type == Tok_Comma )
                expect(Tok_Comma, false, "set");
			element();
		}
	}
	expect(Tok_Rbrace, false, "set");
}

void Parser2::element() {
    Value v; // TODO
    expression();
	if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "element");
        expression();
	}
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
    designator(true);
    const Value lhs = ev->top();
	if( la.d_type == Tok_ColonEq ) {
        const Token tok = la;
		expect(Tok_ColonEq, false, "assignmentOrProcedureCall");
        expression();
        // TODO: avoid assigning to structured return values of functions on left side
        if( !assigCompat( lhs.type, ev->top() ) )
            error(tok, "right side is not assignment compatible with left side");
        else
            ev->assign();
    }else
        ev->pop();
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
    expression();
    ev->assureTopOnMilStack(true);
	expect(Tok_THEN, true, "IfStatement");
    out->then_();
    StatementSequence();
    int level = 0;
	while( FIRST_ElsifStatement(la.d_type) ) {
        // inlined ElsifStatement();
        out->else_();
        expect(Tok_ELSIF, true, "ElsifStatement");
        out->if_();
        expression();
        ev->assureTopOnMilStack(true);
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
    expression();
    Type* t = ev->top().type;
    ev->assureTopOnMilStack(true);
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
	if( FIRST_CaseLabelList(la.d_type) ) {
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
    ConstExpression();
    Value res = ev->pop();
    if( !assigCompat(t, res) )
        error(tok,"label has incompatible type");
    if( res.mode == Declaration::ConstDecl)
    {
        res.mode = Value::Const;
        res.val = res.val.value<Declaration*>()->data;
    }
    return res;
}

void Parser2::WhileStatement() {
	expect(Tok_WHILE, true, "WhileStatement");
    out->while_();
    const Token t = la;
    expression();
    if( ev->top().type->form != BasicType::BOOLEAN )
        error(t,"expecting boolean expression");
    ev->assureTopOnMilStack(true);
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
    expression();
    if( ev->top().type->form != BasicType::BOOLEAN )
        error(t,"expecting boolean expression");
    ev->assureTopOnMilStack(true);
    out->end_();
}

void Parser2::ForStatement() {
	expect(Tok_FOR, true, "ForStatement");
	expect(Tok_ident, false, "ForStatement");
    Declaration* ident = mdl->findDecl(cur.d_val);
    if( ident == 0 || !ident->isLvalue() )
        error(cur,"identifier must reference a variable or parameter");
    if( ident->type == 0 || !ident->type->isInteger() )
        error(cur,"control variable must be of integer type");
    // TODO: support enums as well
    expect(Tok_ColonEq, false, "ForStatement");

    Value v;
    toValue(v,ident);
    ev->push(v);
    ev->desigVar();
    expression();
    ev->assign();

    expect(Tok_TO, true, "ForStatement");
    Declaration* to = addTemp(ident->type);
    toValue(v,to);
    ev->push(v);
    ev->desigVar();
    expression();
    ev->assign();

    int by = 1;
	if( la.d_type == Tok_BY ) {
		expect(Tok_BY, true, "ForStatement");
        Token tok = la;
        ConstExpression();
        v = ev->pop();
        if( ident && !assigCompat(ident->type, v) )
            error(tok,"constant expression is not compatible with control variable");
        else
            by = v.val.toInt();
        if( by == 0 )
            error(tok,"by cannot be zero");
    }
	expect(Tok_DO, true, "ForStatement");

    out->while_();
    toValue(v,ident);
    ev->push(v);
    ev->desigVar();
    ev->derefValue();
    toValue(v,to);
    ev->push(v);
    ev->desigVar();
    ev->derefValue();
    if( by > 0 )
        ev->binaryOp(Tok_Leq);
    else
        ev->binaryOp(Tok_Geq);
    ev->pop();
    out->do_();
    StatementSequence();

    // i := i + 1
    toValue(v,ident);
    ev->push(v);
    ev->desigVar();
    ev->dup();
    ev->derefValue();
    if( ident->type->form == BasicType::INT64 || ident->type->form == BasicType::UINT64 )
        out->ldc_i8(by);
    else
        out->ldc_i4(by);
    out->add_();
    ev->assign();

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

        Declaration* forward = mdl->findDecl(id.name.d_val);
        if( forward && forward->mode != Declaration::ForwardDecl )
            error(id.name, QString("procedure name is not unique: %1").arg(id.name.d_val.constData()) );
        if( forward )
            forward->name.clear(); // so it doesn't intervene name lookup

        Declaration* procDecl = addDecl(id, Declaration::Procedure);

        mdl->openScope(procDecl);
        if( FIRST_FormalParameters(la.d_type) ) {
            procDecl->type = FormalParameters();
        }

        if( forward )
        {
            if( !matchFormals(forward->getParams(), procDecl->getParams() ) &&
                    !matchResultType(forward->type, procDecl->type) &&
                    forward->extern_ != procDecl->extern_ &&
                    forward->inline_ != procDecl->inline_ &&
                    forward->invar != procDecl->invar )
                error(id.name,"procedure declaration is not compatible with preceding forward declaration");
        }

        if( peek(1).d_type == Tok_EXTERN ||
                ( peek(1).d_type == Tok_Semi && peek(2).d_type == Tok_EXTERN ) ) {
            if( la.d_type == Tok_Semi ) {
                expect(Tok_Semi, false, "ProcedureDeclaration");
            }
            expect(Tok_EXTERN, true, "ProcedureDeclaration");
            procDecl->extern_ = true;
            out->beginProc(MicEvaluator::toDesig(procDecl),mdl->getTopScope()->mode == Declaration::Module &&
                           id.visi > 0, MilProcedure::Extern);

            const QList<Declaration*> params = procDecl->getParams();
            foreach( Declaration* p, params )
                out->addArgument(ev->toDesig(p->type),p->name);
            if( procDecl->type && procDecl->type->form != BasicType::NoType )
                out->setReturnType(MicEvaluator::toDesig(procDecl->type));

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
            out->beginProc(MicEvaluator::toDesig(procDecl),mdl->getTopScope()->mode == Declaration::Module &&
                           id.visi > 0, kind);

            const QList<Declaration*> params = procDecl->getParams();
            foreach( Declaration* p, params )
                out->addArgument(ev->toDesig(p->type),p->name);
            if( procDecl->type && procDecl->type->form != BasicType::NoType )
                out->setReturnType(MicEvaluator::toDesig(procDecl->type));

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
        expression();
        if( !assigCompat( mdl->getTopScope()->type, ev->top() ) )
            error(tok,"expression is not compatible with the return type");
        ev->prepareRhs(mdl->getTopScope()->type);
        out->ret_();
    }else if( mdl->getTopScope()->type != 0 && mdl->getTopScope()->type->form != BasicType::NoType )
        error(cur,"this return statement requires an expression");
}

Type* Parser2::FormalParameters() {
	expect(Tok_Lpar, false, "FormalParameters");
	if( FIRST_FPSection(la.d_type) ) {
		FPSection();
		while( ( ( peek(1).d_type == Tok_ident || peek(2).d_type == Tok_ident ) )  ) {
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
    thisMod = m;
    mdl->openScope(m);
    expect(Tok_MODULE, true, "module");
	expect(Tok_ident, false, "module");
    const QByteArray name = cur.d_val;
    m->name = name;
    QByteArrayList path = scanner->path();
    path += name;
    m->data = QVariant::fromValue(path);
    const QString source = cur.d_sourcePath;
    MilMetaParams mps;
    if( FIRST_MetaParams(la.d_type) ) {
        MetaParamList mp = MetaParams();
        for( int i = 0; i < mp.size(); i++ )
        {
            MilMetaParam m;
            m.name = mp[i].name;
            m.type = mp[i].type;
            mps << m;
        }
	}
	if( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "module");
	}
    out->beginModule(name,source,mps);
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
    thisMod = 0;
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
    error(la,"import not yet supported"); // TODO

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

    Import i;
    foreach( const Token& t, path)
        i.path << t.d_val;

	if( FIRST_MetaActuals(la.d_type) ) {
        // inlined MetaActuals();
        expect(Tok_Lpar, false, "MetaActuals");
        ConstExpression();
        Value v = ev->pop();
        i.metaActuals << v;
        while( la.d_type == Tok_Comma || FIRST_ConstExpression(la.d_type) ) {
            if( la.d_type == Tok_Comma ) {
                expect(Tok_Comma, false, "MetaActuals");
            }
            ConstExpression();
            v = ev->pop();
            i.metaActuals << v;
        }
        expect(Tok_Rpar, false, "MetaActuals");
    }

    if( !doublette )
        importDecl->data = QVariant::fromValue(i);

    if( imp )
    {
        importDecl->link = imp->loadModule(i.path);
        // TODO: instantiate if generic
    }
}

bool Parser2::isUnique( const MetaParamList& l, const MetaParam& m)
{
    foreach( const MetaParam& tmp, l )
    {
        if( tmp.name.constData() == m.name.constData() )
            return false;
    }
    return true;
}

Parser2::MetaParamList Parser2::MetaParams() {
	expect(Tok_Lpar, false, "MetaParams");
    MetaParamList res;
    res << MetaSection();
	while( la.d_type == Tok_Semi || FIRST_MetaSection(la.d_type) ) {
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "MetaParams");
		}
        MetaParamList mp = MetaSection();
        res << mp;
	}
	expect(Tok_Rpar, false, "MetaParams");
    return res;
}

Parser2::MetaParamList Parser2::MetaSection() {
    MetaParamList res;
    if( la.d_type == Tok_CONST ) {
        expect(Tok_CONST, true, "MetaSection");
        expect(Tok_ident, false, "MetaSection");
        TokenList names;
        names << cur;
        while( ( ( peek(1).d_type == Tok_Comma || peek(1).d_type == Tok_ident ) && peek(2).d_type == Tok_ident )  ) {
            if( la.d_type == Tok_Comma ) {
                expect(Tok_Comma, false, "MetaSection");
            }
            expect(Tok_ident, false, "MetaSection");
            names << cur;
        }
        expect(Tok_Colon, false, "MetaSection");
        Quali q;
        Type* type = NamedType(&q);
        foreach( const Token& name, names )
        {
            MetaParam mp;
            mp.name = name.d_val;
            mp.type = q;
            res << mp;
            Declaration* d = addDecl(name, 0, Declaration::ConstDecl);
            d->type = type;
        }
    } else if( la.d_type == Tok_TYPE || la.d_type == Tok_ident ) {
        if( la.d_type == Tok_TYPE ) {
            expect(Tok_TYPE, true, "MetaSection");
        }
        expect(Tok_ident, false, "MetaSection");
        TokenList names;
        names << cur;
        while( ( ( peek(1).d_type == Tok_Comma || peek(1).d_type == Tok_ident ) && peek(2).d_type == Tok_ident )  ) {
            if( la.d_type == Tok_Comma ) {
                expect(Tok_Comma, false, "MetaSection");
            }
            expect(Tok_ident, false, "MetaSection");
            names << cur;
        }
        foreach( const Token& name, names )
        {
            MetaParam mp;
            mp.name = name.d_val;
            res << mp;
            Declaration* d = addDecl(name,0,Declaration::TypeDecl);
            d->type = mdl->getType(BasicType::Undefined);
        }
    } else
        invalid("MetaSection");
    return res;
}

