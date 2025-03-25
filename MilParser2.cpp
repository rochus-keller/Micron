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

#include "MilParser2.h"
using namespace Mil;

static inline bool FIRST_Mil(int tt) {
	return tt == Tok_MODULE;
}

static inline bool FIRST_integer(int tt) {
	return tt == Tok_Minus || tt == Tok_unsigned || tt == Tok_Plus;
}

static inline bool FIRST_number(int tt) {
	return tt == Tok_Minus || tt == Tok_unsigned || tt == Tok_float || tt == Tok_Plus;
}

static inline bool FIRST_qualident(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_trident(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_identdef(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ConstDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_TypeDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_type(int tt) {
	switch(tt){
	case Tok_UNION:
	case Tok_STRUCT:
	case Tok_Lbrack:
	case Tok_OBJECT:
	case Tok_ident:
	case Tok_ARRAY:
	case Tok_PROCEDURE:
	case Tok_Hat:
	case Tok_PROC:
	case Tok_POINTER:
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
	return tt == Tok_unsigned;
}

static inline bool FIRST_StructUnionType(int tt) {
	return tt == Tok_UNION || tt == Tok_STRUCT;
}

static inline bool FIRST_FieldList(int tt) {
	return tt == Tok_ident || tt == Tok_2Dot;
}

static inline bool FIRST_IdentList(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ObjectType(int tt) {
	return tt == Tok_OBJECT;
}

static inline bool FIRST_MemberList(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_PointerType(int tt) {
	return tt == Tok_Hat || tt == Tok_POINTER;
}

static inline bool FIRST_ProcedureType(int tt) {
	return tt == Tok_PROCEDURE || tt == Tok_PROC;
}

static inline bool FIRST_VariableDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ProcedureDeclaration(int tt) {
	return tt == Tok_PROCEDURE || tt == Tok_PROC;
}

static inline bool FIRST_Binding(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ProcedureBody(int tt) {
	return tt == Tok_BEGIN || tt == Tok_VAR;
}

static inline bool FIRST_LocalDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_FormalParameters(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_ReturnType(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_FPSection(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_module(int tt) {
	return tt == Tok_MODULE;
}

static inline bool FIRST_ImportList(int tt) {
	return tt == Tok_IMPORT;
}

static inline bool FIRST_import(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_DeclarationSequence(int tt) {
	return tt == Tok_TYPE || tt == Tok_PROCEDURE || tt == Tok_CONST || tt == Tok_PROC || tt == Tok_VAR;
}

static inline bool FIRST_Expression(int tt) {
	switch(tt){
	case Tok_LDELEM_U1:
	case Tok_LDC_I8:
	case Tok_SHL:
	case Tok_LDIND_I8:
	case Tok_LDC_R4:
	case Tok_LDARGA:
	case Tok_CGT:
	case Tok_AND:
	case Tok_NEG:
	case Tok_CALLVI:
	case Tok_LDC_I4_1:
	case Tok_NEWARR:
	case Tok_LDIND_R4:
	case Tok_CALLVIRT:
	case Tok_LDC_I4_4:
	case Tok_DIV:
	case Tok_LDVAR:
	case Tok_CONV_U8:
	case Tok_NEWVLA:
	case Tok_LDLOC_0:
	case Tok_CONV_I2:
	case Tok_MUL:
	case Tok_LDIND_IP:
	case Tok_DUP:
	case Tok_CLT:
	case Tok_LDARG_0:
	case Tok_LDNULL:
	case Tok_LDFLD:
	case Tok_LDFLDA:
	case Tok_CONV_U4:
	case Tok_LDC_I4_8:
	case Tok_LDELEM_R4:
	case Tok_PTROFF:
	case Tok_LDELEM:
	case Tok_LDARG_S:
	case Tok_LDELEM_I4:
	case Tok_SIZEOF:
	case Tok_LDC_I4:
	case Tok_REM:
	case Tok_CONV_R4:
	case Tok_LDELEM_I8:
	case Tok_INITOBJ:
	case Tok_LDARGA_S:
	case Tok_CALLI:
	case Tok_LDSTR:
	case Tok_CONV_U1:
	case Tok_LDELEM_U4:
	case Tok_SHR:
	case Tok_LDC_I4_7:
	case Tok_LDC_I4_0:
	case Tok_LDELEM_IP:
	case Tok_LDIND_I1:
	case Tok_NOP:
	case Tok_OR:
	case Tok_CGT_UN:
	case Tok_ADD:
	case Tok_CONV_I4:
	case Tok_LDARG:
	case Tok_NEWOBJ:
	case Tok_DIV_UN:
	case Tok_LDELEM_I1:
	case Tok_REM_UN:
	case Tok_ISINST:
	case Tok_LDC_I4_3:
	case Tok_SUB:
	case Tok_LDIND_R8:
	case Tok_LDLOC_S:
	case Tok_CONV_U2:
	case Tok_XOR:
	case Tok_LDC_I4_M1:
	case Tok_IIF:
	case Tok_LDLOCA:
	case Tok_LDPROC:
	case Tok_CASTPTR:
	case Tok_LDLOC_2:
	case Tok_LDELEM_I2:
	case Tok_CONV_I1:
	case Tok_LDELEMA:
	case Tok_LDLOC_1:
	case Tok_LDIND_I4:
	case Tok_LDLOC:
	case Tok_LDELEM_U8:
	case Tok_LDIND_U4:
	case Tok_CEQ:
	case Tok_LDARG_3:
	case Tok_LDIND_U2:
	case Tok_LDLOCA_S:
	case Tok_CONV_R8:
	case Tok_LDC_I4_5:
	case Tok_LDVARA:
	case Tok_LDC_I4_S:
	case Tok_LDIND_U1:
	case Tok_LDC_I4_2:
	case Tok_LDELEM_R8:
	case Tok_LDELEM_U2:
	case Tok_CALL:
	case Tok_LDOBJ:
	case Tok_LDIND:
	case Tok_LDIND_U8:
	case Tok_LDLOC_3:
	case Tok_LDARG_2:
	case Tok_LDC_I4_6:
	case Tok_CONV_I8:
	case Tok_CLT_UN:
	case Tok_LDMETH:
	case Tok_LDIND_IPP:
	case Tok_LDARG_1:
	case Tok_NOT:
	case Tok_LDIND_I2:
	case Tok_LDC_R8:
	case Tok_SHR_UN:
	case Tok_CONV_IP:
		return true;
	default: return false;
	}
}

static inline bool FIRST_ExpInstr(int tt) {
	switch(tt){
	case Tok_LDELEM_U1:
	case Tok_LDC_I8:
	case Tok_SHL:
	case Tok_LDIND_I8:
	case Tok_LDC_R4:
	case Tok_LDARGA:
	case Tok_CGT:
	case Tok_AND:
	case Tok_NEG:
	case Tok_CALLVI:
	case Tok_LDC_I4_1:
	case Tok_NEWARR:
	case Tok_LDIND_R4:
	case Tok_CALLVIRT:
	case Tok_LDC_I4_4:
	case Tok_DIV:
	case Tok_LDVAR:
	case Tok_CONV_U8:
	case Tok_NEWVLA:
	case Tok_LDLOC_0:
	case Tok_CONV_I2:
	case Tok_MUL:
	case Tok_LDIND_IP:
	case Tok_DUP:
	case Tok_CLT:
	case Tok_LDARG_0:
	case Tok_LDNULL:
	case Tok_LDFLD:
	case Tok_LDFLDA:
	case Tok_CONV_U4:
	case Tok_LDC_I4_8:
	case Tok_LDELEM_R4:
	case Tok_PTROFF:
	case Tok_LDELEM:
	case Tok_LDARG_S:
	case Tok_LDELEM_I4:
	case Tok_SIZEOF:
	case Tok_LDC_I4:
	case Tok_REM:
	case Tok_CONV_R4:
	case Tok_LDELEM_I8:
	case Tok_INITOBJ:
	case Tok_LDARGA_S:
	case Tok_CALLI:
	case Tok_LDSTR:
	case Tok_CONV_U1:
	case Tok_LDELEM_U4:
	case Tok_SHR:
	case Tok_LDC_I4_7:
	case Tok_LDC_I4_0:
	case Tok_LDELEM_IP:
	case Tok_LDIND_I1:
	case Tok_NOP:
	case Tok_OR:
	case Tok_CGT_UN:
	case Tok_ADD:
	case Tok_CONV_I4:
	case Tok_LDARG:
	case Tok_NEWOBJ:
	case Tok_DIV_UN:
	case Tok_LDELEM_I1:
	case Tok_REM_UN:
	case Tok_ISINST:
	case Tok_LDC_I4_3:
	case Tok_SUB:
	case Tok_LDIND_R8:
	case Tok_LDLOC_S:
	case Tok_CONV_U2:
	case Tok_XOR:
	case Tok_LDC_I4_M1:
	case Tok_IIF:
	case Tok_LDLOCA:
	case Tok_LDPROC:
	case Tok_CASTPTR:
	case Tok_LDLOC_2:
	case Tok_LDELEM_I2:
	case Tok_CONV_I1:
	case Tok_LDELEMA:
	case Tok_LDLOC_1:
	case Tok_LDIND_I4:
	case Tok_LDLOC:
	case Tok_LDELEM_U8:
	case Tok_LDIND_U4:
	case Tok_CEQ:
	case Tok_LDARG_3:
	case Tok_LDIND_U2:
	case Tok_LDLOCA_S:
	case Tok_CONV_R8:
	case Tok_LDC_I4_5:
	case Tok_LDVARA:
	case Tok_LDC_I4_S:
	case Tok_LDIND_U1:
	case Tok_LDC_I4_2:
	case Tok_LDELEM_R8:
	case Tok_LDELEM_U2:
	case Tok_CALL:
	case Tok_LDOBJ:
	case Tok_LDIND:
	case Tok_LDIND_U8:
	case Tok_LDLOC_3:
	case Tok_LDARG_2:
	case Tok_LDC_I4_6:
	case Tok_CONV_I8:
	case Tok_CLT_UN:
	case Tok_LDMETH:
	case Tok_LDIND_IPP:
	case Tok_LDARG_1:
	case Tok_NOT:
	case Tok_LDIND_I2:
	case Tok_LDC_R8:
	case Tok_SHR_UN:
	case Tok_CONV_IP:
		return true;
	default: return false;
	}
}

static inline bool FIRST_CondOp(int tt) {
	return tt == Tok_IIF;
}

static inline bool FIRST_StatementSequence(int tt) {
	switch(tt){
	case Tok_ISINST:
	case Tok_STIND_I4:
	case Tok_STELEM_I2:
	case Tok_LDARG:
	case Tok_SIZEOF:
	case Tok_CGT:
	case Tok_REM:
	case Tok_CONV_U1:
	case Tok_NOP:
	case Tok_LDELEM_U8:
	case Tok_LDARG_3:
	case Tok_LDIND_U1:
	case Tok_RET:
	case Tok_STLOC_1:
	case Tok_CONV_U4:
	case Tok_IIF:
	case Tok_LDIND_IPP:
	case Tok_STARG:
	case Tok_LDC_R8:
	case Tok_LDIND_R4:
	case Tok_STLOC:
	case Tok_MUL:
	case Tok_LDARGA:
	case Tok_XOR:
	case Tok_PTROFF:
	case Tok_STELEM_R4:
	case Tok_LDARG_2:
	case Tok_GOTO:
	case Tok_CALLVI:
	case Tok_LDELEMA:
	case Tok_DUP:
	case Tok_NEWVLA:
	case Tok_LDIND_I4:
	case Tok_LDFLDA:
	case Tok_LINE:
	case Tok_LABEL:
	case Tok_STIND_R4:
	case Tok_LDC_I4_8:
	case Tok_STELEM_I4:
	case Tok_LDELEM_I2:
	case Tok_LDELEM_U2:
	case Tok_CONV_R8:
	case Tok_DIV_UN:
	case Tok_STLOC_3:
	case Tok_LDIND_IP:
	case Tok_CONV_I4:
	case Tok_LDC_I4_5:
	case Tok_STFLD:
	case Tok_LDC_I4_0:
	case Tok_LDC_R4:
	case Tok_POP:
	case Tok_LDELEM_U1:
	case Tok_WHILE:
	case Tok_LDARG_0:
	case Tok_FREE:
	case Tok_STIND_IP:
	case Tok_REPEAT:
	case Tok_STIND_I8:
	case Tok_LDIND_U2:
	case Tok_IFGOTO:
	case Tok_CONV_I1:
	case Tok_LDIND:
	case Tok_LDLOC:
	case Tok_LDMETH:
	case Tok_LDARG_1:
	case Tok_LDIND_R8:
	case Tok_LDELEM_IP:
	case Tok_STIND_I1:
	case Tok_IF:
	case Tok_CONV_I8:
	case Tok_EXIT:
	case Tok_INITOBJ:
	case Tok_LDELEM_U4:
	case Tok_CEQ:
	case Tok_LDC_I4_6:
	case Tok_DIV:
	case Tok_LDC_I4_S:
	case Tok_STIND_I2:
	case Tok_LDELEM:
	case Tok_LDLOCA:
	case Tok_CONV_U8:
	case Tok_LDELEM_I8:
	case Tok_STELEM:
	case Tok_LDIND_I2:
	case Tok_NEWARR:
	case Tok_LDVAR:
	case Tok_CALL:
	case Tok_STIND_R8:
	case Tok_REM_UN:
	case Tok_CONV_I2:
	case Tok_LDARG_S:
	case Tok_STELEM_IP:
	case Tok_LDELEM_I1:
	case Tok_LDC_I4_M1:
	case Tok_STLOC_0:
	case Tok_CLT:
	case Tok_CONV_R4:
	case Tok_SUB:
	case Tok_LDLOCA_S:
	case Tok_LDLOC_0:
	case Tok_LDC_I4_4:
	case Tok_LDELEM_R8:
	case Tok_STVAR:
	case Tok_LDC_I4_1:
	case Tok_OR:
	case Tok_LDOBJ:
	case Tok_LDFLD:
	case Tok_LDIND_I8:
	case Tok_LDC_I4_7:
	case Tok_CGT_UN:
	case Tok_LDC_I8:
	case Tok_CALLVIRT:
	case Tok_LDIND_I1:
	case Tok_STIND:
	case Tok_LDNULL:
	case Tok_LDPROC:
	case Tok_LDC_I4:
	case Tok_CONV_IP:
	case Tok_STLOC_S:
	case Tok_CONV_U2:
	case Tok_LDLOC_2:
	case Tok_AND:
	case Tok_LDARGA_S:
	case Tok_LDELEM_R4:
	case Tok_LDSTR:
	case Tok_NEWOBJ:
	case Tok_STLOC_2:
	case Tok_SHL:
	case Tok_LDLOC_1:
	case Tok_STIND_IPP:
	case Tok_STELEM_I1:
	case Tok_LDC_I4_3:
	case Tok_SHR_UN:
	case Tok_STELEM_R8:
	case Tok_LDLOC_S:
	case Tok_CASTPTR:
	case Tok_LDVARA:
	case Tok_LOOP:
	case Tok_SWITCH:
	case Tok_NOT:
	case Tok_CALLI:
	case Tok_NEG:
	case Tok_LDIND_U8:
	case Tok_LDELEM_I4:
	case Tok_LDIND_U4:
	case Tok_STELEM_I8:
	case Tok_LDLOC_3:
	case Tok_STARG_S:
	case Tok_LDC_I4_2:
	case Tok_SHR:
	case Tok_ADD:
	case Tok_CLT_UN:
		return true;
	default: return false;
	}
}

static inline bool FIRST_Statement(int tt) {
	switch(tt){
	case Tok_STELEM_R8:
	case Tok_IF:
	case Tok_STLOC_2:
	case Tok_STLOC_3:
	case Tok_GOTO:
	case Tok_LOOP:
	case Tok_STLOC_0:
	case Tok_STIND_R8:
	case Tok_STELEM_I1:
	case Tok_LINE:
	case Tok_EXIT:
	case Tok_STIND_I1:
	case Tok_STELEM_I4:
	case Tok_STARG:
	case Tok_STIND_I8:
	case Tok_STIND_I4:
	case Tok_STARG_S:
	case Tok_SWITCH:
	case Tok_IFGOTO:
	case Tok_STLOC_1:
	case Tok_STELEM_I8:
	case Tok_STLOC:
	case Tok_STFLD:
	case Tok_STIND_IPP:
	case Tok_STELEM_R4:
	case Tok_STELEM:
	case Tok_STIND:
	case Tok_REPEAT:
	case Tok_FREE:
	case Tok_LABEL:
	case Tok_STELEM_I2:
	case Tok_STELEM_IP:
	case Tok_STVAR:
	case Tok_WHILE:
	case Tok_STLOC_S:
	case Tok_STIND_IP:
	case Tok_STIND_I2:
	case Tok_STIND_R4:
	case Tok_RET:
	case Tok_POP:
		return true;
	default: return false;
	}
}

static inline bool FIRST_IfThenElse(int tt) {
	return tt == Tok_IF;
}

static inline bool FIRST_Loop(int tt) {
	return tt == Tok_LOOP;
}

static inline bool FIRST_Switch(int tt) {
	return tt == Tok_SWITCH;
}

static inline bool FIRST_RepeatUntil(int tt) {
	return tt == Tok_REPEAT;
}

static inline bool FIRST_WhileDo(int tt) {
	return tt == Tok_WHILE;
}

static inline bool FIRST_MetaActuals(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_MetaParams(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_ConstExpression(int tt) {
	switch(tt){
	case Tok_string:
	case Tok_Minus:
	case Tok_ident:
	case Tok_hexstring:
	case Tok_unsigned:
	case Tok_float:
	case Tok_Plus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_ConstExpression2(int tt) {
	switch(tt){
	case Tok_string:
	case Tok_Minus:
	case Tok_ident:
	case Tok_hexstring:
	case Tok_unsigned:
	case Tok_float:
	case Tok_Plus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_constructor(int tt) {
	return tt == Tok_ident || tt == Tok_hexstring;
}

static inline bool FIRST_component_list(int tt) {
	return tt == Tok_Lbrace;
}

static inline bool FIRST_component(int tt) {
	switch(tt){
	case Tok_string:
	case Tok_Minus:
	case Tok_ident:
	case Tok_hexstring:
	case Tok_unsigned:
	case Tok_Lbrace:
	case Tok_float:
	case Tok_Plus:
		return true;
	default: return false;
	}
}

void Parser2::RunParser() {
	errors.clear();
	next();
	Mil();
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
}

bool Parser2::expect(int tt, bool pkw, const char* where) {
	if( la.d_type == tt || la.d_code == tt) { next(); return true; }
	else { errors << Error(QString("'%1' expected in %2").arg(tokenTypeString(tt)).arg(where),la.d_lineNr, la.d_colNr, la.d_sourcePath); return false; }
}

static inline void dummy() {}

void Parser2::Mil() {
	while( FIRST_module(la.d_type) || FIRST_module(la.d_code) ) {
		module();
	}
}

void Parser2::integer() {
	if( la.d_type == Tok_Plus || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Plus ) {
			expect(Tok_Plus, false, "integer");
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "integer");
		} else
			invalid("integer");
	}
	expect(Tok_unsigned, false, "integer");
}

void Parser2::number() {
	if( la.d_type == Tok_Plus || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Plus ) {
			expect(Tok_Plus, false, "number");
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "number");
		} else
			invalid("number");
	}
	if( la.d_type == Tok_float ) {
		expect(Tok_float, false, "number");
	} else if( la.d_type == Tok_unsigned ) {
		expect(Tok_unsigned, false, "number");
	} else
		invalid("number");
}

void Parser2::qualident() {
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Bang )  ) {
		expect(Tok_ident, false, "qualident");
		expect(Tok_Bang, false, "qualident");
	}
	expect(Tok_ident, false, "qualident");
}

void Parser2::trident() {
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Bang )  ) {
		expect(Tok_ident, false, "trident");
		expect(Tok_Bang, false, "trident");
	}
	expect(Tok_ident, false, "trident");
	expect(Tok_Dot, false, "trident");
	expect(Tok_ident, false, "trident");
}

void Parser2::identdef() {
	expect(Tok_ident, false, "identdef");
	if( la.d_type == Tok_Star ) {
		expect(Tok_Star, false, "identdef");
	}
}

void Parser2::ConstDeclaration() {
	expect(Tok_ident, false, "ConstDeclaration");
	if( la.d_type == Tok_Eq || la.d_type == Tok_Colon ) {
		if( la.d_type == Tok_Eq ) {
			expect(Tok_Eq, false, "ConstDeclaration");
			ConstExpression2();
		} else if( la.d_type == Tok_Colon ) {
			expect(Tok_Colon, false, "ConstDeclaration");
			qualident();
		} else
			invalid("ConstDeclaration");
	}
}

void Parser2::TypeDeclaration() {
	identdef();
	if( la.d_type == Tok_Eq ) {
		expect(Tok_Eq, false, "TypeDeclaration");
		type();
	}
}

void Parser2::type() {
	if( FIRST_ArrayType(la.d_type) || FIRST_ArrayType(la.d_code) ) {
		ArrayType();
	} else if( FIRST_StructUnionType(la.d_type) || FIRST_StructUnionType(la.d_code) ) {
		StructUnionType();
	} else if( FIRST_ObjectType(la.d_type) || FIRST_ObjectType(la.d_code) ) {
		ObjectType();
	} else if( FIRST_PointerType(la.d_type) || FIRST_PointerType(la.d_code) ) {
		PointerType();
	} else if( FIRST_ProcedureType(la.d_type) ) {
		ProcedureType();
	} else if( FIRST_NamedType(la.d_type) ) {
		NamedType();
	} else
		invalid("type");
}

void Parser2::NamedType() {
	qualident();
}

void Parser2::ArrayType() {
	if( la.d_code == Tok_ARRAY ) {
		expect(Tok_ARRAY, true, "ArrayType");
		if( FIRST_length(la.d_type) ) {
			length();
		}
		expect(Tok_OF, true, "ArrayType");
		NamedType();
	} else if( la.d_type == Tok_Lbrack ) {
		expect(Tok_Lbrack, false, "ArrayType");
		if( FIRST_length(la.d_type) ) {
			length();
		}
		expect(Tok_Rbrack, false, "ArrayType");
		NamedType();
	} else
		invalid("ArrayType");
}

void Parser2::length() {
	expect(Tok_unsigned, false, "length");
}

void Parser2::StructUnionType() {
	if( la.d_code == Tok_STRUCT ) {
		expect(Tok_STRUCT, true, "StructUnionType");
	} else if( la.d_code == Tok_UNION ) {
		expect(Tok_UNION, true, "StructUnionType");
	} else
		invalid("StructUnionType");
	while( ( !( peek(1).d_code == Tok_END ) )  ) {
		FieldList();
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "StructUnionType");
		}
	}
	expect(Tok_END, true, "StructUnionType");
}

void Parser2::FieldList() {
	if( FIRST_IdentList(la.d_type) ) {
		IdentList();
		expect(Tok_Colon, false, "FieldList");
		NamedType();
		if( la.d_type == Tok_Colon ) {
			expect(Tok_Colon, false, "FieldList");
			integer();
		}
	} else if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "FieldList");
		integer();
	} else
		invalid("FieldList");
}

void Parser2::IdentList() {
	identdef();
	while( la.d_type == Tok_Comma || FIRST_identdef(la.d_type) ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "IdentList");
		}
		identdef();
	}
}

void Parser2::ObjectType() {
	expect(Tok_OBJECT, true, "ObjectType");
	while( FIRST_MemberList(la.d_type) ) {
		MemberList();
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "ObjectType");
		}
	}
	expect(Tok_END, true, "ObjectType");
}

void Parser2::MemberList() {
	IdentList();
	expect(Tok_Colon, false, "MemberList");
	NamedType();
}

void Parser2::PointerType() {
	if( la.d_code == Tok_POINTER ) {
		expect(Tok_POINTER, true, "PointerType");
		expect(Tok_TO, true, "PointerType");
	} else if( la.d_type == Tok_Hat ) {
		expect(Tok_Hat, false, "PointerType");
	} else
		invalid("PointerType");
	NamedType();
}

void Parser2::ProcedureType() {
	if( la.d_type == Tok_PROCEDURE ) {
		expect(Tok_PROCEDURE, false, "ProcedureType");
	} else if( la.d_type == Tok_PROC ) {
		expect(Tok_PROC, false, "ProcedureType");
	} else
		invalid("ProcedureType");
	if( FIRST_FormalParameters(la.d_type) ) {
		FormalParameters();
	}
}

void Parser2::VariableDeclaration() {
	IdentList();
	expect(Tok_Colon, false, "VariableDeclaration");
	NamedType();
}

void Parser2::ProcedureDeclaration() {
	if( la.d_type == Tok_PROCEDURE ) {
		expect(Tok_PROCEDURE, false, "ProcedureDeclaration");
	} else if( la.d_type == Tok_PROC ) {
		expect(Tok_PROC, false, "ProcedureDeclaration");
	} else
		invalid("ProcedureDeclaration");
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Dot )  ) {
		Binding();
		identdef();
		if( FIRST_FormalParameters(la.d_type) ) {
			FormalParameters();
		}
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "ProcedureDeclaration");
		}
		ProcedureBody();
	} else if( FIRST_identdef(la.d_type) ) {
		identdef();
		if( FIRST_FormalParameters(la.d_type) ) {
			FormalParameters();
		}
		if( la.d_code == Tok_INLINE || la.d_code == Tok_INVAR || la.d_code == Tok_INIT || la.d_type == Tok_Semi || FIRST_ProcedureBody(la.d_type) ) {
			if( la.d_code == Tok_INLINE || la.d_code == Tok_INVAR || la.d_code == Tok_INIT ) {
				if( la.d_code == Tok_INLINE ) {
					expect(Tok_INLINE, true, "ProcedureDeclaration");
				} else if( la.d_code == Tok_INVAR ) {
					expect(Tok_INVAR, true, "ProcedureDeclaration");
				} else if( la.d_code == Tok_INIT ) {
					expect(Tok_INIT, true, "ProcedureDeclaration");
				} else
					invalid("ProcedureDeclaration");
			}
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "ProcedureDeclaration");
			}
			ProcedureBody();
		} else if( ( ( peek(1).d_code == Tok_EXTERN || peek(1).d_type == Tok_Semi ) && ( peek(2).d_code == Tok_EXTERN || peek(2).d_type == Tok_ident ) )  ) {
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "ProcedureDeclaration");
			}
			expect(Tok_EXTERN, true, "ProcedureDeclaration");
			if( la.d_type == Tok_ident ) {
				expect(Tok_ident, false, "ProcedureDeclaration");
			}
		} else
			invalid("ProcedureDeclaration");
	} else
		invalid("ProcedureDeclaration");
}

void Parser2::Binding() {
	expect(Tok_ident, false, "Binding");
	expect(Tok_Dot, false, "Binding");
}

void Parser2::ProcedureBody() {
	if( la.d_type == Tok_VAR ) {
		expect(Tok_VAR, false, "ProcedureBody");
		while( FIRST_LocalDeclaration(la.d_type) ) {
			LocalDeclaration();
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "ProcedureBody");
			}
		}
	}
	expect(Tok_BEGIN, false, "ProcedureBody");
	StatementSequence();
	expect(Tok_END, false, "ProcedureBody");
	expect(Tok_ident, false, "ProcedureBody");
}

void Parser2::LocalDeclaration() {
	IdentList();
	expect(Tok_Colon, false, "LocalDeclaration");
	NamedType();
}

void Parser2::FormalParameters() {
	expect(Tok_Lpar, false, "FormalParameters");
	if( FIRST_FPSection(la.d_type) ) {
		FPSection();
		while( ( peek(1).d_type == Tok_Semi && peek(2).d_type == Tok_ident )  ) {
			expect(Tok_Semi, false, "FormalParameters");
			FPSection();
		}
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "FormalParameters");
			expect(Tok_2Dot, false, "FormalParameters");
		}
	}
	expect(Tok_Rpar, false, "FormalParameters");
	if( la.d_type == Tok_Colon ) {
		expect(Tok_Colon, false, "FormalParameters");
		ReturnType();
	}
}

void Parser2::ReturnType() {
	NamedType();
}

void Parser2::FPSection() {
	expect(Tok_ident, false, "FPSection");
	while( la.d_type == Tok_Comma || la.d_type == Tok_ident ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "FPSection");
		}
		expect(Tok_ident, false, "FPSection");
	}
	expect(Tok_Colon, false, "FPSection");
	NamedType();
}

void Parser2::module() {
	expect(Tok_MODULE, true, "module");
	expect(Tok_ident, false, "module");
	if( FIRST_MetaParams(la.d_type) ) {
		MetaParams();
	}
	if( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "module");
	}
	while( FIRST_ImportList(la.d_type) || FIRST_DeclarationSequence(la.d_type) ) {
		if( FIRST_ImportList(la.d_type) ) {
			ImportList();
		} else if( FIRST_DeclarationSequence(la.d_type) || la.d_type == Tok_IMPORT || la.d_type == Tok_END || la.d_type == Tok_PROC || la.d_type == Tok_CONST || la.d_type == Tok_VAR || la.d_type == Tok_PROCEDURE || la.d_type == Tok_TYPE ) {
			DeclarationSequence();
		} else
			invalid("module");
	}
	expect(Tok_END, false, "module");
	expect(Tok_ident, false, "module");
	if( la.d_type == Tok_Dot ) {
		expect(Tok_Dot, false, "module");
	}
}

void Parser2::ImportList() {
	expect(Tok_IMPORT, false, "ImportList");
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
	while( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Slash )  ) {
		expect(Tok_ident, false, "import");
		expect(Tok_Slash, false, "import");
	}
	expect(Tok_ident, false, "import");
	if( FIRST_MetaActuals(la.d_type) ) {
		MetaActuals();
	}
}

void Parser2::DeclarationSequence() {
	while( la.d_type == Tok_CONST || la.d_type == Tok_TYPE || la.d_type == Tok_VAR || FIRST_ProcedureDeclaration(la.d_type) ) {
		if( la.d_type == Tok_CONST ) {
			expect(Tok_CONST, false, "DeclarationSequence");
			while( FIRST_ConstDeclaration(la.d_type) ) {
				ConstDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
		} else if( la.d_type == Tok_TYPE ) {
			expect(Tok_TYPE, false, "DeclarationSequence");
			while( FIRST_TypeDeclaration(la.d_type) ) {
				TypeDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
		} else if( la.d_type == Tok_VAR ) {
			expect(Tok_VAR, false, "DeclarationSequence");
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

void Parser2::Expression() {
	while( FIRST_ExpInstr(la.d_type) || FIRST_ExpInstr(la.d_code) ) {
		ExpInstr();
	}
}

void Parser2::ExpInstr() {
	if( la.d_code == Tok_ADD ) {
		expect(Tok_ADD, true, "ExpInstr");
	} else if( la.d_code == Tok_AND ) {
		expect(Tok_AND, true, "ExpInstr");
	} else if( la.d_code == Tok_CALL || la.d_code == Tok_CALLI || la.d_code == Tok_CALLVI ) {
		if( la.d_code == Tok_CALL ) {
			expect(Tok_CALL, true, "ExpInstr");
		} else if( la.d_code == Tok_CALLI ) {
			expect(Tok_CALLI, true, "ExpInstr");
		} else if( la.d_code == Tok_CALLVI ) {
			expect(Tok_CALLVI, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		qualident();
	} else if( la.d_code == Tok_CALLVIRT ) {
		expect(Tok_CALLVIRT, true, "ExpInstr");
		trident();
	} else if( la.d_code == Tok_CASTPTR ) {
		expect(Tok_CASTPTR, true, "ExpInstr");
		qualident();
	} else if( la.d_code == Tok_CEQ ) {
		expect(Tok_CEQ, true, "ExpInstr");
	} else if( la.d_code == Tok_CGT ) {
		expect(Tok_CGT, true, "ExpInstr");
	} else if( la.d_code == Tok_CGT_UN ) {
		expect(Tok_CGT_UN, true, "ExpInstr");
	} else if( la.d_code == Tok_CLT ) {
		expect(Tok_CLT, true, "ExpInstr");
	} else if( la.d_code == Tok_CLT_UN ) {
		expect(Tok_CLT_UN, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_I1 ) {
		expect(Tok_CONV_I1, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_I2 ) {
		expect(Tok_CONV_I2, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_I4 ) {
		expect(Tok_CONV_I4, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_I8 ) {
		expect(Tok_CONV_I8, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_R4 ) {
		expect(Tok_CONV_R4, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_R8 ) {
		expect(Tok_CONV_R8, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_U1 ) {
		expect(Tok_CONV_U1, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_U2 ) {
		expect(Tok_CONV_U2, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_U4 ) {
		expect(Tok_CONV_U4, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_U8 ) {
		expect(Tok_CONV_U8, true, "ExpInstr");
	} else if( la.d_code == Tok_CONV_IP ) {
		expect(Tok_CONV_IP, true, "ExpInstr");
	} else if( la.d_code == Tok_DIV ) {
		expect(Tok_DIV, true, "ExpInstr");
	} else if( la.d_code == Tok_DIV_UN ) {
		expect(Tok_DIV_UN, true, "ExpInstr");
	} else if( la.d_code == Tok_DUP ) {
		expect(Tok_DUP, true, "ExpInstr");
	} else if( la.d_code == Tok_INITOBJ ) {
		expect(Tok_INITOBJ, true, "ExpInstr");
		qualident();
	} else if( la.d_code == Tok_ISINST ) {
		expect(Tok_ISINST, true, "ExpInstr");
		qualident();
	} else if( la.d_code == Tok_LDARG || la.d_code == Tok_LDARG_S ) {
		if( la.d_code == Tok_LDARG ) {
			expect(Tok_LDARG, true, "ExpInstr");
		} else if( la.d_code == Tok_LDARG_S ) {
			expect(Tok_LDARG_S, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		if( la.d_type == Tok_unsigned ) {
			expect(Tok_unsigned, false, "ExpInstr");
		} else if( la.d_type == Tok_ident ) {
			expect(Tok_ident, false, "ExpInstr");
		} else
			invalid("ExpInstr");
	} else if( la.d_code == Tok_LDARG_0 ) {
		expect(Tok_LDARG_0, true, "ExpInstr");
	} else if( la.d_code == Tok_LDARG_1 ) {
		expect(Tok_LDARG_1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDARG_2 ) {
		expect(Tok_LDARG_2, true, "ExpInstr");
	} else if( la.d_code == Tok_LDARG_3 ) {
		expect(Tok_LDARG_3, true, "ExpInstr");
	} else if( la.d_code == Tok_LDARGA || la.d_code == Tok_LDARGA_S ) {
		if( la.d_code == Tok_LDARGA ) {
			expect(Tok_LDARGA, true, "ExpInstr");
		} else if( la.d_code == Tok_LDARGA_S ) {
			expect(Tok_LDARGA_S, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		if( la.d_type == Tok_unsigned ) {
			expect(Tok_unsigned, false, "ExpInstr");
		} else if( la.d_type == Tok_ident ) {
			expect(Tok_ident, false, "ExpInstr");
		} else
			invalid("ExpInstr");
	} else if( la.d_code == Tok_LDC_I4 || la.d_code == Tok_LDC_I8 || la.d_code == Tok_LDC_I4_S ) {
		if( la.d_code == Tok_LDC_I4 ) {
			expect(Tok_LDC_I4, true, "ExpInstr");
		} else if( la.d_code == Tok_LDC_I8 ) {
			expect(Tok_LDC_I8, true, "ExpInstr");
		} else if( la.d_code == Tok_LDC_I4_S ) {
			expect(Tok_LDC_I4_S, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		integer();
	} else if( la.d_code == Tok_LDC_R4 || la.d_code == Tok_LDC_R8 ) {
		if( la.d_code == Tok_LDC_R4 ) {
			expect(Tok_LDC_R4, true, "ExpInstr");
		} else if( la.d_code == Tok_LDC_R8 ) {
			expect(Tok_LDC_R8, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		number();
	} else if( la.d_code == Tok_LDC_I4_0 ) {
		expect(Tok_LDC_I4_0, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_1 ) {
		expect(Tok_LDC_I4_1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_2 ) {
		expect(Tok_LDC_I4_2, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_3 ) {
		expect(Tok_LDC_I4_3, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_4 ) {
		expect(Tok_LDC_I4_4, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_5 ) {
		expect(Tok_LDC_I4_5, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_6 ) {
		expect(Tok_LDC_I4_6, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_7 ) {
		expect(Tok_LDC_I4_7, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_8 ) {
		expect(Tok_LDC_I4_8, true, "ExpInstr");
	} else if( la.d_code == Tok_LDC_I4_M1 ) {
		expect(Tok_LDC_I4_M1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDOBJ ) {
		expect(Tok_LDOBJ, true, "ExpInstr");
		constructor();
	} else if( la.d_code == Tok_LDELEM || la.d_code == Tok_LDELEMA ) {
		if( la.d_code == Tok_LDELEM ) {
			expect(Tok_LDELEM, true, "ExpInstr");
		} else if( la.d_code == Tok_LDELEMA ) {
			expect(Tok_LDELEMA, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		qualident();
	} else if( la.d_code == Tok_LDELEM_I1 ) {
		expect(Tok_LDELEM_I1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_I2 ) {
		expect(Tok_LDELEM_I2, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_I4 ) {
		expect(Tok_LDELEM_I4, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_I8 ) {
		expect(Tok_LDELEM_I8, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_U1 ) {
		expect(Tok_LDELEM_U1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_U2 ) {
		expect(Tok_LDELEM_U2, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_U4 ) {
		expect(Tok_LDELEM_U4, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_U8 ) {
		expect(Tok_LDELEM_U8, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_R4 ) {
		expect(Tok_LDELEM_R4, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_R8 ) {
		expect(Tok_LDELEM_R8, true, "ExpInstr");
	} else if( la.d_code == Tok_LDELEM_IP ) {
		expect(Tok_LDELEM_IP, true, "ExpInstr");
	} else if( la.d_code == Tok_LDFLD || la.d_code == Tok_LDFLDA ) {
		if( la.d_code == Tok_LDFLD ) {
			expect(Tok_LDFLD, true, "ExpInstr");
		} else if( la.d_code == Tok_LDFLDA ) {
			expect(Tok_LDFLDA, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		trident();
	} else if( la.d_code == Tok_LDIND_I1 ) {
		expect(Tok_LDIND_I1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_I2 ) {
		expect(Tok_LDIND_I2, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_I4 ) {
		expect(Tok_LDIND_I4, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_I8 ) {
		expect(Tok_LDIND_I8, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_U1 ) {
		expect(Tok_LDIND_U1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_U2 ) {
		expect(Tok_LDIND_U2, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_U4 ) {
		expect(Tok_LDIND_U4, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_R4 ) {
		expect(Tok_LDIND_R4, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_U8 ) {
		expect(Tok_LDIND_U8, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_R8 ) {
		expect(Tok_LDIND_R8, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_IP ) {
		expect(Tok_LDIND_IP, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND_IPP ) {
		expect(Tok_LDIND_IPP, true, "ExpInstr");
	} else if( la.d_code == Tok_LDLOC || la.d_code == Tok_LDLOC_S || la.d_code == Tok_LDLOCA || la.d_code == Tok_LDLOCA_S ) {
		if( la.d_code == Tok_LDLOC ) {
			expect(Tok_LDLOC, true, "ExpInstr");
		} else if( la.d_code == Tok_LDLOC_S ) {
			expect(Tok_LDLOC_S, true, "ExpInstr");
		} else if( la.d_code == Tok_LDLOCA ) {
			expect(Tok_LDLOCA, true, "ExpInstr");
		} else if( la.d_code == Tok_LDLOCA_S ) {
			expect(Tok_LDLOCA_S, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		if( la.d_type == Tok_unsigned ) {
			expect(Tok_unsigned, false, "ExpInstr");
		} else if( la.d_type == Tok_ident ) {
			expect(Tok_ident, false, "ExpInstr");
		} else
			invalid("ExpInstr");
	} else if( la.d_code == Tok_LDLOC_0 ) {
		expect(Tok_LDLOC_0, true, "ExpInstr");
	} else if( la.d_code == Tok_LDLOC_1 ) {
		expect(Tok_LDLOC_1, true, "ExpInstr");
	} else if( la.d_code == Tok_LDLOC_2 ) {
		expect(Tok_LDLOC_2, true, "ExpInstr");
	} else if( la.d_code == Tok_LDLOC_3 ) {
		expect(Tok_LDLOC_3, true, "ExpInstr");
	} else if( la.d_code == Tok_LDNULL ) {
		expect(Tok_LDNULL, true, "ExpInstr");
	} else if( la.d_code == Tok_LDIND ) {
		expect(Tok_LDIND, true, "ExpInstr");
		if( FIRST_qualident(la.d_type) ) {
			qualident();
		}
	} else if( la.d_code == Tok_LDPROC ) {
		expect(Tok_LDPROC, true, "ExpInstr");
		qualident();
	} else if( la.d_code == Tok_LDMETH ) {
		expect(Tok_LDMETH, true, "ExpInstr");
		trident();
	} else if( la.d_code == Tok_LDSTR ) {
		expect(Tok_LDSTR, true, "ExpInstr");
		if( la.d_type == Tok_string ) {
			expect(Tok_string, false, "ExpInstr");
		} else if( la.d_type == Tok_hexstring ) {
			expect(Tok_hexstring, false, "ExpInstr");
		} else
			invalid("ExpInstr");
	} else if( la.d_code == Tok_LDVAR || la.d_code == Tok_LDVARA ) {
		if( la.d_code == Tok_LDVAR ) {
			expect(Tok_LDVAR, true, "ExpInstr");
		} else if( la.d_code == Tok_LDVARA ) {
			expect(Tok_LDVARA, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		qualident();
	} else if( la.d_code == Tok_MUL ) {
		expect(Tok_MUL, true, "ExpInstr");
	} else if( la.d_code == Tok_NEG ) {
		expect(Tok_NEG, true, "ExpInstr");
	} else if( la.d_code == Tok_NEWARR || la.d_code == Tok_NEWVLA || la.d_code == Tok_NEWOBJ ) {
		if( la.d_code == Tok_NEWARR ) {
			expect(Tok_NEWARR, true, "ExpInstr");
		} else if( la.d_code == Tok_NEWVLA ) {
			expect(Tok_NEWVLA, true, "ExpInstr");
		} else if( la.d_code == Tok_NEWOBJ ) {
			expect(Tok_NEWOBJ, true, "ExpInstr");
		} else
			invalid("ExpInstr");
		qualident();
	} else if( la.d_code == Tok_NOT ) {
		expect(Tok_NOT, true, "ExpInstr");
	} else if( la.d_code == Tok_OR ) {
		expect(Tok_OR, true, "ExpInstr");
	} else if( la.d_code == Tok_REM ) {
		expect(Tok_REM, true, "ExpInstr");
	} else if( la.d_code == Tok_REM_UN ) {
		expect(Tok_REM_UN, true, "ExpInstr");
	} else if( la.d_code == Tok_SHL ) {
		expect(Tok_SHL, true, "ExpInstr");
	} else if( la.d_code == Tok_SHR ) {
		expect(Tok_SHR, true, "ExpInstr");
	} else if( la.d_code == Tok_SHR_UN ) {
		expect(Tok_SHR_UN, true, "ExpInstr");
	} else if( la.d_code == Tok_SIZEOF ) {
		expect(Tok_SIZEOF, true, "ExpInstr");
		qualident();
	} else if( la.d_code == Tok_SUB ) {
		expect(Tok_SUB, true, "ExpInstr");
	} else if( la.d_code == Tok_XOR ) {
		expect(Tok_XOR, true, "ExpInstr");
	} else if( la.d_code == Tok_PTROFF ) {
		expect(Tok_PTROFF, true, "ExpInstr");
		qualident();
	} else if( la.d_code == Tok_NOP ) {
		expect(Tok_NOP, true, "ExpInstr");
	} else if( FIRST_CondOp(la.d_type) || FIRST_CondOp(la.d_code) ) {
		CondOp();
	} else
		invalid("ExpInstr");
}

void Parser2::CondOp() {
	expect(Tok_IIF, true, "CondOp");
	Expression();
	expect(Tok_THEN, true, "CondOp");
	Expression();
	expect(Tok_ELSE, true, "CondOp");
	Expression();
	expect(Tok_END, true, "CondOp");
}

void Parser2::StatementSequence() {
	while( FIRST_Statement(la.d_type) || FIRST_Statement(la.d_code) || FIRST_ExpInstr(la.d_type) || FIRST_ExpInstr(la.d_code) ) {
		if( FIRST_Statement(la.d_type) || FIRST_Statement(la.d_code) ) {
			Statement();
		} else if( FIRST_ExpInstr(la.d_type) || FIRST_ExpInstr(la.d_code) ) {
			ExpInstr();
		} else
			invalid("StatementSequence");
	}
}

void Parser2::Statement() {
	if( la.d_code == Tok_FREE ) {
		expect(Tok_FREE, true, "Statement");
	} else if( FIRST_RepeatUntil(la.d_type) || FIRST_RepeatUntil(la.d_code) ) {
		RepeatUntil();
	} else if( la.d_code == Tok_EXIT ) {
		expect(Tok_EXIT, true, "Statement");
	} else if( la.d_code == Tok_GOTO ) {
		expect(Tok_GOTO, true, "Statement");
		expect(Tok_ident, false, "Statement");
	} else if( la.d_code == Tok_IFGOTO ) {
		expect(Tok_IFGOTO, true, "Statement");
		expect(Tok_ident, false, "Statement");
	} else if( FIRST_IfThenElse(la.d_type) || FIRST_IfThenElse(la.d_code) ) {
		IfThenElse();
	} else if( la.d_code == Tok_LABEL ) {
		expect(Tok_LABEL, true, "Statement");
		expect(Tok_ident, false, "Statement");
	} else if( la.d_code == Tok_LINE ) {
		expect(Tok_LINE, true, "Statement");
		expect(Tok_unsigned, false, "Statement");
	} else if( FIRST_Loop(la.d_type) || FIRST_Loop(la.d_code) ) {
		Loop();
	} else if( la.d_code == Tok_POP ) {
		expect(Tok_POP, true, "Statement");
	} else if( la.d_code == Tok_RET ) {
		expect(Tok_RET, true, "Statement");
	} else if( la.d_code == Tok_STARG || la.d_code == Tok_STARG_S ) {
		if( la.d_code == Tok_STARG ) {
			expect(Tok_STARG, true, "Statement");
		} else if( la.d_code == Tok_STARG_S ) {
			expect(Tok_STARG_S, true, "Statement");
		} else
			invalid("Statement");
		if( la.d_type == Tok_unsigned ) {
			expect(Tok_unsigned, false, "Statement");
		} else if( la.d_type == Tok_ident ) {
			expect(Tok_ident, false, "Statement");
		} else
			invalid("Statement");
	} else if( la.d_code == Tok_STELEM ) {
		expect(Tok_STELEM, true, "Statement");
		qualident();
	} else if( la.d_code == Tok_STELEM_I1 ) {
		expect(Tok_STELEM_I1, true, "Statement");
	} else if( la.d_code == Tok_STELEM_I2 ) {
		expect(Tok_STELEM_I2, true, "Statement");
	} else if( la.d_code == Tok_STELEM_I4 ) {
		expect(Tok_STELEM_I4, true, "Statement");
	} else if( la.d_code == Tok_STELEM_I8 ) {
		expect(Tok_STELEM_I8, true, "Statement");
	} else if( la.d_code == Tok_STELEM_R4 ) {
		expect(Tok_STELEM_R4, true, "Statement");
	} else if( la.d_code == Tok_STELEM_R8 ) {
		expect(Tok_STELEM_R8, true, "Statement");
	} else if( la.d_code == Tok_STELEM_IP ) {
		expect(Tok_STELEM_IP, true, "Statement");
	} else if( la.d_code == Tok_STFLD ) {
		expect(Tok_STFLD, true, "Statement");
		trident();
	} else if( la.d_code == Tok_STIND_I1 ) {
		expect(Tok_STIND_I1, true, "Statement");
	} else if( la.d_code == Tok_STIND_I2 ) {
		expect(Tok_STIND_I2, true, "Statement");
	} else if( la.d_code == Tok_STIND_I4 ) {
		expect(Tok_STIND_I4, true, "Statement");
	} else if( la.d_code == Tok_STIND_I8 ) {
		expect(Tok_STIND_I8, true, "Statement");
	} else if( la.d_code == Tok_STIND_R4 ) {
		expect(Tok_STIND_R4, true, "Statement");
	} else if( la.d_code == Tok_STIND_R8 ) {
		expect(Tok_STIND_R8, true, "Statement");
	} else if( la.d_code == Tok_STIND_IP ) {
		expect(Tok_STIND_IP, true, "Statement");
	} else if( la.d_code == Tok_STIND_IPP ) {
		expect(Tok_STIND_IPP, true, "Statement");
	} else if( la.d_code == Tok_STLOC || la.d_code == Tok_STLOC_S ) {
		if( la.d_code == Tok_STLOC ) {
			expect(Tok_STLOC, true, "Statement");
		} else if( la.d_code == Tok_STLOC_S ) {
			expect(Tok_STLOC_S, true, "Statement");
		} else
			invalid("Statement");
		if( la.d_type == Tok_unsigned ) {
			expect(Tok_unsigned, false, "Statement");
		} else if( la.d_type == Tok_ident ) {
			expect(Tok_ident, false, "Statement");
		} else
			invalid("Statement");
	} else if( la.d_code == Tok_STLOC_0 ) {
		expect(Tok_STLOC_0, true, "Statement");
	} else if( la.d_code == Tok_STLOC_1 ) {
		expect(Tok_STLOC_1, true, "Statement");
	} else if( la.d_code == Tok_STLOC_2 ) {
		expect(Tok_STLOC_2, true, "Statement");
	} else if( la.d_code == Tok_STLOC_3 ) {
		expect(Tok_STLOC_3, true, "Statement");
	} else if( la.d_code == Tok_STIND ) {
		expect(Tok_STIND, true, "Statement");
		qualident();
	} else if( la.d_code == Tok_STVAR ) {
		expect(Tok_STVAR, true, "Statement");
		qualident();
	} else if( FIRST_Switch(la.d_type) || FIRST_Switch(la.d_code) ) {
		Switch();
	} else if( FIRST_WhileDo(la.d_type) || FIRST_WhileDo(la.d_code) ) {
		WhileDo();
	} else
		invalid("Statement");
}

void Parser2::IfThenElse() {
	expect(Tok_IF, true, "IfThenElse");
	Expression();
	expect(Tok_THEN, true, "IfThenElse");
	StatementSequence();
	if( la.d_code == Tok_ELSE ) {
		expect(Tok_ELSE, true, "IfThenElse");
		StatementSequence();
	}
	expect(Tok_END, false, "IfThenElse");
}

void Parser2::Loop() {
	expect(Tok_LOOP, true, "Loop");
	StatementSequence();
	expect(Tok_END, false, "Loop");
}

void Parser2::Switch() {
	expect(Tok_SWITCH, true, "Switch");
	Expression();
	while( la.d_code == Tok_CASE ) {
		expect(Tok_CASE, true, "Switch");
		integer();
		while( la.d_type == Tok_Comma || FIRST_integer(la.d_type) ) {
			if( la.d_type == Tok_Comma ) {
				expect(Tok_Comma, false, "Switch");
			}
			integer();
		}
		expect(Tok_THEN, true, "Switch");
		StatementSequence();
	}
	if( la.d_code == Tok_ELSE ) {
		expect(Tok_ELSE, true, "Switch");
		StatementSequence();
	}
	expect(Tok_END, false, "Switch");
}

void Parser2::RepeatUntil() {
	expect(Tok_REPEAT, true, "RepeatUntil");
	StatementSequence();
	expect(Tok_UNTIL, true, "RepeatUntil");
	Expression();
	expect(Tok_END, false, "RepeatUntil");
}

void Parser2::WhileDo() {
	expect(Tok_WHILE, true, "WhileDo");
	Expression();
	expect(Tok_DO, true, "WhileDo");
	StatementSequence();
	expect(Tok_END, false, "WhileDo");
}

void Parser2::MetaActuals() {
	expect(Tok_Lpar, false, "MetaActuals");
	ConstExpression();
	while( la.d_type == Tok_Comma || FIRST_ConstExpression(la.d_type) ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "MetaActuals");
		}
		ConstExpression();
	}
	expect(Tok_Rpar, false, "MetaActuals");
}

void Parser2::MetaParams() {
	expect(Tok_Lpar, false, "MetaParams");
	expect(Tok_ident, false, "MetaParams");
	while( ( ( peek(1).d_type == Tok_Comma || peek(1).d_type == Tok_ident ) && peek(2).d_type == Tok_ident )  ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "MetaParams");
		}
		expect(Tok_ident, false, "MetaParams");
	}
	expect(Tok_Rpar, false, "MetaParams");
}

void Parser2::ConstExpression() {
	if( FIRST_qualident(la.d_type) ) {
		qualident();
	} else if( FIRST_number(la.d_type) ) {
		number();
	} else if( la.d_type == Tok_string ) {
		expect(Tok_string, false, "ConstExpression");
	} else if( la.d_type == Tok_hexstring ) {
		expect(Tok_hexstring, false, "ConstExpression");
	} else
		invalid("ConstExpression");
}

void Parser2::ConstExpression2() {
	if( FIRST_qualident(la.d_type) ) {
		qualident();
		if( FIRST_component_list(la.d_type) ) {
			component_list();
		}
	} else if( FIRST_number(la.d_type) ) {
		number();
	} else if( la.d_type == Tok_string ) {
		expect(Tok_string, false, "ConstExpression2");
	} else if( la.d_type == Tok_hexstring ) {
		expect(Tok_hexstring, false, "ConstExpression2");
	} else
		invalid("ConstExpression2");
}

void Parser2::constructor() {
	if( FIRST_NamedType(la.d_type) ) {
		NamedType();
		component_list();
	} else if( la.d_type == Tok_hexstring ) {
		expect(Tok_hexstring, false, "constructor");
	} else
		invalid("constructor");
}

void Parser2::component_list() {
	expect(Tok_Lbrace, false, "component_list");
	if( FIRST_component(la.d_type) ) {
		component();
		while( la.d_type == Tok_Comma || FIRST_component(la.d_type) ) {
			if( la.d_type == Tok_Comma ) {
				expect(Tok_Comma, false, "component_list");
			}
			component();
		}
	}
	expect(Tok_Rbrace, false, "component_list");
}

void Parser2::component() {
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Eq )  ) {
		expect(Tok_ident, false, "component");
		expect(Tok_Eq, false, "component");
	}
	if( FIRST_ConstExpression(la.d_type) ) {
		ConstExpression();
	} else if( FIRST_component_list(la.d_type) ) {
		component_list();
	} else
		invalid("component");
}

