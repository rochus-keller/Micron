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
#include <QtDebug>
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
    case Tok_LDIND:
    case Tok_LDIND_I8:
	case Tok_LDC_R4:
	case Tok_LDARGA:
	case Tok_CGT:
	case Tok_AND:
	case Tok_NEG:
    case Tok_ABS:
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
		return true;
	default: return false;
	}
}

static inline bool FIRST_ExpInstr(int tt) {
	switch(tt){
	case Tok_LDELEM_U1:
	case Tok_LDC_I8:
	case Tok_SHL:
    case Tok_LDIND:
    case Tok_LDIND_I8:
	case Tok_LDC_R4:
	case Tok_LDARGA:
	case Tok_CGT:
	case Tok_AND:
	case Tok_NEG:
    case Tok_ABS:
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
    case Tok_ABS:
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
    case Tok_STRCPY:
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
    case Tok_STRCPY:
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

Parser2::Parser2(AstModel* m, Scanner2* s, Importer* imp):mdl(m),scanner(s),imp(imp),
    curMod(0),firstModule(true),curDecl(0)
{

}

Parser2::~Parser2()
{
    if( curMod )
        delete curMod;
}

bool Parser2::parseModule()
{
    if( firstModule )
    {
        errors.clear();
        next();
        firstModule = false;
    }
    if( FIRST_module(la.d_type) || FIRST_module(la.d_code) )
    {
        if( curMod )
            delete curMod;
        curMod = 0;
        module();
        foreach( Type* t, unresolved)
        {
            if( t->getType() == 0 && t->quali )
            {
                Declaration* d = qualident2(*t->quali, t->pos);
                if( d && d->kind != Declaration::TypeDecl )
                    error(t->pos, "the reference is no type declaration");
                else if( d )
                    t->setType(d->getType());
            }
        }
        unresolved.clear();
        return true;
    }else
        return false;
}

Declaration*Parser2::takeModule()
{
    Declaration* res = curMod;
    curMod = 0;
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
}

bool Parser2::expect(int tt, bool pkw, const char* where) {
	if( la.d_type == tt || la.d_code == tt) { next(); return true; }
    else { errors << Error(QString("'%1' expected in %2").arg(tokenTypeString(tt)).arg(where),la.d_lineNr, la.d_colNr, la.d_sourcePath); return false; }
}

void Parser2::error(const Token& t, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg,t.d_lineNr, t.d_colNr, t.d_sourcePath);
}

void Parser2::error(const Mic::RowCol& pos, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg, pos.d_row, pos.d_col, scanner->sourcePath());
}

Declaration*Parser2::addDecl(const Token& id, Declaration::Kind k, bool public_)
{
    return addDecl(id.d_val, id.toRowCol(), k, public_);
}

Declaration*Parser2::addDecl(const QByteArray& name, const Mic::RowCol& pos, Declaration::Kind k, bool public_)
{
    Declaration* forward = 0;
    if( !scopeStack.isEmpty() && !name.isEmpty() ) // don't check anonymous fields
    {
        Declaration* d = scopeStack.back()->findSubByName(name);
        if( d )
        {
            if( k == Declaration::Procedure && d->forward )
            {
                forward = d;
                d->name.clear();
            }else
            {
                error(pos, QString("name not unique in scope: '%1'").arg(name.constData()));
                return 0;
            }
        }
    }
    Declaration* res = new Declaration();
    res->kind = k;
    res->name = name;
    res->pos = pos;
    res->public_ = public_;
    if( forward )
        forward->forwardTo = res;
    if( !scopeStack.isEmpty() )
    {
        res->outer = scopeStack.back();
        scopeStack.back()->appendSub(res);
    }
    return res;
}

static inline void dummy() {}

qint64 Parser2::integer() {
    bool isMinus = false;
	if( la.d_type == Tok_Plus || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Plus ) {
			expect(Tok_Plus, false, "integer");
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "integer");
            isMinus = true;
		} else
			invalid("integer");
	}
	expect(Tok_unsigned, false, "integer");
    const qint64 i = cur.d_val.toLongLong();
    if( isMinus )
        return -i;
    else
        return i;
}

void Parser2::number(Constant* c) {
    bool minus = false;
	if( la.d_type == Tok_Plus || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Plus ) {
			expect(Tok_Plus, false, "number");
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "number");
            minus = true;
		} else
			invalid("number");
	}
	if( la.d_type == Tok_float ) {
		expect(Tok_float, false, "number");
        c->d = cur.d_val.toDouble();
        c->kind = Constant::D;
        if( minus )
            c->d = -c->d;
	} else if( la.d_type == Tok_unsigned ) {
		expect(Tok_unsigned, false, "number");
        c->i = cur.d_val.toLongLong();
        c->kind = Constant::I;
        if( minus )
            c->i = -c->i;
    } else
		invalid("number");
}

Declaration* Parser2::qualident(Quali* q) {
    Declaration* m = curMod;
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Bang )  ) {
		expect(Tok_ident, false, "qualident");
        if( q )
            q->first = cur.d_val;
        m = mdl->findModuleByName(cur.d_val);
        if( m == 0 && q == 0 )
            error(cur,QString("cannot find module '%1'").arg(cur.d_val.constData()));
		expect(Tok_Bang, false, "qualident");
	}
	expect(Tok_ident, false, "qualident");
    if( q )
        q->second = cur.d_val;
    if( m == 0 )
        return 0;
    Declaration* res = m->findSubByName(cur.d_val);
    if( res == 0 && m == curMod )
        res = mdl->getGlobals()->findSubByName(cur.d_val);
    if( res == 0 && q == 0 )
        error(cur,QString("cannot find declaration '%1' in module '%2'").
              arg(cur.d_val.constData()).arg(m->name.constData()));
    return res;
}

Declaration* Parser2::qualident2(const Quali& q, const RowCol& pos) {
    Declaration* m = curMod;
    if( !q.first.isEmpty() )
    {
        m = mdl->findModuleByName(q.first);
        if( m == 0 )
            error(pos,QString("cannot find module '%1'").arg(q.first.constData()));
    }
    if( m == 0 )
        return 0;
    Declaration* res = m->findSubByName(q.second);
    if( res == 0 )
        error(pos,QString("cannot find declaration '%1' in module '%2'").
              arg(q.second.constData()).arg(m->name.constData()));
    return res;
}

Declaration* Parser2::trident() {
    Declaration* m = curMod;
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Bang )  ) {
		expect(Tok_ident, false, "trident");
        m = mdl->findModuleByName(cur.d_val);
        if( m == 0 )
        {
            error(cur,QString("cannot find module '%1'").arg(cur.d_val.constData()));
            return 0;
        }
        expect(Tok_Bang, false, "trident");
	}
	expect(Tok_ident, false, "trident");
    Declaration* d = m->findSubByName(cur.d_val);
    if( d == 0 || d->getType() == 0)
    {
        error(cur,QString("cannot find declaration '%1' in module '%2'").
              arg(cur.d_val.constData()).arg(m->name.constData()));
        return 0;
    }
    expect(Tok_Dot, false, "trident");
	expect(Tok_ident, false, "trident");
    if( d->getType()->kind != Type::Struct && d->getType()->kind != Type::Object )
    {
        error(cur,QString("declaration '%1' in module '%2' is not a record nor object").
              arg(cur.d_val.constData()).arg(m->name.constData()));
        return 0;
    }
    Declaration* f = d->getType()->findSubByName(cur.d_val);
    if( f == 0 )
    {
        error(cur,QString("cannot find element '%1' in declaration '%2' in module '%3'").
              arg(cur.d_val.constData()).arg(d->name.constData()).arg(m->name.constData()));
        return 0;
    }
    return f;
}

Declaration* Parser2::identdef(Declaration::Kind k) {
	expect(Tok_ident, false, "identdef");
    Declaration* d = addDecl(cur, k);
    if( d == 0 )
        return 0;
	if( la.d_type == Tok_Star ) {
		expect(Tok_Star, false, "identdef");
        d->public_ = true;
	}
    return d;
}

void Parser2::ConstDeclaration() {
	expect(Tok_ident, false, "ConstDeclaration");
    Declaration* d = addDecl(cur, Declaration::ConstDecl);
    if( d == 0 )
        return;
    if( la.d_type == Tok_Eq ) {
        expect(Tok_Eq, false, "ConstDeclaration");
        d->c = ConstExpression2();
#if 0
        // TODO
    } else if( la.d_type == Tok_Colon ) {
        expect(Tok_Colon, false, "ConstDeclaration");
        qualident();
#endif
    } else
        invalid("ConstDeclaration");
}

void Parser2::TypeDeclaration() {
    Declaration* d = identdef(Declaration::TypeDecl);
    if( d == 0 )
        return;
    curDecl = d;
    if( la.d_type == Tok_Eq )
    {
		expect(Tok_Eq, false, "TypeDeclaration");
        d->setType(type());
	}
    curDecl = 0;
}

Type* Parser2::type() {
	if( FIRST_ArrayType(la.d_type) || FIRST_ArrayType(la.d_code) ) {
        return ArrayType();
	} else if( FIRST_StructUnionType(la.d_type) || FIRST_StructUnionType(la.d_code) ) {
        return StructUnionType();
	} else if( FIRST_ObjectType(la.d_type) || FIRST_ObjectType(la.d_code) ) {
        return ObjectType();
	} else if( FIRST_PointerType(la.d_type) || FIRST_PointerType(la.d_code) ) {
        return PointerType();
	} else if( FIRST_ProcedureType(la.d_type) ) {
        return ProcedureType();
	} else if( FIRST_NamedType(la.d_type) ) {
        return NamedType();
	} else
		invalid("type");
}

Type* Parser2::NamedType(bool allowAny) {
    const Token t = la;
    Declaration* d = 0;
    if( true ) // allowUnresolved )
    {
        Quali q;
        d = qualident(&q);
        if( d == 0 )
        {
            Type* res = new Type();
            res->kind = Type::NameRef;
            res->quali = new Quali();
            res->pos = t.toRowCol();
            *res->quali = q;
            unresolved << res;
            return res;
        }
    }else
        d = qualident();
    if( d == 0 )
        return 0;
    if( d->kind != Declaration::TypeDecl )
        error(t, "the reference is no type declaration");
    Type* res = new Type();
    res->kind = Type::NameRef;
    res->setType(d->getType());
    if( !allowAny && res->getType() && res->getType()->kind == Type::Any )
        error(t, "cannot use type ANY here");
    res->pos = t.toRowCol();
    return res;
}

Type* Parser2::ArrayType() {
    if( la.d_code == Tok_ARRAY ) {
        expect(Tok_ARRAY, true, "ArrayType");
        Type* res = new Type();
        res->kind = Type::Array;
        if( FIRST_length(la.d_type) ) {
            res->len = length();
        }
        expect(Tok_OF, true, "ArrayType");
        res->setType( NamedType() );
        return res;
    } else if( la.d_type == Tok_Lbrack ) {
        expect(Tok_Lbrack, false, "ArrayType");
        Type* res = new Type();
        res->kind = Type::Array;
        if( FIRST_length(la.d_type) ) {
            res->len = length();
        }
        expect(Tok_Rbrack, false, "ArrayType");
        res->setType( NamedType() );
        return res;
    } else
        invalid("ArrayType");
    return 0;
}

quint32 Parser2::length() {
	expect(Tok_unsigned, false, "length");
    return cur.d_val.toULong();
}

static void moveToSubs(Type* res, Declaration* tmp )
{
    Declaration* p = tmp->subs;
    while( p )
    {
        res->subs.append(p);
        Declaration* old = p;
        p = p->next;
        old->next = 0;
        //old->outer = res->decl; // res->decl is not set at this point
    }
    tmp->subs = 0;
}

Type* Parser2::StructUnionType() {
    bool isUnion = false;
    if( la.d_code == Tok_STRUCT ) {
        expect(Tok_STRUCT, true, "StructUnionType");
    } else if( la.d_code == Tok_UNION ) {
        expect(Tok_UNION, true, "StructUnionType");
        isUnion = true;
    } else
        invalid("StructUnionType");

    Type* res = new Type();
    res->kind = isUnion ? Type::Union : Type::Struct;

    Declaration tmp;
    scopeStack.push_back(&tmp);
    while( ( !( peek(1).d_code == Tok_END ) )  ) {
        FieldList();
        if( la.d_type == Tok_Semi ) {
            expect(Tok_Semi, false, "StructUnionType");
        }
    }
    expect(Tok_END, true, "StructUnionType");
    scopeStack.pop_back();

    moveToSubs( res, &tmp );

    return res;
}

void Parser2::FieldList() {
	if( FIRST_IdentList(la.d_type) ) {
        DeclList fields = IdentList(Declaration::Field);
		expect(Tok_Colon, false, "FieldList");
        Type* t = NamedType();
        quint8 bw = 0;
		if( la.d_type == Tok_Colon ) {
			expect(Tok_Colon, false, "FieldList");
            expect(Tok_unsigned, false, "FieldList");
            const quint64 tmp = cur.d_val.toULongLong();
            if( tmp > 64 )
                error(cur, "bitwidth of field too large");
            else
                bw = tmp;
        }
        foreach( Declaration* field, fields )
        {
            field->setType(t);
            field->f.bw = bw;
            field->outer = curDecl;
        }
	} else if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "FieldList");
        expect(Tok_unsigned, false, "FieldList");
        const quint64 tmp = cur.d_val.toULongLong();
        if( tmp > 64 )
            error(cur, "bitwidth of padding field too large");
        Declaration* padding = addDecl("", cur.toRowCol(), Declaration::Field);
        padding->anonymous = true;
        padding->f.bw = tmp;
        padding->outer = curDecl;
    } else
		invalid("FieldList");
}

DeclList Parser2::IdentList(Declaration::Kind k) {
    DeclList res;
    res << identdef(k);
	while( la.d_type == Tok_Comma || FIRST_identdef(la.d_type) ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "IdentList");
		}
        res << identdef(k);
	}
    return res;
}

Type* Parser2::ObjectType() {
    expect(Tok_OBJECT, true, "ObjectType");
    Type* res = new Type();
    res->kind = Type::Object;

    if( la.d_type == Tok_Lpar ) {
        expect(Tok_Lpar, false, "ObjectType");
        const Token tok = la;
        Type* base = NamedType();
        if( base && base->getType() )
        {
            // check if the resolved TypeRef points to an object
            if( base->getType()->kind != Type::Object )
                error(tok, "invalid base type");
        }
        res->setType(base);
        expect(Tok_Rpar, false, "ObjectType");
    }

    Declaration tmp;
    scopeStack.push_back(&tmp);
    while( FIRST_MemberList(la.d_type) ) {
        MemberList();
        if( la.d_type == Tok_Semi ) {
            expect(Tok_Semi, false, "ObjectType");
        }
    }
    expect(Tok_END, true, "ObjectType");
    scopeStack.pop_back();
    moveToSubs( res, &tmp );
    return res;
}

void Parser2::MemberList() {
    DeclList fields = IdentList(Declaration::Field);
	expect(Tok_Colon, false, "MemberList");
    Type* t = NamedType();
    foreach( Declaration* field, fields )
    {
        field->setType(t);
        field->outer = curDecl;
    }
}

Type* Parser2::PointerType() {
    if( la.d_code == Tok_POINTER ) {
        expect(Tok_POINTER, true, "PointerType");
        expect(Tok_TO, true, "PointerType");
    } else if( la.d_type == Tok_Hat ) {
        expect(Tok_Hat, false, "PointerType");
    } else
        invalid("PointerType");
    Type* base = NamedType(true);
    if( base == 0 )
        return 0;
    Type* res = new Type();
    res->kind = Type::Pointer;
    res->setType(base);
    return res;
}

Type* Parser2::ProcedureType() {
    if( la.d_type == Tok_PROCEDURE ) {
        expect(Tok_PROCEDURE, false, "ProcedureType");
    } else if( la.d_type == Tok_PROC ) {
        expect(Tok_PROC, false, "ProcedureType");
    } else
        invalid("ProcedureType");
    Type* res = new Type();
    res->kind = Type::Proc;
    if( la.d_type == Tok_Hat ) {
        expect(Tok_Hat, false, "ProcedureType");
        res->typebound = true;
    }
    if( FIRST_FormalParameters(la.d_type) ) {
        Declaration tmp;
        scopeStack.push_back(&tmp);
        res->setType(FormalParameters());
        res->ownstype = true;
        scopeStack.pop_back();

        moveToSubs( res, &tmp );
    }
    return res;
}

void Parser2::VariableDeclaration() {
    DeclList l = IdentList(Declaration::VarDecl);
	expect(Tok_Colon, false, "VariableDeclaration");
    Type* t = NamedType();
    foreach( Declaration* d , l)
        d->setType(t);
}

void Parser2::ProcedureDeclaration() {
	if( la.d_type == Tok_PROCEDURE ) {
		expect(Tok_PROCEDURE, false, "ProcedureDeclaration");
	} else if( la.d_type == Tok_PROC ) {
		expect(Tok_PROC, false, "ProcedureDeclaration");
	} else
		invalid("ProcedureDeclaration");
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Dot )  ) {

        const Token tok = la;
        const QByteArray receiver = Binding();

        Declaration tmp;
        if( !receiver.isEmpty() )
            scopeStack.push_back(&tmp);
        Declaration* p = identdef(Declaration::Procedure);
        if( !receiver.isEmpty() )
        {
            p->typebound = true;
            scopeStack.pop_back();
            Declaration* object = scopeStack.back()->findSubByName(receiver);
            if( object && object->getType() && object->getType()->kind != Type::Object )
                error(tok, "binding doesn't reference an object type");
            else if( object && object->getType() )
            {
                Type* t = object->getType();
                Declaration* forward = t->findSubByName(p->name, false);
                if( forward && (forward->kind != Declaration::Procedure || !forward->forward) )
                    error(tok, "method name not unique in object");
                else if( forward )
                {
                    forward->name.clear();
                    forward->forwardTo = p;
                }
                tmp.subs = 0;
                t->subs.append(p);
                p->outer = t->decl;
            }
        }

        scopeStack.push_back(p);
		if( FIRST_FormalParameters(la.d_type) ) {
            p->setType(FormalParameters());
		}
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "ProcedureDeclaration");
		}
        if( la.d_code == Tok_FORWARD ) {
            if( la.d_type == Tok_Semi ) {
                expect(Tok_Semi, false, "ProcedureDeclaration");
            }
            expect(Tok_FORWARD, true, "ProcedureDeclaration");
            p->forward = true;
        }else
            ProcedureBody(p);
        scopeStack.pop_back();
	} else if( FIRST_identdef(la.d_type) ) {
        Declaration* p = identdef(Declaration::Procedure);
        scopeStack.push_back(p);
        if( FIRST_FormalParameters(la.d_type) ) {
            p->setType(FormalParameters());
		}
        if( la.d_code == Tok_INLINE || la.d_code == Tok_INVAR || la.d_code == Tok_INIT || la.d_type == Tok_Semi ||
                FIRST_ProcedureBody(la.d_type) ) {
			if( la.d_code == Tok_INLINE || la.d_code == Tok_INVAR || la.d_code == Tok_INIT ) {
				if( la.d_code == Tok_INLINE ) {
					expect(Tok_INLINE, true, "ProcedureDeclaration");
                    p->inline_ = true;
				} else if( la.d_code == Tok_INVAR ) {
					expect(Tok_INVAR, true, "ProcedureDeclaration");
                    p->invar = true;
				} else if( la.d_code == Tok_INIT ) {
					expect(Tok_INIT, true, "ProcedureDeclaration");
                    p->init = true;
				} else
					invalid("ProcedureDeclaration");
			}
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "ProcedureDeclaration");
			}
            ProcedureBody(p);
        } else if( la.d_code == Tok_EXTERN || ( la.d_type == Tok_Semi && peek(2).d_code == Tok_EXTERN ) ) {
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "ProcedureDeclaration");
			}
			expect(Tok_EXTERN, true, "ProcedureDeclaration");
            p->extern_ = true;
#if 0
            // TODO
			if( la.d_type == Tok_ident ) {
				expect(Tok_ident, false, "ProcedureDeclaration");
			}
#endif
        } else if( la.d_code == Tok_FORWARD || ( la.d_type == Tok_Semi && peek(2).d_code == Tok_FORWARD ) ) {
            if( la.d_type == Tok_Semi ) {
                expect(Tok_Semi, false, "ProcedureDeclaration");
            }
            expect(Tok_FORWARD, true, "ProcedureDeclaration");
            p->forward = true;
        } else
			invalid("ProcedureDeclaration");
        scopeStack.pop_back();
	} else
		invalid("ProcedureDeclaration");
}

QByteArray Parser2::Binding() {
	expect(Tok_ident, false, "Binding");
    const QByteArray name = cur.d_val;
	expect(Tok_Dot, false, "Binding");
    return name;
}

void Parser2::ProcedureBody(Declaration* proc) {
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
    proc->body = StatementSequence();
	expect(Tok_END, false, "ProcedureBody");
	expect(Tok_ident, false, "ProcedureBody");
    if( cur.d_val.constData() != proc->name.constData() )
        error(cur, "invalid end identifier");
}

void Parser2::LocalDeclaration() {
    DeclList l = IdentList(Declaration::LocalDecl);
	expect(Tok_Colon, false, "LocalDeclaration");
    Type* t = NamedType();
    foreach( Declaration* d, l )
        d->setType(t);
}

Type* Parser2::FormalParameters() {
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
    Type* res = 0;
	if( la.d_type == Tok_Colon ) {
		expect(Tok_Colon, false, "FormalParameters");
        res = ReturnType();
	}
    return res;
}

Type* Parser2::ReturnType() {
    return NamedType();
}

void Parser2::FPSection() {
	expect(Tok_ident, false, "FPSection");
    TokenList ids;
    ids << cur;
	while( la.d_type == Tok_Comma || la.d_type == Tok_ident ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "FPSection");
		}
		expect(Tok_ident, false, "FPSection");
        ids << cur;
    }
	expect(Tok_Colon, false, "FPSection");
    Type* t = NamedType();
    foreach( const Token& id, ids )
    {
        Declaration* d = addDecl(id,Declaration::ParamDecl,false);
        d->setType(t);
    }
}

void Parser2::module() {
	expect(Tok_MODULE, true, "module");
	expect(Tok_ident, false, "module");

    if( mdl->findModuleByName(cur.d_val) )
    {
        error(cur, "A module with this name already exists");
        return;
    }
    curMod = addDecl(cur, Declaration::Module);
    scopeStack.push_back(curMod);

	if( FIRST_MetaParams(la.d_type) ) {
        MetaParams(); // TODO
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
    if( cur.d_val.constData() != curMod->name.constData() )
        error(cur,"Identifier after END doesn't correspond with module name");
	if( la.d_type == Tok_Dot ) {
		expect(Tok_Dot, false, "module");

	}
    scopeStack.pop_back();
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
    // ok
}

void Parser2::import() {
    // QList<Token> path;
#if 0
	while( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Slash )  ) {
		expect(Tok_ident, false, "import");
        path << cur;
        expect(Tok_Slash, false, "import"); // TODO
	}
#endif
	expect(Tok_ident, false, "import");
    // path << cur;
#if 0
	if( FIRST_MetaActuals(la.d_type) ) {
        MetaActuals(); // TODO
	}
#endif
    Declaration* d = addDecl(cur, Declaration::Import);
    Import i;
    i.moduleName = cur.d_val;
    Declaration* m = imp->loadModule(i);
    if( m == 0 )
        error(cur, QString("cannot import '%1'").arg(cur.d_val.constData()));
    else
        d->imported = m;
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
    // ok
}

Expression* Parser2::Expression_() {
    Expression* res = 0;
	while( FIRST_ExpInstr(la.d_type) || FIRST_ExpInstr(la.d_code) ) {
        Expression* e = ExpInstr();
        if( e == 0 )
        {
            delete res;
            return 0;
        }else if( res == 0 )
            res = e;
        else
            res->append(e);
	}
    return res;
}

Expression* Parser2::ExpInstr() {
    Expression* res = new Expression();
    res->kind = (TokenType)la.d_code;
    res->pos = la.toRowCol();

    if( la.d_code == Tok_ADD ) {
        expect(Tok_ADD, true, "ExpInstr");
    } else if( la.d_code == Tok_AND ) {
        expect(Tok_AND, true, "ExpInstr");
    } else if( la.d_code == Tok_CALL ) {
        expect(Tok_CALL, true, "ExpInstr");
        res->d = qualident();
        if( res->d && (res->d->kind != Declaration::Procedure || res->d->typebound) )
            error(cur, "expecting an unbound procedure");
    } else if( la.d_code == Tok_CALLI ) {
        expect(Tok_CALLI, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->getType() )
        {
            Type* t = res->d->getType();
            if( t->kind != Type::Proc || t->typebound )
                error(cur, "expecting an unbound procedure type");
        }
    } else if( la.d_code == Tok_CALLVI ) {
        expect(Tok_CALLVI, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->getType() )
        {
            Type* t = res->d->getType();
            if( t->kind != Type::Proc || !t->typebound )
                error(cur, "expecting a bound procedure type");
        }
    } else if( la.d_code == Tok_CALLVIRT ) {
        expect(Tok_CALLVIRT, true, "ExpInstr");
        res->d = trident();
        if( res->d && (res->d->kind != Declaration::Procedure || !res->d->typebound) )
            error(cur, "expecting a bound procedure");
    } else if( la.d_code == Tok_CASTPTR ) {
        expect(Tok_CASTPTR, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
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
    } else if( la.d_code == Tok_DIV ) {
        expect(Tok_DIV, true, "ExpInstr");
    } else if( la.d_code == Tok_DIV_UN ) {
        expect(Tok_DIV_UN, true, "ExpInstr");
    } else if( la.d_code == Tok_DUP ) {
        expect(Tok_DUP, true, "ExpInstr");
    } else if( la.d_code == Tok_INITOBJ ) {
        expect(Tok_INITOBJ, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_ISINST ) {
        expect(Tok_ISINST, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl &&
                res->d->getType() && res->d->getType()->kind != Type::Object )
            error(cur, "expecting an object type declaration");
    } else if( la.d_code == Tok_LDARG ) {
        expect(Tok_LDARG, true, "ExpInstr");
        res->id = numberOrIdent(true);
    } else if( la.d_code == Tok_LDARG_S ) {
        expect(Tok_LDARG_S, true, "ExpInstr");
        res->id = numberOrIdent(true);
    } else if( la.d_code == Tok_LDARG_0 ) {
        expect(Tok_LDARG_0, true, "ExpInstr");
        res->id = 0;
    } else if( la.d_code == Tok_LDARG_1 ) {
        expect(Tok_LDARG_1, true, "ExpInstr");
        res->id = 1;
    } else if( la.d_code == Tok_LDARG_2 ) {
        expect(Tok_LDARG_2, true, "ExpInstr");
        res->id = 2;
    } else if( la.d_code == Tok_LDARG_3 ) {
        expect(Tok_LDARG_3, true, "ExpInstr");
        res->id = 3;
    } else if( la.d_code == Tok_LDARGA ) {
        expect(Tok_LDARGA, true, "ExpInstr");
        res->id = numberOrIdent(true);
    } else if( la.d_code == Tok_LDARGA_S ) {
        expect(Tok_LDARGA_S, true, "ExpInstr");
        res->id = numberOrIdent(true);
    } else if( la.d_code == Tok_LDC_I4 ) {
        expect(Tok_LDC_I4, true, "ExpInstr");
        res->i = integer();
    } else if( la.d_code == Tok_LDC_I8 ) {
        expect(Tok_LDC_I8, true, "ExpInstr");
        res->i = integer();
    } else if( la.d_code == Tok_LDC_I4_S ) {
        expect(Tok_LDC_I4_S, true, "ExpInstr");
        res->i = integer();
    } else if( la.d_code == Tok_LDC_R4 ) {
        expect(Tok_LDC_R4, true, "ExpInstr");
        Constant c;
        number(&c);
        if( c.kind == Constant::I )
            res->f = c.i;
        else
            res->f = c.d;
    } else if( la.d_code == Tok_LDC_R8 ) {
        expect(Tok_LDC_R8, true, "ExpInstr");
        Constant c;
        number(&c);
        if( c.kind == Constant::I )
            res->f = c.i;
        else
            res->f = c.d;
    } else if( la.d_code == Tok_LDC_I4_0 ) {
        expect(Tok_LDC_I4_0, true, "ExpInstr");
        res->i = 0;
    } else if( la.d_code == Tok_LDC_I4_1 ) {
        expect(Tok_LDC_I4_1, true, "ExpInstr");
        res->i = 1;
    } else if( la.d_code == Tok_LDC_I4_2 ) {
        expect(Tok_LDC_I4_2, true, "ExpInstr");
        res->i = 2;
    } else if( la.d_code == Tok_LDC_I4_3 ) {
        expect(Tok_LDC_I4_3, true, "ExpInstr");
        res->i = 3;
    } else if( la.d_code == Tok_LDC_I4_4 ) {
        expect(Tok_LDC_I4_4, true, "ExpInstr");
        res->i = 4;
    } else if( la.d_code == Tok_LDC_I4_5 ) {
        expect(Tok_LDC_I4_5, true, "ExpInstr");
        res->i = 5;
    } else if( la.d_code == Tok_LDC_I4_6 ) {
        expect(Tok_LDC_I4_6, true, "ExpInstr");
        res->i = 6;
    } else if( la.d_code == Tok_LDC_I4_7 ) {
        expect(Tok_LDC_I4_7, true, "ExpInstr");
        res->i = 7;
    } else if( la.d_code == Tok_LDC_I4_8 ) {
        expect(Tok_LDC_I4_8, true, "ExpInstr");
        res->i = 8;
    } else if( la.d_code == Tok_LDC_I4_M1 ) {
        expect(Tok_LDC_I4_M1, true, "ExpInstr");
        res->i = -1;
    } else if( la.d_code == Tok_LDOBJ ) {
        expect(Tok_LDOBJ, true, "ExpInstr");
        res->c = constructor();
    } else if( la.d_code == Tok_LDELEM ) {
        expect(Tok_LDELEM, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_LDELEMA ) {
        expect(Tok_LDELEMA, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
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
    } else if( la.d_code == Tok_LDFLD ) {
        expect(Tok_LDFLD, true, "ExpInstr");
        res->d = trident();
        if( res->d && res->d->kind != Declaration::Field )
            error(cur, "expecting a field");
    } else if( la.d_code == Tok_LDFLDA ) {
        expect(Tok_LDFLDA, true, "ExpInstr");
        res->d = trident();
        if( res->d && res->d->kind != Declaration::Field )
            error(cur, "expecting a field");
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
    } else if( la.d_code == Tok_LDLOC ) {
        expect(Tok_LDLOC, true, "ExpInstr");
        res->id = numberOrIdent(false);
    } else if( la.d_code == Tok_LDLOC_S ) {
        expect(Tok_LDLOC_S, true, "ExpInstr");
        res->id = numberOrIdent(false);
    } else if( la.d_code == Tok_LDLOCA ) {
        expect(Tok_LDLOCA, true, "ExpInstr");
        res->id = numberOrIdent(false);
    } else if( la.d_code == Tok_LDLOCA_S ) {
        expect(Tok_LDLOCA_S, true, "ExpInstr");
        res->id = numberOrIdent(false);
    } else if( la.d_code == Tok_LDLOC_0 ) {
        expect(Tok_LDLOC_0, true, "ExpInstr");
        res->id = 0;
    } else if( la.d_code == Tok_LDLOC_1 ) {
        expect(Tok_LDLOC_1, true, "ExpInstr");
        res->id = 1;
    } else if( la.d_code == Tok_LDLOC_2 ) {
        expect(Tok_LDLOC_2, true, "ExpInstr");
        res->id = 2;
   } else if( la.d_code == Tok_LDLOC_3 ) {
        expect(Tok_LDLOC_3, true, "ExpInstr");
        res->id = 3;
    } else if( la.d_code == Tok_LDNULL ) {
        expect(Tok_LDNULL, true, "ExpInstr");
    } else if( la.d_code == Tok_LDIND ) {
        expect(Tok_LDIND, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_LDPROC ) {
        expect(Tok_LDPROC, true, "ExpInstr");
        res->d = qualident();
        if( res->d && (res->d->kind != Declaration::Procedure || res->d->typebound) )
            error(cur, "expecting an unbound procedure");
    } else if( la.d_code == Tok_LDMETH ) {
        expect(Tok_LDMETH, true, "ExpInstr");
        res->d = trident();
        if( res->d && (res->d->kind != Declaration::Procedure || !res->d->typebound) )
            error(cur, "expecting a bound procedure");
    } else if( la.d_code == Tok_LDSTR ) {
        expect(Tok_LDSTR, true, "ExpInstr");
        if( la.d_type == Tok_string ) {
            expect(Tok_string, false, "ExpInstr");
            Constant* c = new Constant();
            c->kind = Constant::S;
            cur.d_val = cur.d_val.mid(1, cur.d_val.size()-2); // dequote
            c->s = (char*)malloc( cur.d_val.size() + 1);
            strcpy(c->s, cur.d_val.constData());
            res->c = c;
        } else if( la.d_type == Tok_hexstring ) {
            expect(Tok_hexstring, false, "ExpInstr");
            Constant* c = new Constant();
            c->kind = Constant::B;
            c->b = new ByteString();
            c->b->len = cur.d_val.size();
            c->b->b = (unsigned char*)malloc(cur.d_val.size());
            memcpy(c->b->b, cur.d_val.constData(), c->b->len);
            res->c = c;
        } else
            invalid("ExpInstr");
    } else if( la.d_code == Tok_LDVAR ) {
        expect(Tok_LDVAR, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::VarDecl )
            error(cur, "expecting a module variable");
    } else if( la.d_code == Tok_LDVARA ) {
        expect(Tok_LDVARA, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::VarDecl )
            error(cur, "expecting a module variable");
    } else if( la.d_code == Tok_MUL ) {
        expect(Tok_MUL, true, "ExpInstr");
    } else if( la.d_code == Tok_NEG ) {
        expect(Tok_NEG, true, "ExpInstr");
    } else if( la.d_code == Tok_ABS ) {
        expect(Tok_ABS, true, "ExpInstr");
    } else if( la.d_code == Tok_NEWARR ) {
        expect(Tok_NEWARR, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl ) // element type
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_NEWVLA ) {
        expect(Tok_NEWVLA, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl ) // element type
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_NEWOBJ ) {
        expect(Tok_NEWOBJ, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl &&
                res->d->getType() && res->d->getType()->kind != Type::Object &&
                res->d->getType()->kind != Type::Struct && res->d->getType()->kind != Type::Union )
            error(cur, "expecting a struct, union or object declaration");
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
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_SUB ) {
        expect(Tok_SUB, true, "ExpInstr");
    } else if( la.d_code == Tok_XOR ) {
        expect(Tok_XOR, true, "ExpInstr");
    } else if( la.d_code == Tok_PTROFF ) {
        expect(Tok_PTROFF, true, "ExpInstr");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl ) // pointer base type
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_NOP ) {
        expect(Tok_NOP, true, "ExpInstr");
    } else if( FIRST_CondOp(la.d_type) || FIRST_CondOp(la.d_code) ) {
        expect(Tok_IIF, true, "CondOp");
        res->e = Expression_();
        res->kind = Tok_IIF;
        expect(Tok_THEN, true, "CondOp");
        Expression* res2 = new Expression();
        res2->kind = Tok_THEN;
        res2->pos = cur.toRowCol();
        res2->e = Expression_();
        res->next = res2;
        expect(Tok_ELSE, true, "CondOp");
        Expression* res3 = new Expression();
        res3->kind = Tok_ELSE;
        res3->pos = cur.toRowCol();
        res3->e = Expression_();
        res2->next = res3;
        expect(Tok_END, true, "CondOp");
    } else
        invalid("ExpInstr");
    return res;
}

Statement* Parser2::StatementSequence() {
    Statement* res = 0;
    while( FIRST_Statement(la.d_type) || FIRST_Statement(la.d_code) ||
           FIRST_ExpInstr(la.d_type) || FIRST_ExpInstr(la.d_code) ) {
        Statement* s = 0;
        if( FIRST_Statement(la.d_type) || FIRST_Statement(la.d_code) ) {
            s = Statement_();
        } else if( FIRST_ExpInstr(la.d_type) || FIRST_ExpInstr(la.d_code) ) {
            s = new Statement();
            s->kind = (TokenType)Statement::ExprStat;
            s->e = ExpInstr();
            if( s->e == 0 )
            {
                if( res )
                    delete res;
                return 0;
            }
            s->pos = s->e->pos;
            while( FIRST_ExpInstr(la.d_type) || FIRST_ExpInstr(la.d_code) )
            {
                // take care that all stretches of expressions are connected under the same ExprStat
                Expression* e = ExpInstr();
                if( e == 0 )
                {
                    if( res )
                        delete res;
                    return 0;
                }
                s->e->append(e);
            }
        } else
            invalid("StatementSequence");
        if( res == 0 )
            res = s;
        else if( s == 0 )
        {
            if( res )
                delete res;
            return 0;
        }else
            res->append(s);
    }
    return res;
}

Statement* Parser2::Statement_()
{
    if( FIRST_RepeatUntil(la.d_type) || FIRST_RepeatUntil(la.d_code) ) {
        return RepeatUntil();
    } else if( FIRST_IfThenElse(la.d_type) || FIRST_IfThenElse(la.d_code) ) {
        return IfThenElse();
    } else if( FIRST_Loop(la.d_type) || FIRST_Loop(la.d_code) ) {
        return Loop();
    } else if( FIRST_Switch(la.d_type) || FIRST_Switch(la.d_code) ) {
        return Switch();
    } else if( FIRST_WhileDo(la.d_type) || FIRST_WhileDo(la.d_code) ) {
        return WhileDo();
    }

    Statement* res = new Statement();
    res->kind = (TokenType)la.d_code;
    res->pos = la.toRowCol();

    if( la.d_code == Tok_FREE ) {
        expect(Tok_FREE, true, "Statement");
    } else if( la.d_code == Tok_EXIT ) {
        expect(Tok_EXIT, true, "Statement");
    } else if( la.d_code == Tok_GOTO ) {
        expect(Tok_GOTO, true, "Statement");
        expect(Tok_ident, false, "Statement");
        res->name = cur.d_val.constData();
    } else if( la.d_code == Tok_LABEL ) {
        expect(Tok_LABEL, true, "Statement");
        expect(Tok_ident, false, "Statement");
        res->name = cur.d_val.constData();
    } else if( la.d_code == Tok_LINE ) {
        expect(Tok_LINE, true, "Statement");
        expect(Tok_unsigned, false, "Statement");
        res->id = cur.d_val.toULong();
    } else if( la.d_code == Tok_POP ) {
        expect(Tok_POP, true, "Statement");
    } else if( la.d_code == Tok_RET ) {
        expect(Tok_RET, true, "Statement");
    } else if( la.d_code == Tok_STARG ) {
        expect(Tok_STARG, true, "Statement");
        res->id = numberOrIdent(true);
    } else if( la.d_code == Tok_STARG_S ) {
        expect(Tok_STARG_S, true, "Statement");
        res->id = numberOrIdent(true);
    } else if( la.d_code == Tok_STELEM ) {
        expect(Tok_STELEM, true, "Statement");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
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
        res->d = trident();
        if( res->d && res->d->kind != Declaration::Field )
            error(cur, "expecting a field");
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
    } else if( la.d_code == Tok_STLOC ) {
        expect(Tok_STLOC, true, "Statement");
        res->id = numberOrIdent(false);
    } else if( la.d_code == Tok_STLOC_S ) {
        expect(Tok_STLOC_S, true, "Statement");
        res->id = numberOrIdent(false);
    } else if( la.d_code == Tok_STLOC_0 ) {
        expect(Tok_STLOC_0, true, "Statement");
        res->id = 0;
    } else if( la.d_code == Tok_STLOC_1 ) {
        expect(Tok_STLOC_1, true, "Statement");
        res->id = 1;
    } else if( la.d_code == Tok_STLOC_2 ) {
        expect(Tok_STLOC_2, true, "Statement");
        res->id = 2;
    } else if( la.d_code == Tok_STLOC_3 ) {
        expect(Tok_STLOC_3, true, "Statement");
        res->id = 3;
    } else if( la.d_code == Tok_STIND ) {
        expect(Tok_STIND, true, "Statement");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::TypeDecl )
            error(cur, "expecting a type declaration");
    } else if( la.d_code == Tok_STRCPY ) {
        expect(Tok_STRCPY, true, "Statement");
    } else if( la.d_code == Tok_STVAR ) {
        expect(Tok_STVAR, true, "Statement");
        res->d = qualident();
        if( res->d && res->d->kind != Declaration::VarDecl )
            error(cur, "expecting a module variable");
    } else
        invalid("Statement");
    return res;
}

Statement* Parser2::IfThenElse() {
    Statement* res = new Statement();
    res->kind = Tok_IF;
    res->pos = la.toRowCol();
	expect(Tok_IF, true, "IfThenElse");
    res->e = Expression_();
	expect(Tok_THEN, true, "IfThenElse");
    res->body = StatementSequence();
	if( la.d_code == Tok_ELSE ) {
        Statement* s = new Statement();
        s->kind = Tok_ELSE;
        s->pos = la.toRowCol();
		expect(Tok_ELSE, true, "IfThenElse");
        s->body = StatementSequence();
        res->append(s);
	}
	expect(Tok_END, false, "IfThenElse");
    return res;
}

Statement* Parser2::Loop() {
    Statement* res = new Statement();
    res->kind = Tok_LOOP;
    res->pos = la.toRowCol();
	expect(Tok_LOOP, true, "Loop");
    res->body = StatementSequence();
	expect(Tok_END, false, "Loop");
    return res;
}

Statement* Parser2::Switch() {
    Statement* res = new Statement();
    res->kind = Tok_SWITCH;
    res->pos = la.toRowCol();
    expect(Tok_SWITCH, true, "Switch");
    res->e = Expression_();
	while( la.d_code == Tok_CASE ) {
        Statement* s = new Statement();
        s->kind = Tok_CASE;
        s->pos = la.toRowCol();
        expect(Tok_CASE, true, "Switch");
        s->e = new Expression();
        s->e->kind = Tok_CASE;
        s->e->pos = la.toRowCol();
        s->e->i = integer();
		while( la.d_type == Tok_Comma || FIRST_integer(la.d_type) ) {
			if( la.d_type == Tok_Comma ) {
				expect(Tok_Comma, false, "Switch");
			}
            Expression* e = new Expression();
            e->kind = Tok_CASE;
            e->pos = la.toRowCol();
            e->i = integer();
            s->e->append(e);
        }
		expect(Tok_THEN, true, "Switch");
        s->body = StatementSequence();
        res->append(s);
	}
	if( la.d_code == Tok_ELSE ) {
        Statement* s = new Statement();
        s->kind = Tok_ELSE;
        s->pos = la.toRowCol();
        expect(Tok_ELSE, true, "Switch");
        s->body = StatementSequence();
        res->append(s);
    }
	expect(Tok_END, false, "Switch");
    return res;
}

Statement* Parser2::RepeatUntil() {
    Statement* res = new Statement();
    res->kind = Tok_REPEAT;
    res->pos = la.toRowCol();
    expect(Tok_REPEAT, true, "RepeatUntil");
    res->body = StatementSequence();
	expect(Tok_UNTIL, true, "RepeatUntil");
    res->e = Expression_();
	expect(Tok_END, false, "RepeatUntil");
    return res;
}

Statement* Parser2::WhileDo() {
    Statement* res = new Statement();
    res->kind = Tok_WHILE;
    res->pos = la.toRowCol();
    expect(Tok_WHILE, true, "WhileDo");
    res->e = Expression_();
	expect(Tok_DO, true, "WhileDo");
    res->body = StatementSequence();
	expect(Tok_END, false, "WhileDo");
    return res;
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
    // TODO
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
    // TODO
}

Constant* Parser2::ConstExpression() {
    const Token t = la;
    if( FIRST_qualident(la.d_type) ) {
        Declaration* d = qualident();
        if( d == 0 )
            return 0;
        if( d->kind != Declaration::ConstDecl )
        {
            error(t, "qualident must reference a constant declaration");
            return 0;
        }
        Constant* c = new Constant();
        c->kind = Constant::R;
        c->r = d;
        return c;
    } else if( FIRST_number(la.d_type) ) {
        Constant* c = new Constant();
        number(c);
        return c;
    } else if( la.d_type == Tok_string ) {
		expect(Tok_string, false, "ConstExpression");
        Constant* c = new Constant();
        c->kind = Constant::S;
        c->s = (char*)malloc( cur.d_val.size() + 1);
        strcpy(c->s, cur.d_val.constData());
        return c;
    } else if( la.d_type == Tok_hexstring ) {
		expect(Tok_hexstring, false, "ConstExpression");
        Constant* c = new Constant();
        c->kind = Constant::B;
        c->b = new ByteString();
        c->b->len = cur.d_val.size();
        c->b->b = (unsigned char*)malloc(cur.d_val.size());
        memcpy(c->b->b, cur.d_val.constData(), c->b->len);
        return c;
    } else
        invalid("ConstExpression");
    return 0;
}

Constant* Parser2::ConstExpression2() {
    const Token tok = la;
    if( FIRST_qualident(la.d_type) ) {
        Declaration* d = qualident();
        if( d == 0 )
            return 0;
        if( FIRST_component_list(la.d_type) )
        {
            if( d->kind != Declaration::TypeDecl )
            {
                error(tok, "qualident must reference a type declaration if a component list follows");
                return 0;
            }
            Constant* c = new Constant();
            c->c = component_list();
            c->kind = Constant::C;
            if( c->c )
                c->c->type = d->getType();
            return c;
        }else
        {
            Constant* c = new Constant();
            c->kind = Constant::R;
            c->r = d;
            return c;
        }
    } else if( FIRST_number(la.d_type) ) {
        Constant* c = new Constant();
        number(c);
        return c;
    } else if( la.d_type == Tok_string ) {
        expect(Tok_string, false, "ConstExpression2");
        Constant* c = new Constant();
        c->kind = Constant::S;
        c->s = (char*)malloc( cur.d_val.size() + 1);
        strcpy(c->s, cur.d_val.constData());
        return c;
    } else if( la.d_type == Tok_hexstring ) {
		expect(Tok_hexstring, false, "ConstExpression2");
        Constant* c = new Constant();
        c->kind = Constant::B;
        c->b = new ByteString();
        c->b->len = cur.d_val.size();
        c->b->b = (unsigned char*)malloc(cur.d_val.size());
        memcpy(c->b->b, cur.d_val.constData(), c->b->len);
        return c;
    } else
        invalid("ConstExpression2");
    return 0;
}

Constant* Parser2::constructor() {
	if( FIRST_NamedType(la.d_type) ) {
        Type* t = NamedType();
        ComponentList* cl = component_list();
        cl->type = t;
        Constant* c = new Constant();
        c->kind = Constant::C;
        c->c = cl;
        return c;
	} else if( la.d_type == Tok_hexstring ) {
		expect(Tok_hexstring, false, "constructor");
        Constant* c = new Constant();
        c->kind = Constant::B;
        c->b = new ByteString();
        c->b->len = cur.d_val.size();
        c->b->b = (unsigned char*)malloc(cur.d_val.size());
        memcpy(c->b->b, cur.d_val.constData(), c->b->len);
        return c;
    } else
        invalid("constructor");
    return 0;
}

ComponentList* Parser2::component_list() {
	expect(Tok_Lbrace, false, "component_list");
    ComponentList* cl = new ComponentList();
	if( FIRST_component(la.d_type) ) {
        Component* c = component();
        if( c == 0 )
            return cl;
        cl->c = c;
		while( la.d_type == Tok_Comma || FIRST_component(la.d_type) ) {
			if( la.d_type == Tok_Comma ) {
				expect(Tok_Comma, false, "component_list");
			}
            Component* cc = component();
            if( cc == 0 )
                return cl;
            c->next = cc;
            c = cc;
		}
	}
	expect(Tok_Rbrace, false, "component_list");
    return cl;
}

Component* Parser2::component() {
    Component* cp = new Component();
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Eq )  ) {
		expect(Tok_ident, false, "component");
        cp->name = cur.d_val;
		expect(Tok_Eq, false, "component");
	}
	if( FIRST_ConstExpression(la.d_type) ) {
        cp->c = ConstExpression();
	} else if( FIRST_component_list(la.d_type) ) {
        ComponentList* cl = component_list();
        cp->c = new Constant();
        cp->c->kind = Constant::C;
        cp->c->c = cl;
	} else
        invalid("component");
    return cp;
}

quint32 Parser2::numberOrIdent(bool param)
{
    if( la.d_type == Tok_unsigned ) {
        expect(Tok_unsigned, false, "numberOrIdent");
        return cur.d_val.toUInt();
    } else if( la.d_type == Tok_ident ) {
        expect(Tok_ident, false, "numberOrIdent");
        Declaration* d = scopeStack.back()->findSubByName(cur.d_val);
        if( d == 0 )
            error(cur, QString("cannot find '%1' in current scope").arg(cur.d_val.constData()));
        else if( param )
        {
            if( d->kind != Declaration::ParamDecl )
                error(cur, QString("'%1' is not a parameter").arg(cur.d_val.constData()));
            scopeStack.back()->indexOf(d);
        }else
        {
            if( d->kind != Declaration::LocalDecl )
                error(cur, QString("'%1' is not a local variable").arg(cur.d_val.constData()));
            scopeStack.back()->indexOf(d); // TODO: currently follows params
        }
    } else
        invalid("numberOrIdent");
}

