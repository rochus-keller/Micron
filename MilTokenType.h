#ifndef __MIL_TOKENTYPE__
#define __MIL_TOKENTYPE__
// This file was automatically generated by EbnfStudio; don't modify it!


#include <QByteArray>

namespace Mil {
	enum TokenType {
		Tok_Invalid = 0,

		TT_Literals,
		Tok_Bang,
		Tok_Hash,
		Tok_Lpar,
		Tok_Latt,
		Tok_Rpar,
		Tok_Star,
		Tok_Ratt,
		Tok_Plus,
		Tok_Comma,
		Tok_Minus,
		Tok_Dot,
		Tok_2Dot,
		Tok_Slash,
		Tok_2Slash,
		Tok_Colon,
		Tok_Semi,
		Tok_Eq,
		Tok_Lbrack,
		Tok_Rbrack,
		Tok_Hat,
		Tok_Lbrace,
		Tok_Rbrace,

		TT_Keywords,
		Tok_ADD,
		Tok_AND,
		Tok_ARRAY,
		Tok_BEGIN,
		Tok_CALL,
		Tok_CALLI,
		Tok_CALLVI,
		Tok_CALLVIRT,
		Tok_CASE,
		Tok_CASTPTR,
		Tok_CEQ,
		Tok_CGT,
		Tok_CGT_UN,
		Tok_CLT,
		Tok_CLT_UN,
		Tok_CONST,
		Tok_CONV_I1,
		Tok_CONV_I2,
		Tok_CONV_I4,
		Tok_CONV_I8,
		Tok_CONV_IP,
		Tok_CONV_R4,
		Tok_CONV_R8,
		Tok_CONV_U1,
		Tok_CONV_U2,
		Tok_CONV_U4,
		Tok_CONV_U8,
		Tok_DIV,
		Tok_DIV_UN,
		Tok_DO,
		Tok_DUP,
		Tok_ELSE,
		Tok_END,
		Tok_EXIT,
		Tok_EXTERN,
		Tok_FORWARD,
		Tok_FREE,
		Tok_GOTO,
		Tok_IF,
		Tok_IFGOTO,
		Tok_IIF,
		Tok_IMPORT,
		Tok_INIT,
		Tok_INITOBJ,
		Tok_INLINE,
		Tok_INVAR,
		Tok_ISINST,
		Tok_LABEL,
		Tok_LDARG,
		Tok_LDARGA,
		Tok_LDARGA_S,
		Tok_LDARG_0,
		Tok_LDARG_1,
		Tok_LDARG_2,
		Tok_LDARG_3,
		Tok_LDARG_S,
		Tok_LDC_I4,
		Tok_LDC_I4_0,
		Tok_LDC_I4_1,
		Tok_LDC_I4_2,
		Tok_LDC_I4_3,
		Tok_LDC_I4_4,
		Tok_LDC_I4_5,
		Tok_LDC_I4_6,
		Tok_LDC_I4_7,
		Tok_LDC_I4_8,
		Tok_LDC_I4_M1,
		Tok_LDC_I4_S,
		Tok_LDC_I8,
		Tok_LDC_R4,
		Tok_LDC_R8,
		Tok_LDELEM,
		Tok_LDELEMA,
		Tok_LDELEM_I1,
		Tok_LDELEM_I2,
		Tok_LDELEM_I4,
		Tok_LDELEM_I8,
		Tok_LDELEM_IP,
		Tok_LDELEM_R4,
		Tok_LDELEM_R8,
		Tok_LDELEM_U1,
		Tok_LDELEM_U2,
		Tok_LDELEM_U4,
		Tok_LDELEM_U8,
		Tok_LDFLD,
		Tok_LDFLDA,
		Tok_LDIND,
		Tok_LDIND_I1,
		Tok_LDIND_I2,
		Tok_LDIND_I4,
		Tok_LDIND_I8,
		Tok_LDIND_IP,
		Tok_LDIND_IPP,
		Tok_LDIND_R4,
		Tok_LDIND_R8,
		Tok_LDIND_U1,
		Tok_LDIND_U2,
		Tok_LDIND_U4,
		Tok_LDIND_U8,
		Tok_LDLOC,
		Tok_LDLOCA,
		Tok_LDLOCA_S,
		Tok_LDLOC_0,
		Tok_LDLOC_1,
		Tok_LDLOC_2,
		Tok_LDLOC_3,
		Tok_LDLOC_S,
		Tok_LDMETH,
		Tok_LDNULL,
		Tok_LDOBJ,
		Tok_LDPROC,
		Tok_LDSTR,
		Tok_LDVAR,
		Tok_LDVARA,
		Tok_LINE,
		Tok_LOOP,
		Tok_MODULE,
		Tok_MUL,
		Tok_NEG,
		Tok_NEWARR,
		Tok_NEWOBJ,
		Tok_NEWVLA,
		Tok_NOP,
		Tok_NOT,
		Tok_OBJECT,
		Tok_OF,
		Tok_OR,
		Tok_POINTER,
		Tok_POP,
		Tok_PROC,
		Tok_PROCEDURE,
		Tok_PTROFF,
		Tok_REM,
		Tok_REM_UN,
		Tok_REPEAT,
		Tok_RET,
		Tok_SHL,
		Tok_SHR,
		Tok_SHR_UN,
		Tok_SIZEOF,
		Tok_STARG,
		Tok_STARG_S,
		Tok_STELEM,
		Tok_STELEM_I1,
		Tok_STELEM_I2,
		Tok_STELEM_I4,
		Tok_STELEM_I8,
		Tok_STELEM_IP,
		Tok_STELEM_R4,
		Tok_STELEM_R8,
		Tok_STFLD,
		Tok_STIND,
		Tok_STIND_I1,
		Tok_STIND_I2,
		Tok_STIND_I4,
		Tok_STIND_I8,
		Tok_STIND_IP,
		Tok_STIND_IPP,
		Tok_STIND_R4,
		Tok_STIND_R8,
		Tok_STLOC,
		Tok_STLOC_0,
		Tok_STLOC_1,
		Tok_STLOC_2,
		Tok_STLOC_3,
		Tok_STLOC_S,
		Tok_STRUCT,
		Tok_STVAR,
		Tok_SUB,
		Tok_SWITCH,
		Tok_THEN,
		Tok_TO,
		Tok_TYPE,
		Tok_UNION,
		Tok_UNTIL,
		Tok_VAR,
		Tok_WHILE,
		Tok_XOR,

		TT_Specials,
		Tok_ident,
		Tok_unsigned,
		Tok_float,
		Tok_string,
		Tok_hexstring,
		Tok_Comment,
		Tok_Eof,

		TT_MaxToken,

		TT_Max
	};

	const char* tokenTypeString( int ); // Pretty with punctuation chars
	const char* tokenTypeName( int ); // Just the names without punctuation chars
	bool tokenTypeIsLiteral( int );
	bool tokenTypeIsKeyword( int );
	bool tokenTypeIsSpecial( int );
	TokenType tokenTypeFromString( const QByteArray& str, int* pos = 0 );
	TokenType tokenTypeFromString( const char* str, quint32 len, int* pos = 0 );
}
#endif // __MIL_TOKENTYPE__
