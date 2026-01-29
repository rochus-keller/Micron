#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>

#include <stdarg.h>

typedef int (*MIC$$PRINTF)(const char *fmt, va_list ap);
MIC$$PRINTF MIC$$printf = 0;

static int my_printf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int result;
    if( MIC$$printf )
        result = MIC$$printf(fmt, args);
    else
        result = vfprintf(stdout, fmt, args);
    va_end(args);
    return result;
}

unsigned int MIC$$relop1(const char* l, const char* r, int op)
{
	switch( op )
	{
	case 1: // EQ
		return strcmp(l,r) == 0;
	case 2: // NEQ
		return strcmp(l,r) != 0;
	case 3: // LT
		return strcmp(l,r) < 0;
	case 4: // LEQ
		return strcmp(l,r) <= 0;
	case 5: // GT
		return strcmp(l,r) > 0;
	case 6: // GEQ
		return strcmp(l,r) >= 0;
	}
	return 0;
}

unsigned int MIC$$relop2(const char* lhs, char rhs, int op)
{
	char ch[2] = "x";
	ch[0] = rhs;
	return MIC$$relop1(lhs,ch,op);
}

unsigned int MIC$$relop3(char lhs, const char* rhs, int op)
{
	char ch[2] = "x";
	ch[0] = lhs;
	return MIC$$relop1(ch,rhs,op);
}

unsigned int MIC$$relop4(char lhs, char rhs, int op)
{
	char l[2] = "x";
	l[0] = lhs;
	char r[2] = "x";
	r[0] = rhs;
	return MIC$$relop1(l,r,op);
}

uint32_t MIC$$SetDiv( uint32_t lhs, uint32_t rhs )
{
    return ~( lhs & rhs ) & ( lhs | rhs );
}

uint32_t MIC$$SetIn( uint32_t lhs, uint32_t rhs )
{
    return ((1<<lhs)&rhs) != 0;
}

void MIC$$printI8(int64_t i)
{
    my_printf("%lld", i);
}

void MIC$$printU8(uint64_t i)
{
    my_printf("%llu", i);
}

void MIC$$printF8(double d)
{
    my_printf("%f", d);
}

void MIC$$printStr(const char* s)
{
    my_printf(s);
}

void MIC$$printCh(char c)
{
    if( c == '\n' )
        my_printf("\n");
    else
        my_printf("%c",c);
}

void MIC$$printBool(uint8_t b)
{
    my_printf("%s", b ? "true" : "false" );
}

void MIC$$printSet(uint32_t s)
{
    my_printf("%x", s);
}

void MIC$$assert(uint8_t cond, uint32_t line, const char* file)
{
    if(!cond)
        fprintf(stderr,"assertion failed in %s line %u\n", file, line);
	assert(cond);
}

extern void _Exit(int res);

void MIC$$exit(int res)
{
    _Exit(res);
}

unsigned int MIC$$strlen(char* str)
{
	return strlen(str);
}
