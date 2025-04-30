#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>

int MIC$$relop1(const char* l, const char* r, int op)
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
}

int MIC$$relop2(const char* lhs, char rhs, int op)
{
	char ch[2] = "x";
	ch[0] = rhs;
	return MIC$$relop1(lhs,ch,op);
}

int MIC$$relop3(char lhs, const char* rhs, int op)
{
	char ch[2] = "x";
	ch[0] = lhs;
	return MIC$$relop1(ch,rhs,op);
}

int MIC$$relop4(char lhs, char rhs, int op)
{
	char l[2] = "x";
	l[0] = lhs;
	char r[2] = "x";
	l[0] = rhs;
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
	printf("%" PRId64, i);
}

void MIC$$printU8(uint64_t i)
{
	printf("%" PRIu64, i);
}

void MIC$$printF8(double d)
{
	printf("%f", d);
}

void MIC$$printStr(const char* s)
{
	printf(s);
}

void MIC$$printCh(char c)
{
	printf("%c",c);
}

void MIC$$printBool(uint8_t b)
{
	printf("%s", b ? "true" : "false" );
}

void MIC$$printSet(uint32_t s)
{
	printf("%x", s);
}

void MIC$$assert(uint8_t cond, uint32_t line, const char* file)
{
    if(!cond)
        fprintf(stderr,"assertion failed in %s line %d\n", file, line);
	assert(cond);
}
