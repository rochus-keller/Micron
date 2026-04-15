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

void* MIC$$alloc(unsigned int size)
{
    return malloc(size);
}

void MIC$$free(void* ptr)
{
    free(ptr);
}


// intrinsics

int __mic$div_i4(int a, int b)
{
    return a / b;
}

unsigned int __mic$div_un_i4(unsigned int a, unsigned int b)
{
    return a / b;
}

int __mic$rem_i4(int a, int b)
{
    return a % b;
}

unsigned int __mic$rem_un_i4(unsigned int a, unsigned int b)
{
    return a % b;
}

// Helper function to perform 64-bit division without gcc intrinsics
static void udivmod64(uint64_t num, uint64_t den, uint64_t *quot, uint64_t *rem)
{
    // Fast path: If both numbers fit in 32 bits, use native 32-bit hardware math.
    // This will naturally fall back to your _i4 functions or native instructions.
    if ((num >> 32) == 0 && (den >> 32) == 0) {
        if (den == 0) {
            // Intentionally trigger a standard 32-bit divide-by-zero hardware trap
            *quot = (uint32_t)num / (uint32_t)den;
            *rem = 0;
            return;
        }
        *quot = (uint32_t)num / (uint32_t)den;
        *rem  = (uint32_t)num % (uint32_t)den;
        return;
    }

    // Software binary long division
    uint64_t q = 0;
    uint64_t r = 0;

    for (int i = 63; i >= 0; i--) {
        // Shift remainder left by 1 and bring down the next bit of the numerator
        r = (r << 1) | ((num >> i) & 1);

        // If remainder is greater than or equal to denominator, subtract and set quotient bit
        if (r >= den) {
            r -= den;
            q |= (1ULL << i);
        }
    }

    *quot = q;
    *rem = r;
}

uint64_t __mic$div_un_i8(uint64_t a, uint64_t b)
{
    uint64_t q, r;
    udivmod64(a, b, &q, &r);
    return q;
}

uint64_t __mic$rem_un_i8(uint64_t a, uint64_t b)
{
    uint64_t q, r;
    udivmod64(a, b, &q, &r);
    return r;
}

int64_t __mic$div_i8(int64_t a, int64_t b)
{
    int neg_a = a < 0;
    int neg_b = b < 0;

    // Cast to unsigned before negating to safely handle INT64_MIN
    uint64_t ua = neg_a ? -(uint64_t)a : (uint64_t)a;
    uint64_t ub = neg_b ? -(uint64_t)b : (uint64_t)b;

    uint64_t q, r;
    udivmod64(ua, ub, &q, &r);

    // Apply sign to quotient
    if (neg_a != neg_b) {
        return -(int64_t)q;
    }
    return (int64_t)q;
}

int64_t __mic$rem_i8(int64_t a, int64_t b)
{
    int neg_a = a < 0;
    int neg_b = b < 0;

    uint64_t ua = neg_a ? -(uint64_t)a : (uint64_t)a;
    uint64_t ub = neg_b ? -(uint64_t)b : (uint64_t)b;

    uint64_t q, r;
    udivmod64(ua, ub, &q, &r);

    // In C, the remainder takes the sign of the dividend (a)
    if (neg_a) {
        return -(int64_t)r;
    }
    return (int64_t)r;
}
