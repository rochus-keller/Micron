#include <stdio.h>
#include <stdarg.h>

typedef int (*MIC$$PRINTF)(const char *fmt, va_list ap);
extern MIC$$PRINTF MIC$$printf; // defined in MIC++.c

static int my_printf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int result;
    if( MIC$$printf )
        MIC$$printf(fmt, args);
    else
        vfprintf(stdout, fmt, args);
    va_end(args);
    return result;
}

void Out$Int(long long i, int n)
{
    my_printf("%*lld", n, i);
}

void Out$Real(float x, int n)
{
    my_printf("%*e", n, x);
}

void Out$LongReal(double x, int n)
{
    my_printf("%*e", n, x);
}

void Out$Ln()
{
    my_printf("\n");
    fflush(stdout);
}

void Out$Char(char c)
{
    my_printf("%c", c );
}

void Out$String(char* str)
{
    my_printf("%s", str);
}

void Out$Open()
{
}

void Out$begin$()
{
}
