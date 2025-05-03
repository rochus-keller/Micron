#include <stdio.h>

void Out$Int(long long i, int n)
{
    printf("%*lld", n, i);
}

void Out$Real(float x, int n)
{
    printf("%*e", n, x);
}

void Out$LongReal(double x, int n)
{
    printf("%*e", n, x);
}

void Out$Ln()
{
	printf("\n");
    fflush(stdout);
}

void Out$Char(char c)
{
	printf("%c", c );
}

void Out$String(char* str)
{
	printf("%s", str);
}

void Out$Open()
{
}

void Out$begin$()
{
}
