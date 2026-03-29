#include <math.h>

float Math$sqrt(float x)
{
    return sqrtf(x);
}

float Math$power(float x, float b)
{
    return pow(b,x);
}

float Math$exp(float x)
{
    return exp(x);
}

float Math$ln(float x)
{
    return log(x);
}

float Math$log(float x, float base)
{
    return log10(x);
}

float Math$round(float x)
{
    return round(x);
}

float Math$sin(float x)
{
    return sin(x);
}

float Math$cos(float x)
{
    return cos(x);
}

float Math$tan(float x)
{
    return tan(x);
}

float Math$arcsin(float x)
{
    return asin(x);
}

float Math$arccos(float x)
{
    return acos(x);
}

float Math$arctan(float x)
{
    return atan(x);
}

float Math$arctan2(float x, float y)
{
    return atan2(x,y);
}

float Math$sinh(float x)
{
    return sinh(x);
}

float Math$cosh(float x)
{
    return cosh(x);
}

float Math$tanh(float x)
{
    return tanh(x);
}

float Math$arcsinh(float x)
{
    return asinh(x);
}

float Math$arccosh(float x)
{
    return acosh(x);
}

float Math$arctanh(float x)
{
    return atanh(x);
}

#ifndef _MIC_NO_BEGIN_
void Math$begin$()
{
}
#endif


