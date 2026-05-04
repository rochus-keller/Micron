#include "Args.h"

static unsigned int argc = 0;
static char** argv = 0;

unsigned int Args$Count()
{
    return argc;
}

char* Args$Arg(unsigned int i)
{
    if( argv )
        return argv[i];
    else
        return "";
}

void Args_setArgcArgv(unsigned int c, char** v)
{
    argc = c;
    argv = v;
}

#ifndef _MIC_NO_BEGIN_
void Args$begin$()
{
}
#endif
