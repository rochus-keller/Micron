#include "Files.h"
#include <assert.h>
#include <stdlib.h>

static Files$Handle Stdin = {0}, Stdout = {0}, Stderr = {0};

struct Files$Handle* Files$Open(const char* name, int mode)
{
    // ReadOnly = 0, Create = 1, Modify = 2
    const char* m = mode == 0 ? "rb" : mode == 1 ? "wb+" : "rb+";
    FILE* f = fopen(name, m);
    if( f )
    {
        Files$Handle* res = (Files$Handle*)malloc(sizeof(Files$Handle));
        res->f = f;
        return res;
    }
    return 0;
}
 
struct Files$Handle* Files$System(int what)
{
    // Stdin = 0, Stdout = 1, Stderr = 2
    switch(what)
    {
    case 0:
        Stdin.f = stdin;
        return &Stdin;
    case 1:
        Stdout.f = stdout;
        return &Stdout;
    case 2:
        Stderr.f = stderr;
        return &Stderr;
    default:
        assert(0);
    }

    return 0;
}

void Files$Close(struct Files$Handle* f)
{
    if( f )
    {
        fclose(f->f);
        free(f);
    }
}

unsigned char Files$Delete(char* name)
{
    return remove(name) == 0;
}

unsigned char Files$Eof(struct Files$Handle* f)
{
    return feof(f->f);
}

unsigned char Files$Read(struct Files$Handle* f, unsigned char* x)
{
    const int count = fread(x, 1, 1, f->f);
    return count == 1;
}

unsigned char Files$Write(struct Files$Handle* f, unsigned char x)
{
    const int count = fwrite(&x, 1, 1, f->f);
    return count == 1;
}


// TODO

