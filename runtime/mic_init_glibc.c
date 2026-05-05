/*
 * mic_init_glibc.c - __mic$init for dynamic linking with glibc.
 *
 * Called from _start in the compiler-generated main+.o (cdecl mode).
 * Calls __mic$main (also in main+.o) which runs all module begin$ procedures,
 * then flushes stdio buffers and exits.
 *
 * glibc's dynamic linker initializes the C runtime (TLS, stdio, etc.)
 * before _start runs, so no explicit libc init is needed here.
 *
 * Compile: gcc -m32 -c mic_init_glibc.c -o mic_init_glibc.o
 */

#include <stdio.h>
#include <unistd.h>

extern int __mic$main(void);
extern void Args_setArgcArgv(int argc, char** argv);

void __mic$init(int argc, char** argv)
{
    Args_setArgcArgv(argc, argv);
    __mic$main();
    fflush(NULL);
    _exit(0);
}
