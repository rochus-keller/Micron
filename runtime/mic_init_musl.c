/*
 * mic_init_musl.c - __mic$init for static linking with musl.
 *
 * Called from _start in the compiler-generated main+.o (cdecl mode).
 * Delegates to musl's __libc_start_main which initializes TLS, stdio,
 * and other runtime state, then calls main_wrapper which invokes
 * __mic$main (also in main+.o) to run all module begin$ procedures.
 *
 * musl's __libc_start_main never returns; it calls exit() after main.
 *
 * Compile: gcc -m32 -nostdinc -isystem <musl>/include -c mic_init_musl.c -o mic_init_musl.o
 */

extern int __mic$main(void);

/* musl's entry point for C runtime initialization */
int __libc_start_main(int (*)(int, char**, char**), int, char**);

static int __mic$main_wrapper(int argc, char** argv, char** envp)
{
    (void)argc;
    (void)argv;
    (void)envp;
    return __mic$main();
}

void __mic$init(int argc, char** argv)
{
    __libc_start_main(__mic$main_wrapper, argc, argv);
}
