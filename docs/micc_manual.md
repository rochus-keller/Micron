# micc – the Micron compiler

    micc [options] <main> [objects...]

`<main>` is either a module (`.mic`, `.mil`, `.mob`) or a project (`.micpro`).
Unlike a C compiler, micc is given only the *main* module: 
all imported modules are found and compiled as well, and the result is linked in one go.
Additional positional arguments are foreign object files (`.o`) and archives (`.a`) which
are handed to the linker unchanged.

    micc --target rv32 -g --base 0 --runtime MIC+.mil -I . -o Main Main.mic

## Modes

The four modes are mutually exclusive; the default is to compile and link.

| mode              | what it does |
|-------------------|--------------|
| *(none)*          | compile all modules for `--target` and link the executable |
| `-c`, `--compile` | compile all modules, don't link |
| `--check`         | only parse and check, generate no code |
| `--run`           | run the program in the interpreter; no code generation, no linking |

`--target` is required in the default and `-c` modes and rejected in `--check` and `--run`.
Nothing is generated and nothing is linked when a module has errors.

## Output

Everything micc generates goes into one directory, `--out-dir`,
which defaults to the directory of the main module:

| file           | content |
|----------------|---------|
| `<Module>.mob` | the object file of a Micron module, with its MIL interface |
| `main+.o`      | the generated program entry, calling the module initializers |
| `*.mil/mll/mrl`| the dumps requested with `--dump` |
| `<Main>`       | the executable, unless `-o` names another path |

`-o` names the executable only, and is a usage error together with `-c`,
`--check` or `--run`, since those modes have no single output. 
A `$` in a module name becomes `+` in the file name, so `MIC$` is written as `MIC+.mob`.

## Modules

`-I <path>` adds a directory in which imported modules are looked up.
For every import, micc searches all `-I` directories for `<Name>.mic` first,
then for `<Name>.mil`, then for `<Name>.mob`, i.e. source is preferred over a prebuilt artifact.
The directory of the main module is searched first.
`--verbose` reports the format chosen for each module.

`--runtime <file>` replaces the built-in declarations of the runtime module `MIC$` by a MIL implementation,
which is what a freestanding target needs.
The argument is a path, or a file name resolved through `-I`.

A project (`.micpro`) lists its module files itself; 
`-I` is used in addition, for imports which are not part of the project.

## Libraries

| option      | meaning |
|-------------|---------|
| `-L <path>` | a directory searched for archives |
| `-l <name>` | link with `lib<name>.a`, searched in the `-L` directories |
| `<file>.o/a`| positional: a foreign object or archive |

`.o` is reserved for foreign objects, which carry no Micron interface.
The `.mob` files of prebuilt modules found through `-I` are added to the link automatically.

## Targets

`--target rv32 | rv32-esp32 | armv7 | x86`

| option         | meaning |
|----------------|---------|
| `--abi default`| the stack-only Micron calling convention, available on every target (default) |
| `--abi c`      | the convention close to the platform's: the RV ABI on rv32, AAPCS32 on armv7, cdecl returns on x86 |
| `--no-float`   | the target has no floating point unit (rv32 only) |
| `--no-hwdiv`   | the target has no divide instruction (rv32 and armv7) |
| `--base <hex>` | the load address of the executable |

`--abi c` is *close to* the platform ABI, not identical to it.
An option which does not exist for the chosen target is a usage error.

## Dumps and diagnostics

`--dump=mil|mll|mrl` writes per module the MIL assembly (`mil`),
the low-level code of the interpreter (`mll`) or
the register-level code (`mrl`) into the output directory. 
The option can be repeated and combines with `--run`.

`-g` generates debug information (and row information in the `mil` dump).
`--verbose` reports each module format and each linker input, `-q` reports only warnings and errors.

Anything after `--` on the command line is passed to the program run by `--run`.

## Exit codes

| code | meaning |
|------|---------|
| 0    | success |
| 1    | a module has errors, or the interpreted program failed |
| 2    | the link failed |
| 3    | the command line is wrong |

## Examples

    # build the Micron System for the RISC-5 like RV32 machine
    micc --target rv32 -g --base 0 --runtime MIC+.mil -I . -o Main Main.mic

    # compile only, into a separate directory
    micc -c --target rv32 --out-dir build -I . Main.mic

    # check the sources
    micc --check -I . Main.mic

    # run on the interpreter and dump the low-level code
    micc --run --dump=mll Arith1.mic

    # link with a foreign library
    micc --target x86 --abi c -L /usr/lib32 -l c crt.o Main.mic
