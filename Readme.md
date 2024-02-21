This project implements a compiler for the [Micron programming language](https://github.com/micron-language/specification).

Micron is a systems programming language with a syntax similar to Oberon+ and the flexibility of C. The name "Micron" is an abbreviation of "MicroOberon". 

The project is the result of the author's many years of work with the Oberon programming language and the analysis and migration of several versions of the Oberon system. It turned out that Oberon as such is in fact not particularly suitable for low-level and system-oriented programming, and that the systems mentioned could only be developed by direct memory manipulation and pointer arithmetic via SYSTEM module and other (partly undocumented) backdoors of the language, for large parts without the support of a type checker. 

The language of choice for such tasks is usually C, with which the author has been able to gain experience over many years and various projects and architectures down to different microcontrollers. A major advantage of C is its flexibility. The compiler provides fairly good support, but does not get in the way. However, C also has considerable disadvantages, such as the lack of modularization, the complications caused by the preprocessor, or the syntax that is by no means intuitive, just to name but a few. 

This project attempts to create a programming language and toolchain that combines the modularization, strictness and simplicity of Oberon with the power and flexibility of C. The language is aimed at experienced developers who know how to deal with the risks of direct memory and pointer manipulation. The preprocessor is replaced by language concepts that can largely substitute its features, supplemented by explicit support for generic programming, inlining and procedures that run at compile time. Thereby the focus is always on simplicity and the greatest effect with a practicable effort.

NOTE that this project is in an early stage and work-in-progress.

#### Planned features

- [x] Implement lexer with directive support
- [x] Implement parser
- [ ] Migrate some notable C projects to Micron as a proof-of-concept and to optimize the language
- [ ] Implement the analyzer/validator 
- [ ] Implement a C99 generator
- [ ] Implement a native backend


#### Status on February 4, 2024

Generated a parser using [the grammar](https://github.com/micron-language/specification/blob/master/Micron_Programming_Language.ebnf) and adopted the lexer from [the Oberon project](https://github.com/rochus-keller/Oberon) and the conditional compilation machinery from the Oberon and the [FreePascal](https://github.com/rochus-keller/FreePascal/) project. The parser is able to successfully read initial test files.

#### Status on February 20, 2024

Meanwhile I prepared a manual Micron parser and designed an intermediate language (see [the grammar](https://github.com/micron-language/specification/blob/master/Micron_Intermediate_Language.ebnf) and [the specification](https://htmlpreview.github.io/?https://github.com/micron-language/specification/blob/master/The_Micron_Intermediate_Language_Specification.html)) which the Micron frontend will generate. There is also an automatically generated parser for the intermediate language which successfully works with initial test files.


#### Precompiled versions

Not available at this time.

#### How to build

Qt Creator 3.x is currently used for development. There are *.pro files which are compatible with qmake, but there is no Qt dependency otherwise.
