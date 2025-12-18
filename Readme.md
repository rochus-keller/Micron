This project implements a compiler for the [Micron programming language](https://github.com/micron-language/specification).

Micron is a systems programming language with a syntax similar to Oberon+ and the flexibility of C. The name "Micron" is an abbreviation of "MicroOberon". 

Code examples can be found [here](https://github.com/rochus-keller/Are-we-fast-yet/tree/main/Micron); and [here](https://github.com/rochus-keller/Micron/blob/master/testcases/mil/awfy.mil) is the corresponding intermediate representation (text version).

The project is the result of the author's many years of work with the Oberon programming language and the analysis and migration of several versions of the Oberon system. It turned out that Oberon as such is in fact not particularly suitable for low-level and system-oriented programming, and that the systems mentioned could only be developed by direct memory manipulation and pointer arithmetic via SYSTEM module and other (partly undocumented) backdoors of the language, for large parts without the support of a type checker. 

The language of choice for such tasks is usually C, with which the author has been able to gain experience over many years and various projects and architectures down to different microcontrollers. A major advantage of C is its flexibility. The compiler provides fairly good support, but does not get in the way. However, C also has considerable disadvantages, such as the lack of modularization, the complications caused by the preprocessor, or the syntax that is by no means intuitive, just to name but a few. 

This project attempts to create a programming language and toolchain that combines the modularization, strictness and simplicity of Oberon with the power and flexibility of C. The language is aimed at experienced developers who know how to deal with the risks of direct memory and pointer manipulation. The preprocessor is replaced by language concepts that can largely substitute its features, supplemented by explicit support for generic programming, inlining and procedures that run at compile time. Thereby the focus is always on simplicity and the greatest effect with a practicable effort.
 
NOTE that this project is in an early stage and work-in-progress.

#### Planned features

- [x] Implement lexer with directive support
- [x] Implement parser and IR generator
- [x] Evaluate an existing compiler backend
- [x] Implement the analyzer/validator 
- [x] Implement a C99 generator
- [x] Implement an IDE with source navigation and debugger
- [x] Complete language and add explicit language levels
- [x] Implement a MIL interpreter for the full language scope (WIP)
- [ ] Implement a native backend
- [ ] Migrate some notable C projects to Micron as a proof-of-concept and to optimize the language


#### Status on February 4, 2024

Generated a parser using [the grammar](https://github.com/micron-language/specification/blob/master/Micron_Programming_Language.ebnf) and adopted the lexer from [the Oberon project](https://github.com/rochus-keller/Oberon) and the conditional compilation machinery from the Oberon and the [FreePascal](https://github.com/rochus-keller/FreePascal/) project. The parser is able to successfully read initial test files.

#### Status on February 20, 2024

Meanwhile I prepared a manual Micron parser and designed an intermediate language (see [the grammar](https://github.com/micron-language/specification/blob/master/Micron_Intermediate_Language.ebnf) and [the specification](https://htmlpreview.github.io/?https://github.com/micron-language/specification/blob/master/The_Micron_Intermediate_Language_Specification.html)) which the Micron frontend will generate. There is also an automatically generated parser for the intermediate language which successfully works with initial test files.

#### Status on May 15, 2024

The programming and intermediate language have matured in parallel with the parser and code generator implementation. The IR code generator is integrated with the parser and operates in the same pass. Though not all parts of the language are yet supported by the IR generator (e.g. imports, VLAs and generics). Instead I was on a long quest for a suitable compiler backend that I could re-use for Micron (and other of my projects). The journey brought me to all sorts of toolkits and re-targetable compilers and I spent a lot of time studying and experimenting. There would be enough material for a treatise of its own. I also invested a lot of time in TCC, trying to modularize the code (that spaghetti lovers would be delighted with) and work out a minimal backend interface; I even started to add another frontend and had to grit my teeth over the TCC functionality, which - despite the introduction of modularization - turned out to be still almost incomprehensible. During this phase, [user tarkonian drew my attention to the Eigen Compiler Suite](https://github.com/rochus-keller/Oberon/discussions/55), which looked very promising. After a thorough review of the large code base and excellent documentation, and an extensive exchange with the author, I came to the conclusion that Eigen is probably the best option currently available. It supports many more architectures than TCC (or e.g. QBE, Firm, , and the linker is also integrated (as with TCC), so you don't need any other dependencies, and the generated code [is significantly faster than that of TCC](https://software.openbrace.org/projects/ecs/activity?from=2024-03-28). I also managed to make the relevant parts [build with GCC 4.8 and MSVC 2015](https://github.com/rochus-keller/EiGen). The current focus is on implementing an Eigen IR generator for Micron; this will likely require more supporting activities (like e.g. an EBNF for the IR and translation examples with Oberon and C).

#### Status on December 18, 2025

The second year of the project is coming to an end, and a lot has happened. The language has matured in every aspect. We now have matured language levels, which optimize language 
features for different use cases, from low-level kernel and driver development up to high-level programming with support of a garbage collector, and which can be set per module. Significant expansions went into the type system and object-oriented capabilities (with IS operator and type case), interface types (similar to Go) and bound procedures for both records and objects, comprehensive support for generic programming through WHERE clauses and improved type constraints. There are now also bit fields (including packed variants), finally sections for resource cleanup (as a simple alternative for C++ destructors). Constructors became more flexible, allowing non-const and index components, and the @ operator now works with constructors. External interoperability improved with extern C declarations. Several features were removed for simplification: varargs support, symbol enums, and proc aliases.
Several large-scale refactorings improved code organization. And IDE was added, adopted from Oberon and Luon projects, integrating comprehensive cross-reference support covering all symbol uses. A MIL interpreter was integrated into the IDE with redirected output to IDE log and export capabilities for low-level bytecode. Most MIC/MIL features from the specification are implemented. The MIL interpreter underwent dramatic performance optimizations, so that it now achieves about 60% of Lua 5.4 (which of course is still too slow). Four distinct code generation backends for the MIL intermediate language have been implemented. The CeeGen (C generator) was fully completed and works with all testcases and AWFY. Experimental work began on EiGen, LLVM 4 IR, and CIL, though these remained work-in-progress.
A lot of time has been invested in the analysis of the Oberon System 3 and AOS/Bluebottle native implementations to assure that this code can be represented in Micron without the SYSTEM module tricks, loss of type checking, or inline assembler.

#### Precompiled versions

Not available at this time.

#### How to build

Qt Creator 3.x is currently used for development. There are *.pro files which are compatible with qmake, but there is no Qt dependency otherwise.
