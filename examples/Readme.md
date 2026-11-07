The examples in this directory are working applications of significant size to demonstrate the capabilities of Micron.

- Smalltalk: a full Smalltalk-80 Bluebook implementation written in Micron, migrated from [the C++ version](https://github.com/rochus-keller/Smalltalk/)
- P4: the original 1976 P4 Pascal compiler; an original Pascal source migrated to Micron
- S9: the "Scheme 9 from Empty Space" virtual machine, originally implemented in C, migrated to Micron
- OberonSystem: the [Project Oberon System](https://github.com/rochus-keller/OberonSystem), written in Oberon, migrated to Micron, 
  but supposed to look like the original code (upper-case keywords and type names, mostly original types).
- MicronSystem: the native RISC-V Project Oberon System version migrated to Micron, see below. 

More relevant examples can be found in the [Are-we-fast-yet repository](https://github.com/rochus-keller/Are-we-fast-yet/tree/main/Micron):

- the procedural implementation was directly migrated from C and demonstrates a plain procedural programming style
- the hybrid implementation uses a programming style with type-bound procedures and delegates, still on level 3
- the interface implementation, based on hybrid, demonstrates the use of Micron interfaces on level 3
- the object implementation was directly migrated from the C++ version and demonstrates the use of the Micron OO features on level 4

Meanwhile there is also a Micron migration of the Oberon System 3 which runs on an extended version
of the rv32vm with color support. It can be found in the [MicronSystem3 repository](https://github.com/rochus-keller/micronsystem3/).
See in the rv32 subdirectory for the build.sh and run.sh scripts. Again micc is assumed in the bin directory,
and the rootfs directory has to be populated and vm has to be built before the system can be built and run.
