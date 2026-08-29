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

The MicronSystem is a 
[faithful migration of the Project Oberon System to Micron natively running on RISC-V](https://github.com/rochus-keller/OberonSystem/tree/micron-rv32),
copied here for convenience. We call it Micron System because, in contrast to the OberonSystem example, the code follows the Micron 
instead of the Oberon style and makes use of many Micron language features. The MicronSystem is also discussed in the forthcoming Micron book. 
It runs on the 
[virtual machine of the Oberon 90 version of the Project Oberon System](https://github.com/rochus-keller/OberonSystem/tree/op2-rv32/vm), 
which is a reconstruction of the machine Wirth described in his 
[Project Oberon Book](http://www.inf.ethz.ch/personal/wirth/ProjectOberon/PO.Computer.pdf),
but based on RISC-V (instead of Wirth's RISC5 processor). 
