The MicronSystem is a 
[faithful migration of the Project Oberon System to Micron natively running on RISC-V](https://github.com/rochus-keller/OberonSystem/tree/micron-rv32),
copied here for convenience. We call it Micron System because, in contrast to the OberonSystem example, the code follows the Micron 
instead of the Oberon style and makes use of many Micron language features.
The MicronSystem is also discussed in [the Micron book](https://github.com/micron-language/textbook/). 
It runs on the 
[virtual machine of the Oberon 90 version of the Project Oberon System](https://github.com/rochus-keller/OberonSystem/tree/op2-rv32/vm), 
which is a reconstruction of the machine Wirth described in his 
[Project Oberon Book](http://www.inf.ethz.ch/personal/wirth/ProjectOberon/PO.Computer.pdf),
but based on RISC-V (instead of Wirth's RISC5 processor). 

# How to build and run

It is recommended that you download the repository from
[github.com/rochus-keller/OberonSystem/tree/micron-rv32](https://github.com/rochus-keller/OberonSystem/archive/refs/heads/micron-rv32.zip)
unpack it, and then copy micc into the bin directory. 
Then open a terminal in the directory and run the following commands:

- `vm/build.sh`
- `./build.sh run`

You should then see the typical Oberon System screen with white text and lines on black background.


If you already have a `rv32vm` executable an a `disk.img`, you can als run the following commands in this directory instead:

- `<path_to>/micc --target rv32 -g --base 0 --runtime MIC+.mil -I . -o system.bin Main.mic`
- `<path_to>/rv32vm --elf --ram 4 --disk <path_to>/disk.img system.bin`


