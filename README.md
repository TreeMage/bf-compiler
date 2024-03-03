## Brainfuck Compiler/Interpreter 
[![Scala CI](https://github.com/TreeMage/bf-compiler/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/TreeMage/bf-compiler/actions/workflows/build.yml)

When completed this is supposed to be able to interpret brainfuck code as well as compiling it to native code.

### Brainfuck Specification

| Instruction | Definition                                                                                                                                                                         |
|-------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| \>          | 	Increment the data pointer by one (to point to the next cell to the right).                                                                                                       |
| <           | 	Decrement the data pointer by one (to point to the next cell to the left).                                                                                                        |
| +           | 	Increment the byte at the data pointer by one.                                                                                                                                    |
| -           | 	Decrement the byte at the data pointer by one.                                                                                                                                    |
| .           | 	Output the byte at the data pointer.                                                                                                                                              |
| ,           | 	Accept one byte of input, storing its value in the byte at the data pointer.                                                                                                      |
| [           | 	If the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command. |
| ]           | 	If the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command. |

Anything following a `//` is considered a comment and ignored.

### Usage
This can process brainfuck programs in the two following ways
#### Interpretation
A given brainfuck program can be interpreted via the `interpret` CLI subcommand:
```shell
./bf-compiler interpret [OPTIONS] <path-to-program>
```
The `interpret` subcommand provides additional options such as a `--debug` flag. 
Check the usage instructions via the `--help` flag for more information.

#### Compilation
Next to interpretation, this tool is able to compile a given brainfuck program into native code.
More specifically, it generates the corresponding assembly for each instruction and then uses an assembler 
(e.g. `as` on macOS) and linker (e.g `ld`) to create an executable binary.
To compile a program, run the following command
```shell
./bf-compiler compile [OPTIONS] <path-to-program> <output-directory>
```
This will generate an executable with the same name as the input file in the `<output-directory>`
Similar to the  `interpret` subcommand, this command also provides additional options such as a `--run` flag.
Check the usage instructions via the `--help` flag for more information.

*NOTE*: Native compilation is currently only supported for Apple silicon.
