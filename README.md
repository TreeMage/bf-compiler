## Brainfuck Compiler/Interpreter

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