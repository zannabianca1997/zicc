**Intcode** is an esoteric programming language created purely to annoy
programmers competing in [Advent of Code 2019](https://adventofcode.com/2019).
It was featured on days 2, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23 and 25.

## Program Structure

An Intcode program is a list of integers (positive and negative) separated by
commas. In other words, programs will look something like this:

```intcode
1, 3, 4, 2, 99
```

Intcode implements a von Neumann architecture with unbounded memory consisting
of unbounded integer cells indexed starting from 0. The program is copied to the
beginning of the memory before execution starts; the remaining cells are
initialized to 0. The Intcode virtual machine has two registers, an *instruction
pointer* which points to the current instruction, and a *relative base pointer*
used for relative addressing. Initially, both registers are 0.

## Advent of Code

| Day | Program                                                   | Link                                           |
|-----|-----------------------------------------------------------|------------------------------------------------|
| 02  | Gravity Assist                                            | [Day 2](https://adventofcode.com/2019/day/2)   |
| 05  | Thermal Environment Supervision Terminal (TEST)           | [Day 5](https://adventofcode.com/2019/day/5)   |
| 07  | Amplification Circuit                                     | [Day 7](https://adventofcode.com/2019/day/7)   |
| 09  | Basic Operation Of System Test (BOOST)                    | [Day 9](https://adventofcode.com/2019/day/9)   |
| 11  | Emergency Hull Painting Robot                             | [Day 11](https://adventofcode.com/2019/day/11) |
| 13  | Arcade Cabinet                                            | [Day 13](https://adventofcode.com/2019/day/13) |
| 15  | Repair Droid                                              | [Day 15](https://adventofcode.com/2019/day/15) |
| 17  | Aft Scaffolding Control and Information Interface (ASCII) | [Day 17](https://adventofcode.com/2019/day/17) |
| 19  | Tractor Beam Testing Drone                                | [Day 19](https://adventofcode.com/2019/day/19) |
| 21  | Springscript For A Springdroid                            | [Day 21](https://adventofcode.com/2019/day/21) |
| 23  | Network Interface Controller (NIC)                        | [Day 23](https://adventofcode.com/2019/day/23) |
| 25  | Cryostasis                                                | [Day 25](https://adventofcode.com/2019/day/25) |

## Instruction Format

The value pointed to by the *instruction pointer* is read as a decimal value
with digits *ABCDE*. The lower digits *DE* define the *opcode*, whereas *C*,
*B*, *A* define the *parameter mode* of the first, second, and third parameter
of the instruction, respectively (if any).

### Opcodes

| Op | Params | Description                                                                                                  |
|----|--------|--------------------------------------------------------------------------------------------------------------|
| 01 | 3      | Adds the first two arguments and stores the result in the third argument.                                    |
| 02 | 3      | Like 1, but for multiplication.                                                                              |
| 03 | 1      | Inputs a single integer and stores it in the first argument.                                                 |
| 04 | 1      | Outputs the first argument.                                                                                  |
| 05 | 2      | If the first argument is non-zero, sets the instruction pointer to second argument.                          |
| 06 | 2      | Like 5, but jumps if the first argument is zero.                                                             |
| 07 | 3      | If the first argument is less than the second argument, writes 1 to the third argument. Otherwise, writes 0. |
| 08 | 3      | Like 7, but check equality instead.                                                                          |
| 09 | 1      | Adds the first argument to the relative base register.                                                       |
| 99 | N/A    | Halts the program.                                                                                           |


Except after jumps, the instruction pointer is advanced to the next instruction
after the completion of the current one.

### Parameter Modes

Parameter modes impact how arguments are read or written. Three parameter modes
are defined.

* Mode 0, *position mode*: the parameter is the address of a cell to be read or
  written.
* Mode 1, *immediate mode*: the parameter is the value read. (This mode is never
  used for writing.)
* Mode 2, *relative mode*: the parameter is added to the relative base register
  to obtain the address of the cell to be read or written.

### Evolution

The features were introduced incrementally over time.

* Day 2, part 1 defined only opcodes 01, 02 and 99, and had no parameter modes
  (all parameters were treated in position mode).
* Day 5, part 1 added opcodes 03 and 04, and immediate mode.
* Day 5, part 2 added opcodes 05 to 08.
* Day 9, part 1 added opcode 09 and relative mode.

### Proposed Assembly Syntax

The most basic syntax that supports labels could look as follows (in EBNF).

```
input ::= { line '\n' } ;

line  ::= [ identifier ':' ] [ instr | directive ] ;

directive ::= 'DATA' { expr [','] } ;

instr ::= op { param [','] } ;

op    ::= 'ADD' (* 01 *) | 'MUL' (* 02 *) | 'IN'  (* 03 *) | 'OUT'  (* 04 *) | 'JNZ'   (* 05 *)
       | 'JZ' (* 06 *) | 'SLT' (* 07 *) | 'SEQ' (* 08 *) | 'INCB' (* 09 *) | 'HALT' (* 99 *) ;

param ::= [ '#' (* mode 1 *) | '@' (* mode 2 *) ] expr ;

expr  ::= expr '+' expr | expr '-' expr
       | expr '*' expr | expr '/' expr
       | '+' expr | '-' expr
       | '(' expr ')' | identifier | number ; (* with the usual precedence resolution *)
```

Many extensions are possible. One interesting idea is to add labels to
parameters. This would facilitate patching instructions (for example, to
implement indirect addressing).

```intcode
param ::= ... | [ '#' | '@' ] '[' identifier ':' expr ']' ;
```

`.ints` could be a good standard file extension for Intcode assembly.

## Example Programs

### Hello, World!

Outputs are interpreted as ASCII characters.

```intcode
4,3,101,72,14,3,101,1,4,4,5,3,16,99,29,7,0,3,-67,-12,87,-8,3,-6,-8,-67,-23,-10
```

### Count from 1 to 10

```intcode
4,17,4,19,1001,17,1,17,8,17,18,16,1006,16,0,99,-1,1,11,32
```

### Calculate factorial for a given number

The following program calculates the factorial of a number, including [zero factorial](https://en.wikipedia.org/wiki/Factorial#Factorial_of_zero)

```intcode
3, 34, 1007, 34, 1, 35, 1005, 35, 30, 1001, 34, 0, 33, 1001, 33, -1, 33, 1006, 33, 27, 2, 34, 33, 34, 1005, 33, 13, 4, 34, 99, 104, 1, 99
```

### Langton's ant

This works for the hull painting robot introduced on [day 11](https://adventofcode.com/2019/day/11).

```intcode
1101,0,11111,0,3,1,102,-1,1,1,101,1,1,1,4,1,4,1,101,-1,2,2,1005,2,4,99
```

## Computational Class

With the addition of control flow on day 5, Intcode is powerful enough to be Turing-complete.
