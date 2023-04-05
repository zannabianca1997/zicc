# Assembler

This is a really basic assembler, capable of generating the basic instructions and some simple combinatioon of thems.

## Reference

### Labels
Labels can be global, local or numeric. Local labels starts with `.`, numeric one start with `$`. usual C identifier rules apply for names of locals and globals.
A line of code can be labelled as such:
```
    label1: label2: [...code...]
```
Every parameter that will be expanded to a single code location can be labelled:
```
    add $1: 1 $2: 2 $3: 3
```

### Parameters
Paramenters can be either numeric values `3`, or labels. Labels can carry an offset: `a`, `a+3`, `a-3`. Mode can be specified with the letters `#` for  immediate, and `@` for relative.
Parameters can be split either by whitespace, or `,`.

### Instructions
The basic intcode instruction are all presents:
| Assembly    |                                           |
| ----------- | ----------------------------------------- |
| `add a b c` | Add `a` and `b` into `c`                  |
| `mul a b c` | Multiply `a` and `b` into `c`             |
| `in a`      | Read input into `a`                       |
| `out a`     | Output the content of `a`                 |
| `jnz a b`   | If `a` is not zero, jump to `b`           |
| `jz a b`    | If `a` is zero, jump to `b`               |
| `slt a b c` | Set `c` to 1 if `a<b`, else set `c` to 0  |
| `seq a b c` | Set `c` to 1 if `a==b`, else set `c` to 0 |
| `incb a`    | Add `a` to the relative base              |
| `halt`      | Stop the machine                          |

### Additional instruction-like directives
Some directive looks just like instruction, and assemble to them
| Assembly        |                                                                                                                               | Warning                                                                                  |
| --------------- | ----------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| `jmp a`         | Unconditionally jumps to `a`                                                                                                  |                                                                                          |
| `mov a b [n]`   | Move the content of `a` into `b`. If `n` is present, move `n`consecutive memory locations <sup>[1]</sup>.                     | `n` is a simple unsigned int. If `n` is different than 1, `a` and `b` cannot be labelled |
| `load a b [n]`  | Move the content of the cell pointed by `a` into `b`. If `n` is present, move `n`consecutive memory locations.                | `n` is a simple unsigned int. If `n` is different than 1, `a` and `b` cannot be labelled |
| `store a b [n]` | Move the content of `a` into the cell pointed by `b`. If `n` is present, move `n`consecutive memory locations <sup>[1]</sup>. | `n` is a simple unsigned int. If `n` is different than 1, `a` and `b` cannot be labelled |
| `push a [n]`    | Push `a` on top of the relative stack. If `n` is present, push `n`consecutive memory locations.                               | `n` is a simple unsigned int. If `n` is different than 1, `a` cannot be labelled         |
| `pop a [n]`     | Pop the top of the relative stack into `a`. If `n` is present, move `n`consecutive memory locations <sup>[1]</sup>.           | `n` is a simple unsigned int. If `n` is different than 1, `a` cannot be labelled         |
1) If `n` is present, and `a` is immediate, the value of `a` is copied on every destination

### Embedded data
These are directives that produce non-code values:
| Assembly      |                           | Warning                                                |
| ------------- | ------------------------- | ------------------------------------------------------ |
| `data a b ..` | Expand to it's parameters | Parameters are simple values, do not put modes on them |
| `zeros n`     | Expand to `n` zeros       | `n` is a simple unsigned int                           |
