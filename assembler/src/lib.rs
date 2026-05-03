#![doc = include_str!("../README.md")]

use std::iter::repeat_n;

use snafu::Snafu;
use zicc_assembler_ast::{
    File, Line,
    directive::Directive,
    expression::{Expr, IntLiteral},
    instruction::Instruction,
    labelled::Labelled,
};
use zicc_assembler_program::{Program, ProgramInfo};
use zicc_intcode::{ReadParamMode, WriteParamMode};
use zicc_value::Value;

pub mod cli;

#[derive(Debug, Snafu)]
pub enum AssembleError {}

/// Assemble a program into an objects unit
pub fn assemble(
    Program {
        info,
        content:
            File {
                lines,
                trailing:
                    Labelled {
                        labels: trailing_labels,
                        ..
                    },
            },
    }: Program,
) -> Result<zicc_linker_program::Program, AssembleError> {
    // Instructions have almost all at least two params
    let mut content = Vec::with_capacity(lines.len() * 3);
    let mut line_content = Vec::with_capacity(4);

    for Labelled { labels, item: line } in lines {
        write_line(line, &mut line_content);
        let mut line_content = line_content.drain(..);

        let mut line_start = line_content
            .next()
            .expect("Each assembler line generates at least a value");
        line_start.merge(Labelled { labels, item: () });
        content.push(line_start);

        content.extend(line_content);
    }

    if !trailing_labels.is_empty() {
        content.push(Labelled {
            labels: trailing_labels,
            item: Expr::Constant {
                value: IntLiteral::ZERO,
            },
        });
    }

    Ok(zicc_linker_program::Program {
        info: program_info(info),
        content,
    })
}

fn write_line(line: Line, line_content: &mut Vec<Labelled<Expr>>) -> Result<(), AssembleError> {
    match line {
        Line::Instruction(instr) => {
            write_instr(instr, line_content);
        }
        Line::Directive(directive) => {
            write_directive(directive, line_content);
        }
    };
    Ok(())
}

fn write_directive(directive: Directive, line_content: &mut Vec<Labelled<Expr>>) {
    use zicc_intcode::Instruction::*;

    match directive {
        Directive::Data(mut values) => line_content.append(&mut values),
        Directive::Zeros(count) => line_content.extend(repeat_n(
            Labelled::unlabelled(Expr::Constant {
                value: IntLiteral::ZERO,
            }),
            Value::from(count).try_into().unwrap(),
        )),
        Directive::Jmp(target) => {
            // `JEZ #0 {target}`
            write_instr(
                Instruction(Jez(
                    (
                        ReadParamMode::Immediate,
                        Labelled::unlabelled(IntLiteral::ZERO.into()),
                    ),
                    target,
                )),
                line_content,
            );
        }
        Directive::Inc((mode, target)) => {
            // `ADD <target> #1 <target>`
            write_instr(
                Instruction(Add(
                    (
                        mode.into(),
                        Labelled::unlabelled(target.clone()),
                    ),
                    (
                        ReadParamMode::Immediate,
                        Labelled::unlabelled(IntLiteral::ONE.into()),
                    ),
                    (
                        mode.into(),
                        Labelled::unlabelled(target),
                    ),
                )),
                line_content,
            );
        }
        Directive::Dec((mode, target)) => {
            // `ADD <target> #-1 <target>`
            write_instr(
                Instruction(Add(
                    (
                        mode.into(),
                        Labelled::unlabelled(target.clone()),
                    ),
                    (
                        ReadParamMode::Immediate,
                        Labelled::unlabelled((-IntLiteral::ONE).into()),
                    ),
                    (
                        mode.into(),
                        Labelled::unlabelled(target),
                    ),
                )),
                line_content,
            );
        }
        Directive::Mov(src, (dst_mode, dst)) => {
            // `ADD <src> #0 <dst>`
            write_instr(
                Instruction(Add(
                    src,
                    (
                        ReadParamMode::Immediate,
                        Labelled::unlabelled(IntLiteral::ZERO.into()),
                    ),
                    (dst_mode.into(), dst),
                )),
                line_content,
            );
        }
        Directive::Push(value) => {
            // `INB #1; MOV <value> @-1`
            write_instr(
                Instruction(Inb((
                    ReadParamMode::Immediate,
                    Labelled::unlabelled(IntLiteral::ONE.into()),
                ))),
                line_content,
            );
            write_directive(
                Directive::Mov(
                    value,
                    (
                        WriteParamMode::Relative,
                        Labelled::unlabelled((-IntLiteral::ONE).into()),
                    ),
                ),
                line_content,
            );
        }
        Directive::Pop(dst) => {
            // `INB #-1 [; MOV @0 <dst>]`
            write_instr(
                Instruction(Inb((
                    ReadParamMode::Immediate,
                    Labelled::unlabelled((-IntLiteral::ONE).into()),
                ))),
                line_content,
            );
            if let Some(dst) = dst {
                write_directive(
                    Directive::Mov(
                        (
                            ReadParamMode::Relative,
                            Labelled::unlabelled(IntLiteral::ZERO.into()),
                        ),
                        dst,
                    ),
                    line_content,
                );
            }
        }
        Directive::Call(_target) => {
            todo!("PUSH # $1; JMP <target>; $1: POP")
        }
        Directive::Ret => todo!("JMP @-1"),
    }
}

fn write_instr(Instruction(instr): Instruction, line_content: &mut Vec<Labelled<Expr>>) {
    line_content.push(Labelled::unlabelled(Expr::Constant {
        value: instr.code().into(),
    }));

    match instr {
        zicc_intcode::Instruction::Add((.., a), (.., b), (.., c))
        | zicc_intcode::Instruction::Mul((.., a), (.., b), (.., c))
        | zicc_intcode::Instruction::Slt((.., a), (.., b), (.., c))
        | zicc_intcode::Instruction::Seq((.., a), (.., b), (.., c)) => {
            line_content.push(a);
            line_content.push(b);
            line_content.push(c);
        }
        zicc_intcode::Instruction::Jnz((.., a), (.., b))
        | zicc_intcode::Instruction::Jez((.., a), (.., b)) => {
            line_content.push(a);
            line_content.push(b);
        }
        zicc_intcode::Instruction::Inp((.., a))
        | zicc_intcode::Instruction::Out((.., a))
        | zicc_intcode::Instruction::Inb((.., a)) => {
            line_content.push(a);
        }
        zicc_intcode::Instruction::Hlt => {}
    }
}

fn program_info(
    ProgramInfo {
        name,
        author,
        metadata,
        ..
    }: ProgramInfo,
) -> zicc_linker_program::ProgramInfo {
    zicc_linker_program::ProgramInfo {
        name,
        author,
        metadata,
        ..Default::default()
    }
}
