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
        Line::Instruction(Instruction(instr)) => {
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
        Line::Directive(Directive::Data(mut values)) => line_content.append(&mut values),
        Line::Directive(Directive::Zeros(count)) => line_content.extend(repeat_n(
            Labelled::unlabelled(Expr::Constant {
                value: IntLiteral::ZERO,
            }),
            Value::from(count).try_into().unwrap(),
        )),
    };
    Ok(())
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
