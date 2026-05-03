#![doc = include_str!("../README.md")]

use std::{collections::BTreeMap, iter::repeat_n, mem};

use snafu::Snafu;
use zicc_assembler_ast::{
    File, Line,
    directive::Directive,
    expression::{Expr, IntLiteral},
    identifier::Identifier,
    instruction::Instruction,
    labelled::Labelled,
};
use zicc_assembler_program::{Program, ProgramInfo};
use zicc_intcode::{ReadParamMode, WriteParamMode};
use zicc_value::Value;

pub mod cli;

#[derive(Debug, Snafu)]
pub enum AssembleError {}

/// Rewrite all the anonymous identifiers in the file AST
///
/// Starts from `start_at`, and moves it forward to track the first free label
fn reassign_anonymous_in_file(file: &mut File, start_at: &mut u32) {
    let mut mapping = BTreeMap::new();
    let mut map_code = |code| {
        *mapping.entry(code).or_insert_with(|| {
            let value = *start_at;
            *start_at += 1;
            value
        })
    };

    for line in file.lines.iter_mut() {
        line.labels = mem::take(&mut line.labels)
            .into_iter()
            .map(|l| match l {
                Identifier::Unnamed { code } => Identifier::Unnamed {
                    code: map_code(code),
                },
                other => other,
            })
            .collect();

        match &mut line.item {
            Line::Instruction(Instruction(inst)) => {
                rewrite_instruction(inst, &mut map_code);
            }
            Line::Directive(directive) => {
                rewrite_directive(directive, &mut map_code);
            }
        }
    }

    file.trailing.labels = mem::take(&mut file.trailing.labels)
        .into_iter()
        .map(|l| match l {
            Identifier::Unnamed { code } => Identifier::Unnamed {
                code: map_code(code),
            },
            other => other,
        })
        .collect();
}

fn rewrite_instruction(
    inst: &mut zicc_intcode::Instruction<Labelled<Expr>>,
    map_code: &mut impl FnMut(u32) -> u32,
) {
    use zicc_intcode::Instruction::*;

    match inst {
        Add((.., a), (.., b), (.., c))
        | Mul((.., a), (.., b), (.., c))
        | Slt((.., a), (.., b), (.., c))
        | Seq((.., a), (.., b), (.., c)) => {
            rewrite_labelled(a, map_code);
            rewrite_labelled(b, map_code);
            rewrite_labelled(c, map_code);
        }
        Jnz((.., a), (.., b)) | Jez((.., a), (.., b)) => {
            rewrite_labelled(a, map_code);
            rewrite_labelled(b, map_code);
        }
        Inp((.., a)) | Out((.., a)) | Inb((.., a)) => {
            rewrite_labelled(a, map_code);
        }
        Hlt => {}
    }
}

fn rewrite_directive(directive: &mut Directive, map_code: &mut impl FnMut(u32) -> u32) {
    match directive {
        Directive::Data(values) => {
            for value in values.iter_mut() {
                rewrite_labelled(value, map_code);
            }
        }
        Directive::Zeros(_) => {}
        Directive::Jmp((_, target)) => rewrite_labelled(target, map_code),
        Directive::Inc((_, target)) | Directive::Dec((_, target)) => {
            if let Expr::Offset { label, .. } = target {
                if let Identifier::Unnamed { code } = label {
                    *code = map_code(*code);
                }
            }
        }
        Directive::Mov((_, src), (_, dst)) => {
            rewrite_labelled(src, map_code);
            rewrite_labelled(dst, map_code);
        }
        Directive::Push((_, value)) => rewrite_labelled(value, map_code),
        Directive::Pop(dst) => {
            if let Some((_, dst)) = dst {
                rewrite_labelled(dst, map_code);
            }
        }
        Directive::Call((_, target)) => rewrite_labelled(target, map_code),
        Directive::Ret => {}
    }
}

fn rewrite_labelled(labelled: &mut Labelled<Expr>, map_code: &mut impl FnMut(u32) -> u32) {
    labelled.labels = mem::take(&mut labelled.labels)
        .into_iter()
        .map(|l| match l {
            Identifier::Unnamed { code } => Identifier::Unnamed {
                code: map_code(code),
            },
            other => other,
        })
        .collect();
    if let Expr::Offset { label, .. } = &mut labelled.item {
        if let Identifier::Unnamed { code } = label {
            *code = map_code(*code);
        }
    }
}

/// Assemble a program into an objects unit
pub fn assemble(
    Program {
        info,
        content: mut file,
    }: Program,
) -> Result<zicc_linker_program::Program, AssembleError> {
    let mut next_free = 0;
    reassign_anonymous_in_file(&mut file, &mut next_free);

    let File {
        lines,
        trailing: Labelled {
            labels: trailing_labels,
            ..
        },
    } = file;

    // Instructions have almost all at least two params
    let mut content = Vec::with_capacity(lines.len() * 3);
    let mut line_content = Vec::with_capacity(4);

    for Labelled { labels, item: line } in lines {
        write_line(line, &mut line_content, &mut next_free);
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

fn write_line(
    line: Line,
    line_content: &mut Vec<Labelled<Expr>>,
    next_free: &mut u32,
) -> Result<(), AssembleError> {
    match line {
        Line::Instruction(instr) => {
            write_instr(instr, line_content);
        }
        Line::Directive(directive) => {
            write_directive(directive, line_content, next_free);
        }
    };
    Ok(())
}

fn write_directive(
    directive: Directive,
    line_content: &mut Vec<Labelled<Expr>>,
    next_free: &mut u32,
) {
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
                    (mode.into(), Labelled::unlabelled(target.clone())),
                    (
                        ReadParamMode::Immediate,
                        Labelled::unlabelled(IntLiteral::ONE.into()),
                    ),
                    (mode.into(), Labelled::unlabelled(target)),
                )),
                line_content,
            );
        }
        Directive::Dec((mode, target)) => {
            // `ADD <target> #-1 <target>`
            write_instr(
                Instruction(Add(
                    (mode.into(), Labelled::unlabelled(target.clone())),
                    (
                        ReadParamMode::Immediate,
                        Labelled::unlabelled((-IntLiteral::ONE).into()),
                    ),
                    (mode.into(), Labelled::unlabelled(target)),
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
                next_free,
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
                    next_free,
                );
            }
        }
        Directive::Call((mode, target)) => {
            let ret_label = *next_free;
            *next_free += 1;

            // PUSH #$ret_label
            write_directive(
                Directive::Push((
                    ReadParamMode::Immediate,
                    Labelled::unlabelled(Identifier::Unnamed { code: ret_label }.into()),
                )),
                line_content,
                next_free,
            );

            // JMP <target>
            write_directive(Directive::Jmp((mode, target)), line_content, next_free);

            // $ret_label: POP
            let pop_start = line_content.len();
            write_directive(Directive::Pop(None), line_content, next_free);
            line_content[pop_start]
                .labels
                .insert(Identifier::Unnamed { code: ret_label });
        }
        Directive::Ret => {
            // JMP @-1
            write_directive(
                Directive::Jmp((
                    ReadParamMode::Relative,
                    Labelled::unlabelled((-IntLiteral::ONE).into()),
                )),
                line_content,
                next_free,
            );
        }
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
