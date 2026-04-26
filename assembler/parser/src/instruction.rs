use chumsky::{Parser, error::Rich, prelude::just, text::inline_whitespace};
use zicc_assembler_ast::{expression::Expr, instruction::Instruction, labelled::Labelled};
use zicc_intcode::Instruction::*;
use zicc_intcode::{ReadParamMode, WriteParamMode};

use crate::{ParserExtra, expr::expr, labelled::labelled};

fn read_param_mode<'s>() -> impl Parser<'s, &'s str, ReadParamMode, ParserExtra<'s>> {
    just("#")
        .to(ReadParamMode::Immediate)
        .or(just("@").to(ReadParamMode::Relative))
        .or_not()
        .map(|m| m.unwrap_or(ReadParamMode::Absolute))
        .labelled("param mode")
}

fn write_param_mode<'s>() -> impl Parser<'s, &'s str, WriteParamMode, ParserExtra<'s>> {
    read_param_mode()
        .try_map(|mode, span| WriteParamMode::try_from(mode).map_err(|err| Rich::custom(span, err)))
        .labelled("write param mode")
}

pub(crate) fn read_param<'s>() -> impl Parser<'s, &'s str, ReadParam, ParserExtra<'s>> {
    read_param_mode()
        .then_ignore(inline_whitespace())
        .then(labelled(expr()))
        .labelled("read param")
}

pub(crate) fn write_param<'s>() -> impl Parser<'s, &'s str, WriteParam, ParserExtra<'s>> {
    write_param_mode()
        .then_ignore(inline_whitespace())
        .then(labelled(expr()))
        .labelled("write param")
}

type ReadParam = (ReadParamMode, Labelled<Expr>);
type WriteParam = (WriteParamMode, Labelled<Expr>);

fn rr_params<'s>() -> impl Parser<'s, &'s str, (ReadParam, ReadParam), ParserExtra<'s>> {
    read_param()
        .then_ignore(inline_whitespace().at_least(1))
        .then(read_param())
}

fn rrw_params<'s>() -> impl Parser<'s, &'s str, (ReadParam, ReadParam, WriteParam), ParserExtra<'s>>
{
    rr_params()
        .then_ignore(inline_whitespace().at_least(1))
        .then(write_param())
        .map(|((a, b), c)| (a, b, c))
}

/// A single instruction
pub(crate) fn instruction<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    add()
        .or(mul())
        .or(inp())
        .or(out())
        .or(jnz())
        .or(jez())
        .or(slt())
        .or(seq())
        .or(inb())
        .or(hlt())
        .labelled("instruction")
}

fn add<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("ADD")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(rrw_params())
        .map(|(a, b, c)| Instruction(Add(a, b, c)))
        .labelled("add instruction")
}

fn mul<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("MUL")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(rrw_params())
        .map(|(a, b, c)| Instruction(Mul(a, b, c)))
        .labelled("mul instruction")
}

fn inp<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("INP")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(write_param())
        .map(|a| Instruction(Inp(a)))
        .labelled("inp instruction")
}

fn out<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("OUT")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param())
        .map(|a| Instruction(Out(a)))
        .labelled("out instruction")
}

fn jnz<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("JNZ")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(rr_params())
        .map(|(a, b)| Instruction(Jnz(a, b)))
        .labelled("jnz instruction")
}

fn jez<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("JEZ")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(rr_params())
        .map(|(a, b)| Instruction(Jez(a, b)))
        .labelled("jez instruction")
}

fn slt<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("SLT")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(rrw_params())
        .map(|(a, b, c)| Instruction(Slt(a, b, c)))
        .labelled("slt instruction")
}

fn seq<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("SEQ")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(rrw_params())
        .map(|(a, b, c)| Instruction(Seq(a, b, c)))
        .labelled("seq instruction")
}

fn inb<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("INB")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param())
        .map(|a| Instruction(Inb(a)))
        .labelled("inb instruction")
}

fn hlt<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    just("HLT").to(Instruction(Hlt)).labelled("hlt instruction")
}
