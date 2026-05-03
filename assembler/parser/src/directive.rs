use chumsky::{IterParser, Parser, error::Rich, prelude::just, text::inline_whitespace};
use zicc_assembler_ast::directive::Directive;
use zicc_intcode::ReadParamMode;

use crate::{
    ParserExtra,
    expr::{expr, int_literal},
    instruction::{read_param, write_param},
    labelled::labelled,
};

fn data<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("DATA")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(
            labelled(expr())
                .separated_by(inline_whitespace().at_least(1))
                .collect(),
        )
        .map(Directive::Data)
        .labelled("data directive")
}

fn zeros<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("ZEROS")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(int_literal())
        .map(Directive::Zeros)
        .labelled("zeros directive")
}

fn jmp<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("JMP")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param())
        .map(Directive::Jmp)
        .labelled("jmp directive")
}

fn inc<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("INC")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(
            write_param().try_map(|(mode, labelled), span| {
                if labelled.is_labelled() {
                    Err(Rich::custom(span, "value cannot be labelled"))
                } else {
                    Ok((mode, labelled.item))
                }
            }),
        )
        .map(Directive::Inc)
        .labelled("inc directive")
}

fn dec<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("DEC")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(
            write_param().try_map(|(mode, labelled), span| {
                if labelled.is_labelled() {
                    Err(Rich::custom(span, "value cannot be labelled"))
                } else {
                    Ok((mode, labelled.item))
                }
            }),
        )
        .map(Directive::Dec)
        .labelled("dec directive")
}

fn mov<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("MOV")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param())
        .then_ignore(inline_whitespace().at_least(1))
        .then(write_param())
        .map(|(a, b)| Directive::Mov(a, b))
        .labelled("mov directive")
}

fn push<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("PUSH")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param())
        .map(Directive::Push)
        .labelled("push directive")
}

fn pop<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("POP")
        .then(
            inline_whitespace()
                .at_least(1)
                .ignore_then(write_param())
                .or_not(),
        )
        .map(|(_, dst)| Directive::Pop(dst))
        .labelled("pop directive")
}

fn call<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("CALL")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param().try_map(|(mode, labelled), span| {
            if mode == ReadParamMode::Relative && labelled.is_labelled() {
                Err(Rich::custom(
                    span,
                    "CALL with relative parameter cannot be labelled",
                ))
            } else {
                Ok(Directive::Call((mode, labelled)))
            }
        }))
        .labelled("call directive")
}

fn ret<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("RET")
        .to(Directive::Ret)
        .labelled("ret directive")
}

fn load<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("LOAD")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param())
        .then_ignore(inline_whitespace().at_least(1))
        .then(write_param())
        .map(|(ptr, dst)| Directive::Load(ptr, dst))
        .labelled("load directive")
}

fn store<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("STORE")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(read_param())
        .then_ignore(inline_whitespace().at_least(1))
        .then(read_param())
        .map(|(src, ptr)| Directive::Store(src, ptr))
        .labelled("store directive")
}

/// A single directive
pub(crate) fn directive<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    data()
        .or(zeros())
        .or(jmp())
        .or(inc())
        .or(dec())
        .or(mov())
        .or(push())
        .or(pop())
        .or(call())
        .or(ret())
        .or(load())
        .or(store())
        .labelled("directive")
}
