use itertools::Itertools;
use lex_from_rust::ica;
use lexer::{Lexer, Token};

#[test]
fn add() {
    general_test(
        "in a
    in b
    add # a:0 # b:0 c
    out # c:0
    halt",
        ica!(
            in a;
            in b;
            add # a:0 # b:0 c;
            out # c:0;
            halt
        ),
    )
}

#[test]
fn raw() {
    general_test("r#a", ica!(r#a))
}

#[test]
fn with_anon() {
    general_test(
        "in $0
    in $1
    add # $0:0 # $1:0 $2
    out # $2:0
    halt",
        ica!(
            in $0;
            in $1;
            add # $0:0 # $1:0 $2;
            out # $2:0;
            halt
        ),
    )
}
#[test]
fn with_braces() {
    let a = Token::Identifier(lexer::Identifier::Named("a"));
    general_test(
        "in a; out # a:0",
        ica!(
            in {[a]}; out # {[a]}:0
        ),
    )
}

fn general_test<'a>(source: &str, relexed: impl Iterator<Item = Token<'a>>) {
    let lexed = Lexer::new(source)
        .map(|r| r.map(|(_, t, _)| t))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let relexed = relexed.collect_vec();
    assert_eq!(lexed, relexed)
}
