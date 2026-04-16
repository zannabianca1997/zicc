//! Full lexer tests

use logos::Logos;
use paste::paste;
use string_interner::StringInterner;

use crate::{LexerExtras, Token};

fn test_body(source: &'static str, tokens: &'static [Token]) {
    let mut interner = StringInterner::new();
    let lexer = Token::lexer_with_extras(
        source,
        LexerExtras {
            interner: &mut interner,
        },
    )
    .collect::<Result<Vec<_>, _>>()
    .expect("Sources should lex");

    assert_eq!(tokens, lexer)
}

macro_rules! lex_tests {
    (
        $(
            $name:ident : $source:literal => [$( $tok:expr ),* $(,)?]
        );* $(;)?
    ) => {
        paste!{
        $(
            #[test]
            fn [< $name _should_lex_correctly >]() {
                test_body($source, &[$($tok),*])
            }
        )*
        }
    };
}

lex_tests! {
    empty: "" => [];
    spaces: "  \n \t" => [];
    single_line_comment: "// hello, commented\n" => [];
    single_line_comment_eof: "// hello, commented" => [];
    multi_line_comment: "/* Hello \n * Multiline!\n */" => [];


}
