//! Full lexer tests

use std::cell::RefCell;

use logos::Logos;
use paste::paste;
use string_interner::StringInterner;

use assert_matches::assert_matches;

use crate::{
    LexerExtras, Token, int_literal::IntLiteral, keywords::Keyword, punctuators::Punctuator,
};

fn test_body(source: &'static str, checkers: &'static [fn(&Token)]) {
    let interner = RefCell::new(StringInterner::new());
    let lexer = Token::lexer_with_extras(
        source,
        LexerExtras {
            interner: &interner,
        },
    )
    .collect::<Result<Vec<_>, _>>()
    .expect("Sources should lex");

    assert_eq!(
        checkers.len(),
        lexer.len(),
        "wrong token count\n  source: {source:?}\n  lexed:  {lexer:#?}"
    );

    for (token, check) in lexer.iter().zip(checkers) {
        check(token);
    }
}

macro_rules! lex_tests {
    (
        $(
            $name:ident : $source:literal => [$( $tok:pat ),* $(,)?]
        );* $(;)?
    ) => {
        paste!{
        $(
            #[test]
            fn [< $name _should_lex_correctly >]() {
                test_body($source, &[$(|t: &Token| assert_matches!(t, $tok)),*])
            }
        )*
        }
    };
}

// whitespace and comment skipping
lex_tests! {
    empty: "" => [];
    spaces: "  \n \t" => [];
    single_line_comment: "// hello, commented\n" => [];
    single_line_comment_eof: "// hello, commented" => [];
    multi_line_comment: "/* Hello \n * Multiline!\n */" => [];
    empty_block_comment: "/**/" => [];
    block_comment_with_stars: "/*** hi ***/" => [];
    mixed_comments_and_whitespace: "// a\n  /* b */\t// c" => [];
    comment_between_tokens: "a /* x */ + /* y */ 1" => [
        Token::Identifier(_),
        Token::Punctuator(Punctuator::Plus(_)),
        Token::IntLiteral(_),
    ];
}

// identifiers
lex_tests! {
    ident: "hello" => [Token::Identifier(_)];
    uppercase_ident: "Foo" => [Token::Identifier(_)];
    ident_with_digits: "foo123" => [Token::Identifier(_)];
    snake_ident: "foo_bar_baz" => [Token::Identifier(_)];
    leading_underscore_ident: "_foo" => [Token::Identifier(_)];
    double_underscore_ident: "__foo" => [Token::Identifier(_)];
    bare_underscore_is_punct: "_" => [Token::Punctuator(Punctuator::Underscore(_))];
    double_bare_underscore: "__" => [
        Token::Punctuator(Punctuator::Underscore(_)),
        Token::Punctuator(Punctuator::Underscore(_)),
    ];
}

// keywords and their disambiguation from identifiers
lex_tests! {
    kw_fn: "fn" => [Token::Keyword(Keyword::Fn(_))];
    kw_return: "return" => [Token::Keyword(Keyword::Return(_))];
    kw_let: "let" => [Token::Keyword(Keyword::Let(_))];
    kw_type: "type" => [Token::Keyword(Keyword::Type(_))];
    kw_int: "int" => [Token::Keyword(Keyword::Int(_))];
    kw_prefix_is_ident: "integer" => [Token::Identifier(_)];
    kw_suffix_is_ident: "_fn" => [Token::Identifier(_)];
    kw_with_digit_is_ident: "fn1" => [Token::Identifier(_)];
}

// integer literals
lex_tests! {
    int_zero: "0" => [Token::IntLiteral(_)];
    int_leading_zeros: "007" => [Token::IntLiteral(_)];
    int_big: "99999999999999999999999999999999" => [Token::IntLiteral(_)];
    negative_is_two_tokens: "-42" => [Token::Punctuator(Punctuator::Minus(_)), Token::IntLiteral(_)];
    positive_sign_is_two_tokens: "+42" => [Token::Punctuator(Punctuator::Plus(_)), Token::IntLiteral(_)];
}

// one test per punctuator variant — covers the logos token path separately from from_str
lex_tests! {
    punct_semicolon: ";" => [Token::Punctuator(Punctuator::Semicolon(_))];
    punct_colon: ":" => [Token::Punctuator(Punctuator::Colon(_))];
    punct_comma: "," => [Token::Punctuator(Punctuator::Comma(_))];
    punct_eq: "=" => [Token::Punctuator(Punctuator::Eq(_))];
    punct_underscore: "_" => [Token::Punctuator(Punctuator::Underscore(_))];
    punct_ampersand: "&" => [Token::Punctuator(Punctuator::Ampersand(_))];
    punct_at: "@" => [Token::Punctuator(Punctuator::At(_))];
    punct_dot: "." => [Token::Punctuator(Punctuator::Dot(_))];
    punct_plus: "+" => [Token::Punctuator(Punctuator::Plus(_))];
    punct_minus: "-" => [Token::Punctuator(Punctuator::Minus(_))];
    punct_star: "*" => [Token::Punctuator(Punctuator::Star(_))];
    punct_eqeq: "==" => [Token::Punctuator(Punctuator::EqEq(_))];
    punct_neq: "!=" => [Token::Punctuator(Punctuator::Neq(_))];
    punct_lt: "<" => [Token::Punctuator(Punctuator::Lt(_))];
    punct_le: "<=" => [Token::Punctuator(Punctuator::Le(_))];
    punct_gt: ">" => [Token::Punctuator(Punctuator::Gt(_))];
    punct_ge: ">=" => [Token::Punctuator(Punctuator::Ge(_))];
    punct_and: "&&" => [Token::Punctuator(Punctuator::And(_))];
    punct_or: "||" => [Token::Punctuator(Punctuator::Or(_))];
    punct_not: "!" => [Token::Punctuator(Punctuator::Not(_))];
    punct_paren_open: "(" => [Token::Punctuator(Punctuator::ParenthesesOpen(_))];
    punct_paren_close: ")" => [Token::Punctuator(Punctuator::ParenthesesClose(_))];
    punct_bracket_open: "[" => [Token::Punctuator(Punctuator::BracketOpen(_))];
    punct_bracket_close: "]" => [Token::Punctuator(Punctuator::BracketClose(_))];
    punct_brace_open: "{" => [Token::Punctuator(Punctuator::BraceOpen(_))];
    punct_brace_close: "}" => [Token::Punctuator(Punctuator::BraceClose(_))];
}

// maximal munch: space splits two-char punctuators; ambiguous prefixes consume correctly
lex_tests! {
    lt_space_eq_splits: "< =" => [
        Token::Punctuator(Punctuator::Lt(_)),
        Token::Punctuator(Punctuator::Eq(_)),
    ];
    bang_before_ident: "!a" => [Token::Punctuator(Punctuator::Not(_)), Token::Identifier(_)];
    ampersand_before_ident: "&x" => [Token::Punctuator(Punctuator::Ampersand(_)), Token::Identifier(_)];
}

// realistic source snippets
lex_tests! {
    simple_expr: "a + 2" => [Token::Identifier(_), Token::Punctuator(Punctuator::Plus(_)), Token::IntLiteral(_)];
    fn_decl: "fn main() { return 0; }" => [
        Token::Keyword(Keyword::Fn(_)),
        Token::Identifier(_),
        Token::Punctuator(Punctuator::ParenthesesOpen(_)),
        Token::Punctuator(Punctuator::ParenthesesClose(_)),
        Token::Punctuator(Punctuator::BraceOpen(_)),
        Token::Keyword(Keyword::Return(_)),
        Token::IntLiteral(_),
        Token::Punctuator(Punctuator::Semicolon(_)),
        Token::Punctuator(Punctuator::BraceClose(_)),
    ];
    let_typed: "let x: int = 42;" => [
        Token::Keyword(Keyword::Let(_)),
        Token::Identifier(_),
        Token::Punctuator(Punctuator::Colon(_)),
        Token::Keyword(Keyword::Int(_)),
        Token::Punctuator(Punctuator::Eq(_)),
        Token::IntLiteral(_),
        Token::Punctuator(Punctuator::Semicolon(_)),
    ];
    indexing: "a[0]" => [
        Token::Identifier(_),
        Token::Punctuator(Punctuator::BracketOpen(_)),
        Token::IntLiteral(_),
        Token::Punctuator(Punctuator::BracketClose(_)),
    ];
    comparison_chain: "a<=b && b<c" => [
        Token::Identifier(_),
        Token::Punctuator(Punctuator::Le(_)),
        Token::Identifier(_),
        Token::Punctuator(Punctuator::And(_)),
        Token::Identifier(_),
        Token::Punctuator(Punctuator::Lt(_)),
        Token::Identifier(_),
    ];
    field_access: "a.b.c" => [
        Token::Identifier(_),
        Token::Punctuator(Punctuator::Dot(_)),
        Token::Identifier(_),
        Token::Punctuator(Punctuator::Dot(_)),
        Token::Identifier(_),
    ];
}

/// The same identifier string should intern to the same symbol across tokens
#[test]
fn same_identifier_should_share_symbol() {
    let interner = RefCell::new(StringInterner::new());
    let tokens = Token::lexer_with_extras(
        "foo foo bar",
        LexerExtras {
            interner: &interner,
        },
    )
    .collect::<Result<Vec<_>, _>>()
    .expect("Should lex");

    let [
        Token::Identifier(foo1),
        Token::Identifier(foo2),
        Token::Identifier(bar),
    ] = tokens.as_slice()
    else {
        panic!("unexpected token shapes");
    };
    assert_eq!(foo1, foo2);
    assert_ne!(foo1, bar);
}

/// IntLiteral tokens should carry the correct Value
#[test]
fn int_literal_should_carry_correct_value() {
    let interner = RefCell::new(StringInterner::new());
    let tokens = Token::lexer_with_extras(
        "123 0 99999999999999999999",
        LexerExtras {
            interner: &interner,
        },
    )
    .collect::<Result<Vec<_>, _>>()
    .expect("Should lex");

    let [
        Token::IntLiteral(a),
        Token::IntLiteral(b),
        Token::IntLiteral(c),
    ] = tokens.as_slice()
    else {
        panic!("unexpected token shapes");
    };
    assert_eq!(a, &"123".parse::<IntLiteral>().unwrap());
    assert_eq!(b, &"0".parse::<IntLiteral>().unwrap());
    assert_eq!(c, &"99999999999999999999".parse::<IntLiteral>().unwrap());
}

/// An unknown character should produce a lex error
#[test]
fn unknown_character_should_error() {
    let interner = RefCell::new(StringInterner::new());
    let result = Token::lexer_with_extras(
        "#",
        LexerExtras {
            interner: &interner,
        },
    )
    .collect::<Result<Vec<_>, _>>();
    assert!(result.is_err());
}

/// An unterminated block comment should produce a lex error
#[test]
fn unterminated_block_comment_should_error() {
    let interner = RefCell::new(StringInterner::new());
    let result = Token::lexer_with_extras(
        "/* not closed",
        LexerExtras {
            interner: &interner,
        },
    )
    .collect::<Result<Vec<_>, _>>();
    assert!(result.is_err());
}
