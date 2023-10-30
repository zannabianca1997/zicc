#![feature(never_type)]
#![feature(box_into_inner)]
#![feature(box_patterns)]

pub use ast::ParseError;
use errors::Accumulator;

pub mod ast;

pub fn parse<'s, A>(source: &'s str, errors: &mut A) -> Option<ast::File<'s>>
where
    A: Accumulator,
    A::Error: From<ParseError>,
{
    ast::File::parse(source, &mut errors.as_mapped(|err: ParseError| err.into()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_sources::test_sources;

    #[test_sources]
    fn parse_sources(source: &str) {
        use itertools::Itertools;

        let mut errors = errors::RootAccumulator::<ParseError>::new();
        let ast = parse(source, &mut errors);

        match errors.finish_with(ast) {
            Ok(Some(_)) => (),
            Ok(None) => panic!("No error was thrown, but no ast was produced!"),
            Err(errs) => panic!("Errors in parsing:\n\n{}", errs.into_iter().format("\n")),
        }
    }

    mod ast_serialization {
        use errors::PanicAccumulator;
        use test_sources::test_sources;

        use crate::{parse, ParseError};

        #[test_sources]
        fn serde(source: &str) {
            let unit = parse(source, &mut PanicAccumulator::<ParseError>::new()).unwrap();
            let serialized = serde_json::to_string(&unit).expect("The ast should serialize");
            let deserialized =
                serde_json::from_str(&serialized).expect("The ast should deserialize");
            assert_eq!(
                unit, deserialized,
                "The deserialized ast should be the same"
            )
        }

        #[test_sources]
        fn bincode(source: &str) {
            let unit = parse(source, &mut PanicAccumulator::<ParseError>::new()).unwrap();
            let serialized = ::bincode::encode_to_vec(&unit, ::bincode::config::standard())
                .expect("The ast should encode");
            let deserialized =
                ::bincode::borrow_decode_from_slice(&serialized, ::bincode::config::standard())
                    .expect("The ast should decode")
                    .0;
            assert_eq!(
                unit, deserialized,
                "The deserialized ast should be the same"
            )
        }
    }
}
