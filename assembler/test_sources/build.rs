#![feature(os_str_bytes)]

use std::{
    borrow::Cow,
    collections::BTreeMap,
    env,
    error::Error,
    fmt::Debug,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    str::from_utf8,
};

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use serde::{de::Error as _, Deserialize};

use vm::VMInt;

struct Examples(BTreeMap<Ident, Example>);
impl Examples {
    fn read(examples_dir: impl AsRef<Path>) -> Result<Self, Box<dyn Error>> {
        let mut collected = BTreeMap::new();
        for item in examples_dir.as_ref().read_dir()? {
            let item = item?;
            if item.file_type()?.is_file()
                && item.path().extension().is_some_and(|ext| ext == "ints")
            {
                let name = syn::parse_str::<Ident>(from_utf8(
                    item.path().file_stem().unwrap().as_os_str_bytes(),
                )?)?;
                let content = fs::read_to_string(item.path())?;
                // split the yaml part
                let (head, content) = content.split_once("\n---\n").ok_or_else(|| {
                    format!(
                        "File {} does not contain the yaml header",
                        item.path().display()
                    )
                })?;
                // parse the yaml part
                let mut example: Example = serde_yaml::from_str(head)?;
                example.source.source = content.to_owned();
                collected.insert(name, example);
            }
        }
        Ok(Self(collected))
    }
}
impl ToTokens for Examples {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let examples = self.0.iter().map(|(name, example)| {
            let name = name.to_string();
            quote!( (#name, #example) )
        });
        quote!(
            &[
                #(#examples),*
            ]
        )
        .to_tokens(tokens)
    }
}

#[derive(Debug, Deserialize)]
struct Example {
    #[serde(flatten)]
    source: Source,
    #[serde(default)]
    tests: Tests,
}

impl ToTokens for Example {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Example { source, tests } = self;
        quote!(
            SourceFile {
                source: #source,
                tests: #tests
            }
        )
        .to_tokens(tokens)
    }
}

#[derive(Debug, Deserialize)]
struct Source {
    #[serde(default)]
    descr: Option<String>,
    #[serde(skip_deserializing)]
    source: String,
}
impl ToTokens for Source {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Source { descr, source } = self;
        let descr = descr
            .as_ref()
            .map(|d| quote!(Some(#d)))
            .unwrap_or_else(|| quote!(None));
        quote!(
            Source {
                descr: #descr,
                source: #source,
            }
        )
        .to_tokens(tokens)
    }
}

#[derive(Debug, Deserialize, Default)]
#[serde(untagged)]
enum Tests {
    Named(BTreeMap<DeserIdent, Test>),
    Unnamed(Vec<Test>),
    #[default]
    None,
}
impl Tests {
    fn iter_tests(&self) -> Box<dyn Iterator<Item = (Cow<Ident>, &Test)> + '_> {
        match self {
            Tests::Named(tests) => Box::new(tests.iter().map(|(n, t)| (Cow::Borrowed(&n.0), t))),
            Tests::Unnamed(tests) => Box::new(
                tests
                    .iter()
                    .enumerate()
                    .map(|(n, t)| (Cow::Owned(format_ident!("_{n}")), t)),
            ),
            Tests::None => Box::new([].into_iter()),
        }
    }
}
impl ToTokens for Tests {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tests = self.iter_tests().map(|(name, test)| {
            let name = name.to_string();
            quote!((#name, #test))
        });
        quote!(
            &[
                #(#tests),*
            ]
        )
        .to_tokens(tokens)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DeserIdent(Ident);

impl Debug for DeserIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Ident as Debug>::fmt(&self.0, f)
    }
}
impl<'de> Deserialize<'de> for DeserIdent {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = <Cow<'de, str> as Deserialize>::deserialize(deserializer)?;
        Ok(Self(syn::parse_str(&s).map_err(D::Error::custom)?))
    }
}

#[derive(Debug, Deserialize)]
struct Test {
    #[serde(default)]
    descr: Option<String>,
    #[serde(default)]
    r#in: Vec<VMInt>,
    #[serde(default)]
    out: Vec<VMInt>,
}
impl ToTokens for Test {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Test { descr, r#in, out } = self;
        let descr = descr
            .as_ref()
            .map(|d| quote!(Some(#d)))
            .unwrap_or_else(|| quote!(None));
        quote!(
           Test {
                descr: #descr,
                r#in: &[#(#r#in),*],
                out: &[#(#out),*],
            }
        )
        .to_tokens(tokens)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let examples_dir = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("sources");
    let out_file_path = PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("examples.rs");
    cargo_emit::rerun_if_changed!(examples_dir.display());

    let examples = Examples::read(examples_dir)?;
    let tokens = examples.into_token_stream();
    let mut out_file = File::create(&out_file_path)?;
    write!(out_file, "{}", tokens)?;

    cargo_emit::rustc_env!("SOURCES", "{}", out_file_path.display());
    Ok(())
}
