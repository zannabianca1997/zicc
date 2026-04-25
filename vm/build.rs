use std::{
    collections::HashMap,
    env,
    ffi::OsStr,
    fs::{create_dir_all, read, write},
    path::{Component, Path, PathBuf},
};

use glob::glob;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use serde::de::IgnoredAny;
use slugify::slugify;
use zicc_vm_program::Program;

fn main() {
    generate_tests();
}

#[derive(Default, Debug)]
struct TestSourcesDir {
    dirs: HashMap<Ident, TestSourcesDir>,
    functions: HashMap<Ident, (PathBuf, String)>,
}

const TEST_DIR: &str = "../sources/ints";

fn generate_tests() {
    cargo_emit::rerun_if_changed!(TEST_DIR);

    let sources = crawl_test_sources();
    let tests = test_module_inner(sources);

    let out_file = Path::new(&env::var_os("OUT_DIR").unwrap()).join("tests/sources.rs");

    create_dir_all(out_file.parent().unwrap()).unwrap();
    write(&out_file, tests.to_string()).unwrap();

    cargo_emit::rustc_env!("ZICC_VM_TEST_SOURCES", "{}", out_file.display());
}

fn test_module_inner(TestSourcesDir { dirs, functions }: TestSourcesDir) -> TokenStream {
    let nested_modules = dirs.into_iter().map(|(name, content)| {
        let content = test_module_inner(content);
        quote! {
            mod #name {
                #content
            }
        }
    });

    let functions = functions.into_iter().map(|(name, (source, case))| {
        // Path from the `tests` dir
        let binding = Path::new("..").join(source);
        let source = binding.to_str().unwrap();
        quote! {
            #[test]
            fn #name () {
                crate::test_harness(include_bytes!(#source), #case)
            }
        }
    });

    quote! {
        #( #nested_modules )*
        #( #functions )*
    }
}

fn crawl_test_sources() -> TestSourcesDir {
    let mut sources: TestSourcesDir = TestSourcesDir::default();
    for source_file in glob(&format!("{TEST_DIR}/**/*.ints")).unwrap() {
        let source_file = match source_file {
            Ok(p) => p,
            Err(err) => {
                cargo_emit::warning!("{}", err);
                continue;
            }
        };

        let mut sources = &mut sources;

        let components = source_file
            .parent()
            .unwrap()
            .strip_prefix(TEST_DIR)
            .unwrap()
            .components()
            .map(component_to_ident);

        for component in components {
            sources = sources.dirs.entry(component).or_default();
        }
        let functions = &mut sources
            .dirs
            .entry(os_str_to_ident(source_file.file_stem().unwrap()))
            .or_default()
            .functions;

        let source_file = source_file.canonicalize().unwrap();

        let prog = Program::parse(&read(&source_file).unwrap())
            .unwrap()
            .info
            .get_metadata::<HashMap<String, IgnoredAny>, _>("tests")
            .unwrap()
            .into_keys();

        for case in prog {
            functions.insert(str_to_ident(&case), (source_file.clone(), case));
        }
    }
    sources
}

fn component_to_ident(c: Component) -> Ident {
    let Component::Normal(c) = c else {
        unreachable!()
    };
    os_str_to_ident(c)
}

fn os_str_to_ident(c: &OsStr) -> Ident {
    str_to_ident(&c.to_string_lossy())
}

fn str_to_ident(c: &str) -> Ident {
    let c = slugify!(&c, separator = "_");
    Ident::new(&c, Span::call_site())
}
