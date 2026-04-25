//! Test source crawling utilities
//!
//! Provides [`generate_tests`] to be used from `build.rs` scripts to
//! auto-generate test modules from source files in `sources/{subdir}`.

use std::{
    collections::HashMap,
    env,
    error::Error,
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

/// A tree of test source files matching the directory structure.
#[derive(Default, Debug)]
pub struct TestSourcesDir {
    dirs: HashMap<Ident, TestSourcesDir>,
    functions: HashMap<Ident, (PathBuf, String)>,
}

/// Generate test modules from source files in `sources/{subdir}/**/*.{ext}`.
///
/// Writes the generated code to `$OUT_DIR/tests/{subdir_slug}.rs` and sets
/// the environment variable `{subdir_upper}_TEST_SOURCES` to its path.
///
/// Call from a `build.rs`:
///
/// ```ignore
/// fn main() {
///     zicc_test_crawler::generate_tests("ints", "ints");
/// }
/// ```
pub fn generate_tests(subdir: &str, ext: &str) {
    let sources_dir = format!("../sources/{subdir}");

    cargo_emit::rerun_if_changed!(&sources_dir);

    let sources = crawl_test_sources(&sources_dir, ext);
    let tests = test_module_inner(sources);

    let subdir_slug = slugify!(subdir, separator = "_");
    let subdir_upper = subdir_slug.to_uppercase();

    let out_file =
        Path::new(&env::var_os("OUT_DIR").unwrap()).join(format!("tests/{subdir_slug}.rs"));

    create_dir_all(out_file.parent().unwrap()).unwrap();
    write(&out_file, tests.to_string()).unwrap();

    // cargo-emit's rustc_env! requires a literal key, so use println directly
    println!(
        "cargo::rustc-env=ZICC_{}_TEST_SOURCES={}",
        subdir_upper,
        out_file.display()
    );
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

fn crawl_test_sources(base_dir: &str, ext: &str) -> TestSourcesDir {
    let mut sources: TestSourcesDir = TestSourcesDir::default();
    let pattern = format!("{base_dir}/**/*.{ext}");

    for source_file in glob(&pattern).unwrap() {
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
            .strip_prefix(base_dir)
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

        let cases = match read(&source_file)
            .map_err(|err| Box::new(err) as Box<dyn Error + 'static>)
            .and_then(|content| {
                Program::parse(&content).map_err(|err| Box::new(err) as Box<dyn Error + 'static>)
            })
            .and_then(|prog| {
                prog.info
                    .get_metadata::<HashMap<String, IgnoredAny>, _>("tests")
                    .map_err(|err| Box::new(err) as Box<dyn Error + 'static>)
            })
            .map(HashMap::into_keys)
        {
            Ok(cases) => cases,
            Err(err) => {
                cargo_emit::warning!("Error in parsing file {}: {}", source_file.display(), err);
                continue;
            }
        };

        for case in cases {
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
    let c = slugify!(c, separator = "_");
    Ident::new(&c, Span::call_site())
}
