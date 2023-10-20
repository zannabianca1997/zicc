use std::{
    collections::BTreeSet,
    env,
    error::Error,
    fs::{self, File},
    io::{self, BufRead, BufReader, Write},
    path::{Path, PathBuf},
};

use keyword_expand::KEYWORDS;
use lazy_regex::regex_captures;

fn main() -> Result<(), Box<dyn Error>> {
    let work_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let grammar_path = PathBuf::from("src/grammar.lalrpop");
    let expanded_path = work_dir.join("grammar.lalrpop");

    // check if the generated grammar is older
    cargo_emit::rerun_if_changed!(grammar_path.display());
    if !expanded_path.exists()
        || grammar_path.metadata()?.modified()? > expanded_path.metadata()?.modified()?
        || !check_hash(&expanded_path)?
    {
        // read grammar, lex and expand it, write it to a temp file
        let grammar = fs::read_to_string(&grammar_path)?;
        let grammar = keyword_expand::expand_string(&grammar);
        let mut expanded = File::create(expanded_path)?;
        write_hash(&mut expanded)?;
        expanded.write_all(grammar.as_bytes())?;
    }
    // building grammar
    lalrpop::Configuration::new()
        .use_colors_if_tty()
        .set_out_dir(&work_dir)
        .set_in_dir(&work_dir)
        .emit_rerun_directives(false)
        .emit_report(true)
        .process()
}

fn write_hash(mut expanded: impl io::Write) -> io::Result<()> {
    let keywords = BTreeSet::from_iter(KEYWORDS.into_iter().copied());
    writeln!(
        expanded,
        "// KEYWORDS: {}",
        serde_json::to_string(&keywords).unwrap()
    )
}

fn check_hash(expanded_path: &Path) -> io::Result<bool> {
    let mut reader = BufReader::new(File::open(expanded_path)?);
    while let Some(line) = {
        let mut buffer = String::new();
        match reader.read_line(&mut buffer)? {
            0 => None,
            _ => Some(buffer),
        }
    } {
        if let Some((_, kwds)) = regex_captures!(
            r#"\/\/\s*KEYWORDS\s*:\s*(\[\s*"\w+"(?:\s*,\s*"\w+")*\s*\])"#,
            &line
        ) {
            return Ok(serde_json::from_str::<BTreeSet<&str>>(kwds).unwrap()
                == BTreeSet::from_iter(KEYWORDS.into_iter().copied()));
        }
    }
    Ok(false)
}
