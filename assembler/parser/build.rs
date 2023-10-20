use std::{env, error::Error, fs, path::PathBuf};

fn main() -> Result<(), Box<dyn Error>> {
    let work_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let grammar_path = PathBuf::from("src/grammar.lalrpop");
    let expanded_path = work_dir.join("grammar.lalrpop");
    // check if the generated grammar is older
    cargo_emit::rerun_if_changed!(grammar_path.display());
    if !expanded_path.exists()
        || grammar_path.metadata()?.modified()? > expanded_path.metadata()?.modified()?
    {
        // read grammar, lex and expand it, write it to a temp file
        let grammar = fs::read_to_string(&grammar_path)?;
        let grammar = keyword_expand::expand_string(&grammar);
        fs::write(expanded_path, grammar)?;
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
