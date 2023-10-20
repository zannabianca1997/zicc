use std::{env, error::Error, fs, path::PathBuf};

fn main() -> Result<(), Box<dyn Error>> {
    let work_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    // read grammar, lex and expand it, write it to a temp file
    let grammar = fs::read_to_string("src/grammar.lalrpop")?;
    let grammar = keyword_expand::expand_string(&grammar);
    fs::write(work_dir.join("grammar.lalrpop"), grammar)?;
    // building grammar
    lalrpop::Configuration::new()
        .use_colors_if_tty()
        .set_out_dir(&work_dir)
        .set_in_dir(&work_dir)
        .emit_rerun_directives(true)
        .emit_report(true)
        .process()
}
