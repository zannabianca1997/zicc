use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // building grammar
    lalrpop::Configuration::new()
        .use_colors_if_tty()
        .use_cargo_dir_conventions()
        .emit_rerun_directives(true)
        .emit_report(true)
        .process()
}
