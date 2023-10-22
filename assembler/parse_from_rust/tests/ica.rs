use parse_from_rust::ica;
use parser::ast::File;

#[test]
fn test() {
    let _: File<'static> = ica!(
        a: b: ;

    );
}
