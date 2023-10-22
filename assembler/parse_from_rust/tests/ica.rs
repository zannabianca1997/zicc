use parse_from_rust::ica;
use parser::ast::File;

#[test]
fn test() {
    let _: File<'static> = ica!(
        ints(3 * a) - 2, a: r"hello\d{-1}", 25;
        add @1 #2 3;
        inc 3
    );
}
