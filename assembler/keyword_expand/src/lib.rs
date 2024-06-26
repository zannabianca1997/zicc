pub static KEYWORDS: &[&str] = &[
    "ints", "add", "mul", "in", "out", "jz", "jnz", "slt", "seq", "incb", "halt", "inc", "dec",
    "jmp", "mov", "zeros", "call", "ret", "export", "entry", "load", "store", "loadr", "storer",
];

mod expand_string;
pub use expand_string::expand_string;

mod expand_token_stream;
pub use expand_token_stream::expand_token_stream;
