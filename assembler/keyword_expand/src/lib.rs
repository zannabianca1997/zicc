pub static KEYWORDS: &[&str] = &[
    "ints", "add", "mul", "in", "out", "jz", "jnz", "slt", "seq", "incb", "halt", "inc", "dec",
    "jmp", "mov", "zeros", "push", "pop", "call", "ret", "export", "entry",
];

mod expand_string;
pub use expand_string::expand_string;

mod expand_token_stream;
pub use expand_token_stream::expand_token_stream;
