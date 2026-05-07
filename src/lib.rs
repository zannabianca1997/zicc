#![doc=include_str!("../README.md")]

pub mod cli;
pub mod router;

pub use zicc_assembler as assembler;
pub use zicc_linker as linker;
pub use zicc_vm as vm;
