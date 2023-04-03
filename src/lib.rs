#![feature(const_trait_impl)]
#![feature(const_slice_index)]
#![feature(const_mut_refs)]
#![feature(iter_intersperse)]
#![feature(assert_matches)]
#![feature(is_some_and)]
#![feature(iterator_try_reduce)]
#![feature(map_try_insert)]
#![feature(map_entry_replace)]
#![feature(error_reporter)]

pub mod intcode;

pub mod icfile;

pub mod assembler;

mod identifier;

pub mod machine;
