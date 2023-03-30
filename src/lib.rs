#![feature(const_trait_impl)]
#![feature(const_slice_index)]
#![feature(const_mut_refs)]
#![feature(iter_intersperse)]
#![feature(assert_matches)]

mod intcode;
pub use intcode::{ICProgram, ICValue};

mod icfile;
pub use icfile::{ByteOrder, ICFormat};
