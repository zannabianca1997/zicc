#![feature(const_trait_impl)]
#![feature(const_slice_index)]
#![feature(const_mut_refs)]

mod icvalue;
pub use icvalue::ICValue;

mod icprogram;
pub use icprogram::ICProgram;
