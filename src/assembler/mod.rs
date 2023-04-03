mod assembly_file;
mod directive;
mod instruction;
mod label;
mod parser;
mod relocatable;

pub use assembly_file::AssemblyFile;
pub use parser::parse;
pub use relocatable::ICProgramFragment;

#[cfg(test)]
mod tests;
