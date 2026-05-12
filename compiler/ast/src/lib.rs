#![doc = include_str!("../README.md")]

use crate::type_def::ItemTypeDef;

pub mod function;
pub mod punctuated;
pub mod type_def;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item {
    TypeDef(ItemTypeDef),
}
