#![doc = include_str!("../README.md")]

pub mod identifier;
pub mod tokens;

struct File {
    items: Vec<Item>,
}

enum Item {}
