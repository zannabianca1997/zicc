//! contain the definitions of non-objects (struct and enum)

use std::collections::HashMap;

use super::typedef::{EnumDef, StructDef, TypeDef, UnionDef};

struct Defs {
    structs: HashMap<String, StructDef>,
    unions: HashMap<String, UnionDef>,
    enums: HashMap<String, EnumDef>,
    types: HashMap<String, TypeDef>,
}
