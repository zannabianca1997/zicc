//! Directives
//!
//! Assembly commands that do not translate to single instructions

use std::fmt::Display;

use super::{label::Labelled, relocatable::RlValue};

pub enum Directive {
    DATA(Vec<Labelled<RlValue>>),
    ZEROS(usize),
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
