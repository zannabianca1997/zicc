//! Directives
//!
//! Assembly commands that do not translate to single instructions

use super::{label::Labelled, relocatable::RlValue};

pub enum Directive {
    DATA(Vec<Labelled<RlValue>>),
    ZEROS(usize),
}
