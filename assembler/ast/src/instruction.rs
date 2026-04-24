use crate::{expression::Expr, labelled::Labelled};

pub type Instruction = zicc_intcode::Instruction<Labelled<Expr>>;
