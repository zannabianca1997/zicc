use display_context::DisplayWithContext;
use either::Either;
use string_interner::DefaultStringInterner;

use crate::{
    ast_node::AstNode, punctuated::Punctuated, Identifier, PunctAmpersand, PunctAt,
    PunctBracketClose, PunctBracketOpen, PunctComma, PunctDot, PunctEq, PunctEqEq, PunctGe,
    PunctGt, PunctLe, PunctLt, PunctNoEq, PunctParenClose, PunctParenOpen,
};

use super::{Literal, PunctMinus, PunctPlus, PunctStar};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Literal(Box<Literal>),
    Name(Box<Identifier>),
    Set(Box<BinExpr<PunctEq>>),
    Eq(Box<BinExpr<PunctEqEq>>),
    NoEq(Box<BinExpr<PunctNoEq>>),
    Ge(Box<BinExpr<PunctGe>>),
    Gt(Box<BinExpr<PunctGt>>),
    Le(Box<BinExpr<PunctLe>>),
    Lt(Box<BinExpr<PunctLt>>),
    MemberAccess(Box<MemberAccess>),
    IndexAccess(Box<IndexAccess>),
    Deref(Box<UnExpr<PunctStar>>),
    TakeRef(Box<UnExpr<Either<PunctAmpersand, PunctAt>>>),
    Call(Box<ExpressionCall>),
    Add(Box<BinExpr<PunctPlus>>),
    Sub(Box<BinExpr<PunctMinus>>),
    Neg(Box<UnExpr<PunctMinus>>),
    Mul(Box<BinExpr<PunctStar>>),
    Parenthesized(Box<Parenthesized>),
}

impl AstNode for Expression {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        match self {
            Expression::Literal(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Name(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Set(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Eq(box expr) => expr.visited_by(&mut child_visitor),
            Expression::NoEq(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Ge(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Gt(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Le(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Lt(box expr) => expr.visited_by(&mut child_visitor),
            Expression::MemberAccess(box expr) => expr.visited_by(&mut child_visitor),
            Expression::IndexAccess(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Deref(box expr) => expr.visited_by(&mut child_visitor),
            Expression::TakeRef(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Call(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Add(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Sub(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Neg(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Mul(box expr) => expr.visited_by(&mut child_visitor),
            Expression::Parenthesized(box expr) => expr.visited_by(&mut child_visitor),
        };
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        match self {
            Expression::Literal(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Name(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Set(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Eq(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::NoEq(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Ge(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Gt(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Le(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Lt(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::MemberAccess(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::IndexAccess(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Deref(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::TakeRef(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Call(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Add(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Sub(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Neg(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Mul(box expr) => expr.visited_by_mut(&mut child_visitor),
            Expression::Parenthesized(box expr) => expr.visited_by_mut(&mut child_visitor),
        };
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for Expression {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Expression::Literal(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Name(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Set(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Eq(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::NoEq(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Ge(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Gt(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Le(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Lt(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::MemberAccess(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::IndexAccess(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Deref(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::TakeRef(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Call(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Add(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Sub(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Neg(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Mul(expr) => DisplayWithContext::fmt(expr, f, context),
            Expression::Parenthesized(expr) => DisplayWithContext::fmt(expr, f, context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberAccess {
    pub lhs: Expression,
    pub dot: PunctDot,
    pub member: Identifier,
}
impl AstNode for MemberAccess {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.lhs.visited_by(&mut child_visitor);
        self.dot.visited_by(&mut child_visitor);
        self.member.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.lhs.visited_by_mut(&mut child_visitor);
        self.dot.visited_by_mut(&mut child_visitor);
        self.member.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}
impl DisplayWithContext<DefaultStringInterner> for MemberAccess {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.lhs, f, context)?;
        DisplayWithContext::fmt(&self.dot, f, context)?;
        DisplayWithContext::fmt(&self.member, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressionCall {
    pub fun: Identifier,
    pub open_par: PunctParenOpen,
    pub inputs: Punctuated<Expression, PunctComma>,
    pub close_par: PunctParenClose,
}
impl AstNode for ExpressionCall {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.fun.visited_by(&mut child_visitor);
        self.open_par.visited_by(&mut child_visitor);
        for child in self.inputs.iter_all() {
            match child {
                Either::Left(inp) => {
                    inp.visited_by(&mut child_visitor);
                }
                Either::Right(comma) => {
                    comma.visited_by(&mut child_visitor);
                }
            }
        }
        self.close_par.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.fun.visited_by_mut(&mut child_visitor);
        self.open_par.visited_by_mut(&mut child_visitor);
        for child in self.inputs.iter_all_mut() {
            match child {
                Either::Left(inp) => {
                    inp.visited_by_mut(&mut child_visitor);
                }
                Either::Right(comma) => {
                    comma.visited_by_mut(&mut child_visitor);
                }
            }
        }
        self.close_par.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}
impl DisplayWithContext<DefaultStringInterner> for ExpressionCall {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.fun, f, context)?;
        DisplayWithContext::fmt(&self.open_par, f, context)?;
        for input_or_comma in self.inputs.iter_all() {
            match input_or_comma {
                Either::Left(inp) => DisplayWithContext::fmt(inp, f, context)?,
                Either::Right(comma) => {
                    DisplayWithContext::fmt(comma, f, context)?;
                    f.write_str(" ")?;
                }
            }
        }
        DisplayWithContext::fmt(&self.close_par, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndexAccess {
    pub lhs: Expression,
    pub braket_open: PunctBracketOpen,
    pub index: Identifier,
    pub braket_close: PunctBracketClose,
}
impl AstNode for IndexAccess {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.lhs.visited_by(&mut child_visitor);
        self.braket_open.visited_by(&mut child_visitor);
        self.index.visited_by(&mut child_visitor);
        self.braket_close.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.lhs.visited_by_mut(&mut child_visitor);
        self.braket_open.visited_by_mut(&mut child_visitor);
        self.index.visited_by_mut(&mut child_visitor);
        self.braket_close.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}
impl DisplayWithContext<DefaultStringInterner> for IndexAccess {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.lhs, f, context)?;
        DisplayWithContext::fmt(&self.braket_open, f, context)?;
        DisplayWithContext::fmt(&self.index, f, context)?;
        DisplayWithContext::fmt(&self.braket_close, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinExpr<Op> {
    pub lhs: Expression,
    pub op: Op,
    pub rhs: Expression,
}
impl<Op: AstNode> AstNode for BinExpr<Op> {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.lhs.visited_by(&mut child_visitor);
        self.op.visited_by(&mut child_visitor);
        self.rhs.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.lhs.visited_by_mut(&mut child_visitor);
        self.op.visited_by_mut(&mut child_visitor);
        self.rhs.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}
impl<Op> DisplayWithContext<DefaultStringInterner> for BinExpr<Op>
where
    Op: DisplayWithContext<DefaultStringInterner>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.lhs, f, context)?;
        DisplayWithContext::fmt(&self.op, f, context)?;
        DisplayWithContext::fmt(&self.rhs, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnExpr<Op> {
    pub op: Op,
    pub rhs: Expression,
}
impl<Op: AstNode> AstNode for UnExpr<Op> {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.op.visited_by(&mut child_visitor);
        self.rhs.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.op.visited_by_mut(&mut child_visitor);
        self.rhs.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}
impl<OpR: AstNode, OpL: AstNode> AstNode for UnExpr<Either<OpL, OpR>> {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        match &self.op {
            Either::Left(op) => op.visited_by(&mut child_visitor),
            Either::Right(op) => op.visited_by(&mut child_visitor),
        };
        self.rhs.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        match &mut self.op {
            Either::Left(op) => op.visited_by_mut(&mut child_visitor),
            Either::Right(op) => op.visited_by_mut(&mut child_visitor),
        };
        self.rhs.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}
impl<Op> DisplayWithContext<DefaultStringInterner> for UnExpr<Op>
where
    Op: DisplayWithContext<DefaultStringInterner>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.op, f, context)?;
        DisplayWithContext::fmt(&self.rhs, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parenthesized {
    pub open_par: PunctParenOpen,
    pub inner: Expression,
    pub close_par: PunctParenClose,
}

impl AstNode for Parenthesized {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.open_par.visited_by(&mut child_visitor);
        self.inner.visited_by(&mut child_visitor);
        self.close_par.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.open_par.visited_by_mut(&mut child_visitor);
        self.inner.visited_by_mut(&mut child_visitor);
        self.close_par.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for Parenthesized {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.open_par, f, context)?;
        DisplayWithContext::fmt(&self.inner, f, context)?;
        DisplayWithContext::fmt(&self.close_par, f, context)?;
        Ok(())
    }
}
