//! Utilities to collect multiple errors
#![feature(never_type)]
use std::{error::Error, fmt::Debug, marker::PhantomData, mem, ops::Range};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use nonempty::{nonempty, NonEmpty};

#[derive(Debug, Clone)]
pub struct Multiple<Error> {
    pub childs: NonEmpty<Error>,
}

impl<E> From<E> for Multiple<E> {
    fn from(value: E) -> Self {
        Self {
            childs: nonempty![value],
        }
    }
}

impl<E> IntoIterator for Multiple<E> {
    type Item = E;

    type IntoIter = <NonEmpty<E> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.childs.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct RootAccumulator<Error> {
    errors: Vec<Error>,
}

impl<E> RootAccumulator<E> {
    pub fn new() -> Self {
        RootAccumulator { errors: vec![] }
    }

    pub fn checkpoint(&mut self) -> Result<(), Multiple<E>> {
        match NonEmpty::from_vec(mem::take(&mut self.errors)) {
            None => Ok(()),
            Some(childs) => Err(Multiple { childs }),
        }
    }

    pub fn finish<F, T>(self, f: F) -> Result<T, Multiple<E>>
    where
        F: FnOnce() -> T,
    {
        match NonEmpty::from_vec(self.errors) {
            None => Ok(f()),
            Some(childs) => Err(Multiple { childs }),
        }
    }

    pub fn finish_with<T>(self, t: T) -> Result<T, Multiple<E>> {
        match NonEmpty::from_vec(self.errors) {
            None => Ok(t),
            Some(childs) => Err(Multiple { childs }),
        }
    }
}

impl<E> Accumulator for RootAccumulator<E> {
    type Error = E;

    fn push<EI: Into<Self::Error>>(&mut self, err: EI) {
        self.errors.push(err.into())
    }
}

#[derive(Debug)]
pub struct MappedAccumulator<'inner, A, F, E>(
    &'inner mut A,
    F,
    PhantomData<fn(&mut A, E) -> A::Error>,
)
where
    A: Accumulator,
    F: FnMut(E) -> A::Error;

impl<A, F, E> Accumulator for MappedAccumulator<'_, A, F, E>
where
    A: Accumulator,
    F: FnMut(E) -> A::Error,
{
    type Error = E;

    fn push<EI: Into<Self::Error>>(&mut self, err: EI) {
        self.0.push(self.1(err.into()))
    }
}

// Accumulator trait

pub trait Accumulator {
    type Error;

    fn push<EI: Into<Self::Error>>(&mut self, err: EI);

    fn handle<T, EI: Into<Self::Error>>(&mut self, res: Result<T, EI>) -> Option<T> {
        match res {
            Ok(t) => Some(t),
            Err(e) => {
                self.push(e.into());
                None
            }
        }
    }

    fn handle_iter<'s, I>(&'s mut self, iter: I) -> HandledIter<'s, Self, I::IntoIter>
    where
        Self: Sized,
        I: IntoIterator,
        HandledIter<'s, Self, I::IntoIter>: Iterator,
    {
        HandledIter {
            acc: self,
            inner: iter.into_iter(),
        }
    }

    fn as_mapped<'s, F, E>(&'s mut self, fun: F) -> MappedAccumulator<'s, Self, F, E>
    where
        Self: Sized,
        F: FnMut(E) -> Self::Error,
    {
        MappedAccumulator(self, fun, PhantomData)
    }
}

impl<B> Accumulator for &mut B
where
    B: Accumulator,
{
    type Error = B::Error;

    fn push<EI: Into<Self::Error>>(&mut self, err: EI) {
        B::push(self, err)
    }

    fn handle<T, EI: Into<Self::Error>>(&mut self, res: Result<T, EI>) -> Option<T> {
        B::handle(self, res)
    }
}

pub struct HandledIter<'a, A, I> {
    acc: &'a mut A,
    inner: I,
}

impl<I, A, T, EI> Iterator for HandledIter<'_, A, I>
where
    A: Accumulator,
    I: Iterator<Item = Result<T, EI>>,
    EI: Into<A::Error>,
{
    type Item = Option<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|res| self.acc.handle(res))
    }
}

pub trait IteratorExt<T, EI>
where
    Self: IntoIterator<Item = Result<T, EI>>,
{
    fn collect_errs<A, E>(self, acc: &mut A) -> HandledIter<'_, A, Self::IntoIter>
    where
        A: Accumulator,
        EI: Into<A::Error>;
}
impl<T, EI> IteratorExt<T, EI> for T
where
    T: IntoIterator<Item = Result<T, EI>>,
{
    fn collect_errs<A, E>(self, acc: &mut A) -> HandledIter<'_, A, Self::IntoIter>
    where
        A: Accumulator,
        EI: Into<A::Error>,
    {
        acc.handle_iter(self)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PanicAccumulator<T>(PhantomData<T>);

impl<T> PanicAccumulator<T> {
    pub fn new() -> Self {
        PanicAccumulator(PhantomData)
    }
}

impl<T> Accumulator for PanicAccumulator<T>
where
    T: Debug,
{
    type Error = T;

    fn push<EI: Into<Self::Error>>(&mut self, err: EI) {
        panic!("Error accumulated: {:?}", <EI as Into<T>>::into(err))
    }
}

// Span stuff

pub trait Spanned {
    fn span(&self) -> Range<usize>;
}

pub use codespan_reporting::diagnostic::Severity;
pub trait SourceError: Error + Spanned {
    fn severity(&self) -> Severity;
    fn into_diagnostic<F>(self, file_id: F) -> Diagnostic<F>
    where
        Self: Sized,
    {
        Diagnostic::new(self.severity())
            .with_message(self.to_string())
            .with_labels(vec![Label::primary(file_id, self.span())])
    }
}
