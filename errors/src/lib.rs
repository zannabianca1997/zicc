//! Utilities to collect multiple errors
#![feature(never_type)]
use std::{error::Error, mem, ops::Range};

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
pub struct Accumulator<Error> {
    errors: Vec<Error>,
}

impl<E> Accumulator<E> {
    pub fn new() -> Self {
        Accumulator { errors: vec![] }
    }

    pub fn push<EI: Into<E>>(&mut self, err: EI) {
        self.errors.push(err.into())
    }

    pub fn handle<T, EI: Into<E>>(&mut self, res: Result<T, EI>) -> Option<T> {
        match res {
            Ok(t) => Some(t),
            Err(e) => {
                self.push(e.into());
                None
            }
        }
    }

    pub fn handle_flattened<T, I>(&mut self, res: Result<T, I>) -> Option<T>
    where
        I: IntoIterator,
        I::Item: Into<E>,
    {
        match res {
            Ok(t) => Some(t),
            Err(e) => {
                self.extend(e);
                None
            }
        }
    }

    pub fn handle_iter<'s, I>(&'s mut self, iter: I) -> HandleIter<'s, E, I::IntoIter>
    where
        I: IntoIterator,
        HandleIter<'s, E, I::IntoIter>: Iterator,
    {
        HandleIter {
            acc: self,
            inner: iter.into_iter(),
        }
    }

    pub fn handle_iter_flattened<'s, I>(
        &'s mut self,
        iter: I,
    ) -> HandleIterFlattened<'s, E, I::IntoIter>
    where
        I: IntoIterator,
        HandleIterFlattened<'s, E, I::IntoIter>: Iterator,
    {
        HandleIterFlattened {
            acc: self,
            inner: iter.into_iter(),
        }
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

impl<E, EI: Into<E>> Extend<EI> for Accumulator<E> {
    fn extend<T: IntoIterator<Item = EI>>(&mut self, iter: T) {
        self.errors.extend(iter.into_iter().map(Into::into))
    }
}

pub struct HandleIter<'a, E, I> {
    acc: &'a mut Accumulator<E>,
    inner: I,
}

impl<E, I, T, EI> Iterator for HandleIter<'_, E, I>
where
    I: Iterator<Item = Result<T, EI>>,
    EI: Into<E>,
{
    type Item = Option<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|res| self.acc.handle(res))
    }
}
pub struct HandleIterFlattened<'a, E, I> {
    acc: &'a mut Accumulator<E>,
    inner: I,
}

impl<E, I, T, II> Iterator for HandleIterFlattened<'_, E, I>
where
    I: Iterator<Item = Result<T, II>>,
    II: IntoIterator,
    II::Item: Into<E>,
{
    type Item = Option<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|res| self.acc.handle_flattened(res))
    }
}

pub trait IteratorExt<T, EI>
where
    Self: IntoIterator<Item = Result<T, EI>>,
{
    fn collect_errs<E>(self, acc: &mut Accumulator<E>) -> HandleIter<'_, E, Self::IntoIter>
    where
        EI: Into<E>;
}
impl<T, EI> IteratorExt<T, EI> for T
where
    T: IntoIterator<Item = Result<T, EI>>,
{
    fn collect_errs<E>(self, acc: &mut Accumulator<E>) -> HandleIter<'_, E, Self::IntoIter>
    where
        EI: Into<E>,
    {
        acc.handle_iter(self)
    }
}
pub trait IteratorFlattenExt<T, EI>
where
    Self: IntoIterator<Item = Result<T, EI>>,
{
    fn collect_errs_flattened<E>(
        self,
        acc: &mut Accumulator<E>,
    ) -> HandleIterFlattened<'_, E, Self::IntoIter>
    where
        EI: IntoIterator,
        EI::Item: Into<E>;
}
impl<T, EI> IteratorFlattenExt<T, EI> for T
where
    T: IntoIterator<Item = Result<T, EI>>,
{
    fn collect_errs_flattened<E>(
        self,
        acc: &mut Accumulator<E>,
    ) -> HandleIterFlattened<'_, E, Self::IntoIter>
    where
        EI: IntoIterator,
        EI::Item: Into<E>,
    {
        acc.handle_iter_flattened(self)
    }
}

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
