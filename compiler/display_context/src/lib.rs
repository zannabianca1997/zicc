use std::{fmt::Display, rc::Rc, sync::Arc};

use either::Either;

/// Objects that can be displayed with additional context
pub trait DisplayWithContext<Context>
where
    Context: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &Context) -> std::fmt::Result;

    fn with_context<'s>(&'s self, context: &'s Context) -> impl Display + 's {
        WithContext(self, context)
    }
}

#[derive(Debug, Clone, Copy)]
/// Wrapper that contains both the object and the context, and can be displayied directly
pub struct WithContext<'s, T: ?Sized, C: ?Sized>(&'s T, &'s C);

impl<'s, T, C> Display for WithContext<'s, T, C>
where
    T: DisplayWithContext<C> + ?Sized,
    C: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f, &self.1)
    }
}

// easy implementation: if T implement display, it can be shown with all contexts
#[macro_export]
macro_rules! display_with_any_context {
    ($T:ty) => {
        impl<C> DisplayWithContext<C> for $T
        where
            C: ?Sized,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _: &C) -> std::fmt::Result {
                <$T as std::fmt::Display>::fmt(self, f)
            }

            fn with_context<'s>(&'s self, _: &'s C) -> impl Display + 's {
                self
            }
        }
    };
}

display_with_any_context! {str}

impl<T, C> DisplayWithContext<C> for &T
where
    T: DisplayWithContext<C> + ?Sized,
    C: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &C) -> std::fmt::Result {
        <T as DisplayWithContext<C>>::fmt(self, f, context)
    }

    fn with_context<'s>(&'s self, context: &'s C) -> impl Display + 's {
        <T as DisplayWithContext<C>>::with_context(self, context)
    }
}
impl<T, C> DisplayWithContext<C> for &mut T
where
    T: DisplayWithContext<C> + ?Sized,
    C: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &C) -> std::fmt::Result {
        <T as DisplayWithContext<C>>::fmt(self, f, context)
    }

    fn with_context<'s>(&'s self, context: &'s C) -> impl Display + 's {
        <T as DisplayWithContext<C>>::with_context(self, context)
    }
}
impl<T, C> DisplayWithContext<C> for Box<T>
where
    T: DisplayWithContext<C> + ?Sized,
    C: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &C) -> std::fmt::Result {
        <T as DisplayWithContext<C>>::fmt(self, f, context)
    }

    fn with_context<'s>(&'s self, context: &'s C) -> impl Display + 's {
        <T as DisplayWithContext<C>>::with_context(self, context)
    }
}
impl<T, C> DisplayWithContext<C> for Rc<T>
where
    T: DisplayWithContext<C> + ?Sized,
    C: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &C) -> std::fmt::Result {
        <T as DisplayWithContext<C>>::fmt(self, f, context)
    }

    fn with_context<'s>(&'s self, context: &'s C) -> impl Display + 's {
        <T as DisplayWithContext<C>>::with_context(self, context)
    }
}
impl<T, C> DisplayWithContext<C> for Arc<T>
where
    T: DisplayWithContext<C> + ?Sized,
    C: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &C) -> std::fmt::Result {
        <T as DisplayWithContext<C>>::fmt(self, f, context)
    }

    fn with_context<'s>(&'s self, context: &'s C) -> impl Display + 's {
        <T as DisplayWithContext<C>>::with_context(self, context)
    }
}

impl<L, R, C> DisplayWithContext<C> for Either<L, R>
where
    L: DisplayWithContext<C>,
    R: DisplayWithContext<C>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, context: &C) -> std::fmt::Result {
        match self {
            Either::Left(left) => left.fmt(f, context),
            Either::Right(right) => right.fmt(f, context),
        }
    }
}
