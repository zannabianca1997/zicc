use std::fmt::{Display as StdDisplay, Formatter, Result};

use string_interner::DefaultStringInterner;

pub trait DisplayWith {
    /// Format this object with the given interner
    fn fmt_with(&self, f: &mut Formatter<'_>, interner: &DefaultStringInterner) -> Result;

    /// Obtain an adapter implementing display
    fn display<'s>(&'s self, interner: &'s DefaultStringInterner) -> Display<'s, Self> {
        Display(self, interner)
    }
}

// Default implementation for types implementing Display
impl<T: StdDisplay> DisplayWith for T {
    fn fmt_with(&self, f: &mut Formatter<'_>, _interner: &DefaultStringInterner) -> Result {
        StdDisplay::fmt(self, f)
    }
}

/// Adapter implementing [`std::fmt::Display`]
pub struct Display<'s, T: ?Sized>(&'s T, &'s DefaultStringInterner);

impl<T: DisplayWith> StdDisplay for Display<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        DisplayWith::fmt_with(self.0, f, self.1)
    }
}
