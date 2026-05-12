use std::{option, slice, vec};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punctuated<T, P> {
    pub items: Vec<(T, P)>,
    pub trailing: Option<T>,
}

impl<T, P> Punctuated<T, P> {
    /// New empty list
    pub fn new() -> Self {
        Self {
            items: vec![],
            trailing: None,
        }
    }

    /// Add an item
    ///
    /// Generate the punctuator if not pushed
    pub fn push(&mut self, item: T)
    where
        P: Default,
    {
        if let Some(trailing) = self.trailing.replace(item) {
            self.items.push((trailing, P::default()));
        }
    }

    /// Add an item
    ///
    /// Panics if an item has been pushed without a punctuator.
    pub fn push_item(&mut self, item: T) {
        assert!(
            self.trailing.is_none(),
            "A punctuator must be pushed between two items"
        );
        self.trailing = Some(item)
    }

    /// Add a punctuator
    ///
    /// Panics if no item has been pushed
    pub fn push_punctuator(&mut self, punct: P) {
        let Some(item) = self.trailing.take() else {
            panic!("An item must be pushed before any punctuator")
        };
        self.items.push((item, punct));
    }

    /// Iter through all items
    pub fn iter<'s>(&'s self) -> Iter<'s, T, P> {
        Iter {
            items: self.items.iter(),
            trailing: self.trailing.iter(),
        }
    }

    /// Mutably iter through all items
    pub fn iter_mut<'s>(&'s self) -> Iter<'s, T, P> {
        Iter {
            items: self.items.iter(),
            trailing: self.trailing.iter(),
        }
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.items.len() + self.trailing.is_some().then_some(1).unwrap_or(0)
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty() && self.trailing.is_none()
    }
}

impl<T, P> Default for Punctuated<T, P> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct WithPunctuators<Iter>(Iter);

impl<Iter> WithPunctuators<Iter> {
    /// Stop yielding punctuators
    pub fn without_punctuators(self) -> Iter {
        self.0
    }
}

impl<T, P> IntoIterator for Punctuated<T, P> {
    type Item = T;

    type IntoIter = IntoIter<T, P>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            items: self.items.into_iter(),
            trailing: self.trailing.into_iter(),
        }
    }
}

pub struct IntoIter<T, P> {
    items: vec::IntoIter<(T, P)>,
    trailing: option::IntoIter<T>,
}

impl<T, P> IntoIter<T, P> {
    /// Yield the punctuators with the items
    pub fn with_punctuators(self) -> WithPunctuators<Self> {
        WithPunctuators(self)
    }
}

impl<T, P> Iterator for IntoIter<T, P> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.items
            .next()
            .map(|(t, _)| t)
            .or_else(|| self.trailing.next())
    }
}

impl<T, P> Iterator for WithPunctuators<IntoIter<T, P>> {
    type Item = (T, Option<P>);

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .items
            .next()
            .map(|(t, p)| (t, Some(p)))
            .or_else(|| self.0.trailing.next().map(|t| (t, None)))
    }
}

pub struct Iter<'p, T, P> {
    items: slice::Iter<'p, (T, P)>,
    trailing: option::Iter<'p, T>,
}

impl<T, P> Iter<'_, T, P> {
    /// Yield the punctuators with the items
    pub fn with_punctuators(self) -> WithPunctuators<Self> {
        WithPunctuators(self)
    }
}

impl<'p, T, P> Iterator for Iter<'p, T, P> {
    type Item = &'p T;

    fn next(&mut self) -> Option<Self::Item> {
        self.items
            .next()
            .map(|(t, _)| t)
            .or_else(|| self.trailing.next())
    }
}

impl<'p, T, P> Iterator for WithPunctuators<Iter<'p, T, P>> {
    type Item = (&'p T, Option<&'p P>);

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .items
            .next()
            .map(|(t, p)| (t, Some(p)))
            .or_else(|| self.0.trailing.next().map(|t| (t, None)))
    }
}

pub struct IterMut<'p, T, P> {
    items: slice::IterMut<'p, (T, P)>,
    trailing: option::IterMut<'p, T>,
}

impl<T, P> IterMut<'_, T, P> {
    /// Yield the punctuators with the items
    pub fn with_punctuators(self) -> WithPunctuators<Self> {
        WithPunctuators(self)
    }
}

impl<'p, T, P> Iterator for IterMut<'p, T, P> {
    type Item = &'p mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.items
            .next()
            .map(|(t, _)| t)
            .or_else(|| self.trailing.next())
    }
}

impl<'p, T, P> Iterator for WithPunctuators<IterMut<'p, T, P>> {
    type Item = (&'p mut T, Option<&'p mut P>);

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .items
            .next()
            .map(|(t, p)| (t, Some(p)))
            .or_else(|| self.0.trailing.next().map(|t| (t, None)))
    }
}
