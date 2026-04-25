#![doc = include_str!("../README.md")]

use std::convert::Infallible;

use zicc_limits::Value;

pub mod std_io;

pub trait Reader {
    type Error;

    fn read(&mut self) -> Result<Option<Value>, Self::Error>;
}

pub trait Writer {
    type Error;

    fn write(&mut self, value: Value) -> Result<(), Self::Error>;
}

// References

impl<R> Reader for &mut R
where
    R: Reader,
{
    type Error = R::Error;

    fn read(&mut self) -> Result<Option<Value>, Self::Error> {
        R::read(self)
    }
}

impl<W> Writer for &mut W
where
    W: Writer,
{
    type Error = W::Error;

    fn write(&mut self, value: Value) -> Result<(), Self::Error> {
        W::write(self, value)
    }
}

// Memory implementations

impl Reader for &[Value] {
    type Error = Infallible;

    fn read(&mut self) -> Result<Option<Value>, Self::Error> {
        let Some((value, rest)) = self.split_first() else {
            return Ok(None);
        };

        *self = rest;

        Ok(Some(value.clone()))
    }
}

impl Writer for Vec<Value> {
    type Error = Infallible;

    fn write(&mut self, value: Value) -> Result<(), Self::Error> {
        self.push(value);
        Ok(())
    }
}
