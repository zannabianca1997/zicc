use zicc_limits::Value;

pub trait Writer {
    type Error;

    fn write(&mut self, value: Value) -> Result<(), Self::Error>;
}

pub trait Reader {
    type Error;

    fn read(&mut self) -> Result<Option<Value>, Self::Error>;
}

pub mod std_io;
