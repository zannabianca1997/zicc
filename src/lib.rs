use snafu::Snafu;

pub mod cli;

#[derive(Debug, Snafu)]
pub enum Error {}

/// Router function
///
/// Auto deduce the action to take
pub fn router() -> Result<(), Error> {
    todo!()
}
