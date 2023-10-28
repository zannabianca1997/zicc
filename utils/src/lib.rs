use bincode::{enc::Encoder, error::EncodeError, Encode};

/// Encode a vec of T, even if it isn't `'static`.
/// Does not treat Vec<u8> as special
pub fn encode_vec<T: Encode, E: Encoder>(vec: &Vec<T>, encoder: &mut E) -> Result<(), EncodeError> {
    // encode lenght
    (vec.len() as u64).encode(encoder)?;
    // encode items
    for item in vec.iter() {
        item.encode(encoder)?;
    }
    Ok(())
}
