use std::collections::BTreeMap;

use either::Either;

/// Function type
#[derive(Debug, Clone, Hash)]
struct Function {
    result: Either<Box<DataType>, Void>,
    params: Vec<DataType>,
}

/// Composite datatype.
///
/// Fields can be overlapping
#[derive(Debug, Clone, Hash)]
struct Composite {
    /// Fields, with the offset from the start of the struct
    fields: BTreeMap<String, (usize, DataType)>,
}
impl Composite {
    fn size(&self) -> usize {
        self.fields
            .values()
            .map(|(offset, field_type)| offset + field_type.size()) // take the end of each field
            .max()
            .unwrap_or(0) // if no field is present, the size is 0
    }
}

/// Void type
#[derive(Debug, Clone, Hash)]
struct Void;

/// Basic scalar type
///
/// Correspond to a single intcode cell
#[derive(Debug, Clone, Hash)]
struct Scalar;
impl Scalar {
    fn size(&self) -> usize {
        1
    }
}

/// Pointer type
#[derive(Debug, Clone, Hash)]
struct Pointer {
    dest: Box<Type>,
}
impl Pointer {
    fn size(&self) -> usize {
        1
    }
}

/// Fixed lenght array type
#[derive(Debug, Clone, Hash)]
struct Array {
    element: Box<DataType>,
    lenght: usize,
}
impl Array {
    fn size(&self) -> usize {
        self.element.size() * self.lenght
    }
}

/// A ICC type of data
#[derive(Debug, Clone, Hash)]
struct DataType {
    const_: bool,
    content: DataTypeContent,
}

#[derive(Debug, Clone, Hash)]
enum DataTypeContent {
    Scalar(Scalar),
    Pointer(Pointer),
    Array(Array),
    Composite(Composite),
}
impl DataType {
    /// Size of the data
    fn size(&self) -> usize {
        use DataTypeContent::*;
        match &self.content {
            Scalar(s) => s.size(),
            Pointer(p) => p.size(),
            Composite(u) => u.size(),
            Array(a) => a.size(),
        }
    }
}

/// A general ICC type
#[derive(Debug, Clone, Hash)]
enum Type {
    Data(DataType),
    Void(Void),
    Function(Function),
}
