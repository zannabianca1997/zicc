use std::{collections::BTreeMap, fmt::Display};

/// A general ICC type
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(super) enum Type {
    DataType(DataType),
    /// Void type
    Void,
    /// Function type
    Function {
        /// Eventual result of the function
        result: Option<DataType>,
        /// Argument of the function
        args: Vec<DataType>,
    },
}
impl Type {
    pub(crate) fn is_scalar(&self) -> bool {
        if let Self::DataType(d) = self {
            d.is_scalar()
        } else {
            false
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::DataType(d) => write!(f, "{d}"),
            Type::Void => write!(f, "void"),
            Type::Function { result, args } => {
                match result {
                    Some(r) => write!(f, "{r}"),
                    None => write!(f, "{}", Type::Void),
                }?;
                write!(f, "(")?;
                for elem in args.iter().map(Some).intersperse(None) {
                    match elem {
                        Some(d) => write!(f, "{d}")?,
                        None => write!(f, ",")?,
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

/// A ICC data type
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(super) enum DataType {
    /// Scalar type
    ///
    /// Single memory cell
    Scalar,
    /// Pointer type
    Pointer(Box<Type>),
    /// Array type
    Array {
        element: Box<DataType>,
        lenght: usize,
    },
    /// Composite type
    ///
    /// This can be either a struct, or a union. Fields can overlap
    Composite {
        fields: BTreeMap<String, (usize, DataType)>,
    },
}
impl DataType {
    /// Calculate size of the type
    pub(super) fn size(&self) -> usize {
        match &self {
            DataType::Scalar => 1,
            DataType::Pointer(_) => 1,
            DataType::Array { element, lenght } => element.size() * lenght,
            DataType::Composite { fields } => fields
                .values()
                .map(|(offset, element)| offset + element.size())
                .max()
                .unwrap_or(0),
        }
    }

    /// Returns `true` if the data is [`Scalar`].
    ///
    /// [`Scalar`]: Data::Scalar
    #[must_use]
    pub(super) fn is_scalar(&self) -> bool {
        matches!(self, Self::Scalar)
    }
}
impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Scalar => write!(f, "int"),
            DataType::Pointer(d) => write!(f, "{d}*"),
            DataType::Array { element, lenght } => write!(f, "{element}[{lenght}]"),
            DataType::Composite { fields: _ } => write!(f, "(composite)"),
        }
    }
}
