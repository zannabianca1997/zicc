use std::{collections::BTreeMap, fmt::Display};

/// A general ICC type
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(super) enum Type {
    Data(Data),
    /// Void type
    Void,
    /// Function type
    Function {
        /// Eventual result of the function
        result: Option<Data>,
        /// Argument of the function
        args: Vec<Data>,
    },
}
impl Type {
    pub(crate) fn is_scalar(&self) -> bool {
        if let Self::Data(d) = self {
            d.is_scalar()
        } else {
            false
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Data(d) => write!(f, "{d}"),
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
pub(super) enum Data {
    /// Scalar type
    ///
    /// Single memory cell
    Scalar,
    /// Pointer type
    Pointer(Box<Type>),
    /// Array type
    Array { element: Box<Data>, lenght: usize },
    /// Composite type
    ///
    /// This can be either a struct, or a union. Fields can overlap
    Composite {
        fields: BTreeMap<String, (usize, Data)>,
    },
}
impl Data {
    /// Calculate size of the type
    pub(super) fn size(&self) -> usize {
        match &self {
            Data::Scalar => 1,
            Data::Pointer(_) => 1,
            Data::Array { element, lenght } => element.size() * lenght,
            Data::Composite { fields } => fields
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
impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Scalar => write!(f, "int"),
            Data::Pointer(d) => write!(f, "{d}*"),
            Data::Array { element, lenght } => write!(f, "{element}[{lenght}]"),
            Data::Composite { fields: _ } => write!(f, "(composite)"),
        }
    }
}
