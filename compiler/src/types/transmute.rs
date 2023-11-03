use std::iter::zip;

use super::{
    FieldUnnamed, Type, TypeArray, TypeData, TypeInt, TypePointer, TypeSized, TypeStruct, TypeUnion,
};

pub trait Trasmutable<Dest: TypeSized>: TypeSized {
    /// Check if this type can be copied byte by byte on a type of dest
    fn trasmutable(&self, dest: &Dest) -> bool;
}

trait SimmetrizeTransmutable<Dest: TypeSized>: Trasmutable<Dest> {}
impl<A: SimmetrizeTransmutable<B>, B: TypeSized> Trasmutable<A> for B {
    fn trasmutable(&self, dest: &A) -> bool {
        dest.trasmutable(self)
    }
}

impl Trasmutable<TypeData> for TypeData {
    fn trasmutable(&self, dest: &TypeData) -> bool {
        match self {
            TypeData::Int(this) => this.trasmutable(dest),
            TypeData::Array(this) => this.trasmutable(dest),
            TypeData::Struct(this) => this.trasmutable(dest),
            TypeData::Union(this) => this.trasmutable(dest),
            TypeData::Pointer(this) => this.trasmutable(dest),
        }
    }
}
macro_rules! data_subtype {
    ($($dest:ty)*) => {
       $(
        impl Trasmutable<$dest> for TypeData {
            fn trasmutable(&self, dest: &$dest) -> bool {
                match self {
                    TypeData::Int(this) => this.trasmutable(dest),
                    TypeData::Array(this) => this.trasmutable(dest),
                    TypeData::Struct(this) => this.trasmutable(dest),
                    TypeData::Union(this) => this.trasmutable(dest),
                    TypeData::Pointer(this) => this.trasmutable(dest),
                }
            }
        }
        impl SimmetrizeTransmutable<$dest> for TypeData {}
        )*
    };
}
data_subtype! {TypeInt TypeArray TypeStruct TypeUnion TypePointer}

impl Trasmutable<TypeInt> for TypeInt {
    fn trasmutable(&self, TypeInt: &TypeInt) -> bool {
        true
    }
}
impl Trasmutable<TypePointer> for TypeInt {
    fn trasmutable(&self, _: &TypePointer) -> bool {
        false
    }
}
impl SimmetrizeTransmutable<TypePointer> for TypeInt {}

impl Trasmutable<TypeArray> for TypeArray {
    fn trasmutable(&self, dest: &TypeArray) -> bool {
        self.lenght == dest.lenght && self.element.trasmutable(&*dest.element)
    }
}
macro_rules! array_as_single {
    ($($dest:ty)*) => {
        $(
        impl Trasmutable<$dest> for TypeArray {
            fn trasmutable(&self, dest: &$dest) -> bool {
                self.lenght == 1 && self.element.trasmutable(dest)
            }
        }
        impl SimmetrizeTransmutable<$dest> for TypeArray {}
    )*
    };
}
array_as_single! {TypeInt TypeUnion TypePointer}

impl Trasmutable<TypeStruct> for TypeStruct {
    fn trasmutable(&self, dest: &TypeStruct) -> bool {
        self.names() == dest.names()
            && self.types().len() == dest.types().len()
            && zip(self.types(), dest.types()).all(|(s, d)| s.trasmutable(d))
    }
}
impl Trasmutable<TypeArray> for TypeStruct {
    fn trasmutable(&self, dest: &TypeArray) -> bool {
        // the tuple and array have the same lenght and all the elements are transmutable
        self.types().len() == dest.lenght
            && self.types().iter().all(|f| f.trasmutable(&*dest.element))
    }
}
impl SimmetrizeTransmutable<TypeArray> for TypeStruct {}
macro_rules! struct_as_single {
    ($($dest:ty)*) => {
        $(
        impl Trasmutable<$dest> for TypeStruct {
            fn trasmutable(&self, dest: &$dest) -> bool {
                let [field] = self.types() else {return false};
                field.trasmutable(dest)
            }
        }
        impl SimmetrizeTransmutable<$dest> for TypeStruct {}
    )*
    };
}
struct_as_single! {TypeInt TypeUnion TypePointer}

impl Trasmutable<TypeUnion> for TypeUnion {
    fn trasmutable(&self, dest: &TypeUnion) -> bool {
        self.variants().all(|(n1, t1)| {
            dest.variants()
                .any(|(n2, t2)| n1 == n2 && t1.trasmutable(t2))
        })
    }
}
macro_rules! union_as_single {
    ($($dest:ty)*) => {
        $(
        impl Trasmutable<$dest> for TypeUnion {
            fn trasmutable(&self, dest: &$dest) -> bool {
                let [field] = &**self.types() else {return false};
                field.trasmutable(dest)
            }
        }
        impl SimmetrizeTransmutable<$dest> for TypeUnion {}
    )*
    };
}
union_as_single! {TypeInt TypePointer}

impl Trasmutable<TypePointer> for TypePointer {
    fn trasmutable(&self, dest: &TypePointer) -> bool {
        match (self, dest) {
            (TypePointer::Absolute { pointed: p1 }, TypePointer::Absolute { pointed: p2 })
            | (TypePointer::Relative { pointed: p1 }, TypePointer::Relative { pointed: p2 }) => {
                // check pointer equivalence
                match (&**p1, &**p2) {
                    // unknow is a catch-all
                    (Type::Unknow(_), _) | (_, Type::Unknow(_)) => true,
                    // pointer conversion is a transmute
                    (Type::Data(d1), Type::Data(d2)) => d1.trasmutable(d2),
                    // function pointers are equivalents if one function can be transmuted into the other
                    (Type::Function(f1), Type::Function(f2)) => {
                        Trasmutable::trasmutable(
                            &TypeStruct::Unnamed(FieldUnnamed {
                                types: f1.inputs.clone(),
                            }),
                            &TypeStruct::Unnamed(FieldUnnamed {
                                types: f2.inputs.clone(),
                            }),
                        ) && f1.output.trasmutable(&f2.output)
                    }
                    // otherwise, nope
                    _ => false,
                }
            }
            // Absolute and relative do not mix
            _ => false,
        }
    }
}
