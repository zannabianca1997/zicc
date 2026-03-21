use derive_more::Eq;

use super::TypeTable;
use crate::{Int, Pointer, PointerKind, SizedType, Type, Unknown};

enum NoNames {}

fn test_table() -> TypeTable<NoNames> {
    TypeTable::new()
}

/// Test that Int returns the correct type id when queried from the table
#[test]
fn int_should_return_correct_type_id() {
    let table = test_table();
    let int_type = Type::int();
    let int_id = table.id(&int_type);

    assert_eq!(int_id, Int::type_id().into());
}

/// Test that Unknown returns the correct type id when queried from the table
#[test]
fn unknown_should_return_correct_type_id() {
    let table = test_table();
    let unknown_type = Type::unknown();
    let unknown_id = table.id(&unknown_type);

    assert_eq!(unknown_id, Unknown::type_id());
}

/// Test that the type id of Int returns the Int type
#[test]
fn int_type_id_should_return_int_type() {
    let table = test_table();
    let int_id = Int::type_id().into();
    let retrieved_type = table.r#type(int_id);

    assert_eq!(retrieved_type, Type::int());
}

/// Test that the type id of Unknown returns the Unknown type
#[test]
fn unknown_type_id_should_return_unknown_type() {
    let table = test_table();
    let unknown_id = Unknown::type_id();
    let retrieved_type = table.r#type(unknown_id);

    assert_eq!(retrieved_type, Type::unknown());
}

/// Test that pointer types correctly roundtrip through the table
#[test]
fn pointer_should_roundtrip_through_table() {
    let table = test_table();

    // Create a pointer type
    let pointed_id = Int::type_id().into();
    let pointer_type = Type::pointer(PointerKind::Absolute, pointed_id);

    // Get the id from the table
    let pointer_id = table.id(&pointer_type);

    // Get the type back from the id
    let retrieved_type = table.r#type(pointer_id);

    // Verify it matches
    assert_eq!(retrieved_type, pointer_type);

    // Also verify it's a pointer with the correct properties
    if let Type::Sized(SizedType::Pointer(ptr)) = retrieved_type {
        assert_eq!(ptr.kind, PointerKind::Absolute);
        assert_eq!(ptr.pointed, pointed_id);
    } else {
        panic!("Expected pointer type");
    }
}

/// Test that array types correctly roundtrip through the table
#[test]
fn array_should_roundtrip_through_table() {
    let table = test_table();

    // Create an array type
    let element_id = Int::type_id();
    let length = 10;
    let array_type = Type::array(element_id, length);

    // Get the id from the table
    let array_id = table.id(&array_type);

    // Get the type back from the id
    let retrieved_type = table.r#type(array_id);

    // Verify it matches
    assert_eq!(retrieved_type, array_type);

    // Also verify it's an array with the correct properties
    if let Type::Sized(SizedType::Array(arr)) = retrieved_type {
        assert_eq!(arr.element, element_id);
        assert_eq!(arr.length, length);
    } else {
        panic!("Expected array type");
    }
}

/// Test that unknown type is unsized
#[test]
fn unknown_should_be_unsized() {
    let table = test_table();
    let unknown_id = Unknown::type_id();

    let size = table.size_of(unknown_id);
    assert!(size.is_none());
}

/// Test that int has size Int::size
#[test]
fn int_should_have_correct_size() {
    let table = test_table();
    let int_id = Int::type_id();

    let size = table.size_of_sized(int_id);
    assert_eq!(size, Int::size());
}

/// Test that pointer to int has size Pointer::size
#[test]
fn pointer_to_int_should_have_correct_size() {
    let table = test_table();

    // Create a pointer to int
    let pointed_id = Int::type_id().into();
    let pointer_type = SizedType::pointer(PointerKind::Absolute, pointed_id);
    let pointer_id = table.id_sized(&pointer_type);

    let size = table.size_of_sized(pointer_id);
    assert_eq!(size, Pointer::size());
}

/// Test that pointer to unknown has size Pointer::size
#[test]
fn pointer_to_unknown_should_have_correct_size() {
    let table = test_table();

    // Create a pointer to unknown
    let pointed_id = Unknown::type_id();
    let pointer_type = SizedType::pointer(PointerKind::Absolute, pointed_id);
    let pointer_id = table.id_sized(&pointer_type);

    let size = table.size_of_sized(pointer_id);
    assert_eq!(size, Pointer::size());
}

/// Test that array of ints has size Int::size * length
#[test]
fn array_of_ints_should_have_correct_size() {
    let table = test_table();

    // Create an array of ints
    let element_id = Int::type_id();
    let length = 10;
    let array_type = SizedType::array(element_id, length);
    let array_id = table.id_sized(&array_type);

    let size = table.size_of_sized(array_id);
    assert_eq!(size, Int::size() * length);
}

/// Test that 2D array [[int; 3]; 4] has size 12 * Int::size
#[test]
fn two_dimensional_array_of_ints_should_have_correct_size() {
    let table = test_table();

    // Create inner array [int; 3]
    let element_id = Int::type_id();
    let inner_length = 3;
    let inner_array_type = SizedType::array(element_id, inner_length);
    let inner_array_id = table.id_sized(&inner_array_type);

    // Create outer array [[int; 3]; 4]
    let outer_length = 4;
    let outer_array_type = SizedType::array(inner_array_id, outer_length);
    let outer_array_id = table.id_sized(&outer_array_type);

    let size = table.size_of_sized(outer_array_id);
    assert_eq!(size, Int::size() * inner_length * outer_length);
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum ColorNames {
    Red,
    Green,
    Blue,
}

/// Test that we can define a name and retrieve it
#[test]
fn should_define_and_retrieve_name() {
    let table: TypeTable<ColorNames> = TypeTable::new();

    // Define a name for Int type
    let int_id = Int::type_id().into();
    table.define(ColorNames::Red, int_id).unwrap();

    // Retrieve the type by name
    let retrieved_id = table.named(&ColorNames::Red);
    assert_eq!(retrieved_id, Some(int_id));
}

/// Test that defining a duplicate name returns an error
#[test]
fn should_error_on_duplicate_name() {
    let table: TypeTable<ColorNames> = TypeTable::new();

    // Define a name for Int type
    let int_id = Int::type_id().into();
    table.define(ColorNames::Red, int_id).unwrap();

    // Try to define the same name again with a different type
    let unknown_id = Unknown::type_id();
    let result = table.define(ColorNames::Red, unknown_id);

    // Should return an error with the conflicting type
    assert!(result.is_err());
    let (name, occupied) = result.unwrap_err();
    assert_eq!(name, ColorNames::Red);
    assert_eq!(occupied, int_id);
}

/// Test that we can define multiple names and retrieve them
#[test]
fn should_define_and_retrieve_multiple_names() {
    let table: TypeTable<ColorNames> = TypeTable::new();

    // Define names for different types
    let int_id = Int::type_id().into();
    let unknown_id = Unknown::type_id();
    let pointer_id = table.id(&Type::pointer(PointerKind::Absolute, int_id));

    table.define(ColorNames::Red, int_id).unwrap();
    table.define(ColorNames::Green, unknown_id).unwrap();
    table.define(ColorNames::Blue, pointer_id).unwrap();

    // Retrieve all types by name
    assert_eq!(table.named(&ColorNames::Red), Some(int_id));
    assert_eq!(table.named(&ColorNames::Green), Some(unknown_id));
    assert_eq!(table.named(&ColorNames::Blue), Some(pointer_id));
}

/// Test that retrieving an undefined name returns None
#[test]
fn should_return_none_for_undefined_name() {
    let table: TypeTable<ColorNames> = TypeTable::new();

    // Try to retrieve a name that was never defined
    let retrieved_id = table.named(&ColorNames::Red);
    assert_eq!(retrieved_id, None);
}
