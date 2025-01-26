// Type-level kind
pub trait BoolKind {}

// Type-level ADT constructor types
pub struct True;
pub struct False;

// Necessary connection of the ADT constructor type and the kind
impl BoolKind for True {}
impl BoolKind for False {}

