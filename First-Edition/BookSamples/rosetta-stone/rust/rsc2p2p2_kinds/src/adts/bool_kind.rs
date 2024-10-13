// Type-level kind
pub trait BoolKind {}

// Type-level ADT constructor types
pub enum True {}
pub enum False {}

// Necessary connection of the ADT constructor type and the kind
impl BoolKind for True {}
impl BoolKind for False {}

