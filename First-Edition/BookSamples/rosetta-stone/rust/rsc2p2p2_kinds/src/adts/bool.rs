use std::marker::PhantomData;
use crate::generate_kind_evidence;


// Type-level kind
pub trait BoolKind {}

// Type-level ADT type
pub struct Bool<T: BoolKind> {
  pub _marker: PhantomData<T>
}

// Type-level ADT constructor types
pub enum True {}
pub enum False {}


// Necessary connection of the ADT constructor type and the kind
impl BoolKind for False {}

// Evidence for the compiler to do the check
#[allow(dead_code)]
const FALSE_EVIDENCE: Bool<False> = Bool::<False> { _marker: PhantomData::<False> };

// This one will be generated with a macro
// impl BoolKind for True {}
generate_kind_evidence!(True, BoolKind, Bool, TRUE_EVIDENCE);


// Won't compile:
// const INVALID_EVIDENCE: Bool<i32> = Bool::<i32> { _marker: PhantomData::<i32> };

