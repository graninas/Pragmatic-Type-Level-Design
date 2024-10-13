use std::marker::PhantomData;

pub trait PersonKind {}

pub enum Teacher {}
pub enum Student {}

impl PersonKind for Teacher {}
impl PersonKind for Student {}

#[allow(dead_code)]
pub trait UserKind {}

pub struct PowerUser<P: PersonKind, const VERIFIED: bool> {
  pub _marker: PhantomData<P>
}

impl<P: PersonKind, const VERIFIED: bool>
  UserKind for PowerUser<P, VERIFIED> {}

pub struct RegularUser<P: PersonKind, const VERIFIED: bool> {
  pub _marker: PhantomData<P>
}

impl<P: PersonKind, const VERIFIED: bool>
  UserKind for RegularUser<P, VERIFIED> {}


