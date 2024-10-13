use std::marker::PhantomData;

use crate::adts::bool_kind::BoolKind;

pub trait PersonKind {}

pub enum Teacher {}
pub enum Student {}

impl PersonKind for Teacher {}
impl PersonKind for Student {}

#[allow(dead_code)]
pub trait UserKind {}

pub struct PowerUser<P: PersonKind, V: BoolKind> {
  pub _marker: PhantomData<(P,V)>
}

pub struct RegularUser<P: PersonKind, V: BoolKind> {
  pub _marker: PhantomData<(P,V)>
}

impl<P: PersonKind, V: BoolKind>
  UserKind for PowerUser<P, V> {}

impl<P: PersonKind, V: BoolKind>
  UserKind for RegularUser<P, V> {}


