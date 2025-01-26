use std::marker::PhantomData;

// Interface of type-level interfaces
pub trait IInterface<I> {
  type Interface;
  // Could be defaulted to I but the feature is not supported:
  // type Interface = I;
}

// Type-level existential wrapper
pub struct Wrapper<I, T> (PhantomData::<(I, T)>);

impl<I, T> IInterface<I> for Wrapper<I, T> {
  type Interface = I;
}

pub trait Eval<Verb, Res>{
  fn eval() -> Res;
}
