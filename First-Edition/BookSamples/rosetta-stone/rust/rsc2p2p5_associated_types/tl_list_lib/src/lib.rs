use std::marker::PhantomData;


pub struct Nil;
pub struct Cons<T, Rest>(PhantomData::<(T, Rest)>);


// Append operation

pub trait AppendImpl<T> {
  type Output;
}

impl<T> AppendImpl<T> for Nil {
  type Output = Cons<T, Nil>;
}

impl<T, M, Rest> AppendImpl<T> for Cons<M, Rest> {
  type Output = Cons<T, Cons<M, Rest>>;
}

// Head operation

pub trait HeadImpl {
  type Output;
}

impl<T, Rest> HeadImpl for Cons<T, Rest> {
  type Output = T;
}

// Cannot exist:
// impl HeadImpl for Nil {
//   type Output = ???   /// Nothing to return here
// }

// Tail operation

pub trait TailImpl {
  type Output;
}

impl TailImpl for Nil {
  type Output = Nil;
}

impl<T, Rest> TailImpl for Cons<T, Rest> {
  type Output = Rest;
}

// Eq operation

pub trait EqImpl<Other> {
  type A_;
  type B_;
  type Output;
}

// -- Eq for bool types

pub enum True {}
pub enum False {}

impl EqImpl<True> for True {
  type A_ = True;
  type B_ = True;
  type Output = True;
}

impl EqImpl<False> for False {
  type A_ = False;
  type B_ = False;
  type Output = True;
}

// Boolean operations

pub trait And<T1> {
  type A_;
  type B_;
  type Output;
}

impl And<True> for True {
  type A_ = True;
  type B_ = True;
  type Output = True;
}

impl And<False> for False {
  type A_ = False;
  type B_ = False;
  type Output = False;
}

impl And<False> for True {
  type A_ = True;
  type B_ = False;
  type Output = False;
}

impl And<True> for False {
  type A_ = False;
  type B_ = True;
  type Output = False;
}

// -- Eq for lists

impl EqImpl<Nil> for Nil {
  type A_ = Nil;
  type B_ = Nil;
  type Output = True;
}

impl<I> EqImpl<Nil> for Cons<I, Nil> {
  type A_ = Cons<I, Nil>;
  type B_ = Nil;
  type Output = False;
}

impl<I> EqImpl<Cons<I, Nil>> for Nil {
  type A_ = Nil;
  type B_ = Cons<I, Nil>;
  type Output = False;
}

impl<I1, I2, Rest1, Rest2>
  EqImpl<Cons<I1, Rest1>>
  for Cons<I2, Rest2>
  where
    Rest1: EqImpl<Rest2>,
    I1: EqImpl<I2>,
    <Rest1 as EqImpl<Rest2>>::Output: And<<I1 as EqImpl<I2>>::Output>
{
  type A_ = Cons<I2, Rest2>;
  type B_ = Cons<I1, Rest1>;

  type Output =
    <<Rest1 as EqImpl<Rest2>>::Output as And<<I1 as EqImpl<I2>>::Output>>::Output;
}



// Interface
pub type Append<T, List> = <List as AppendImpl<T>>::Output;
pub type Head<List> = <List as HeadImpl>::Output;
pub type Tail<List> = <List as TailImpl>::Output;

pub type Eq<T1, T2> = <T1 as EqImpl<T2>>::Output;


// Macro for a type-level list.
//
// Usage:
//
// enum A {}
// enum B {}
// type EnumList = tl_list![A, B];
#[macro_export]
macro_rules! tl_list {
    () => {
        Nil
    };

    ($head:ty $(, $tail:ty)*) => {
        Cons<$head, tl_list!($($tail),*)>
    };
}
