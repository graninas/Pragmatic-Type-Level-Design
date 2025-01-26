use std::marker::PhantomData;

// Advanced custom kind system
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

// Universal evaluation mechanism
pub trait Eval<Verb, Res>{
  fn eval() -> Res;
}

pub trait EvalCtx<Ctx, Verb, Res>{
  fn eval_ctx(ctx: Ctx) -> Res;
}


// Checking if the types are identical.
// Returns True or False.
pub trait IsSame<Other> {
  type Output;
}

// Verifying type equality.
// Only has instance for equal types.
pub trait TypeEq<Other> {}

impl<T> TypeEq<T> for T {}

pub trait IsTrue {}
pub trait IsFalse {}

impl IsTrue for True {}
impl IsFalse for False {}



pub enum True {}
pub enum False {}

pub trait BoolKind {
  fn to_bool() -> bool;
}

impl BoolKind for True {
  fn to_bool() -> bool {
    true
  }
}

impl BoolKind for False {
  fn to_bool() -> bool {
    false
  }
}

// If combinator

pub trait If<IfTrue, IfFalse> {
  type Output;
}

impl <IfTrue, IfFalse>
  If<IfTrue, IfFalse>
  for True
{
  type Output = IfTrue;
}

impl <IfTrue, IfFalse>
  If<IfTrue, IfFalse>
  for False
{
  type Output = IfFalse;
}


// Workaround for getting type equality.
// Generates equality and inequality instances for every pair
// from the list.
//
// If T1 == T2, generates IsSame with Output = True.
// Otherwise, generates IsSame with Output = False.
//
// Usage:
// gen_equalities![A, B, C];
//
// gen_inequalities is a helper macro
#[macro_export]
macro_rules! gen_inequalities {

  ($T1:ty) => {
    impl type_level::IsSame<$T1> for $T1 {
      type Output = type_level::True;
    }
  };

  ($T1:ty, $T2:ty $(, $tail:tt)*) => {
    impl type_level::IsSame<$T2> for $T1 {
      type Output = type_level::False;
    }

    impl type_level::IsSame<$T1> for $T2 {
      type Output = type_level::False;
    }

    type_level::gen_inequalities!($T1 $(, $tail)*);
  };
}

#[macro_export]
macro_rules! gen_equalities {
  () => {
  };

  ($T1:ty) => {
    impl type_level::IsSame<$T1> for $T1 {
      type Output = type_level::True;
    }
  };

  ($T1:ty, $T2:ty $(, $tail:tt)*) => {
    type_level::gen_inequalities!($T1, $T2 $(, $tail)*);
    type_level::gen_equalities!($T2 $(, $tail)*);
  };
}



/// taken from https://docs.rs/assert-type-eq/latest/src/assert_type_eq/lib.rs.html#46-62
/// and changed

#[macro_export]
macro_rules! assert_type_eq {
  ( $t1:ty, $t2:ty ) => {
    gensym::gensym!{ type_level::assert_type_eq_impl!{ $t1, $t2 } }
  };
}

#[macro_export]
macro_rules! assert_type_eq_impl {
    ($gensym:ident, $t1:ty, $t2:ty ) => {
        mod $gensym {
            #[allow(unused_imports)]
            use super::*;

            struct MatchingType<T>(T);

            #[allow(dead_code, unreachable_patterns)]
            fn assert_type_eq(mine: MatchingType<$t1>) {
                match mine {
                  MatchingType::<$t2>(_) => ()
                }
            }
        }
    }
}
