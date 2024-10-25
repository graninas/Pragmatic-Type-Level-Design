
// Advanced custom kind system
pub trait IInterface<I> {
  type Interface;
  // Could be defaulted to I but the feature is not supported:
  // type Interface = I;
}

// Evaluation of an arbitrary verb
pub trait Eval<Verb, Res>{
  fn eval() -> Res;
}

// Type equality
pub trait TypeEq<Other> {
  type Output;
}

pub enum True {}
pub enum False {}

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
// If T1 == T2, generates TypeEq with Output = True.
// Otherwise, generates TypeEq with Output = False.
//
// Usage:
// gen_equalities![A, B, C];
#[macro_export]
macro_rules! gen_inequalities {

  ($T1:ty) => {
    impl TypeEq<$T1> for $T1 {
      type Output = True;
    }
  };

  ($T1:ty, $T2:ty $(, $tail:tt)*) => {
    impl TypeEq<$T2> for $T1 {
      type Output = False;
    }

    impl TypeEq<$T1> for $T2 {
      type Output = False;
    }

    type_level::gen_inequalities!($T1 $(, $tail)*);
  };
}

#[macro_export]
macro_rules! gen_equalities {
  () => {
  };

  ($T1:ty) => {
    impl TypeEq<$T1> for $T1 {
      type Output = True;
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
