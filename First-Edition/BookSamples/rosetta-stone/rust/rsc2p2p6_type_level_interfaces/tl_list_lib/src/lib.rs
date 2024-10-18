use std::marker::PhantomData;
use assert_type_eq::assert_type_eq;

pub trait IInterface<I> {
  type Interface;
}

pub struct N_;
pub struct C_<I, Item:IInterface<I>, Tail>(PhantomData::<(I, Item, Tail)>);

struct HList<T:IInterface<T>>(PhantomData<T>);

// #[macro_export]
// macro_rules! tl_list {
//   () => {
//       N_
//   };
//   ($head:ty $(, $tail:ty)*) => {
//       C_<$head, tl_list!($($tail),*)>
//   };
// }


#[macro_export]
macro_rules! tl_list_impl {
    ($asserts:tt, $head:ty) => {
      // $asserts
      C_<$head, N_>
    };

    ($asserts:tt, $head:ty, $next:ty $(, $tail:ty)*) => {
      // $asserts
      C_<$head, tl_list_impl!((assert_type_eq!($head, $next)), $next $(, $tail)*)>
    };
}


#[macro_export]
macro_rules! tl_list {
  () => {
      N_
  };
  ($head:ty) => {
      C_<$head, N_>
  };

  ($head:ty, $next:ty $(, $tail:ty)*) => {
    tl_list_impl!((assert_type_eq!(u8, u8)), $head, $next $(, $tail)*)
    // tl_list_impl!((assert_type_eq!($head::IInterface, $next::IInterface)), $head, $next $(, $tail)*)
  };
}


