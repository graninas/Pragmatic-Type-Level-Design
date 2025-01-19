use std::marker::PhantomData;
use type_level::IInterface;

// Type-level heterogeneous kinded list

pub trait HList<I> {
  type Interface;
}

pub struct TlN_<I>(PhantomData::<I>);
pub struct TlC_<I, Item: IInterface<I>, Tail>(PhantomData::<(I, Item, Tail)>);

impl<I> HList<I> for TlN_<I> {
  type Interface = I;
}

impl<I, Item, Tail> HList<I> for TlC_<I, Item, Tail>
  where
    Item: IInterface<I>,
    Tail: HList<I>
{
  type Interface = I;
}


// Type-level constants list

pub struct CNI32_;

pub struct CCI32_<const HEAD: i32, Tail>(Tail);

pub trait I32List {
}

impl I32List for CNI32_ {}
impl<const C: i32, Tail> I32List for CCI32_<C, Tail> {}


// Macro for a type-level kinded list.
//
// Usage:
// type StateList = tl_list![IState, A, D];
#[macro_export]
macro_rules! tl_list {
  ($iface:ty) => {
    tl_list_lib::TlN_<$iface>
  };

  ($iface:ty, $head:ty $(, $tail:ty)*) => {
    tl_list_lib::TlC_<$iface, $head, tl_list_lib::tl_list!($iface $(, $tail)*)>
  };
}


// Macro for a type-level i32 constants list.
//
// Usage: type Numbers = tl_i32_list![0,1,2];
#[macro_export]
macro_rules! tl_i32_list {
  () => {
    tl_list_lib::CNI32_
  };

  ($head:literal $(, $tail:tt)*) => {
    tl_list_lib::CCI32_<$head, tl_list_lib::tl_i32_list!($($tail),*)>
  };
}
