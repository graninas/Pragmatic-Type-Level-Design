use std::marker::PhantomData;
use assert_type_eq::assert_type_eq;

pub trait IInterface<I> {
  type Interface;
}

pub struct N_<I>(PhantomData::<I>);
pub struct C_<I, Item:IInterface<I>, Tail>(PhantomData::<(I, Item, Tail)>);

#[macro_export]
macro_rules! tl_list_impl {
    ($iface:ty) => {
      N_<$iface>
    };

    ($iface:ty, $head:ty) => {
      C_<$iface, $head, N_<$iface>>
    };

    ($iface:ty, $head:ty, $next:ty $(, $tail:ty)*) => {
      C_<$iface, $head, tl_list_impl!($iface, $next $(, $tail)*)>
    };
}


#[macro_export]
macro_rules! tl_list {
  ($iface:ty) => {
      N_<$iface>
  };

  ($iface:ty, $head:ty, $next:ty $(, $tail:ty)*) => {
    tl_list_impl!($iface, $head, $next $(, $tail)*)
  };
}


