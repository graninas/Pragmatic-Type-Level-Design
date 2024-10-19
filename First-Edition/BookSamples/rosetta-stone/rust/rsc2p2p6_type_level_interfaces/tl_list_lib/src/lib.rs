use std::marker::PhantomData;
use assert_type_eq::assert_type_eq;

pub trait IInterface<I> {
  type Interface;
}

pub struct N_<I>(PhantomData::<I>);
pub struct C_<I, Item:IInterface<I>, Tail>(PhantomData::<(I, Item, Tail)>);


#[macro_export]
macro_rules! tl_list {
  ($iface:ty) => {
    N_<$iface>
  };

  ($iface:ty, $head:ty $(, $tail:ty)*) => {
    C_<$iface, $head, tl_list!($iface $(, $tail)*)>
  };
}

