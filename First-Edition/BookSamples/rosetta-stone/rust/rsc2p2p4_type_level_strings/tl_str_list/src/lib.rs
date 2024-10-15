use std::marker::PhantomData;

// Type-level list for type-level strings
pub struct N_;
pub struct C_<const CH: char, Tail>(PhantomData<Tail>);

pub trait TlStr {
  fn to_string() -> String;
}

impl TlStr for N_ {
  fn to_string() -> String {
    "".to_string()
  }
}

impl<const CH: char, Tail> TlStr for C_<CH, Tail>
  where
    Tail: TlStr,
{
  fn to_string() -> String {
    format!("{}{}", CH, Tail::to_string())
  }
}


#[macro_export]
macro_rules! tl_chars {
    () => {
        N_
    };

    ($ch:literal $($tail:tt)*) => {
        C_<$ch, tl_chars!($($tail)*)>
    };
}
