use std::marker::PhantomData;

// Type-level list for type-level strings
pub struct N_;
pub struct C_<const CH: char, Tail>(PhantomData<Tail>);

// Type-level short string list
pub struct ShortString<
const CH0: char, const CH1: char, const CH2: char,
const CH3: char, const CH4: char, const CH5: char,
const CH6: char, const CH7: char, const CH8: char,
const CH9: char>;


pub trait TlStr {
  fn to_string() -> String;
}

impl TlStr for N_ {
  fn to_string() -> String {
    "".to_string()
  }
}

impl<const CH: char, Tail:TlStr > TlStr for C_<CH, Tail> {
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
