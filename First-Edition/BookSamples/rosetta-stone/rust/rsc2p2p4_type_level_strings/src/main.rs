use tl_str_macro::tl_str;
use tl_str_list::N_;
use tl_str_list::C_;
use tl_str_list::TlStr;
use tl_str_list::tl_chars;

use std::marker::PhantomData;

// Description

trait Description {
  fn describe() -> String;
}

// User-defined types

struct Person<Name> (PhantomData<Name>);

impl<Name> Description for Person<Name>
  where Name: TlStr
{
  fn describe() -> String {
    format!("Person {}", Name::to_string())
  }
}


fn main() {
  type Pushkin1 = Person< C_<'P', C_<'u', C_<'s', C_<'h', C_<'k', C_<'i', C_<'n', N_>>>>>>> >;
  println!("{}", Pushkin1::describe());
  // > Person Pushkin

  type Pushkin2 = Person<tl_chars!('P' 'u' 's' 'h' 'k' 'i' 'n')>;
  println!("{}", Pushkin2::describe());
  // > Person Pushkin

  type Pushkin3 = Person<tl_str!("Pushkin")>;
  println!("{}", Pushkin3::describe());
  // > Person Pushkin
}



