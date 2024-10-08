use std::marker::PhantomData;


enum EmptyADT1 {}
struct EmptyADT2 {}

struct Proxy<T> {
  _marker: PhantomData<T>,
}

impl<T> Proxy<T> {
  fn new() -> Self {
    Proxy {
      _marker: PhantomData,
    }
  }
}

trait Description {
  fn describe() -> &'static str;
}


impl Description for EmptyADT1 {
  fn describe() -> &'static str {
    "EmptyADT1"
  }
}

impl Description for EmptyADT2 {
  fn describe() -> &'static str {
    "EmptyADT2"
  }
}

fn main() {
  #[allow(unused_variables)]
  let empty_adt1_proxy = Proxy::<EmptyADT1>::new();
  #[allow(unused_variables)]
  let empty_adt2_proxy = Proxy::<EmptyADT2>::new();

  println!("{}", EmptyADT1::describe());
  println!("{}", EmptyADT2::describe());
}
