use std::marker::PhantomData;


trait Description {
  fn describe(&self) -> String;
}

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

// Valuefied wrapper
struct Wrapper {
  describe_f: Box<dyn Fn() -> String>,
}

impl Wrapper {
  fn new<T: 'static + Description>(val: T) -> Self {
    let boxed = Box::new(val);
    Wrapper {
      describe_f: Box::new(move || boxed.describe()),
    }
  }

  fn describe(&self) -> String {
    (self.describe_f)()
  }
}


enum EmptyADT1 {}
struct EmptyADT2 {}

impl Description for Proxy<EmptyADT1> {
  fn describe(&self) -> String {
    String::from("EmptyADT1")
  }
}

impl Description for Proxy<EmptyADT2> {
  fn describe(&self) -> String {
    String::from("EmptyADT2")
  }
}


fn main() {
  let empty_adt1_proxy = Proxy::<EmptyADT1>::new();
  let empty_adt2_proxy = Proxy::<EmptyADT2>::new();

  let proxies: Vec<Wrapper> = vec![
    Wrapper::new(empty_adt1_proxy),
    Wrapper::new(empty_adt2_proxy),
  ];

  for proxy in proxies {
    println!("{}", proxy.describe());
  }

}
