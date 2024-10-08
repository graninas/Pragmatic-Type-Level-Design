
enum EmptyADT1 {}
struct EmptyADT2 {}

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
  println!("{}", EmptyADT1::describe());
  println!("{}", EmptyADT2::describe());
}
