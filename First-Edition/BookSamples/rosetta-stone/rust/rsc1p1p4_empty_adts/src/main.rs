
enum EmptyADT1 {}
struct EmptyADT2;

trait StaticDescription {
  fn static_describe() -> &'static str;
}

impl StaticDescription for EmptyADT1 {
  fn static_describe() -> &'static str {
    "EmptyADT1"
  }
}

impl StaticDescription for EmptyADT2 {
  fn static_describe() -> &'static str {
    "EmptyADT2"
  }
}


fn main() {
  println!("{}", EmptyADT1::static_describe());
  println!("{}", EmptyADT2::static_describe());
}
