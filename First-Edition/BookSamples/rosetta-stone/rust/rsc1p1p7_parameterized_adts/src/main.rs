use std::marker::PhantomData;

trait Description {
  fn describe() -> String;
}

struct Benoit;
struct Mandelbrot<T> (PhantomData<T>);

type Fractal = Mandelbrot<Mandelbrot<Mandelbrot<Benoit>>>;

impl Description for Benoit {
  fn describe() -> String {
      String::from("Benoit")
  }
}

impl<T: Description> Description for Mandelbrot<T> {
  fn describe() -> String {
    String::from("Mandelbrot ") + &T::describe()
  }
}

fn main() {
  println!("{}", Benoit::describe());
  println!("{}", Fractal::describe());
}
