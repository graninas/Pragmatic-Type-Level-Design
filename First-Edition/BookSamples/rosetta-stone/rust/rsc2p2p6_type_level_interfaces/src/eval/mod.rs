
use std::marker::PhantomData;


pub trait Eval<T, R>{
  fn eval(it: PhantomData::<T>) -> R;
}
