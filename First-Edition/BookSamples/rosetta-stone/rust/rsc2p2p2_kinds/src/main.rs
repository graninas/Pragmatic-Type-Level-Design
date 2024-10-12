use std::marker::PhantomData;

mod evidence_macro;
mod adts;

use adts::bool::Bool;
use adts::bool::True;
use adts::bool::False;



// User-defined types

type Enabled = Bool<True>;
type Disabled = Bool<False>;

#[allow(dead_code)]
type Invalid = Bool<i32>;       // will compile unless used



fn main() {

  let _enabled: Enabled = Bool::<True> { _marker: PhantomData::<True> };

  let _disabled: Disabled = Bool::<False> { _marker: PhantomData::<False> };

  // won't compile
  // let invalid: Invalid = Bool::<i32> { _marker: PhantomData::<i32> };

}
