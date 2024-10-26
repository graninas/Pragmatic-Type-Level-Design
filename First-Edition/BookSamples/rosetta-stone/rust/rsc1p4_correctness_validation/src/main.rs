use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use tl_list_lib::tl_list;
use tl_list_lib::HList;

use tl_str_macro::tl_str;

use type_level::IInterface;
use type_level::TypeEq;
use type_level::True;
use type_level::False;
use type_level::If;
use type_level::gen_equalities;
use type_level::assert_type_eq;
use type_level::Eval;


use std::marker::PhantomData;

mod cellular;

use cellular::language::automaton;
use cellular::language::automaton::IState;
use cellular::language::extensions::State;
use cellular::language::integrity::Verify;
use cellular::language::integrity::StateInList;
use cellular::assets::game_of_life::A;
use cellular::assets::game_of_life::D;
use cellular::assets::game_of_life::Unknown;
use cellular::assets::game_of_life::GoLRule;

// Dictionary
type StatesList = tl_list![IState, A, D];

type Verified = <StatesList as Verify<StateInList<A>>>::Output;
assert_type_eq!(True, Verified);

// won't compile
// type UnkownTypeVerified = <StatesList as Verify<StateInList<Unknown>>>::Output;
// assert_type_eq!(True, UnkownTypeVerified);

fn main () {
  let res = GoLRule::eval();

  println!("{}", res);
}
