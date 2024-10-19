use tl_list_lib::IInterface;
use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use tl_list_lib::tl_list;

use assert_type_eq::assert_type_eq;
use std::marker::PhantomData;

mod cellular;
mod eval;

use cellular::language::automaton;
use cellular::language::automaton::IState;
use cellular::assets::game_of_life::A;
use cellular::assets::game_of_life::D;
use cellular::assets::game_of_life::GoLRule;
use cellular::implementation;

use eval::Eval;

// Testing the state list
const EVIDENCE_A: PhantomData::<A> = PhantomData;
const EVIDENCE_D: PhantomData::<D> = PhantomData;

type StateList1 = TlC_<IState, A, TlC_<IState, D, TlN_<IState>>>;
const STATE_LIST1_EVIDENCE: PhantomData::<StateList1> = PhantomData;

type StateList2 = tl_list![IState, A, D];
const STATE_LIST2_EVIDENCE: PhantomData::<StateList2> = PhantomData;

assert_type_eq!(StateList1, StateList2);




fn main () {

  let rule: PhantomData::<GoLRule> = PhantomData;
  let res = <GoLRule as Eval<GoLRule, String>>::eval(rule);

  println!("{}", res);
}
