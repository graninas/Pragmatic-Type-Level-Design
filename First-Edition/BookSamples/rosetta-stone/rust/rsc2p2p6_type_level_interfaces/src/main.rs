use tl_list_lib::IInterface;
use tl_list_lib::N_;
use tl_list_lib::C_;
use tl_list_lib::tl_list;
use tl_list_lib::tl_list_impl;

use assert_type_eq::assert_type_eq;
use std::marker::PhantomData;

mod cellular;

use cellular::language::automaton;
use cellular::language::automaton::IState;
use cellular::language::automaton::ICellCondition;
use cellular::assets::game_of_life;
use cellular::assets::game_of_life::A;
use cellular::assets::game_of_life::D;



const EVIDENCE_A: PhantomData::<A> = PhantomData;
const EVIDENCE_D: PhantomData::<D> = PhantomData;

type StateList1 = C_<IState, A, C_<IState, D, N_<IState>>>;
const STATE_LIST1_EVIDENCE: PhantomData::<StateList1> = PhantomData;

type StateList2 = tl_list![IState, A, D];
const STATE_LIST2_EVIDENCE: PhantomData::<StateList2> = PhantomData;

assert_type_eq!(StateList1, StateList2);

fn main () {

}
