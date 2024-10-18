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

type StateList = C_<IState, A, C_<IState, D, N_>>;
const STATE_LIST_EVIDENCE: PhantomData::<StateList> = PhantomData;


fn main () {

}
