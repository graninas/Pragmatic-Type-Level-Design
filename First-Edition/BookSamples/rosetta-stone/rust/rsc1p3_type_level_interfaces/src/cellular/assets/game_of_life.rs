
use crate::automaton::IStateTransition;
use crate::automaton::StateTransition;
use crate::automaton::Step;
use crate::cellular::language::extensions::State;
use crate::cellular::language::extensions::UnknownState;
use crate::cellular::language::extensions::NeighborsCount;
use crate::cellular::language::extensions::AdjacentsLvl;
use crate::cellular::language::extensions::Rule;

use tl_str_macro::tl_str;
use tl_list_lib::tl_list;
use tl_list_lib::tl_i32_list;

use std::marker::PhantomData;

pub type A = State<tl_str!("Alive"), 1>;
pub type D = State<tl_str!("Dead"), 0>;

pub type Neighbors3  = NeighborsCount<A, tl_i32_list![3]>;
pub type Neighbors23 = NeighborsCount<A, tl_i32_list![2, 3]>;

pub type GolTransitions = tl_list![
  IStateTransition,
  StateTransition<D, A, Neighbors3>,
  StateTransition<A, A, Neighbors23>,
  StateTransition<UnknownState, UnknownState, Neighbors3>
  ];

pub type GoLStep = Step<D, GolTransitions>;

pub type GoLRule = Rule<
  tl_str!("Game of Life"),
  tl_str!("gol"),
  AdjacentsLvl<1>,
  GoLStep>;

#[allow(dead_code)]
const GOL_RULE_EVIDENCE: PhantomData::<GoLRule> = PhantomData;

