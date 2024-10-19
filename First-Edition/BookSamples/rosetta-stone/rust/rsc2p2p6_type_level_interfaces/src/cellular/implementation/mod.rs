
use crate::eval::Eval;
use tl_list_lib::IInterface;
use std::marker::PhantomData;

use crate::automaton::IRule;

// use crate::automaton::CellConditionWrapper;
// use crate::automaton::IStateTransition;
// use crate::automaton::IState;
// use crate::automaton::StateWrapper;
// use crate::automaton::StateTransition;
// use crate::automaton::Step;
// use crate::cellular::language::extensions::State;
// use crate::cellular::language::extensions::NeighborsCount;
// use crate::cellular::language::extensions::AdjacentsLvl;
// use crate::cellular::language::extensions::Rule;

// use tl_str_macro::tl_str;
// use tl_str_list::N_;
// use tl_str_list::C_;
// use tl_list_lib::tl_list;
// use tl_list_lib::tl_i32_list;
// use tl_list_lib::CNI32_;        // TODO: can we improve the macro to avoid importing these?
// use tl_list_lib::CCI32_;
// use tl_list_lib::TlN_;
// use tl_list_lib::TlC_;



impl<R: IInterface<IRule>> Eval<R, String> for R {
  fn eval(_: PhantomData::<R>) -> String {
    "Evaluated.".to_string()
  }
}

