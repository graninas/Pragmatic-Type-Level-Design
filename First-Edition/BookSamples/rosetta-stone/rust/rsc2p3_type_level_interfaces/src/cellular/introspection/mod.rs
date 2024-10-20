
use type_level::IInterface;
use type_level::Eval;
use tl_list_lib::HList;
use tl_str_list::TlStr;

use crate::automaton::IState;
use crate::automaton::IStep;
use crate::automaton::INeighborhood;
use crate::automaton::IStateTransition;
use crate::automaton::RuleWrapper;
use crate::automaton::Step;
use crate::cellular::language::extensions::RuleImpl;


// This code proves that interpretation of the model is possible
// as we did it in Haskell.
//
// TODO: fully interpret all the notions of the automaton.

#[allow(dead_code)]
pub enum Introspect{}

impl<DefSt, Ts>
  Eval<Introspect, String>
  for Step<DefSt, Ts>
  where
    DefSt: IInterface<IState>,
    Ts: HList<IStateTransition>
{
  fn eval() -> String {
    "State transitions: (TODO)".to_string()
  }
}

impl<N, C, NH, S>
  Eval<Introspect, String> for RuleWrapper<RuleImpl<N, C, NH, S>>
  where
    N: TlStr,
    C: TlStr,
    NH: IInterface<INeighborhood>,
    S: IInterface<IStep> + Eval<Introspect, String>,
{
  fn eval() -> String {
    "Rule <".to_string()
      + &N::to_string()
      + &"> [".to_string()
      + &C::to_string()
      + &"]\n"
      + &S::eval()
  }
}

