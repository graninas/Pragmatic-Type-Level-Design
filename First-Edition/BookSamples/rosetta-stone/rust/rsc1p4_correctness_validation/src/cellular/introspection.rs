
use type_level::IInterface;
use type_level::Wrapper;
use type_level::Eval;
use tl_list_lib::HList;
use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use tl_list_lib::I32List;
use tl_list_lib::CNI32_;
use tl_list_lib::CCI32_;
use tl_str_list::TlStr;

use crate::automaton::IState;
use crate::automaton::IStep;
use crate::automaton::INeighborhood;
use crate::automaton::IStateTransition;
use crate::automaton::ICellCondition;
use crate::automaton::StateTransition;
use crate::automaton::StateWrapper;
use crate::automaton::IRule;
use crate::automaton::Step;
use crate::cellular::language::extensions::RuleImpl;
use crate::cellular::language::extensions::AdjacentsLvlImpl;
use crate::cellular::language::extensions::StateImpl;
use crate::cellular::language::extensions::NeighborsCountImpl;
use crate::cellular::language::integrity::Verify;
use crate::cellular::language::integrity::StateInList;
use crate::cellular::language::integrity::WithIntegrity;

// This code proves that interpretation of the model is possible
// as we did it in Haskell.

#[allow(dead_code)]
pub enum Introspect{}


impl<I>
  Eval<Introspect, String>
  for TlN_<I>
{
  fn eval() -> String {
    "".to_string()
  }
}

impl<I, Item, Tail>
  Eval<Introspect, String>
  for TlC_<I, Item, Tail>
  where
    Item: IInterface<I> + Eval<Introspect, String>,
    Tail: HList<I> + Eval<Introspect, String>
{
  fn eval() -> String {
    "\n    - ".to_string()
      + &Item::eval()
      + &Tail::eval()
  }
}

impl Eval<Introspect, String> for CNI32_ {
  fn eval() -> String {
    "".to_string()
  }
}

impl<const C: i32, Tail>
  Eval<Introspect, String>
  for CCI32_<C, Tail>
  where
    Tail: Eval<Introspect, String>
{
  fn eval() -> String {
    format!("{} {}", C, Tail::eval())
  }
}

impl<Name, const IDX: u8>
  Eval<Introspect, String>
  for StateWrapper<StateImpl<Name, IDX>>
  where
    Name: TlStr
{
  fn eval() -> String {
    format!("{} [{}]", Name::to_string(), IDX)
  }
}

impl<S, Cnts>
  Eval<Introspect, String>
  for Wrapper<ICellCondition, NeighborsCountImpl<S, Cnts>>
  where
    S: IInterface<IState> + Eval<Introspect, String>,
    Cnts: I32List + Eval<Introspect, String>
{
  fn eval() -> String {
    format!("{}, {}", S::eval(), Cnts::eval())
  }
}

impl<FromState, ToState, Condition>
  Eval<Introspect, String>
  for StateTransition<FromState, ToState, Condition>
  where
    FromState: IInterface<IState> + Eval<Introspect, String>,
    ToState: IInterface<IState> + Eval<Introspect, String>,
    Condition: IInterface<ICellCondition> + Eval<Introspect, String>
{
  fn eval() -> String {
    FromState::eval()
      + " -> "
      + &ToState::eval()
      + &": "
      + &Condition::eval()
  }
}

impl<StDict, DefSt, Ts>
  Eval<Introspect, String>
  for WithIntegrity<StDict, Step<DefSt, Ts>>
  where
    DefSt: IInterface<IState> + Eval<Introspect, String>,
    Ts: HList<IStateTransition> + Eval<Introspect, String>,
    StDict: HList<IState> + Verify<StateInList<DefSt>>
{
  fn eval() -> String {
    // TODO: verify states in state transitions
    return format!("\n    Valid: {}\n  Default state: {}", StDict::verify(), DefSt::eval())
      + &"\n  State transitions:".to_string()
      + &Ts::eval();
  }
}

impl<const LVL: u8>
  Eval<Introspect, String>
  for Wrapper<INeighborhood, AdjacentsLvlImpl<LVL>>
{
  fn eval() -> String {
    "Lvl ".to_string() + &format!("{}", LVL)
  }
}

impl<StDict, Name, Code, Nh, Step>
  Eval<Introspect, String>
  for Wrapper<IRule, RuleImpl<StDict, Name, Code, Nh, Step>>
  where
    StDict: HList<IState>,
    Name: TlStr,
    Code: TlStr,
    Nh: IInterface<INeighborhood> + Eval<Introspect, String>,
    Step: IInterface<IStep>,
    WithIntegrity<StDict, Step> : Eval<Introspect, String>
{
  fn eval() -> String {
    "Rule <".to_string()
      + &Name::to_string()
      + &"> [".to_string()
      + &Code::to_string()
      + &"]\n  Neighborhood: "
      + &Nh::eval()
      + &<WithIntegrity<StDict, Step>>::eval()
  }
}

