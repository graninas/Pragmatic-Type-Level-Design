use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use tl_list_lib::HList;
use tl_str_list::TlStr;

use type_level::IInterface;
use type_level::Wrapper;
use type_level::IsSame;
use type_level::BoolKind;
use type_level::True;
use type_level::False;
use type_level::If;

use std::marker::PhantomData;

use crate::automaton::IState;
use crate::automaton::IStep;
use crate::automaton::IStateTransition;
use crate::automaton::INeighborhood;
use crate::automaton::IRule;
use crate::automaton::Step;
use crate::cellular::language::extensions::RuleImpl;


// Verification mechanism

pub trait Verify<Validator> {
  type X_;                  // helper variables
  type Y_;
  type Output;

  fn verify() -> bool;
}

// Helper

pub struct WithIntegrity<StDict: HList<IState>, T>
  (PhantomData::<(StDict, T)>);


// Verify state in list

pub struct StateInList<St> (PhantomData::<St>);

impl<St>
  Verify<StateInList<St>>
  for TlN_<IState>
{
  type X_ = False;
  type Y_ = False;
  type Output = False;

  fn verify() -> bool {
    false
  }
}

impl<St, CurSt, Rest>
  Verify<StateInList<St>>
  for TlC_<IState, CurSt, Rest>
  where
    St: IInterface<IState>,
    CurSt: IInterface<IState>,
    Rest: HList<IState> + Verify<StateInList<St>>,
    St: IsSame<CurSt>,
    <St as IsSame<CurSt>>::Output:
        If<True, <Rest as Verify<StateInList<St>>>::Output>,

    <<St as IsSame<CurSt>>::Output
        as If<type_level::True,
                <Rest as Verify<StateInList<St>>>::Output>>::Output: BoolKind
{
  type Y_ = <Rest as Verify<StateInList<St>>>::Output;
  type X_ = <St as IsSame<CurSt>>::Output;

  type Output = <Self::X_ as If<True, Self::Y_>>::Output;

  fn verify() -> bool {
    <Self::Output as BoolKind>::to_bool()
  }
}


// Verify rule states are okay


pub struct StatesValid;

impl<StDict, DefSt, Ts>
  Verify<StatesValid>
  for WithIntegrity<StDict, Step<DefSt, Ts>>
  where
    DefSt: IInterface<IState>,
    Ts: HList<IStateTransition>,
    StDict: HList<IState> + Verify<StateInList<DefSt>>
{
  type X_ = True;
  type Y_ = True;
  type Output = <StDict as Verify<StateInList<DefSt>>>::Output;

  fn verify() -> bool {
    // TODO: verify states in state transitions
    StDict::verify()
  }
}

impl<StDict, N, C, Nh, Step>
  Verify<StatesValid>
  for Wrapper<IRule, RuleImpl<StDict, N, C, Nh, Step>>
  where
    StDict: HList<IState>,
    N: TlStr,
    C: TlStr,
    Nh: IInterface<INeighborhood>,
    Step: IInterface<IStep>,
    WithIntegrity<StDict, Step> : Verify<StatesValid>
{
  type X_ = True;
  type Y_ = True;
  type Output = <WithIntegrity<StDict, Step> as Verify<StatesValid>>::Output;

  fn verify() -> bool {
    <WithIntegrity<StDict, Step>>::verify()
  }
}
