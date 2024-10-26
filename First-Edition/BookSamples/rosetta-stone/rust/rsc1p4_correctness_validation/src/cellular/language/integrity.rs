use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use tl_list_lib::tl_list;
use tl_list_lib::HList;

use tl_str_macro::tl_str;

use type_level::IInterface;
use type_level::TypeEq;
use type_level::BoolKind;
use type_level::True;
use type_level::False;
use type_level::If;
use type_level::gen_equalities;
use type_level::assert_type_eq;

use std::marker::PhantomData;

use crate::automaton;
use crate::automaton::IState;
use crate::automaton::StatesDict;


pub trait Verify<Verb> {
  type X_;                  // helper variables
  type Y_;
  type Output;

  fn verify() -> bool;
}

pub struct StateInList<St> (PhantomData::<St>);

impl<St>
  Verify<StateInList<St>>
  for TlN_<IState>
{
  type X_ = False;
  type Y_ = False;
  type Output = False;

  fn verify() -> bool {
    <Self::Output as BoolKind>::to_bool()
  }
}

impl<St, CurSt, Rest>
  Verify<StateInList<St>>
  for TlC_<IState, CurSt, Rest>
  where
    St: IInterface<IState>,
    CurSt: IInterface<IState>,
    Rest: HList<IState> + Verify<StateInList<St>>,
    St: TypeEq<CurSt>,
    <St as TypeEq<CurSt>>::Output:
        If<True, <Rest as Verify<StateInList<St>>>::Output>,
    <<St as TypeEq<CurSt>>::Output as If<type_level::True, <Rest as Verify<StateInList<St>>>::Output>>::Output: BoolKind
{
  type Y_ = <Rest as Verify<StateInList<St>>>::Output;
  type X_ = <St as TypeEq<CurSt>>::Output;

  type Output = <Self::X_ as If<True, Self::Y_>>::Output;

  fn verify() -> bool {
    <Self::Output as BoolKind>::to_bool()
  }
}
