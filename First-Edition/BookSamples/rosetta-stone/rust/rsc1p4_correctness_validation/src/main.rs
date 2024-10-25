use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use tl_list_lib::tl_list;
use tl_list_lib::HList;

use tl_str_macro::tl_str;
use tl_str_list::N_;
use tl_str_list::C_;

use type_level::IInterface;
use type_level::Eval;
use type_level::TypeEq;
use type_level::True;
use type_level::False;
use type_level::If;
use type_level::gen_equalities;
use type_level::assert_type_eq;

use std::marker::PhantomData;

mod cellular;

use cellular::language::automaton;
use cellular::language::automaton::IState;
use cellular::language::automaton::StatesDict;
use cellular::language::extensions::State;
use cellular::assets::game_of_life::A;
use cellular::assets::game_of_life::D;
use cellular::assets::game_of_life::GoLRule;

// Dictionary
type StatesList = tl_list![IState, A, D];


// Unknown type not present in the dictionary
pub type Unknown = State<tl_str!("Unknown"), 100>;

gen_equalities![A, D, Unknown];



pub trait Verify<Verb> {
  type X_;
  type Y_;
  type Output;
}

pub struct StateInList<St> (PhantomData::<St>);

impl<St>
  Verify<StateInList<St>>
  for TlN_<IState>
{
  type X_ = False;
  type Y_ = False;
  type Output = False;
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
        If<True, <Rest as Verify<StateInList<St>>>::Output>
{
  type Y_ = <Rest as Verify<StateInList<St>>>::Output;
  type X_ = <St as TypeEq<CurSt>>::Output;

  type Output = <Self::X_ as If<True, Self::Y_>>::Output;
}



type Verified = <StatesList as Verify<StateInList<A>>>::Output;
assert_type_eq!(True, Verified);

// won't compile
type UnkownTypeVerified = <StatesList as Verify<StateInList<Unknown>>>::Output;
assert_type_eq!(False, UnkownTypeVerified);


fn main () {


  // println!("{}", verified);
}
