use tl_list_lib::ListItem;
use tl_list_lib::N_;
use tl_list_lib::C_;
use tl_list_lib::tl_list;
use tl_list_lib::tl_list_impl;

use assert_type_eq::assert_type_eq;
use std::marker::PhantomData;


// Color kind and interface

struct IColor;

struct ColorWrapper<T> (PhantomData<T>);
impl<T> ListItem for ColorWrapper<T> {
  type Kind = IColor;
}

// Color implementations
struct ColorImpl<const R: u8, const G: u8, const B: u8>;
type RedColor   = ColorWrapper<ColorImpl<255, 0, 0>>;
type GreenColor = ColorWrapper<ColorImpl<0, 255, 0>>;

// State kind and interface
struct IState;

struct StateWrapper<T> (PhantomData::<T>);
impl<T> ListItem for StateWrapper<T> {
  type Kind = IState;
}

// State implementations
struct State1Impl<const VAL: u8>;
type State42   = StateWrapper<State1Impl<42>>;
type StateZero = StateWrapper<State1Impl<0>>;


// lists

type StateList1 = C_<State42, C_<StateZero, N_>>;
const STATE_LIST1_EVIDENCE: PhantomData::<StateList1> = PhantomData;

type StateList2 = tl_list![State42, StateZero];
const STATE_LIST2_EVIDENCE: PhantomData::<StateList2> = PhantomData;

type ColorsList = C_<RedColor, C_<GreenColor, N_>>;
const COLOR_LIST_EVIDENCE: PhantomData::<ColorsList> = PhantomData;

// invalid lists

// Allowed to construct bare invalid lists manually
type InvalidBareList = C_<State42, C_<RedColor, N_>>;
const INVALID_BARE_LIST_EVIDENCE_ALLOWED: PhantomData::<InvalidBareList> = PhantomData;

// For now, there is no mechanism to prevent this
type InvalidList = tl_list![State42, RedColor];
const INVALID_LIST_EVIDENCE: PhantomData::<InvalidList> = PhantomData;
// Need to inject this into the macro:
// assert_type_eq!(<State42 as ListItem>::Kind, <RedColor as ListItem>::Kind);
// This should be a procedural macro.

