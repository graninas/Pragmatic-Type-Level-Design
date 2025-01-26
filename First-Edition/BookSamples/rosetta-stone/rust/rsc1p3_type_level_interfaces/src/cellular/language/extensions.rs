use crate::automaton::IState;
use crate::automaton::INeighborhood;
use crate::automaton::IStep;
use crate::automaton::ICellCondition;
use crate::automaton::IRule;

use std::marker::PhantomData;
use type_level::IInterface;
use type_level::Wrapper;
use tl_list_lib::I32List;
use tl_str_list::TlStr;


pub struct NeighborsCountImpl<S, Cnts: I32List>
  (PhantomData::<(S, Cnts)>);

pub type NeighborsCount<S, Cnts> =
  Wrapper<ICellCondition, NeighborsCountImpl<S, Cnts>>;


pub struct AdjacentsLvlImpl<const LVL: u8>;

pub type AdjacentsLvl<const LVL: u8> =
  Wrapper<INeighborhood, AdjacentsLvlImpl<LVL>>;


pub struct StateImpl<Name: TlStr, const IDX: u8> (PhantomData::<Name>);

pub type State<Name, const IDX: u8> =
  Wrapper<IState, StateImpl<Name, IDX>>;


pub struct RuleImpl <
    Name: TlStr,
    Code: TlStr,
    Neighborhood: IInterface<INeighborhood>,
    Step: IInterface<IStep>>
      (PhantomData::<(Name, Code, Neighborhood, Step)>);

pub type Rule<N, C, Nh, S> =
  Wrapper<IRule, RuleImpl<N, C, Nh, S>>;
