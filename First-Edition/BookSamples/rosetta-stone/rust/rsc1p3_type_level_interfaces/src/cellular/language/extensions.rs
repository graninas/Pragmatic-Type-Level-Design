use crate::automaton::CellConditionWrapper;
use crate::automaton::INeighborhood;
use crate::automaton::NeighborhoodWrapper;
use crate::automaton::StateWrapper;
use crate::automaton::IStep;
use crate::automaton::RuleWrapper;

use std::marker::PhantomData;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_str_list::TlStr;


pub struct NeighborsCountImpl<S, Cnts: I32List>
  (PhantomData::<(S, Cnts)>);

pub type NeighborsCount<S, Cnts> =
  CellConditionWrapper<NeighborsCountImpl<S, Cnts>>;


pub struct AdjacentsLvlImpl<const LVL: u8>;

pub type AdjacentsLvl<const LVL: u8> = NeighborhoodWrapper<AdjacentsLvlImpl<LVL>>;


pub struct StateImpl<Name: TlStr, const IDX: u8> (PhantomData::<Name>);

pub type State<Name, const IDX: u8> = StateWrapper<StateImpl<Name, IDX>>;


pub struct RuleImpl <
    Name: TlStr,
    Code: TlStr,
    Neighborhood: IInterface<INeighborhood>,
    Step: IInterface<IStep>>
      (PhantomData::<(Name, Code, Neighborhood, Step)>);

pub type Rule<N, C, NH, S> = RuleWrapper<RuleImpl<N, C, NH, S>>;