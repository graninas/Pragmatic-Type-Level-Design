use type_level::IInterface;
use tl_list_lib::HList;
use std::marker::PhantomData;

// -- Interfaces

// -- -- State

pub struct IState;

pub struct StateWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IState> for StateWrapper<T> {
  type Interface = IState;
}


// -- -- Cell condition

pub struct ICellCondition;

pub struct CellConditionWrapper<T> (PhantomData::<T>);
impl<T> IInterface<ICellCondition> for CellConditionWrapper<T> {
  type Interface = ICellCondition;
}


// -- -- Neighborhood

pub struct INeighborhood;

pub struct NeighborhoodWrapper<T> (PhantomData::<T>);
impl<T> IInterface<INeighborhood> for NeighborhoodWrapper<T> {
  type Interface = INeighborhood;
}


// -- Customizable domain model

pub struct IStateTransition;

pub struct StateTransition <
    FromState: IInterface<IState>,
    ToState: IInterface<IState>,
    Condition: IInterface<ICellCondition>>
      (PhantomData::<(FromState, ToState, Condition)>);


impl<F, T, C> IInterface<IStateTransition>
  for StateTransition<F, T, C>
  where
    F: IInterface<IState>,
    T: IInterface<IState>,
    C: IInterface<ICellCondition>
{
  type Interface = IStateTransition;
}

// Trait for static type-level values (such as dictionaries)
pub trait StatesDict: HList<IState> {}
impl<T: HList<IState>> StatesDict for T {}


pub struct IStep<States: StatesDict>(PhantomData::<States>);

pub struct Step <
    DefState: IInterface<IState>,
    Transitions: HList<IStateTransition>>
      (PhantomData::<(DefState, Transitions)>);

impl<StDict, D, T>
  IInterface<IStep<StDict>>
  for Step<D, T>
  where
    StDict: StatesDict,
    D: IInterface<IState>,
    T: HList<IStateTransition>
{
  type Interface = IStep<StDict>;
}


// -- -- Rule

pub struct IRule;

pub struct RuleWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IRule> for RuleWrapper<T> {
  type Interface = IRule;
}
