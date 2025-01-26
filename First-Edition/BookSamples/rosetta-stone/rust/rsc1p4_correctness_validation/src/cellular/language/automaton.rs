use type_level::IInterface;
use tl_list_lib::HList;
use std::marker::PhantomData;

// -- Interfaces

pub struct IState;
pub struct ICellCondition;
pub struct INeighborhood;
pub struct IStateTransition;
pub struct IStep;
pub struct IRule;

// For some reason, abstract Wrapper is incompatible with the
// type equality mechanism. Only types with custom wrappers
// can be used with gen_equalities! macro.
pub struct StateWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IState> for StateWrapper<T> {
  type Interface = IState;
}


// -- Customizable domain model

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


pub struct Step <
    DefState: IInterface<IState>,
    Transitions: HList<IStateTransition>>
      (PhantomData::<(DefState, Transitions)>);

impl<D, T>
  IInterface<IStep>
  for Step<D, T>
  where
    D: IInterface<IState>,
    T: HList<IStateTransition>
{
  type Interface = IStep;
}


