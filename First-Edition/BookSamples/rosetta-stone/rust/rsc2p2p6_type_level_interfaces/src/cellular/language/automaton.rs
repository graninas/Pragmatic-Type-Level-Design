use tl_list_lib::IInterface;
use std::marker::PhantomData;

// -- Interfaces

// -- -- Cell condition

pub struct ICellCondition;

pub struct CellConditionWrapper<T> (PhantomData::<T>);
impl<T> IInterface<ICellCondition> for CellConditionWrapper<T> {
  type Interface = ICellCondition;
}


// -- -- State

pub struct IState;

pub struct StateWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IState> for StateWrapper<T> {
  type Interface = IState;
}

// -- -- Neighborhood

pub struct INeighborhood;

pub struct NeighborhoodWrapper<T> (PhantomData::<T>);
impl<T> IInterface<INeighborhood> for NeighborhoodWrapper<T> {
  type Interface = INeighborhood;
}

// -- -- Rule

pub struct IRule;

pub struct RuleWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IRule> for RuleWrapper<T> {
  type Interface = IRule;
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


pub struct DefaultState <State: IInterface<IState>> (PhantomData::<State>);




// pub struct Step<DEF_STATE, TRANSITIONS: LIST>
//   (PhantomData::<DEF_STATE, TRANSITIONS>);
