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
    FROM_STATE: IInterface<IState>,
    TO_STATE: IInterface<IState>,
    CONDITION: IInterface<ICellCondition>>
      (PhantomData::<(FROM_STATE, TO_STATE, CONDITION)>);


impl<F, T, C> IInterface<StateTransition<F, T, C>>
  for StateTransition<F, T, C>
  where
    F: IInterface<IState>,
    T: IInterface<IState>,
    C: IInterface<ICellCondition>
{
  type Interface = IStateTransition;
}


pub struct DefaultState <STATE: IInterface<IState>> (PhantomData::<STATE>);




// pub struct Step<DEF_STATE, TRANSITIONS: LIST>
//   (PhantomData::<DEF_STATE, TRANSITIONS>);
