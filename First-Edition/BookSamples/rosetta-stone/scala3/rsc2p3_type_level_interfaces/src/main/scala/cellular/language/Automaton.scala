package cellular
package language


// Interfaces

sealed trait IState
type MkState[A] <: IState

sealed trait ICellCondition
type MkCellCondition[A] <: ICellCondition

sealed trait INeighborhood
type MkNeighborhood[A] <: INeighborhood

sealed trait IStateTransition
type MkStateTransition[A] <: IStateTransition

sealed trait IStep
type MkStep[A] <: IStep

sealed trait IRule
type MkRule[A] <: IRule
