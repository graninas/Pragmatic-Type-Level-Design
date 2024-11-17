package cellular
package language

import graninas.typelevel._


// -- Interfaces

// -- -- State

sealed trait IState

object IStateObj:                // option 1: opaque types
  type MkState[A] <: IState      // TODO: replace with type families


sealed trait ICellCondition

object ICellConditionObj:
  type MkCellCondition[A] <: ICellCondition


sealed trait INeighborhood

object INeighborhoodObj:
  type MkNeighborhood[A] <: INeighborhood


// // -- Customizable domain model

sealed trait IStateTransition

object IStateTransitionObj:
  type MkStateTransition[A] <: IStateTransition


sealed trait IStep

object IStepObj:
  type MkStep[A] <: IStep


sealed trait IRule

object IRuleObj:
  type MkRule[A] <: IRule
