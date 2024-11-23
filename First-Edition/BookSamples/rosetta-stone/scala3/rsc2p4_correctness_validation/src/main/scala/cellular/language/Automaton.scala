package cellular
package language

import graninas.typelevel._

// Interfaces

sealed trait IState extends IInterface
type MkState[A] = IState {
  type Impl = A
}

sealed trait ICellCondition extends IInterface
type MkCellCondition[A] = ICellCondition {
  type Impl = A
}

sealed trait INeighborhood extends IInterface
type MkNeighborhood[A] = INeighborhood {
  type Impl = A
}

sealed trait IStateTransition extends IInterface
type MkStateTransition[A] = IStateTransition {
  type Impl = A
}

sealed trait IStep extends IInterface
type MkStep[A] = IStep {
  type Impl = A
}

sealed trait IRule extends IInterface
type MkRule[A] = IRule {
  type Impl = A
}
