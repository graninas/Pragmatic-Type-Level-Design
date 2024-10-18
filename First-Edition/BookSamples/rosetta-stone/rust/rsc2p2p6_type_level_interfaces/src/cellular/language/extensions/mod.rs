use crate::automaton::ICellCondition;
use crate::automaton::CellConditionWrapper;
use crate::automaton::IState;
use crate::automaton::StateWrapper;

pub struct StateImpl<const IDX: u8>;

pub type State<const IDX: u8> = StateWrapper<StateImpl<IDX>>;


// -- -- Implementations

// data NeighborsCountImpl
//   (state :: IState)
//   (counts :: [Nat])
// type NeighborsCount s c =
//   MkCellCondition (NeighborsCountImpl s c)

// data AdjacentsLvlImpl
//   (lvl :: Nat)
// type AdjacentsLvl lvl = MkNeighborhood (AdjacentsLvlImpl lvl)

// data StateImpl
//   (name :: Symbol)
//   (idx  :: Nat)
// type State n i = MkState (StateImpl n i)

// data RuleImpl
//   (name :: Symbol)
//   (code :: Symbol)
//   (nh   :: INeighborhood)
//   (step :: CustomStep)
// type Rule n c nh s = MkRule (RuleImpl n c nh s)

