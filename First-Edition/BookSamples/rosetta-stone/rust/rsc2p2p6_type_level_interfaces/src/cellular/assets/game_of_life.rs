
use crate::automaton::ICellCondition;
use crate::automaton::CellConditionWrapper;
use crate::automaton::IState;
use crate::automaton::StateWrapper;

use crate::cellular::language::extensions::State;

pub type A = State<0>;
pub type D = State<1>;


// type Neighbors3  = NeighborsCount A '[3  ]
// type Neighbors23 = NeighborsCount A '[2,3]

// type GoLStep = 'Step ('DefState D)
//   '[ 'StateTransition D A Neighbors3
//    , 'StateTransition A A Neighbors23
//    ]

// type GoLRule = Rule
//   "Game of Life"
//   "gol"
//   (AdjacentsLvl 1)
//   GoLStep
