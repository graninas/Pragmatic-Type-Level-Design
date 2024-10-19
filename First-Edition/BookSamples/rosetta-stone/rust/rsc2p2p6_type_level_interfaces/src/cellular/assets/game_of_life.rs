
use crate::automaton::ICellCondition;
use crate::automaton::CellConditionWrapper;
use crate::automaton::IState;
use crate::automaton::StateWrapper;
use crate::cellular::language::extensions::State;
use crate::cellular::language::extensions::NeighborsCount;

use tl_list_lib::tl_list;
use tl_list_lib::tl_i32_list;
use tl_list_lib::CNI32_;        // TODO: can we improve the macro to avoid importing these?
use tl_list_lib::CCI32_;

use std::marker::PhantomData;

pub type A = State<0>;
pub type D = State<1>;

pub type Ns3 = tl_i32_list![3];
pub type Ns23 = tl_i32_list![2, 3];
pub type Neighbors3 = NeighborsCount<A, Ns3>;
pub type Neighbors23 = NeighborsCount<A, Ns23>;

const NEIGHBORS3_EVIDENCE: PhantomData::<Neighbors3> = PhantomData;
const NEIGHBORS23_EVIDENCE: PhantomData::<Neighbors23> = PhantomData;



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
