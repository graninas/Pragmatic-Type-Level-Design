use tl_list_lib::tl_list;
use tl_list_lib::tl_i32_list;
use tl_list_lib::HList;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_str_list::TlStr;
use tl_str_macro::tl_str;
use type_level::Eval;

use assert_type_eq::assert_type_eq;
use std::marker::PhantomData;

mod master;

use crate::master::language::model::{*};
use crate::master::language::web::{*};
use crate::master::language::extensions::{*};


pub struct Game;
pub struct Board;


// "start" :> Post '[JSON] Game
pub type StartRoute =
  Route<
    tl_str!("start"),
    tl_list![IClause,
      POST<
        tl_list![ISupportedFormat, JSON],
        DataType<Game>>
      ]>;

// "move"
//        :> Capture "id" String
//        :> Capture "sign" String
//        :> QueryParam "h" Int
//        :> QueryParam "v" Int
//        :> Post '[JSON] String
pub type MoveRoute =
  Route<
    tl_str!("move"),
    tl_list![IClause,
      Capture<tl_str!("id"), StringType>,
      Capture<tl_str!("sign"), StringType>,
      QueryParam<tl_str!("h"), IntType>,
      QueryParam<tl_str!("v"), IntType>,
      POST<
        tl_list![ISupportedFormat, JSON],
        StringType>
      ]>;

//   :<|> "board"
//        :> Capture "id" String
//        :> Get '[JSON] Board
pub type BoardRoute =
  Route<
    tl_str!("board"),
    tl_list![IClause,
      Capture<tl_str!("id"), StringType>,
      GET<
        tl_list![ISupportedFormat, JSON],
        DataType<Board>
      >
    ]>;


pub type TicTacToeAPI = tl_list!
  [ IRoute
  , StartRoute
  , MoveRoute
  , BoardRoute
  ];

const TEST: PhantomData<TicTacToeAPI> = PhantomData;



fn main () {

  println!("Hello world!");
}
