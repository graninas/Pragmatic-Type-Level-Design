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





pub type Start =
  Route<
    tl_str!("start"),
    tl_list![IClause]>;

pub type Move =
  Route<
    tl_str!("move"),
    tl_list![IClause]>;

pub type Board =
  Route<
    tl_str!("board"),
    tl_list![IClause]>;


pub type TicTacToeAPI = tl_list!
  [ IRoute
  , Start
  , Move
  ];

const TEST: PhantomData<TicTacToeAPI> = PhantomData;

// type AstroAPI =
//   (  "meteors"
//     :> QueryParam "mass" Int32
//     :> QueryParam "size" Int32
//     :> Get '[JSON] Meteors
//     )
//   :<|>
//     (  "meteor"
//     :> ReqBody '[JSON] API.MeteorTemplate
//     :> Post '[JSON] MeteorId
//     )
//   :<|>
//     (  "asteroid"
//     :> ReqBody '[JSON] API.AsteroidTemplate
//     :> Post '[JSON] AsteroidId
//     )
//   :<|>
//     (  "object_template"                                  -- route POST "/object_template"
//     :> ReqBody '[JSON] API.AstroObjectTemplate
//     :> Post '[JSON] AstroObjectId
//     )
//   :<|>
//     "object" :>
//     (
//        ( Capture "object_id" AstroObjectId                -- route GET "/object"
//        :> Get '[JSON] (Maybe AstroObject)
//        )
//      :<|>
//        ( "orbital"                                        -- route POST "/object/orbital"
//        :> Capture "object_id" AstroObjectId
//        :> ReqBody '[JSON] Orbital
//        :> Post '[JSON] AstroObjectId
//        )
//      :<|>
//        ( "physical"                                       -- route POST "/object/physical
//        :> Capture "object_id" AstroObjectId
//        :> ReqBody '[JSON] Physical
//        :> Post '[JSON] AstroObjectId
//        )
//     )





fn main () {

  println!("Hello world!");
}
