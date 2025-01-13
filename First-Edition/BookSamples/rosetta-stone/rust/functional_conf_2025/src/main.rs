use tl_list_lib::tl_list;
use tl_list_lib::tl_i32_list;
use tl_list_lib::HList;

use type_level::IInterface;
use tl_list_lib::I32List;
use tl_str_list::TlStr;
use tl_str_macro::tl_str;

use assert_type_eq::assert_type_eq;
use std::marker::PhantomData;

mod cellular;

use cellular::language::automaton;
use cellular::language::automaton::IState;
use cellular::assets::game_of_life::A;
use cellular::assets::game_of_life::D;
use cellular::assets::game_of_life::GoLRule;

use type_level::Eval;


pub struct IRoute;

pub struct RouteWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IRoute> for RouteWrapper<T> {
  type Interface = IRoute;
}

pub struct IQueryParam;

pub struct QueryParamWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IQueryParam> for QueryParamWrapper<T> {
  type Interface = IQueryParam;
}


pub struct RouteImpl<
  R: TlStr,
  QueryParams: HList<IQueryParam>>
  (PhantomData::<(R, QueryParams)>);

pub type Route<R, QueryParams> = RouteWrapper<RouteImpl<R, QueryParams>>;






pub type Start =
  Route<
    tl_str!("start"),
    tl_list![IQueryParam]>;

pub type Move =
  Route<
    tl_str!("move"),
    tl_list![IQueryParam]>;


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
  let res = GoLRule::eval();

  println!("{}", res);
}
