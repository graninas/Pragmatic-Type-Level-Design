use tl_list_lib::tl_list;
use tl_list_lib::tl_i32_list;
use tl_list_lib::HList;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_str_list::TlStr;
use tl_str_macro::tl_str;
use type_level::EvalCtx;
use tl_list_lib::TlN_;
use tl_list_lib::TlC_;

use assert_type_eq::assert_type_eq;
use std::marker::PhantomData;

mod master;

use crate::master::language::model::{*};
use crate::master::language::web::{*};
use crate::master::language::extensions::{*};

use axum::{
    extract::{Path, Query},
    response::Json,
    routing::{get, post},
    Router,
};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

// Game data
#[derive(Serialize)]
struct Game {
    game_id: String,
}

// Board data
#[derive(Serialize)]
struct Board {
    cells: Vec<Vec<String>>, // A 3x3 Tic-Tac-Toe board
}

impl Board {
    fn new() -> Self {
        Board {
            cells: vec![vec!["".to_string(); 3]; 3],
        }
    }
}

// Shared state
type SharedState = Arc<Mutex<HashMap<String, Board>>>;






// "start" :> Post '[JSON] Game
pub type StartRoute =
  Route<
    tl_str!("start"),
    POST<tl_list![ISupportedFormat, JSON],
         DataType<Game>>,
    tl_list![IClause]>;

// "move"
//        :> Capture "id" String
//        :> Capture "sign" String
//        :> QueryParam "h" Int
//        :> QueryParam "v" Int
//        :> Post '[JSON] String
pub type MoveRoute =
  Route<
    tl_str!("move"),

    POST<tl_list![ISupportedFormat, JSON],
         StringType>,

    tl_list![IClause,
      Capture<tl_str!("id"), StringType>,
      Capture<tl_str!("sign"), StringType>,
      QueryParam<tl_str!("h"), IntType>,
      QueryParam<tl_str!("v"), IntType>
    ]>;

//   :<|> "board"
//        :> Capture "id" String
//        :> Get '[JSON] Board
pub type BoardRoute =
  Route<
    tl_str!("board"),

    GET<tl_list![ISupportedFormat, JSON],
        DataType<Board>>,

    tl_list![IClause,
      Capture<tl_str!("id"), StringType>
    ]>;


pub type TicTacToeAPI =
  Api<tl_list!
      [ IRoute
      , StartRoute
      , MoveRoute
      , BoardRoute
      ]>;

const TEST: PhantomData<TicTacToeAPI> = PhantomData;



pub enum BuildRouter{}
pub enum MethodBody{}


// End of routes - return the final route
impl<IRoute>
  EvalCtx<(SharedState, Router), BuildRouter, Router>
  for TlN_<IRoute>
{
  fn eval_ctx(ctx: (SharedState, Router)) -> Router {
    let (_state, router) = ctx;
    router
  }
}

// Getting the next route
impl<IRoute, Route, Tail>
  EvalCtx<(SharedState, Router), BuildRouter, Router>
  for TlC_<IRoute, Route, Tail>
  where
    Route: IInterface<IRoute>
      + EvalCtx<(SharedState, Router), BuildRouter, Router>,

      Tail: HList<IRoute>
      + EvalCtx<(SharedState, Router), BuildRouter, Router>,
{
  fn eval_ctx(ctx: (SharedState, Router)) -> Router {
    let (_state, router) = ctx;
    router
  }
}

// Building a particular route
impl<Path_, Method_, Clauses_>
  EvalCtx<(SharedState, Router), BuildRouter, Router>
  for RouteWrapper<RouteImpl<Path_, Method_, Clauses_>>
  where
      Path_: TlStr,
      Method_: IInterface<IMethod>
        + EvalCtx<(SharedState, Router, String), BuildRouter, Router>,
      Clauses_: HList<IClause>,
{
    fn eval_ctx(ctx: (SharedState, Router)) -> Router {
        let (state, router) = ctx;
        let path = Path_::to_string();
        Method_::eval_ctx((state, router, path))
    }
}

// Building the POST method for the route
impl<Formats, ReturnType>
  EvalCtx<(SharedState, Router, String), BuildRouter, Router>
  for MethodWrapper<PostMethodImpl<Formats, ReturnType>>
  where
      Formats: HList<ISupportedFormat>,
      ReturnType: IInterface<IType>,
{
    fn eval_ctx(ctx: (SharedState, Router, String)) -> Router {
        let (_state, router, _path) = ctx;
        router
    }
}

// Building the GET method for the route
impl<Formats, ReturnType>
  EvalCtx<(SharedState, Router, String), BuildRouter, Router>
  for MethodWrapper<GetMethodImpl<Formats, ReturnType>>
  where
      Formats: HList<ISupportedFormat>,
      ReturnType: IInterface<IType>,
{
    fn eval_ctx(ctx: (SharedState, Router, String)) -> Router {
        let (_state, router, _path) = ctx;
        router
    }
}


// Building the router for API
impl<Routes>
  EvalCtx<SharedState, BuildRouter, Router>
  for Api<Routes>
  where
    Routes: HList<IRoute>
      + EvalCtx<(SharedState, Router), BuildRouter, Router>,
{
  fn eval_ctx(state: SharedState) -> Router {

    let router = Router::new();

    Routes::eval_ctx((state, router))


    // router.route("/start",
    //       post({
    //           let new_state = Arc::clone(&state);
    //           let game_id = Uuid::new_v4().to_string();
    //           || async move {
    //               let mut games = new_state.lock().unwrap();
    //               games.insert(game_id.clone(), Board::new());
    //               Json(Game { game_id })
    //           }
    //       }
    //     )
    // )
  }
}


// // POST /start
// async fn start_game(state: SharedState) -> Json<Game> {
//     let game_id = Uuid::new_v4().to_string();
//     let mut games = state.lock().unwrap();
//     games.insert(game_id.clone(), Board::new());
//     Json(Game { game_id })
// }



#[tokio::main]
async fn main() {
    let state: SharedState = Arc::new(Mutex::new(HashMap::new()));

    let app = TicTacToeAPI::eval_ctx(state);
      // .with_state(state);

    println!("Server running at http://localhost:8080");
    axum::Server::bind(&"0.0.0.0:8080".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

