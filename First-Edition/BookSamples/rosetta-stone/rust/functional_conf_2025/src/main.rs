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
    routing::{get, post, MethodRouter},
    Router,
};
use axum::handler::Handler;

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

type SharedState = Arc<Mutex<HashMap<String, Board>>>;

pub type Ctx = (SharedState, Router);





pub enum BuildRouter{}
pub enum BuildRoute{}
pub struct BuildMethod<Path: TlStr>
  (PhantomData::<Path>);




impl
  EvalCtx<SharedState,
          BuildMethod<tl_str!("/start")>,
          (SharedState, MethodRouter)>
  for MethodWrapper<PostMethodImpl>
{
  fn eval_ctx(state: SharedState) -> (SharedState, MethodRouter) {
    let new_state = state.clone();

    (new_state.clone(),
      post(
        || async move {
            let game_id = Uuid::new_v4().to_string();
            let mut games = new_state.lock().unwrap();
            games.insert(game_id.clone(), Board::new());

            // todo: formats, return type
            Json(Game { game_id })
        })
    )
  }
}


/////////// service evaluators

// Building a particular route
impl<Method, Path, Clauses, Formats, ReturnType>
  EvalCtx<Ctx, BuildRoute, Ctx>
  for RouteWrapper<RouteImpl<Method, Path, Clauses, Formats, ReturnType>>
  where
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    Formats: HList<ISupportedFormat>,
    ReturnType: IInterface<IType>,
    Method: EvalCtx<SharedState, BuildMethod<Path>, (SharedState, MethodRouter)>
{
    fn eval_ctx((state, router): Ctx) -> Ctx {

    let (new_state, handler) = <Method as
          EvalCtx<
            SharedState,
            BuildMethod<Path>,
            (SharedState, MethodRouter)
            >>::eval_ctx(state);

    let path = Path::to_string();
    let new_router = router.route(&path, handler);

    (new_state, new_router)
    }
}

// End of routes - return the final result
impl<IRoute>
  EvalCtx<Ctx, BuildRouter, Ctx>
  for TlN_<IRoute>
{
  fn eval_ctx(ctx: Ctx) -> Ctx {
    ctx
  }
}

// Getting the next route
impl<Route, Routes>
  EvalCtx<Ctx, BuildRouter, Ctx>
  for TlC_<IRoute, Route, Routes>
  where
    Route: IInterface<IRoute>
      + EvalCtx<Ctx, BuildRoute, Ctx>,

    Routes: HList<IRoute>
      + EvalCtx<Ctx, BuildRouter, Ctx>,
{
  fn eval_ctx(ctx: Ctx) -> Ctx {
    let new_ctx = Route::eval_ctx(ctx);   // building this route
    Routes::eval_ctx(new_ctx)             // building other routes
  }
}

// Building the router for API
impl<Routes>
  EvalCtx<SharedState, BuildRouter, Router>
  for Api<Routes>
  where
    Routes: HList<IRoute>
      + EvalCtx<Ctx, BuildRouter, Ctx>,
{
  fn eval_ctx(state: SharedState) -> Router {
    let router = Router::new();
    let (_, new_router) = Routes::eval_ctx((state, router));
    new_router
  }
}

// "start" :> Post '[JSON] Game
pub type StartRoute =
  Route<
    POST,
    tl_str!("/start"),
    tl_list![IClause],
    tl_list![ISupportedFormat, JSON],
    DataType<Game>
  >;

// "move"
//        :> Capture "id" String
//        :> Capture "sign" String
//        :> QueryParam "h" Int
//        :> QueryParam "v" Int
//        :> Post '[JSON] String
pub type MoveRoute =
  Route<
    POST,
    tl_str!("/move"),
    tl_list![IClause,
      Capture<tl_str!("id"), StringType>,
      Capture<tl_str!("sign"), StringType>,
      QueryParam<tl_str!("h"), IntType>,
      QueryParam<tl_str!("v"), IntType>],
    tl_list![ISupportedFormat, JSON],
    StringType
  >;

//   :<|> "board"
//        :> Capture "id" String
//        :> Get '[JSON] Board
pub type BoardRoute =
  Route<
    GET,
    tl_str!("/board"),
    tl_list![IClause, Capture<tl_str!("id"), StringType>],
    tl_list![ISupportedFormat, JSON],
    DataType<Board>
  >;


pub type TicTacToeAPI =
  Api<tl_list![IRoute,
    StartRoute
    // MoveRoute,
    // BoardRoute
  ]>;

const TEST: PhantomData<TicTacToeAPI> = PhantomData;

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

