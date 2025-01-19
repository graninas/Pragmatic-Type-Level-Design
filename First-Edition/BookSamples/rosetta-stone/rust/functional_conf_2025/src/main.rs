use tl_list_lib::tl_list;
use tl_list_lib::tl_i32_list;
use tl_list_lib::HList;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_str_list::TlStr;
use tl_str_macro::tl_str;
use type_level::EvalCtx;
use type_level::Eval;
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
    handler::Handler,
    Router,
};

use tiny_http::{Server, Request, Response};
use std::io::Read;
use std::io::Cursor;

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




///////////// axum /////////////////

pub type Ctx = (SharedState, Router);


pub enum AxumBuildRouter{}
pub enum AxumBuildRoute{}
pub struct AxumBuildMethod<Path: TlStr>
  (PhantomData::<Path>);


impl
  EvalCtx<SharedState,
          AxumBuildMethod<tl_str!("/start")>,
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

// Building a particular route
impl<Method, Path, Clauses, Formats, ReturnType>
  EvalCtx<Ctx, AxumBuildRoute, Ctx>
  for RouteWrapper<RouteImpl<Method, Path, Clauses, Formats, ReturnType>>
  where
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    Formats: HList<ISupportedFormat>,
    ReturnType: IInterface<IType>,
    Method: EvalCtx<SharedState, AxumBuildMethod<Path>, (SharedState, MethodRouter)>
{
    fn eval_ctx((state, router): Ctx) -> Ctx {

    let (new_state, handler) = <Method as
          EvalCtx<
            SharedState,
            AxumBuildMethod<Path>,
            (SharedState, MethodRouter)
            >>::eval_ctx(state);

    let path = Path::to_string();
    let new_router = router.route(&path, handler);

    (new_state, new_router)
    }
}

// List of routes / end of list - return the final result
impl<IRoute>
  EvalCtx<Ctx, AxumBuildRouter, Ctx>
  for TlN_<IRoute>
{
  fn eval_ctx(ctx: Ctx) -> Ctx {
    ctx
  }
}

// List of routes / next route item
impl<Route, Routes>
  EvalCtx<Ctx, AxumBuildRouter, Ctx>
  for TlC_<IRoute, Route, Routes>
  where
    Route: IInterface<IRoute>
      + EvalCtx<Ctx, AxumBuildRoute, Ctx>,

    Routes: HList<IRoute>
      + EvalCtx<Ctx, AxumBuildRouter, Ctx>,
{
  fn eval_ctx(ctx: Ctx) -> Ctx {
    let new_ctx = Route::eval_ctx(ctx);   // building this route
    Routes::eval_ctx(new_ctx)             // building other routes
  }
}

// Building the router for API
impl<Routes>
  EvalCtx<SharedState, AxumBuildRouter, Router>
  for Api<Routes>
  where
    Routes: HList<IRoute>
      + EvalCtx<Ctx, AxumBuildRouter, Ctx>,
{
  fn eval_ctx(state: SharedState) -> Router {
    let router = Router::new();
    let (_, new_router) = Routes::eval_ctx((state, router));
    new_router
  }
}

// #[tokio::main]
async fn main_axum() {
    let state: SharedState = Arc::new(Mutex::new(HashMap::new()));

    let app =
      <TicTacToeAPI as
        EvalCtx<SharedState, AxumBuildRouter, Router>
      >::eval_ctx(state);

    println!("Server running at http://localhost:8080");
    axum::Server::bind(&"0.0.0.0:8080".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}



/////////////// tiny http //////////////////////


pub enum TinyBuildRouter{}
pub enum TinyBuildRoute{}
pub enum TinyBuildMethod{}

type MethodResponse = Response<Cursor<Vec<u8>>>;
type TinyRouter = Arc<Mutex<HashMap<(String, String),
                                    Arc<Box<dyn Fn() -> MethodResponse >>>>>;
pub type TinyCtx = (SharedState, TinyRouter);




// Handler for starting a game
fn start_game(state: SharedState) -> MethodResponse {
    let game_id = Uuid::new_v4().to_string();
    let mut games = state.lock().unwrap();
    games.insert(game_id.clone(), Board::new());
    let response_body = serde_json::to_string(&Game { game_id }).unwrap();
    Response::from_string(response_body)
      .with_header(
          "Content-type: application/json"
              .parse::<tiny_http::Header>()
              .unwrap(),
      )
}



impl Eval<TinyBuildMethod, String>
  for MethodWrapper<PostMethodImpl> {
    fn eval() -> String {
      "POST".to_string()
    }
}

impl Eval<TinyBuildMethod, String>
  for MethodWrapper<GetMethodImpl> {
    fn eval() -> String {
      "GET".to_string()
    }
}



// Building a particular route
impl<Method, Path, Clauses, Formats, ReturnType>
  EvalCtx<TinyCtx, TinyBuildRoute, ()>
  for RouteWrapper<RouteImpl<Method, Path, Clauses, Formats, ReturnType>>
  where
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    Formats: HList<ISupportedFormat>,
    ReturnType: IInterface<IType>,
    Method: Eval<TinyBuildMethod, String>
{
  fn eval_ctx((state, router): TinyCtx) -> () {

    let path = Path::to_string();
    let method = Method::eval();
    let path_ = path.clone();
    let method_ = method.clone();
    println!("To add: {} {}", method, path);
    {
      let mut routes = router.lock().unwrap();

      let handler = move ||
        { match (method.as_str(), path.as_str()) {
          ("POST", "/start") => start_game(state.clone()) ,
          _ => Response::from_string("Not Found").with_status_code(404),
        }
      };

      routes.insert((method_, path_), Arc::new(Box::new(handler)));
    }
  }
}




// List of routes / end of list - return the final result
impl<IRoute>
  EvalCtx<TinyCtx, TinyBuildRouter, ()>
  for TlN_<IRoute>
{
  fn eval_ctx(_ctx: TinyCtx) -> () {
  }
}

// List of routes / next route item
impl<Route, Routes>
  EvalCtx<TinyCtx, TinyBuildRouter, ()>
  for TlC_<IRoute, Route, Routes>
  where
    Route: IInterface<IRoute>
      + EvalCtx<TinyCtx, TinyBuildRoute, ()>,

    Routes: HList<IRoute>
      + EvalCtx<TinyCtx, TinyBuildRouter, ()>,
{
  fn eval_ctx(ctx: TinyCtx) -> () {
    Route::eval_ctx(ctx.clone());   // building this route
    Routes::eval_ctx(ctx)           // building other routes
  }
}

// Building the router for API
impl<Routes>
  EvalCtx<TinyCtx, TinyBuildRouter, ()>
  for Api<Routes>
  where
    Routes: HList<IRoute>
      + EvalCtx<TinyCtx, TinyBuildRouter, ()>,
{
  fn eval_ctx(ctx: TinyCtx) -> () {
    <Routes as
        EvalCtx<TinyCtx, TinyBuildRouter, ()>
    >::eval_ctx(ctx)
  }
}

fn main_tiny_http() {
  let state: SharedState = Arc::new(Mutex::new(HashMap::new()));
  let router = Arc::new(Mutex::new(HashMap::new()));

  <TicTacToeAPI as
      EvalCtx<TinyCtx, TinyBuildRouter, ()>
  >::eval_ctx((state.clone(), router.clone()));


  let server = Server::http("0.0.0.0:8080").unwrap();
  println!("Server running on http://localhost:8080");

  loop {
    for request in server.incoming_requests() {
        let method = request.method().as_str();
        let url = request.url();

        let mut method_call;
        {
          let d = router.lock().unwrap();
          method_call = d.get(&(method.to_string(), url.to_string())).cloned();
        }

        match method_call
        {
          Some(call) => {
            let response = call();
            request.respond(response).unwrap();
          }
          _ => {
            println!("Method and path not found: {} {}", method, url);

            let keys: Vec<_>;
            {
                let d = router.lock().unwrap();
                keys = d.keys().cloned().collect();
            }

            // Process the keys without holding the lock
            for (k, v) in keys {
                println!("{k} {v}");
            }

            request.respond(Response::from_string("Not Found").with_status_code(404)).unwrap();
          }
        }

        // let response = match (method, url) {
        //     ("POST", "/start") => start_game(state.clone()),
        //     // ("POST", url) if url.starts_with("/move/") => make_move(state.clone(), request),
        //     // ("GET", url) if url.starts_with("/board/") => get_board(state.clone(), request),
        //     _ => Response::from_string("Not Found").with_status_code(404),
        // };

        // request.respond(response).unwrap();
    }
  }
}

fn main() {
  main_tiny_http();
}
