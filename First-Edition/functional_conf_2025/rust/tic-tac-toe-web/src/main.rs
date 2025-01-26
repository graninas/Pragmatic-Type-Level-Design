use tl_list_lib::tl_list;
use tl_list_lib::HList;
use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use type_level::IInterface;
use type_level::Wrapper;
use tl_str_list::TlStr;
use tl_str_macro::tl_str;
use type_level::EvalCtx;
use type_level::Eval;

use std::marker::PhantomData;

mod master;

use crate::master::language::model::{*};
use crate::master::language::extensions::{*};

use axum::{
    response::Json,
    routing::{post, MethodRouter},
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


pub type StartRoute =
  Route<
    POST,
    tl_str!("/start"),
    tl_list![IClause],
    tl_list![IFormat, JSON],
    DataType<Game>
  >;

type MoveRouteClauses =
  tl_list![IClause,
      Capture<tl_str!("id"), StringType>,
      Capture<tl_str!("sign"), StringType>,
      QueryParam<tl_str!("h"), IntType>,
      QueryParam<tl_str!("v"), IntType>
  ];


pub type MoveRoute = Route<
  POST,
  tl_str!("/move"),
  MoveRouteClauses,
  tl_list![IFormat, JSON],
  StringType>;

pub type BoardRoute =
  Route<
    GET,
    tl_str!("/board"),
    tl_list![IClause, Capture<tl_str!("id"), StringType>],
    tl_list![IFormat, JSON],
    DataType<Board>
  >;


pub type TicTacToeAPI =
  Api<tl_list![IRoute,
    StartRoute,
    MoveRoute,
    BoardRoute
  ]>;

const TEST: PhantomData<TicTacToeAPI> = PhantomData;

////// many type classes interpretation /////////////

pub trait InterpretQueryClause {
  fn interpret() -> ();
}

impl<Name, Type>
  InterpretQueryClause
  for Wrapper<IClause,
              QueryParamImpl<Name, Type>>
  where
    Name: TlStr,
    Type: IInterface<IType>
{
  fn interpret() -> () {
  }
}


pub trait InterpretRoute {
  fn interpret() -> ();
}

impl<Method, Path, Clauses, Formats, ReturnType>
  InterpretRoute
  for Wrapper<IRoute,
              RouteImpl<Method, Path, Clauses, Formats, ReturnType>>
  where
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    Formats: HList<IFormat>,
    ReturnType: IInterface<IType>,
    Method: Eval<TinyBuildMethod, String>
{
  fn interpret() -> () {
  }
}



///////////// axum /////////////////

pub type Ctx = (SharedState, Router);


pub struct AxumBuildRouter;
pub struct AxumBuildRoute;
pub struct AxumBuildMethod<Path: TlStr>
  (PhantomData::<Path>);


impl
  EvalCtx<SharedState,
          AxumBuildMethod<tl_str!("/start")>,
          (SharedState, MethodRouter)>
  for Wrapper<IMethod, PostMethodImpl>
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

impl
  EvalCtx<SharedState,
          AxumBuildMethod<tl_str!("/move")>,
          (SharedState, MethodRouter)>
  for Wrapper<IMethod, PostMethodImpl>
{
  fn eval_ctx(state: SharedState) -> (SharedState, MethodRouter) {
    todo!()
  }
}

impl
  EvalCtx<SharedState,
          AxumBuildMethod<tl_str!("/board")>,
          (SharedState, MethodRouter)>
  for Wrapper<IMethod, GetMethodImpl>
{
  fn eval_ctx(state: SharedState) -> (SharedState, MethodRouter) {
    todo!()
  }
}


// Building a particular route
impl<Method, Path, Clauses, Formats, ReturnType>
  EvalCtx<Ctx, AxumBuildRoute, Ctx>
  for Wrapper<IRoute, RouteImpl<Method, Path, Clauses, Formats, ReturnType>>
  where
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    Formats: HList<IFormat>,
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


struct TinyBuildRouter;
struct TinyBuildRoute;
struct TinyBuildMethod;

type MethodResponse = Response<Cursor<Vec<u8>>>;
type TinyRouter = Arc<Mutex<HashMap<(String, String),
                                    Arc<Box<dyn Fn(&Request) -> MethodResponse >>>>>;
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

// Handler for making a move
fn make_move(state: SharedState, request: &Request) -> MethodResponse {
  // TODO: build url_parts and query according to the type-level representation
    let url_parts: Vec<&str> = request.url().split('/').collect();
    if url_parts.len() < 4 {
        return Response::from_string("Invalid URL").with_status_code(400);
    }

    let game_id = url_parts[2].to_string();
    let mut sign = url_parts[3].to_string();
    sign = if sign.contains("cross") { "cross".to_owned() }
           else { "circle".to_owned() };

    let query: HashMap<String, String> = request
        .url()
        .split('?')
        .nth(1)
        .unwrap_or("")
        .split('&')
        .filter_map(|pair| {
            let mut parts = pair.split('=');
            Some((parts.next()?.to_string(), parts.next()?.to_string()))
        })
        .collect();

    let h: usize = query.get("h").and_then(|v| v.parse().ok()).unwrap_or(usize::MAX);
    let v: usize = query.get("v").and_then(|v| v.parse().ok()).unwrap_or(usize::MAX);

    if h >= 3 || v >= 3 {
        return Response::from_string("Invalid move").with_status_code(400);
    }

    let mut games = state.lock().unwrap();
    if let Some(board) = games.get_mut(&game_id) {
        if board.cells[h][v].is_empty() {
            board.cells[h][v] = sign;
            return Response::from_string("Move made").with_status_code(200);
        }
        return Response::from_string("Cell already occupied").with_status_code(400);
    }

    Response::from_string("Game not found").with_status_code(404)
}

// Handler for getting the board
fn get_board(state: SharedState, request: &Request) -> MethodResponse {
    let url_parts: Vec<&str> = request.url().split('/').collect();
    if url_parts.len() < 3 {
        return Response::from_string("Invalid URL").with_status_code(400);
    }

    let game_id = url_parts[2].to_string();
    let games = state.lock().unwrap();
    if let Some(board) = games.get(&game_id) {
        let response_body = serde_json::to_string(board).unwrap();
        Response::from_string(response_body).with_header(
            "Content-Type: application/json"
              .parse::<tiny_http::Header>()
              .unwrap(),
        )
    } else {
        Response::from_string("Game not found").with_status_code(404)
    }
}

impl Eval<TinyBuildMethod, String>
  for Wrapper<IMethod, PostMethodImpl> {
    fn eval() -> String {
      "POST".to_string()
    }
}

impl Eval<TinyBuildMethod, String>
  for Wrapper<IMethod, GetMethodImpl> {
    fn eval() -> String {
      "GET".to_string()
    }
}



// Building a particular route
impl<Method, Path, Clauses, Formats, ReturnType>
  EvalCtx<TinyCtx, TinyBuildRoute, ()>
  for Wrapper<IRoute, RouteImpl<Method, Path, Clauses, Formats, ReturnType>>
  where
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    Formats: HList<IFormat>,
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

      let handler = move |request: &Request|
        { match (method.as_str(), path.as_str()) {
          ("POST", "/start") => start_game(state.clone()),
          ("POST", "/move") => make_move(state.clone(), request),
          ("GET", "/board") => get_board(state.clone(), request),
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
        let url_parts: Vec<&str> = request.url().split('/').collect();
        let mut url = url_parts[1].to_string();
        url = "/".to_owned() + &url;

        let mut method_call;
        {
          let d = router.lock().unwrap();
          method_call = d.get(&(method.to_string(), url.to_string())).cloned();
        }

        match method_call
        {
          Some(call) => {
            let response = call(&request);
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
    }
  }
}

fn main() {
  main_tiny_http();
}
