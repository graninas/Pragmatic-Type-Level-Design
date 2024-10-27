use type_level::True;
use type_level::False;
use type_level::Eval;
use type_level::If;
use type_level::assert_type_eq;

mod cellular;

use cellular::language::automaton;
use cellular::language::integrity::Verify;
use cellular::language::integrity::StateInList;
use cellular::language::integrity::StatesValid;
use cellular::assets::game_of_life::Unknown;
use cellular::assets::game_of_life::GoLRule;
use cellular::assets::game_of_life::GoLStates;
use cellular::assets::game_of_life::A;


type StateVerified = <GoLStates as Verify<StateInList<A>>>::Output;
assert_type_eq!(True, StateVerified);

type UnkownTypeVerified = <GoLStates as Verify<StateInList<Unknown>>>::Output;
assert_type_eq!(False, UnkownTypeVerified);
// won't compile
// assert_type_eq!(True, UnkownTypeVerified);


// Rule standalone verification
type RuleStatesVerified = <GoLRule as Verify<StatesValid>>::Output;
assert_type_eq!(True, RuleStatesVerified);


type Result = <True as If<char, u8>>::Output;
const RESULT_VAR: Result = 'a';

fn main () {
  let res = GoLRule::eval();

  println!("{}", res);
  println!("{}", RESULT_VAR);
}
