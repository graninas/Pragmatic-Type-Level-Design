class Secrecy[R](val ruleInstance: R)(implicit val fightClubRule: FightClubRule[R]) {
  def explain: String = fightClubRule.explain(ruleInstance)
}

object Secrecy {
  def apply[R](ruleInstance: R)(implicit fightClubRule: FightClubRule[R]): Secrecy[R] = {
    new Secrecy(ruleInstance)
  }
}
