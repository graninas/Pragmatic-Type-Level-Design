class Secrecy[R]
  (val ruleInstance: R)
  (implicit val fightClubRule: FightClubRule[R]) {

  def explain: String =
    fightClubRule.explain(ruleInstance)
}

