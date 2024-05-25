
object Rules {
  case object FirstRule
  case object SecondRule
  case object ThirdRule

  implicit val firstRuleInstance:
    FightClubRule[FirstRule.type] = new FightClubRule[FirstRule.type] {

    def explain(rule: FirstRule.type): String =
      "You do not talk about Fight Club."
  }

  implicit val secondRuleInstance: FightClubRule[SecondRule.type] = new FightClubRule[SecondRule.type] {
    def explain(rule: SecondRule.type): String = "You DO NOT talk about Fight Club."
  }

  implicit val thirdRuleInstance: FightClubRule[ThirdRule.type] = new FightClubRule[ThirdRule.type] {
    def explain(rule: ThirdRule.type): String = "If someone says stop, goes limp, or taps out, the fight is over."
  }
}

object Main extends App {

  val rules: List[Secrecy[_]] = List(
    new Secrecy(Rules.FirstRule),
    new Secrecy(Rules.SecondRule),
    new Secrecy(Rules.ThirdRule)
  )

  rules.foreach { secrecy =>
    println(secrecy.explain)
  }
}
