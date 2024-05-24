
object FirstRule {
  case object Rule
  implicit val firstRuleInstance: FightClubRule[Rule.type] = new FightClubRule[Rule.type] {
    def explain(rule: Rule.type): String = "You do not talk about Fight Club."
  }
}

object SecondRule {
  case object Rule
  implicit val secondRuleInstance: FightClubRule[Rule.type] = new FightClubRule[Rule.type] {
    def explain(rule: Rule.type): String = "You DO NOT talk about Fight Club."
  }
}

object ThirdRule {
  case object Rule
  implicit val thirdRuleInstance: FightClubRule[Rule.type] = new FightClubRule[Rule.type] {
    def explain(rule: Rule.type): String = "If someone says stop, goes limp, or taps out, the fight is over."
  }
}

object Main extends App {

  val rules: List[Secrecy[_]] = List(
    Secrecy(FirstRule.Rule),
    Secrecy(SecondRule.Rule),
    Secrecy(ThirdRule.Rule)
  )

  rules.foreach { secrecy =>
    println(secrecy.explain)
  }
}
