object FirstRule extends FightClubRule {
  def explain: String = "You do not talk about Fight Club."
}

object SecondRule extends FightClubRule {
  def explain: String = "You DO NOT talk about Fight Club."
}

object ThirdRule extends FightClubRule {
  def explain: String = "If someone says stop, goes limp, or taps out, the fight is over."
}

object Main extends App {
  val rules: List[Secrecy] = List(
    Secrecy(FirstRule),
    Secrecy(SecondRule),
    Secrecy(ThirdRule)
  )

  rules.foreach { secrecy =>
    println(secrecy.rule.explain)
  }
}
