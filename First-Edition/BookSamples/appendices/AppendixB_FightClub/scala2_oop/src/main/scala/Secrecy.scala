class Secrecy(val rule: FightClubRule)

object Secrecy {
  def apply(rule: FightClubRule): Secrecy = new Secrecy(rule)
}

