package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._

object GoalPool {
  def create = GoalPool(List())
}

case class GoalPool(goals:List[Goal]) {
  def reserve(g:Goal) = {
    val i = goals.indexOf(g)
    if (i == -1) {
      this
    } else {
      copy(goals = goals.updated(i, g.reserve))
    }
  }
  def surrender(g:Goal) = {
    val i = goals.indexOf(g)
    if (i == -1) {
      this
    } else {
      copy(goals=goals.updated(i, g.free))
    }

  }
  def finish(g:Goal) = copy(goals=goals.filter { gg => gg != g})

  def giveGoal(b:Builder) = {
    goals.splitFind((g:Goal) => !g.isReserved) match {
      case Some((g, gpp)) =>
        println("found one!")
        b.giveGoal(g) @@ reserve(g)
      case None => b @@ this
    }
  }
}
