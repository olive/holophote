package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._

object GoalPool {
  def create = GoalPool(List())
}

case class GoalPool(goals:List[Goal]) {
  def reserve(g:Goal, gs:List[Goal]) = {
    copy(goals = g.reserve +: gs)
  }
  def surrender(g:Goal) = {
    println("Goal " + g.id + " dropped")
    val i = goals.indexOf(g)
    if (i == -1) {
      this
    } else {
      copy(goals=goals.updated(i, g.free))
    }

  }
  def finish(g:Goal) = {
    println("goal " + g.id + " finished")
    copy(goals=goals.filter { gg => gg != g})
  }

  def giveGoal(b:Builder) = {
    goals.splitFind((g:Goal) => !g.isReserved) match {
      case Some((g, gpp)) =>
        println("found one! " + g.id)
        b.giveGoal(g) @@ reserve(g, gpp)
      case None => b @@ this
    }
  }
}
