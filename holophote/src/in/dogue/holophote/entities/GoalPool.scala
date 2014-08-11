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
    val i = goals.indexOf(g)
    if (i == -1) {
      this
    } else {
      copy(goals=goals.updated(i, g.free))
    }

  }
  def finish(g:Goal) = {
    copy(goals=goals.filter { gg => gg != g})
  }

  def giveGoal(b:Worker) = {
    val (bb, gp) = goals.splitFind((g:Goal) => !g.isReserved) match {
      case Some((g, gpp)) =>
        b.giveGoal(g) @@ reserve(g, gpp)
      case None => b @@ this
    }
    (bb, gp)
  }
}
