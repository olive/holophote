package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._

case class GoalPool(goals:List[Goal]) {
  def reserve(g:Goal) = copy(goals=goals :+ g.reserve)
  def surrender(g:Goal) = copy(goals=goals.updated(goals.indexOf(g), g.free))
  def finish(g:Goal) = copy(goals=goals.filter { gg => gg != g})

  def giveGoal(b:Builder) = {
    goals.splitFind((g:Goal) => !g.isReserved) match {
      case Some((g, gpp)) => b.giveGoal(g) @@ reserve(g)
      case None => b @@ this
    }
  }
}
