package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.world.{ResourceManager, World}

object GoalPool {
  def create = GoalPool(Map().withDefaultValue(List()))
}

case class GoalPool(goals:Map[Job,List[Goal]]) {
  def reserve(job:Job, g:Goal, gs:List[Goal]) = {
    copy(goals = goals.updated(job, g.reserve +: gs))
  }
  def surrender(job:Job, g:Goal) = {
    val i = goals(job).indexOf(g)
    if (i == -1) {
      this
    } else {
      copy(goals=goals.updated(job, goals(job).updated(i, g.free)))
    }

  }
  def finish(job:Job, g:Goal) = {
    copy(goals=goals.updated(job, goals(job).filter { gg => gg != g}))
  }

  def giveGoal(b:Worker, rm:ResourceManager, w:World) = {
    val (bb, gp) = goals(b.job).splitFind((g:Goal) => !g.isReserved && g.isPossibleFor(b, rm, w)) match {
      case Some((g, gpp)) =>
        b.giveGoal(g) @@ reserve(b.job, g, gpp)
      case None => b @@ this
    }
    (bb, gp)
  }
}
