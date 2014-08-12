package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.world.{ResourceManager, World}

object GoalPool {

  def create = {
    val m = Map[Job, List[Goal]]().withDefaultValue(List())
    GoalPool(m,m)
  }
}

case class GoalPool(major:Map[Job,List[Goal]], minor:Map[Job,List[Goal]]) {
  def reserve(job:Job, g:Goal, gs:List[Goal]) = {
    copy(major = major.updated(job, g.reserve +: gs))
  }
  def surrender(job:Job, g:Goal) = {
    val i = major(job).indexOf(g)
    if (i == -1) {
      this
    } else {
      copy(major=major.updated(job, major(job).updated(i, g.free)))
    }

  }
  def finish(job:Job, g:Goal) = {
    copy(major=major.updated(job, major(job).filter { gg => gg != g}))
  }



  def giveGoal(b:Worker, rm:ResourceManager, w:World) = {
    val (bb, gp) = major(b.job).splitFind((g:Goal) => !g.isReserved && g.isPossibleFor(b, rm, w)) match {
      case Some((g, gpp)) =>
        b.giveGoal(g) @@ reserve(b.job, g, gpp)
      case None =>
        minor(b.job).splitFind((g:Goal) => !g.isReserved && g.isPossibleFor(b, rm, w)) match {
          case Some((g, gpp)) =>
            b.giveGoal(g) @@ this
          case None => b @@ this
        }
    }
    (bb, gp)
  }
}
