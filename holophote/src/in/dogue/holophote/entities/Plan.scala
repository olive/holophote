package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.world.{ResourceManager, World}
import in.dogue.holophote.blueprints.Blueprint
import in.dogue.antiqua.data.Graph


final class PlanId(val id:BigInt) extends AnyVal {
  override def toString = id.toString()
}

object Plan {
  var id:BigInt = 0
  def create(major:Map[Job,List[Goal]], minor:Map[Job,List[Goal]]) = {
    val id = new PlanId(Plan.id)
    val result = Plan(major.withDefaultValue(List()), minor.withDefaultValue(List()), id)
    Plan.id += 1
    result
  }

  def fromBlueprint(bp:Blueprint, g:Graph[Vox,Vox]) = {
    val (major, minor) = bp.generate(g)
    fromRaw(major, minor)
  }

  def fromRaw(major: Seq[(Job, (PlanId) => Goal)], minor: Seq[(Job, (PlanId) => Goal)]) = {
    val p = create(Map(), Map()) //fixme code clones
    val mj = major.map{ case (j, f) => j -> f(p.id) }.groupBy(_._1).mapValues {_.map{_._2}.toList}.withDefaultValue(List())
    val mn = minor.map { case (j, f) => j -> f(p.id) }.groupBy(_._1).mapValues {_.map{_._2}.toList}.withDefaultValue(List())
    p.set(mj, mn)
  }
}

case class Plan private (major:Map[Job,List[Goal]], minor:Map[Job,List[Goal]], id:PlanId) {
  private def set(mj:Map[Job,List[Goal]], mn:Map[Job,List[Goal]]) = copy(major=mj, minor=mn)

  def isFinished = major.forall { case (_,l) => l.length == 0}
  def reserve(job:Job, g:Goal, gs:List[Goal]) = {
    copy(major = major.updated(job, g.reserve +: gs))
  }
  def surrender(job:Job, g:Goal): Plan = {
    val i = major(job).indexOf(g)
    if (i == -1) {
      this
    } else {
      copy(major=major.updated(job, major(job).updated(i, g.free)))
    }

  }
  def finish(job:Job, g:Goal): Plan = {
    copy(major=major.updated(job, major(job).filter { gg => gg != g}))
  }



  def giveGoal(b:Worker, rm:ResourceManager, w:World): (Worker, Plan) = {
    val (bb, plan) = major(b.job).splitFind((g:Goal) => !g.isReserved && g.isPossibleFor(b, rm, w)) match {
      case Some((g, gpp)) =>
        b.giveGoal(g) @@ reserve(b.job, g, gpp)
      case None =>
        minor(b.job).splitFind((g:Goal) => !g.isReserved && g.isPossibleFor(b, rm, w)) match {
          case Some((g, gpp)) =>
            b.giveGoal(g) @@ this
          case None => b @@ this
        }
    }
    (bb, plan)
  }

  override def equals(other:Any):Boolean = {
    throw new RuntimeException("dont use this")
  }
}
