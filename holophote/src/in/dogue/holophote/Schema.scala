package in.dogue.holophote

import in.dogue.holophote.entities._
import in.dogue.holophote.world.{ResourceManager, World}
import in.dogue.antiqua.Antiqua
import Antiqua._


object Schema {
  def create = new Schema(Map(), Plan.create(Map(), Map()))
}

case class Schema private (activePlans:Map[PlanId,Plan], netherPlan:Plan) {
  def update = set(activePlans.filter { case (_, p) => !p.isFinished})

  def insertPlan(pls: (Seq[(Job, (PlanId) => Goal)],Seq[(Job, (PlanId) => Goal)])):Schema = {
    val (mj, mn) = pls
    val p = Plan.fromRaw(mj, mn)
    println(mj.length)
    set(activePlans + (p.id -> p))
  }

  def remaining(id:PlanId) = activePlans.get(id).map{pl => pl.major.values.toList.flatten.length}.getOrElse(0)

  def set(ap:Map[PlanId,Plan]) = copy(activePlans=ap)
  def surrender(j:Job, g:Goal):Schema = {
    set(activePlans.get(g.parent).map { ap =>
      activePlans.updated(g.parent, ap.surrender(j, g))
    }.getOrElse(activePlans))
  }

  def choosePlan = activePlans.headOption.map{_._2}.getOrElse(netherPlan)

  def employ(wk:Worker, rm:ResourceManager, w:World):(Worker, Schema) = {
    val plan = choosePlan
    val (nwk, npl) = plan.giveGoal(wk, rm, w)
    activePlans.get(plan.id).map { ap =>
      nwk @@ set(activePlans.updated(plan.id, npl))
    }.getOrElse {
      wk @@ this
    }

  }
  def finish(j:Job, g:Goal):Schema = {
    set(activePlans.get(g.parent).map { ap =>
      activePlans.updated(g.parent, ap.finish(j, g))
    }.getOrElse{
      activePlans
    })
  }

  def createNether(f:PlanId=>Goal) = f(netherPlan.id)
}
