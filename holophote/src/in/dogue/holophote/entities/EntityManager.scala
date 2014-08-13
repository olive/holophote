package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.world.{ResourceManager, World}
import in.dogue.antiqua.data.{Direction3}
import scala.util.Random
import in.dogue.holophote.Schema

class EntityManager {


  def weirdFold[T,B](s:List[T], init:B, f:(T,List[T], B) => (T,B)) = {
    val v = s.toVector
    val (nv, nb, _) = v.foldLeft((v, init, 0)) { case ((ts, b, i), t) =>
      val (newT, newB) = f(t, ts.toList, b)
      (ts.updated(i, newT), newB, i+1)
    }
    (nv.toList, nb)
  }

  def coordinateTasks(bs:List[Worker], sc:Schema, w:World):(List[Worker], Schema, World) = {
    var vs = bs.toVector
    var schema = sc
    var world = w
    for (i <- 0 until vs.length) {
      val b = vs(i)
      b.task.allowed(b, new BuilderProxy(vs), world) match {
        case TaskAvailable =>
          val (bb, pp, ww) = Worker.performTask(b, schema, world)
          vs = vs.updated(i, bb)
          schema = pp
          world = ww
        case TaskBlocked(blocker) =>
          val k = vs.indexOf(blocker)
          val d = Direction3.Planar.randomR(new Random())
          val pos = w.traceDown(blocker.pos --> d --> d)
          val move = sc.createNether(Move.create(pos))
          val (blk, pp) = blocker.removeGoal(FailureReason.Jam(b)).giveGoal(move).update(new BuilderProxy(vs), world, schema)
          schema = pp
          vs = vs.updated(k, blk)
          schema = schema.surrender(blocker.job, blocker.goal)
          vs = vs.updated(i, b.removeGoal(FailureReason.Jam(blk)))
          schema = schema.surrender(b.job, b.goal)
        case TaskUnavailable =>
          vs = vs.updated(i, b.removeGoal(FailureReason.Unknown/*fixme -- why is it unavailable?*/))
          schema = schema.surrender(b.job, b.goal)
      }
    }
    (vs.toList, schema, world)
  }


  def manageGoal(rm:ResourceManager, w:World)(b:Worker, sc:Schema): (Worker, Schema) = {
    if (b.noGoal) {
      sc.employ(b, rm, w)
    } else {
      (b, sc)
    }
  }
}
