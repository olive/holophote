package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.world.World
import in.dogue.antiqua.data.{Direction3}
import scala.util.Random

class EntityManager {


  def weirdFold[T,B](s:List[T], init:B, f:(T,List[T], B) => (T,B)) = {
    val v = s.toVector
    val (nv, nb, _) = v.foldLeft((v, init, 0)) { case ((ts, b, i), t) =>
      val (newT, newB) = f(t, ts.toList, b)
      (ts.updated(i, newT), newB, i+1)
    }
    (nv.toList, nb)
  }

  def coordinateTasks(bs:List[Worker], gp:GoalPool, w:World):(List[Worker], GoalPool, World) = {
    var vs = bs.toVector
    var pool = gp
    var world = w
    for (i <- 0 until vs.length) {
      val b = vs(i)
      b.task.allowed(b, new BuilderProxy(vs), world) match {
        case TaskAvailable =>
          val (bb, pp, ww) = Worker.performTask(b, pool, world)
          vs = vs.updated(i, bb)
          pool = pp
          world = ww
        case TaskBlocked(blocker) =>
          val k = vs.indexOf(blocker)
          val d = Direction3.Planar.randomR(new Random(0))
          val (blk, pp) = blocker.removeGoal.giveGoal(Move.create(blocker.pos --> d --> d)).update(new BuilderProxy(vs), world, pool)
          pool = pp
          vs = vs.updated(k, blk)
          pool = pool.surrender(blocker.goal)
          vs = vs.updated(i, b.removeGoal)
          pool = pool.surrender(b.goal)
        case TaskUnavailable =>
          vs = vs.updated(i, b.removeGoal)
          pool = pool.surrender(b.goal)
      }
    }
    (vs.toList, pool, world)
  }


  def manageGoal(b:Worker, gp:GoalPool): (Worker, GoalPool) = {
    if (b.noGoal) {
      gp.giveGoal(b)
    } else {
      (b, gp)
    }
  }
}
