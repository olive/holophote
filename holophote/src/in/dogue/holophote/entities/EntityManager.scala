package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.world.World

class EntityManager {




  def weirdFold[T,B](s:List[T], init:B, f:(T,List[T], B) => (T,B)) = {
    val v = s.toVector
    val (nv, nb, _) = v.foldLeft((v, init, 0)) { case ((ts, b, i), t) =>
      val (newT, newB) = f(t, ts.toList, b)
      (ts.updated(i, newT), newB, i+1)
    }
    (nv.toList, nb)
  }

  def coordinateTasks(bs:List[Builder], gp:GoalPool, w:World):(List[Builder], GoalPool, World) = {
    var vs = bs.toVector
    var pool = gp
    var world = w
    for (i <- 0 until vs.length) {
      val b = vs(i)
      if (b.task.allowed(b, new BuilderProxy(vs), world)) {
        val (bb, pp, ww) = Builder.performTask(b, pool, world)
        vs = vs.updated(i, bb)
        pool = pp
        world = ww
      } else {
        vs = vs.updated(i, b.removeGoal)
        pool = b.goal.map{g => pool.surrender(g) }.getOrElse(throw new RuntimeException())
        world = w
      }
    }
    (vs.toList, pool, world)
  }


  //def manageTask(b:Builder, bs:Seq[Builder], gp:GoalPool, w:World):(Builder, GoalPool, World) = {
  //  if (b.task.allowed(b, new BuilderProxy(bs), w)) {
  //    Builder.performTask(b, gp, w)
  //  } else {
  //    b.removeGoal @@ b.goal.map{g => gp.surrender(g) }.getOrElse(gp) @@ w
  //  }
  //}



  def manageGoal(b:Builder, gp:GoalPool): (Builder, GoalPool) = {
    if (b.noGoal) {
      gp.giveGoal(b)
    } else {
      (b, gp)
    }
  }
}
