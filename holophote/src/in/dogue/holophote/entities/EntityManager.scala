package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.world.World

class EntityManager {

  def manageTask(b:Builder, gp:GoalPool, w:World):(Builder, GoalPool, World) = {
    if (b.task.allowed(b, w)) {
      Builder.performTask(b, gp, w)
    } else {
      println("not allowed")
      b.removeGoal @@ b.goal.map{g => gp.surrender(g) }.getOrElse(gp) @@ w
    }
  }



  def manageGoal(b:Builder, gp:GoalPool): (Builder, GoalPool) = {
    if (b.noGoal) {
      gp.giveGoal(b)
    } else {
      (b, gp)
    }
  }
}
