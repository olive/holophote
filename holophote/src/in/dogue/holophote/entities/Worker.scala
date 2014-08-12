package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import scala.util.Random
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.resources.{Stone, Resource}
import in.dogue.holophote.world.{ResourceManager, World}

object Worker {
  var count = 0
  def create(cols:Int, rows:Int, pos:Vox, job:Job,  r:Random) = {
    val tile = CP437.B.mkTile(Color.Black, Color.White)
    count += 1
    Worker(pos, tile,  r, 0, NoTask, NoOrder, NoGoal, Stone.some, job, count)
  }

  def performTask(builder:Worker, gp:GoalPool, world:World) = {
    val (b, w) = builder.task.perform(builder, world, gp)
    if (b.task.isNone && !b.goal.isNone && !b.order.isNone) {
      val (no, ntOpt) = b.order.next
      ntOpt match {
        case Some(t) =>
          b.setOrder(t, no) @@ gp @@ w
        case None =>
          if (!b.goal.check(b, w)) {
            throw new RuntimeException("Postcondition was not met\n%s\n%s".format(b.pos, b.goal))
          }
          finishGoal(b, gp) @@ w
      }
    } else {
      b @@ gp @@ w
    }

  }
  def finishGoal(b:Worker, gp:GoalPool):(Worker, GoalPool) = {
    b.removeGoal @@ gp.finish(b.job, b.goal)
  }

}

case class Worker(pos:Vox, tile:Tile, r:Random, t:Int, task:Task, order:Order, goal:Goal, inv:Option[Resource], job:Job, id:Int) {
  println(job + " " + goal + " " + order + " " + task)
  def noOrder = order.isNone
  def noTask = task.isNone
  def noGoal = goal.isNone
  def invFree = inv.isEmpty
  def give(r:Resource) = copy(inv = r.some)
  def hasStone = inv == Some(Stone)
  def spendStone = copy(inv=None)
  private def updateOrder(p:BuilderProxy, w:World, pool:GoalPool) = {
    if (goal.isNone) {
      this @@ pool
    } else {
      goal.toOrder(this, new ResourceManager(w), w.toGraph(p)) match {
        case Some(ord) =>
          copy(order = ord) @@ pool
        case None =>
          removeGoal @@ pool.surrender(job, goal)
      }
    }
  }

  private def getColor(t:Task) = {
    t match {
      case _:Path => Color.Yellow
      case _:Place => Color.Red
      case _:Gather.type => Color.Blue
      case _:Drop.type => Color.Grey
      case _:MoveTask => Color.Orange
      case tsk if tsk.isNone => Color.White
      case _ => Color.Black
    }
  }

  def update(p:BuilderProxy, w:World, pool:GoalPool): (Worker, GoalPool) = {
    if (noOrder) {
      updateOrder(p, w, pool)
    } else {
      this @@ pool
    }
  }

  def removeGoal = copy(task=task.none, order=order.none, goal=goal.none)

  def setOrder(t:Task, o:Order):Worker = {
    copy(task=t, order=o)
  }

  def move(c:Vox) = copy(pos=c)

  def setTask(t:Task):Worker = copy(task=t)

  def giveGoal(g:Goal) = {
    copy(goal=g)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (pos.xy, tile.setFg(getColor(task)))
  }
}
