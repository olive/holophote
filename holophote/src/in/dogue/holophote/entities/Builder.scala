package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import scala.util.Random
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.{Direction, FiniteGraph, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.ai.Dijkstra
import in.dogue.holophote.Holophote
import Holophote._
import in.dogue.holophote.resources.{Stone, Resource}
import in.dogue.holophote.world.World

object Builder {
  var count = 0
  def create(cols:Int, rows:Int, pos:Cell,  r:Random) = {
    val tile = CP437.B.mkTile(Color.Black, Color.White)
    count += 1
    Builder(pos, tile,  r, 0, NoTask, NoOrder, NoGoal, Stone.some, count)
  }

  def performTask(builder:Builder, gp:GoalPool, world:World) = {
    val (b, w) = builder.task.perform(builder, world, gp)
    if (b.task.isNone && !b.goal.isNone && !b.order.isNone) {
      val (no, ntOpt) = b.order.next
      ntOpt match {
        case Some(t) =>
          b.setOrder(t, no) @@ gp @@ w
        case None =>
          if (!b.goal.check(b, w)) {
            throw new RuntimeException()
          }
          finishGoal(b, gp) @@ w
      }
    } else {
      b @@ gp @@ w
    }

  }
  def finishGoal(b:Builder, gp:GoalPool):(Builder, GoalPool) = {
    b.removeGoal @@ gp.finish(b.goal)
  }

}

case class Builder(pos:Cell, tile:Tile, r:Random, t:Int, task:Task, order:Order, goal:Goal, inv:Option[Resource], id:Int) {
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
      goal.toOrder(this, w.toGraph(p)) match {
        case Some(ord) => copy(order = ord) @@ pool
        case None =>
          removeGoal @@ pool.surrender(goal)
      }
    }
  }

  def update(p:BuilderProxy, w:World, pool:GoalPool): (Builder, GoalPool) = {
    if (noOrder) {
      updateOrder(p, w, pool)
    } else {
      this @@ pool
    }
  }

  def removeGoal = copy(task=task.none, order=order.none, goal=goal.none)

  def setOrder(t:Task, o:Order):Builder = {
    copy(task=t, order=o)
  }

  def move(c:Cell) = copy(pos=c)

  def setTask(t:Task):Builder = copy(task=t)

  def giveGoal(g:Goal) = copy(goal=g)


  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (pos, tile)
  }
}
