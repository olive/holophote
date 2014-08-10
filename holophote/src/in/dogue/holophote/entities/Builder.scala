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
    Builder(pos, tile,  r, 0, NoTask, NoOrder, None, Seq(Stone, Stone, Stone), count)
  }

  def performTask(builder:Builder, gp:GoalPool, world:World) = {
    val (b, w) = builder.task.perform(builder, world, gp)
    if (b.task.isNone) {
      val (no, ntOpt) = builder.order.next
      ntOpt match {
        case Some(t) =>
          b.setTask(t, no) @@ gp @@ w
        case None =>
          finishGoal(b, gp) @@ w
      }
    } else {
      b @@ gp @@ w
    }

  }
  def finishGoal(b:Builder, gp:GoalPool):(Builder, GoalPool) = {
    b.removeGoal @@ b.goal.map {g => gp.finish(g)}.getOrElse(gp)
  }

}

case class Builder(pos:Cell, tile:Tile, r:Random, t:Int, task:Task, order:Order, goal:Option[Goal], inv:Seq[Resource], id:Int) {
  def noOrder = order.isNone
  def noTask = task.isNone
  def noGoal = goal.isEmpty

  private def updateOrder(w:World) = {
    goal match {
      case Some(g) =>
        val ord = g.toOrder(this, w.toGraph).getOrElse(order.none)
        copy(order=ord)
      case _ => this
    }
  }

  def update(w:World) = {
    if (noOrder) {
      updateOrder(w)
    } else {
      this
    }
  }

  def removeGoal = copy(task=task.none, order=order.none, goal=None)

  def setTask(t:Task, o:Order):Builder = {
    if (!o.isNone) {
      println(o)
    }
    copy(task=t, order=o)
  }

  def move(c:Cell) = copy(pos=c)

  def setTask(t:Task):Builder = copy(task=t)

  def giveGoal(g:Goal) = copy(goal=g.some)


  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (pos, tile)
  }
}
