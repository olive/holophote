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
  def create(cols:Int, rows:Int, pos:Cell,  r:Random) = {
    val tile = CP437.B.mkTile(Color.Black, Color.White)
    Builder(pos, tile,  r, 0, NoTask, NoOrder, None, Seq(Stone, Stone, Stone))
  }

  def performTask(builder:Builder, gp:GoalPool, world:World) = {
    val (b, w) = builder.task.perform(builder, world, gp)
    val (no, ntOpt) = builder.order.next
    ntOpt match {
      case Some(f) => b.setTask(f(b.pos), no) @@ gp @@ w
      case None => finishGoal(b, gp) @@ w
    }
  }
  def finishGoal(b:Builder, gp:GoalPool):(Builder, GoalPool) = {
    b.removeGoal @@ b.goal.map {g => gp.finish(g)}.getOrElse(gp)
  }

}

case class Builder(pos:Cell, tile:Tile, r:Random, t:Int, task:Task, order:Order, goal:Option[Goal], inv:Seq[Resource]) {
  def noOrder = order.isNone
  def noTask = task.isNone
  def noGoal = goal.isEmpty
  def update = this

  def removeGoal = copy(task=task.none, order=order.none, goal=None)

  def setTask(t:Task, o:Order):Builder = {
    copy(task=t, order=o)
  }

  def giveGoal(g:Goal) = copy(goal=g.some)


  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (pos, tile)
  }

  def toEntity = Entity[Builder](_.update, _.draw, this)
}
