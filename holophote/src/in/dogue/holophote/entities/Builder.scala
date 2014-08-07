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
import in.dogue.holophote.actions.{Exist, Build, Action}
import in.dogue.holophote.structures.Order

object Builder {
  def create(cols:Int, rows:Int, pos:Cell, g:FiniteGraph[Cell,Cell], r:Random) = {
    val tile = CP437.B.mkTile(Color.Black, Color.White)
    val dest = (r.nextInt(cols), r.nextInt(rows))
    val path = Dijkstra.pfind(pos, dest, g)
    Builder(pos, tile,  r, 0, path.map(Move).getOrElse(Wait), Seq(Stone, Stone, Stone))
  }
}

case class Builder(pos:Cell, tile:Tile, r:Random, t:Int, order:Option[Order], inv:Seq[Resource]) {
  def update:(Builder, Action) = {
    //println(path)
    (task match {
      case Wait => this @@ Exist
      case Move(s) => updatePath(s)
    }).incr

  }

  def move(toPos:Cell) = copy(pos=toPos)
  def cancel = copy(order=None) @@ order
  private def updatePath(s:List[Cell]) = {
    s match {
      case x :: xs if t % 10 == 0 => copy(pos=x, task=Move(xs))
      case Nil => copy(task=Wait)
      case _ => this
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (pos, tile)
  }

  def toEntity = Entity[Builder](_.update, _.draw, this)
}
