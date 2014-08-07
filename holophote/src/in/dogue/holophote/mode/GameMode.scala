package in.dogue.holophote.mode

import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Rect}
import in.dogue.holophote.input.Controls
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.entities.{Builder, Entity}
import in.dogue.holophote.world.World

object GameMode {
  def create(cols:Int, rows:Int, r:Random) = {
    val r = new Random(0)
    val world = World.create(cols, rows, r)
    val es = (0 until 10) map { i =>
      val x = r.nextInt(cols)
      val y = r.nextInt(rows)
      Builder.create(cols, rows, (x, y), world.toGraph, r).toEntity
    }
    GameMode(cols, rows, world, es)
  }
}

case class GameMode private (cols:Int, rows:Int, world:World, es:Seq[Entity]) {
  def update = copy(es=es.map{_.update}).toMode
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< world.draw <++< es.map { _.draw _ }
  }
  def toMode:Mode = Mode[GameMode](_.update, _.draw, this)

}
