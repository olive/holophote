package in.dogue.holophote.world

import in.dogue.antiqua.data._
import in.dogue.antiqua.Antiqua.Cell
import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.entities.BuilderProxy
import in.dogue.holophote.resources.{Resource, Stone}
import com.deweyvm.gleany.input.MouseHelper
import scalaz.Scalaz

object World {
  def create(cols:Int, rows:Int, layers:Int, r:Random) = {
    val tiles = Array3d.tabulate(cols, rows, layers) { case p =>
      val (body, floor) = if (p.z == 4) {
        (Free, Solid)
      } else if (p.z > 4) {
        (Free, Free)
      } else {
        (Solid, Solid)
      }
      WorldTile.create(body, floor, (r.nextDouble > 0.8 && body == Free && floor == Solid).select(0, 1))
    }
    World(tiles)
  }
  private val dirs = Direction3.Planar.map{ d => d.dx @@ d.dy @@ d.dz }
}

case class World private (tiles:Array3d[WorldTile]) {
  val cols = tiles.cols
  val rows = tiles.rows
  val layers = tiles.layers
  def toGraph(es:BuilderProxy):Graph[Vox,Vox] = new Graph[Vox,Vox] {
    val dirs = World.dirs
    def get(c:Vox) = c
    def getNeighbors(c:Vox) = {

      def occupied(t:Vox) = false//es.isOccupied(t)
      def inRange(t:Vox) = t.xy.inRange((0,0,cols,rows)) && t.z >= 0 && t.z <= layers - 1
      val adj = dirs.map { p =>
        val t = p |+|+| c
        t.onlyIfs(inRange(t) && isPassable(t) && hasFloor(t) && !occupied(t))
      }
      //oo
      //☺x
      //x?
      val up = dirs.map{ p =>
        val upw = (p |+|+| c) --> Upward
        val ups = upw.onlyIfs(inRange(upw) && isPassable(upw) && hasFloor(upw) && !occupied(upw))
        ups
      }
      //☺o
      //xo
      //?x
      val down = dirs.map{ p =>
        val over = p |+|+| c
        val dw = over --> Downward
        dw.onlyIfs(inRange(dw) && !occupied(dw) && !occupied(over) && isPassable(over) && !hasFloor(over) && hasFloor(dw))
      }
      val ns = adj.flatten ++ up.flatten ++ down.flatten
      ns
    }
  }
  def addResource(c:Vox, r:Resource) = copy(tiles=tiles.update(c, _.add(r)))
  def removeResource(c:Vox, r:Resource) = copy(tiles=tiles.update(c, _.remove(r)))
  def hasStone(c:Vox) = tiles.getOption(c).exists(t => t.items.contains(Stone))
  def buildAt(c:Vox) = {
    val current = tiles.updated(c, WorldTile.create(Solid, Solid, 0))
    val newTiles = if (!tiles.get(c --> Upward).hasFloor) {
      current.updated(c --> Upward, WorldTile.create(Free, Solid, 0))
    } else {
      current
    }
    copy(tiles=newTiles)
  }
  def hasFloor(c:Vox):Boolean = tiles.getOption(c).exists(_.hasFloor)
  def isPassable(c:Vox):Boolean = tiles.getOption(c).exists(_.isPassable)
  def isWalkable(c:Vox):Boolean =  tiles.getOption(c).exists(t => t.isPassable && t.hasFloor)
  def isSolid(c:Vox):Boolean = tiles.getOption(c).exists(_.isSolid)
}
