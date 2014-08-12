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
      val ttype = if (p.z == 4) {
        FloorSolid
      } else if (p.z > 4) {
        Free
      } else {
        WholeSolid
      }
      WorldTile.create(ttype, (r.nextDouble > 0.8 && ttype == FloorSolid).select(0, 1))
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
      //☺o
      val adj = dirs.map { p =>
        val t = p |+|+| c
        t.onlyIfs(inRange(t) && isPassable(t) && isStandable(t) && !occupied(t))
      }
      //o_
      //☺o
      val up = dirs.map{ p =>
        val upw = (p |+|+| c) --> Upward
        val ups = upw.onlyIfs(inRange(upw) && isPassable(upw) && isStandable(upw) && !occupied(upw))
        ups
      }
      //☺o
      //?_
      val down = dirs.map{ p =>
        val over = p |+|+| c
        val dw = over --> Downward
        dw.onlyIfs(inRange(dw) && !occupied(dw) && !occupied(over) && isPassable(over) && !isStandable(over) && isStandable(dw))
      }
      val ns = adj.flatten ++ up.flatten ++ down.flatten
      ns
    }
  }
  def addResource(c:Vox, r:Resource) = copy(tiles=tiles.update(c, _.add(r)))
  def removeResource(c:Vox, r:Resource) = copy(tiles=tiles.update(c, _.remove(r)))
  def hasStone(c:Vox) = tiles.getOption(c).exists(t => t.items.contains(Stone))
  def buildAt(c:Vox) = {
    val current = tiles.updated(c, WorldTile.create(WholeSolid, 0))
    val newTiles = if (!tiles.get(c --> Upward).isStandable) {
      current.updated(c --> Upward, WorldTile.create(FloorSolid,  0))
    } else {
      current
    }
    copy(tiles=newTiles)
  }
  @inline def exists(c:Vox, f:WorldTile => Boolean) = tiles.getOption(c).exists(f)
  def isStandable(c:Vox):Boolean = exists(c, _.isStandable)
  def isStair(c:Vox):Boolean =     exists(c, _.isStair)
  def isPassable(c:Vox):Boolean =  exists(c, _.isPassable)
  def isWalkable(c:Vox):Boolean =  exists(c, t => t.isPassable && t.isStandable)
  def isSolid(c:Vox):Boolean =     exists(c, _.isSolid)

  def placeStair(c:Vox):World = {
    copy(tiles=tiles.updated(c, WorldTile.create(Stair,  0)))
  }
}
