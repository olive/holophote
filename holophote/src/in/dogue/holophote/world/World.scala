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

object World {
  def create(cols:Int, rows:Int, layers:Int, r:Random) = {
    val tiles = Array3d.tabulate(cols, rows, layers) { case p =>
      val ttype =if (p.z > 3) {
        Free
      } else {
        Solid
      }
      WorldTile.create(ttype, (r.nextDouble > 0.8 && ttype == Free && p.z == 4).select(0, 1))
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
      def get(t:Vox) = t.onlyIfs(t.xy.inRange((0,0, cols, rows)) && !isSolid(t) && !es.isOccupied(t))
      dirs.map( p=> get(p |+| c)).flatten
    }
  }
  def addResource(c:Vox, r:Resource) = copy(tiles=tiles.update(c, _.add(r)))
  def removeResource(c:Vox, r:Resource) = copy(tiles=tiles.update(c, _.remove(r)))
  def hasStone(c:Vox) = tiles.getOption(c).exists(t => t.items.contains(Stone))
  def buildAt(c:Vox) = copy(tiles=tiles.updated(c, WorldTile.create(Solid, 0)))
  def isSolid(c:Vox):Boolean = tiles.getOption(c).exists(_.isSolid)

}
