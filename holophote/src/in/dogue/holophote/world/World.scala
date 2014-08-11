package in.dogue.holophote.world

import in.dogue.antiqua.data.{Direction, FiniteGraph, Array2d}
import in.dogue.antiqua.Antiqua.Cell
import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.entities.BuilderProxy
import in.dogue.holophote.resources.{Resource, Stone}

object World {
  def create(cols:Int, rows:Int, r:Random) = {
    val tiles = Array2d.tabulate(cols, rows) { case p =>
      val ttype = Free
      WorldTile.create(ttype, (r.nextDouble > 0.8).select(0, 1))
    }
    val allCells = for (i <- 0 until cols; j <- 0 until rows) yield (i,j)
    World(tiles, allCells)
  }
  private val dirs = Direction.All.map{ d => d.dx @@ d.dy }
}

case class World private (tiles:Array2d[WorldTile], allCells:Seq[Cell]) {
  val cols = tiles.cols
  val rows = tiles.rows
  def toGraph(es:BuilderProxy):FiniteGraph[Cell,Cell] = new FiniteGraph[Cell,Cell] {
    val all = allCells.filter{t => !isSolid(t)}
    val dirs = World.dirs
    def get(c:Cell) = c
    def getNeighbors(c:Cell) = {
      def get(t:Cell) = t.onlyIfs(t.inRange((0,0, cols, rows)) && !isSolid(t) && !es.isOccupied(t))
      dirs.map( p=> get(p |+| c)).flatten
    }
    def getAll = all
  }
  def removeResource(c:Cell, r:Resource) = copy(tiles=tiles.update(c, _.remove(r)))
  def hasStone(c:Cell) = tiles.getOption(c).exists(t => t.items.contains(Stone))
  def buildAt(c:Cell) = copy(tiles=tiles.updated(c, WorldTile.create(Solid, 0)))
  def isSolid(c:Cell):Boolean = tiles.getOption(c).exists(_.isSolid)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< tiles.flatten.map { case (p, t) => t.draw(p) _ }
  }
}
