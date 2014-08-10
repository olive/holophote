package in.dogue.holophote.world

import in.dogue.antiqua.data.{Direction, FiniteGraph, Array2d}
import in.dogue.antiqua.Antiqua.Cell
import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._

object World {
  def create(cols:Int, rows:Int, r:Random) = {
    val tiles = Array2d.tabulate(cols, rows) { case p =>
      val ttype = (r.nextDouble > 0.9).select(Free, Solid)
      WorldTile.create(ttype)
    }
    val allCells = for (i <- 0 until cols; j <- 0 until rows) yield (i,j)
    World(tiles, allCells)
  }
  private val dirs = Direction.All.map{ d => d.dx @@ d.dy }
}

case class World private (tiles:Array2d[WorldTile], allCells:Seq[Cell]) {
  val cols = tiles.cols
  val rows = tiles.rows
  def toGraph:FiniteGraph[Cell,Cell] = new FiniteGraph[Cell,Cell] {
    val all = allCells.filter{t => !isSolid(t)}
    val dirs = World.dirs
    def get(c:Cell) = c
    def getNeighbors(c:Cell) = {
      def get(t:Cell) = t.onlyIfs(t.inRange((0,0, cols, rows)) && !isSolid(t))
      dirs.map( p=> get(p |+| c)).flatten
    }
    def getAll = all
  }
  def buildAt(c:Cell) = copy(tiles=tiles.updated(c, WorldTile.create(Solid)))
  def isSolid(c:Cell):Boolean = false //!tiles.getOption(c).forall(_.isWalkable)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< tiles.flatten.map { case (p, t) => t.draw(p) _ }
  }
}
