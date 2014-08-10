package in.dogue.holophote.blueprints

import in.dogue.antiqua.data.Graph
import in.dogue.antiqua.Antiqua._
import in.dogue.holophote.entities.{Build, Goal}

trait Blueprint {
  def generate(g:Graph[Cell,Cell]):List[Goal]
}

case class RectWall(r:(Int,Int,Int,Int)) {
  self =>
  def generate(g:Graph[Cell,Cell]) = {
    val cols = r._3
    val rows = r._4
    (for (i <- 0 until cols; j <- 0 until rows; if i == 0 || j == 0 || i == cols-1 || j == rows-1) yield {
      val p = (i, j) |+| ((r._1, r._2))
      val adj =
        if (i == 0) {
          p |- 1
        } else if (j == 0) {
          p -| 1
        } else if (i == cols - 1) {
          p |+ 1
        } else {
          p +| 1
        }
      Build.create(adj, p)
    }).toList
  }

  def toBlueprint = new Blueprint {
    override def generate(g: Graph[Cell,Cell]): List[Goal] = self.generate(g)
  }
}
