package in.dogue.holophote.blueprints

import in.dogue.antiqua.data.Graph
import in.dogue.antiqua.Antiqua._
import in.dogue.holophote.entities._

trait Blueprint {
  def generate(g:Graph[Vox,Vox]):(Map[Job, List[Goal]],Map[Job, List[Goal]])
}

case class RectWall(from:Vox, k:Int, r:(Int,Int,Int,Int)) {
  self =>
  val resourcePoint = (r._1+r._3/2, r._2+r._4/2, k)
  def generate(g:Graph[Vox,Vox]) = {
    val cols = r._3
    val rows = r._4
    val list = (for (i <- 0 until cols;
                     j <- 0 until rows;
                     if (i == 0 || j == 0 || i == cols-1 || j == rows-1) && !(i == cols/2 && j == 0)) yield {
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
      (p, adj)
    }).sortBy { case (p, adj) =>
      math.atan2(p.y - r._2 - rows/2, p.x - r._1 - cols/2)
    }.map { case (p, adj) =>
      Build.create(adj @@ k, p @@ k)
    }.toList

    val major = Map[Job, List[Goal]](Builder -> list).withDefaultValue(List())
    val minor = Map[Job, List[Goal]](Gatherer -> List(Stock.create(resourcePoint, from))).withDefaultValue(List())
    major @@ minor
  }

  def toBlueprint = new Blueprint {
    override def generate(g: Graph[Vox,Vox]):(Map[Job, List[Goal]],Map[Job, List[Goal]]) = {
      self.generate(g)
    }
  }
}
