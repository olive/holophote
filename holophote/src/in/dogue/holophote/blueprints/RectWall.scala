package in.dogue.holophote.blueprints

import in.dogue.antiqua.Antiqua.Vox
import in.dogue.antiqua.data.Graph
import in.dogue.holophote.entities._
import in.dogue.antiqua.Antiqua
import Antiqua._

case class RectWall(from:Vox, k:Int, r:(Int,Int,Int,Int)) {
  self =>
  val to = (r._1+1, r._2+1, r._3-2,r._4-2)
  def generate(g:Graph[Vox,Vox]):(Seq[(Job, PlanId=>Goal)], Seq[(Job, PlanId=>Goal)]) = {
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
      (Builder:Job) -> Build.create(adj @@ k, p @@ k) _
    }.toList

    val major = list
    val minor = Seq[(Job, PlanId=>Goal)](
      Gatherer -> Stock.create(k, to, from) _

    )
    major @@ minor
  }

  def toBlueprint = new Blueprint {
    override def generate(g: Graph[Vox,Vox]) = {
      self.generate(g)
    }
  }
}
