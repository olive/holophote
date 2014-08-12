package in.dogue.holophote.world

import in.dogue.antiqua.Antiqua
import Antiqua._
import scalaz.{-\/, \/-, \/}
import in.dogue.holophote.entities.FailureReason

class ResourceManager(w:World) {
  def nearest(c:Vox):FailureReason \/ Vox = {
    w.tiles.flatten.filter{ case (ijk, t) =>
      t.hasStone
    }.sortBy{ case (ijk, t) =>
      (ijk |-|-| c).mag
    }.map {
      _._1
    }.headOption match {
      case Some(p) => \/-(p)
      case None => -\/(FailureReason.NoResource)
    }
  }

  def sparse(k:Int, r:(Int,Int,Int,Int)):FailureReason \/ Vox = {
    val x = r._1
    val y = r._2
    val cols = r._3
    val rows = r._4
    val f = (for (i <- 0 until cols; j <- 0 until rows) yield {
      val p = ((i, j) |+| ((x, y))) @@ k
      (p, w.countStone(p).getOrElse(Int.MaxValue))
    }).minBy(_._2)
    \/-(f._1)
  }

  def isOccupied(c:Vox):Boolean = w.hasStone(c)
}
