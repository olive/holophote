package in.dogue.holophote.world

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import scalaz.{-\/, \/-, \/}
import in.dogue.holophote.entities.FailureReason

class ResourceManager(w:World) {
  def nearest(c:Vox):FailureReason \/ Vox= {
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

  def isOccupied(c:Vox):Boolean = w.hasStone(c)
}
