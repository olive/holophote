package in.dogue.holophote.world

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
class ResourceManager(w:World) {
  def nearest(c:Cell):Option[Cell] = {
    w.tiles.flatten.filter{ case (ij, t) =>
      t.hasStone
    }.sortBy{ case (ij, t) =>
      (ij |-| c).mag
    }.map {
      _._1
    }.headOption
  }

  def isOccupied(c:Cell):Boolean = w.hasStone(c)
}
