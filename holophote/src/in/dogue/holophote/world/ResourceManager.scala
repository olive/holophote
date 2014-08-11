package in.dogue.holophote.world

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
class ResourceManager(w:World) {
  def nearest(c:Vox):Option[Vox] = {
    w.tiles.flatten.filter{ case (ijk, t) =>
      t.hasStone
    }.sortBy{ case (ijk, t) =>
      (ijk |-| c).mag
    }.map {
      _._1
    }.headOption
  }

  def isOccupied(c:Vox):Boolean = w.hasStone(c)
}
