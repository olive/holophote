package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell

object Path {
  def create(ij:Cell)(xt:Double => Double)(yt: Double => Double) = {
    Path(xt, yt, ij, 0)
  }
}

case class Path private (xt:Double=>Double, yt:Double=>Double, ij:Cell, t:Int) {
  def update = copy(t=t+1)
  def getPos = ij |+| pathPos
  private def pathPos = (xt(t).toInt, yt(t).toInt)
}
