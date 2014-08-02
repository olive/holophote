package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua
import Antiqua._

object Path {
  def create(r0:Int)(xt:Double => Double)(yt: Double => Double) = {
    Path(xt, yt, r0)
  }
}

case class Path private (xt:Double=>Double, yt:Double=>Double, t:Int) {
  def update = copy(t=t+1)
  def getPos(ij:Cell) = {
    ij |+| pathPos
  }
  private def pathPos = (xt(t/10f), yt(t/10f)).map{_.toInt}
}
