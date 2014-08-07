package in.dogue.holophote.mode

import in.dogue.antiqua.graphics.{Tile, Filter, TileRenderer}
import in.dogue.antiqua.Antiqua.Cell
import com.deweyvm.gleany.graphics.Color

object TransitionMode {
  def create(cols:Int, rows:Int, from:Mode, to:Mode) = {
    TransitionMode(cols, rows, from, to, 0, 60)
  }
}

case class TransitionMode private (cols:Int, rows:Int, from:Mode, to:Mode, t:Int, tMax:Int) {

  def update = {
    if (t > tMax) {
      to
    } else {
      copy(t=t+1).toMode
    }
  }

  private def getDim = {
    if (t < tMax / 2) {
      t / (tMax/2.toDouble)
    } else {
      1 - (t - tMax/2)/(tMax/2.toDouble)
    }
  }

  private def getMode = {
    if (t < tMax/2) {
      from
    } else {
      to
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val dim = getDim
    def process(c:Cell)(t:Tile) = {
      def mix(c:Color) = c.mix(Color.Black, dim)
      t.mapBg(mix).mapFg(mix)
    }
    tr.withFilter(process) { r => r <+< getMode.draw}
  }

  def toMode:Mode = Mode[TransitionMode](_.update, _.draw, this)
}
