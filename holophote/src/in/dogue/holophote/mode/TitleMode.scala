package in.dogue.holophote.mode

import in.dogue.antiqua.graphics.TileRenderer
import scala.util.Random

object TitleMode {
  def create(cols:Int, rows:Int, r:Random) = {
    TitleMode(cols, rows)
  }
}

case class TitleMode private (cols:Int, rows:Int) {
  def update = this.toMode
  def draw(tr:TileRenderer):TileRenderer = {
    tr
  }
  def toMode:Mode = Mode[TitleMode](_.update, _.draw, this)

}
