package in.dogue.holophote.mode

import in.dogue.antiqua.graphics.{Rect, Tile, TileRenderer}
import scala.util.Random
import in.dogue.holophote.{Game, Helper}
import in.dogue.antiqua.Antiqua.TileGroup
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.input.Controls
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import Antiqua._
object TitleMode {
  def create(cols:Int, rows:Int, r:Random) = {
    def mkTile(r:Random) = {
      val bg = Color.Green.dim(12 + r.nextDouble)
      val fg = Color.Green.dim(9 + r.nextDouble)
      val code = Vector(CP437.♪, CP437.♫, CP437.` `, CP437.` `).randomR(r)
      code.mkTile(bg, fg)
    }
    val rect = Rect.createTextured(cols, rows, mkTile, r)
    val title = Helper.default.mkMap("holophote")
    val span = title.getSpan
    val version = Helper.default.tf.create(Game.Version).toTileGroup |++| ((0, rows - 1))
    val titleOff = (cols - span.width, rows - span.height).map{_/2}
    TitleMode(cols, rows, title |++| titleOff, version, rect, r)
  }
}

case class TitleMode private (cols:Int, rows:Int, title:TileGroup, version:TileGroup, rect:Rect, r:Random) {
  def update = {
    if (Controls.Space.justPressed) {
      val mode = GameMode.create(cols, rows, r)
      TransitionMode.create(cols, rows, this.toMode, mode.toMode).toMode
    } else {
      this.toMode
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< rect.draw((0,0)) <|| title <|| version
  }
  def toMode:Mode = Mode[TitleMode](_.update, _.draw, this)

}
