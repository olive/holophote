package in.dogue.holophote

import in.dogue.antiqua.graphics.{Tile, TextFactory}
import in.dogue.antiqua.Antiqua.TileGroup
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
object Helper {
  val default = {
    def mkMap(s:String) = Tile.groupFromFile(s, "tiles", CP437.intToCode, _.mkTile(Color.Black, Color.White))
    Helper(TextFactory(Color.Black, Color.White, CP437.unicodeToCode), mkMap)
  }
}

case class Helper(tf:TextFactory, mkMap: String => TileGroup) {

}
