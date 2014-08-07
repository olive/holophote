package in.dogue.holophote.procgen

import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.graphics.{TileRenderer, Tile}

object Dungeon {
  def create(cols:Int, rows:Int) = {

  }
}

class Dungeon(tiles:Array2d[Tile]) {
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ tiles.flatten
  }
}
