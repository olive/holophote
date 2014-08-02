package in.dogue.holophote.entities

import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

object PewPew {
  def create(ij:Cell) = {
    val tile = CP437.T.mkTile(Color.Black, Color.White)
    PewPew(ij, tile)
  }
}

case class PewPew private (ij:Cell, t:Tile) {
  def getPos = ij
  def update = Seq(toEntity)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, t)
  }
  def toEntity:Entity = Entity[PewPew](this, _.getPos, _.update, _.draw, _.toEntity)
}
