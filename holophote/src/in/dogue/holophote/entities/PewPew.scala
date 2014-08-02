package in.dogue.holophote.entities

import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

object PewPew {
  def create(ij:Cell) = {
    val tile = CP437.T.mkTile(Color.Black, Color.White)
    PewPew(ij, tile, 0)
  }
}

case class PewPew private (ij:Cell, tile:Tile, t:Int) {
  def getPos = ij
  def update = {
    val hue = (t/100.0) % 1
    val newT = t + 1
    if (newT % 3 == 0) {
      val p = Projectile.createSpiral(ij, Color.fromHsb(hue,1, 1), newT)
      Seq(copy(t=newT).toEntity, p.toEntity)
    } else {
      Seq(copy(t=newT).toEntity)
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, tile)
  }
  def toEntity:Entity = Entity[PewPew](this, _.getPos, _.update, _.draw, _.toEntity)
}
