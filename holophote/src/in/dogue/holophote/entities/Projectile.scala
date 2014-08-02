package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.{CP437, Direction}
import com.deweyvm.gleany.graphics.Color

object Projectile {
  def createSpiral(ij:Cell, color:Color, t0:Int)= {
    val t = CP437.*.mkTile(Color.Black, color)
    val angle = (math.Pi*2 * math.sin( t0/50.0)) % (2*math.Pi)
    val dx = math.cos(angle)
    val dy = math.sin(angle)
    val path = Path.create(15) { t =>
      t*dx
    } { t =>
      t*dy
    }
    create(ij, path, t)
  }
  def create(ij:Cell, path:Path, tile:Tile)= {
    Projectile(ij, path, tile)
  }
}

case class Projectile private (ij:Cell, path:Path, t:Tile) {
  def update = copy(path=path.update).toEntity.seq
  def getPos = path.getPos(ij)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <| (getPos, t)
  }

  def toEntity:Entity = Entity[Projectile](this, _.getPos, _.update, _.draw, _.toEntity)
}
