package in.dogue.holophote.world

import in.dogue.holophote.resources.{Stone, Resource}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._

object TileType {
  val ALl = Vector(Solid,Free)
}
sealed trait TileType
case object Solid extends TileType
case object Free extends TileType


object WorldTile {
  def create(ttype:TileType, stones:Int) = {
    val floor = CP437.`"`.mkTile(Color.Black, Color.DarkGreen)
    val solid = CP437.≡.mkTile(Color.Grey.dim(3), Color.Grey)
    WorldTile(ttype, floor, solid, (0 until stones).map{_ => Stone})
  }
}

case class WorldTile private (ttype:TileType, floor:Tile, solid:Tile, items:Seq[Resource]) {
  def isWalkable:Boolean = ttype == Free
  def isSolid:Boolean = !isWalkable
  def hasStone = items.contains(Stone)
  def remove(r:Resource) = copy(items=items.drop(1))//fixme, remove only r's
  def add(r:Resource) = copy(items=items :+ r)
  def draw(c:Cell, below:Option[WorldTile])(tr:TileRenderer):TileRenderer = {
    val tile =  ttype match {
        case Solid => solid
        case Free =>
          below.map{t => (t.ttype, t.solid)}.map {
            case (Solid, t) => floor
            case (Free, t) => CP437.`.`.mkTile(Color.Black, t.fgColor)
          }.getOrElse(CP437.` `.mkTile(Color.Black, Color.Black))


      }
    val t2 = if (items.isEmpty) {
      tile
    } else {
      tile.setCode(CP437.●).setFg(Color.DarkGrey)
    }


    tr <+ (c, t2)
  }
}
