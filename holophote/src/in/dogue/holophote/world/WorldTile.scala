package in.dogue.holophote.world

import in.dogue.holophote.resources.{Stone, Resource}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.graphics.TileRenderer
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
    WorldTile(ttype, (0 until stones).map{_ => Stone})
  }
}

case class WorldTile private (ttype:TileType, items:Seq[Resource]) {
  def isWalkable:Boolean = ttype == Free
  def isSolid:Boolean = !isWalkable
  def hasStone = items.contains(Stone)
  def remove(r:Resource) = copy(items=items.drop(1))//fixme, remove only r's
  def add(r:Resource) = copy(items=items :+ r)
  def draw(c:Cell)(tr:TileRenderer):TileRenderer = {
    val tile = ttype match {
      case Solid => CP437.`#`.mkTile(Color.Black, Color.White)
      case Free =>
        if (items.contains(Stone)) {
          CP437.●.mkTile(Color.Green.dim(6), Color.Grey.dim(3))
        } else {
          CP437.▒.mkTile(Color.Green.dim(6), Color.Green.dim(3))
        }


    }
    tr <+ (c, tile)
  }
}
