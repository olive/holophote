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
  def create(body:TileType, floor:TileType, stones:Int) = {
    val floorTile = CP437.`"`.mkTile(Color.Black, Color.DarkGreen)
    val bodyTile = CP437.≡.mkTile(Color.Grey.dim(3), Color.Grey)
    WorldTile(body, floor, bodyTile, floorTile, (0 until stones).map{_ => Stone})
  }
}

case class WorldTile private (body:TileType, floor:TileType, bodyTile:Tile, floorTile:Tile, items:Seq[Resource]) {
  def hasFloor = floor == Solid
  def isPassable = body == Free
  def isSolid = body == Solid && floor == Solid
  def hasStone = items.contains(Stone)
  def remove(r:Resource) = copy(items=items.drop(1))//fixme, remove only r's
  def add(r:Resource) = copy(items=items :+ r)
  def draw(c:Cell, below:Option[WorldTile])(tr:TileRenderer):TileRenderer = {
    val tile = (body, floor, below) match {
      case (Solid,Solid,_) | (Solid,Free,_) => bodyTile
      case (Free,Solid,_) => floorTile
      case (Free,Free, Some(t)) if t.body == Solid || t.floor == Solid => CP437.`.`.mkTile(Color.Black, t.bodyTile.fgColor)
      case _ => CP437.` `.mkTile(Color.Black, Color.Black)
    }

    val t2 = if (items.isEmpty) {
      tile
    } else {
      tile.setCode(CP437.●).setFg(Color.DarkGrey)
    }


    tr <+ (c, t2)
  }
}
