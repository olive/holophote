package in.dogue.holophote.world

import in.dogue.holophote.resources.{Stone, Resource}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._

object TileType {
  val ALl = Vector(WholeSolid, FloorSolid, Stair, Free)
}
sealed trait TileType
case object Stair extends TileType
case object FloorSolid extends TileType
case object WholeSolid extends TileType
case object Free extends TileType


object WorldTile {
  def create(ttype:TileType, stones:Int) = {
    val floorTile = CP437.`"`.mkTile(Color.Black, Color.DarkGreen)
    val bodyTile = CP437.≡.mkTile(Color.Grey.dim(3), Color.Grey)
    WorldTile(ttype, bodyTile, floorTile, (0 until stones).map{_ => Stone})
  }
}

case class WorldTile private (ttype:TileType, bodyTile:Tile, floorTile:Tile, items:Seq[Resource]) {
  def isStandable = ttype == Stair || ttype == FloorSolid
  def isPassable = ttype != WholeSolid
  def isSolid = ttype == WholeSolid
  def isStair = ttype == Stair
  def hasStone = items.contains(Stone)
  def isFree = ttype == Free
  def remove(r:Resource) = copy(items=items.drop(1))//fixme, remove only r's
  def add(r:Resource) = copy(items=items :+ r)
  def draw(c:Cell, below:Option[WorldTile])(tr:TileRenderer):TileRenderer = {
    val tile = (ttype, below) match {
      case (WholeSolid, _) => bodyTile
      case (FloorSolid,_) => floorTile
      case (Free, Some(t)) if !t.isFree => CP437.`.`.mkTile(Color.Black, t.bodyTile.fgColor)
      case (Free, Some(t)) if !t.isStair => CP437.`v`.mkTile(Color.Black, Color.Brown)
      case (Stair, _) => CP437.`X`.mkTile(Color.Black, Color.Brown)
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
