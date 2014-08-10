package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua._

sealed trait Order {
  def next:(Order, Option[Cell => Task])
  def isNone:Boolean = this == NoOrder
  def none:Order = NoOrder
}

case object NoOrder extends Order {
  def next = NoOrder @@ None
}



