package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.Graph

sealed trait Goal {
  def toOrder(b:Builder, gr:Graph[Cell,Cell]):Order
  def isReserved:Boolean
  def reserve:Goal
  def free:Goal
}






