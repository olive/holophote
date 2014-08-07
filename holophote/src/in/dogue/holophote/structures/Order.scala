package in.dogue.holophote.structures

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.holophote.entities.{Task, Builder}
import in.dogue.holophote.resources.ResourceManager

trait Order {
  def next:(Option[Order], Task)
  def rewind:Order
}

