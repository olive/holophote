package in.dogue.holophote.entities

import in.dogue.holophote.actions.Action
import in.dogue.holophote.world.World
import in.dogue.holophote.structures.Order

class EntityManager {
  val orders:List[Order]
  def process(w:World, b:Builder) = {
    val (nb, action)  = b.update
    a match {
      case Move(c) =>

    }
  }

  def performAction(b:Builder, a:Action):(Builder, Option[Order])
}
