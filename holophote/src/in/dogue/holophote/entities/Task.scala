package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.holophote.world.World

sealed trait Task {
  def isNone = this == NoTask
  def none = NoTask
  def allowed(b:Builder, w:World):Boolean = false
  def perform(b:Builder, w:World, gp:GoalPool):(Builder, World) = (b, w)
}
case class Move(path:List[Cell]) extends Task
case object NoTask extends Task
