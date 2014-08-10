package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.holophote.world.World
import in.dogue.antiqua.Antiqua
import Antiqua._
sealed trait Task {
  def isNone = this == NoTask
  def none = NoTask
  def allowed(b:Builder, w:World):Boolean = true
  def perform(b:Builder, w:World, gp:GoalPool):(Builder, World) = (b, w)
}
case class Path(path:List[Cell]) extends Task {
  override def allowed(b:Builder, w:World) = {
    path.isEmpty || path.headOption.exists{p => !w.isSolid(p)}
  }
  override def perform(b:Builder, w:World, gp:GoalPool) = {
    val bb = path match {
      case x::xs =>
        b.move(x).setTask(Path(xs))
      case _ => b.setTask(b.task.none)
    }
    bb @@ w
  }
}
case class Place(c:Cell) extends Task {
  override def allowed(b:Builder, w:World) = {
    !w.isSolid(c)
  }
  override def perform(b:Builder, w:World, gp:GoalPool):(Builder, World) = (b, w.buildAt(c))
}
case object NoTask extends Task
