package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.holophote.world.World
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.resources.Stone


sealed trait TaskResult
case object TaskAvailable extends TaskResult
case class TaskBlocked(b:Builder) extends TaskResult
case object TaskUnavailable extends TaskResult


sealed trait Task {
  def isNone = this == NoTask
  def none = NoTask
  def allowed(b:Builder, p:BuilderProxy, w:World):TaskResult = TaskAvailable
  def perform(b:Builder, w:World, gp:GoalPool):(Builder, World) = (b, w)
}
case class Path(path:List[Cell]) extends Task {
  override def allowed(b:Builder, p:BuilderProxy, w:World) = {
    if (path.isEmpty) {
      TaskAvailable
    } else if (path.headOption.exists{c => w.isSolid(c)}) {
      TaskUnavailable
    } else {
      path.headOption.map {
        c => p.getOccupant(c)
      }.flatten match {
        case Some(b) => TaskBlocked(b)
        case None => TaskAvailable
      }
    }

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
  override def allowed(b:Builder, p:BuilderProxy, w:World) = {
    p.getOccupant(c) match {
      case Some(bb) => TaskBlocked(bb)
      case None =>
        if ((b.pos |-| c).mag2 == 1 && !w.isSolid(c) && b.hasStone) {
          TaskAvailable
        } else {
          TaskUnavailable
        }

    }

  }
  override def perform(b:Builder, w:World, gp:GoalPool):(Builder, World) = (b.setTask(b.task.none).spendStone, w.buildAt(c))
}


case class Gather(c:Cell) extends Task {
  override def allowed(b:Builder, p:BuilderProxy, w:World) = {
    (b.invFree && b.pos == c && w.hasStone(b.pos)).select(TaskUnavailable, TaskAvailable)
  }

  override def perform(b:Builder, w:World, gp:GoalPool):(Builder, World) = {
    (b.setTask(b.task.none).give(Stone), w.removeResource(b.pos, Stone))
  }
}

case object NoTask extends Task
