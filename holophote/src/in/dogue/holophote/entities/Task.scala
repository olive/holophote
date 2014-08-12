package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.holophote.world.World
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.resources.Stone


sealed trait TaskResult
case object TaskAvailable extends TaskResult
case class TaskBlocked(b:Worker) extends TaskResult
case object TaskUnavailable extends TaskResult


sealed trait Task {
  def isNone = this == NoTask
  def none = NoTask
  def allowed(b:Worker, p:BuilderProxy, w:World):TaskResult = TaskAvailable
  def perform(b:Worker, w:World, gp:GoalPool):(Worker, World) = (b, w)
}
case class Path(path:List[Vox]) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    if (path.isEmpty) {
      TaskAvailable
    } else if (path.headOption.exists{c => !w.isPassable(c)}) {
      TaskUnavailable
    } else {
      path.headOption.map {
        c => p.getOccupant(c)
      }.flatten match {
        case Some(bb) if bb.pos != b.pos => TaskBlocked(bb)
        case _ => TaskAvailable
      }
    }

  }
  override def perform(b:Worker, w:World, gp:GoalPool) = {
    val bb = path match {
      case x::xs =>
        b.move(x).setTask(Path(xs))
      case Nil => b.setTask(b.task.none)
    }
    bb @@ w
  }
}
case class Place(c:Vox) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    p.getOccupant(c) match {
      case Some(bb) if bb.pos != b.pos => TaskBlocked(bb)
      case _ =>
        if ((b.pos |-|-| c).mag2 == 1 && w.isPassable(c) && b.hasStone) {
          TaskAvailable
        } else {
          TaskUnavailable
        }

    }

  }
  override def perform(b:Worker, w:World, gp:GoalPool):(Worker, World) = (b.setTask(b.task.none).spendStone, w.buildAt(c))
}


case object Gather extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    (b.invFree && w.hasStone(b.pos)).select(TaskUnavailable, TaskAvailable)
  }

  override def perform(b:Worker, w:World, gp:GoalPool):(Worker, World) = {
    (b.setTask(b.task.none).give(Stone), w.removeResource(b.pos, Stone))
  }
}

case object Drop extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    b.hasStone.select(TaskUnavailable, TaskAvailable)
  }

  override def perform(b:Worker, w:World, gp:GoalPool):(Worker, World) = {
    val newW = b.inv.map{ r => w.addResource(b.pos, r)}.getOrElse(w)
    (b.setTask(b.task.none).spendStone, newW)
  }
}
case class MoveTask(dst:Vox) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    if ((b.pos |-|-| dst).mag != 1 || !w.isPassable(dst)) {
      TaskUnavailable
    } else {
      p.getOccupant(dst) match {
        case Some(blk) if blk.pos != b.pos => TaskBlocked(blk)
        case _ => TaskAvailable
      }
    }
  }

  override def perform(b:Worker, w:World, gp:GoalPool):(Worker, World) = {
    (b.setTask(b.task.none).move(dst), w)
  }
}

case object NoTask extends Task
