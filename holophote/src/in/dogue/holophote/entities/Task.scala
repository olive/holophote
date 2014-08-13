package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.holophote.world.World
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.resources.Stone
import in.dogue.holophote.Schema
import scalaz.{IList}


sealed trait TaskResult
case object TaskAvailable extends TaskResult
case class TaskBlocked(b:Worker) extends TaskResult
case class TaskUnavailable(reason:String) extends TaskResult

object Task {
  def check(ls:IList[(Boolean,String)]) = {
    rawCheck(ls)(TaskAvailable)
  }

  def rawCheck(ls:IList[(Boolean,String)])(res: =>TaskResult) = {
    ls.dropWhile{ case (b,_) => b }.headOption.fold[TaskResult](res){ case (_, s) => TaskUnavailable(s)}
  }

  def checkAdjacent(p1:Vox, p2:Vox) = ((p1 |-|-| p2).mag == 1) -> "%s<->%s not adjacent".format(p1, p2)
  def checkStone(w:Worker) = w.hasStone -> "Worker(#%s) has no resource".format(w.id)
}

sealed trait Task {
  def isNone = this == NoTask
  def none = NoTask
  def allowed(b:Worker, p:BuilderProxy, w:World):TaskResult = TaskAvailable
  def perform(b:Worker, w:World, gp:Schema):(Worker, World) = (b, w)
}
case class Path(path:List[Vox]) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    if (path.isEmpty) {
      TaskAvailable
    } else if (path.headOption.exists{c => !w.isPassable(c)}) {
      TaskUnavailable("Location %s impassable".format(path.headOption.fold("")(_.toString)))
    } else {
      path.headOption.map {
        c => p.getOccupant(c)
      }.flatten match {
        case Some(bb) if bb.pos != b.pos => TaskBlocked(bb)
        case _ => TaskAvailable
      }
    }

  }
  override def perform(b:Worker, w:World, gp:Schema) = {
    val bb = path match {
      case x::xs =>
        b.move(x).setTask(Path(xs))
      case Nil => b.setTask(b.task.none)
    }
    bb @@ w
  }

  override def toString = {
    path match {
      case Nil => "Path()"
      case x :: xs => xs.reverse match {
        case Nil => "Path(%s)".format(x)
        case end :: _ => "Path(%s..%s)".format(x, end)
      }
    }
  }
}
case class Place(c:Vox) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    p.getOccupant(c) match {
      case Some(bb) if bb.pos != b.pos => TaskBlocked(bb)
      case _ =>
        val checks = IList(
          Task.checkAdjacent(b.pos, c),
          w.isPassable(c) -> "%s impassable".format(c),
          Task.checkStone(b)
        )
        Task.check(checks)

    }

  }
  override def perform(b:Worker, w:World, gp:Schema):(Worker, World) = (b.setTask(b.task.none).spendStone, w.buildAt(c))
}

case class DigStair(pt:Vox) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    w.isSolid(pt).select(TaskUnavailable("%s is impassable".format(pt)), TaskAvailable)
  }

  override def perform(b:Worker, w:World, gp:Schema):(Worker, World) = {
    (b.setTask(b.task.none), w.digStair(pt))
  }
}

case class DigTunnel(pt:Vox) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    val checks = IList(
      w.isSolid(pt) -> "%s is already dug".format(pt),
      Task.checkAdjacent(pt, b.pos)
    )
    Task.check(checks)
  }

  override def perform(b:Worker, w:World, gp:Schema):(Worker, World) = {
    (b.setTask(b.task.none), w.dig(pt))
  }
}

case class BuildStair(pt:Vox) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    val checks = IList(
      Task.checkStone(b),
      (!w.isSolid(pt)) -> "Build site %s blocked".format(pt)
    )
    Task.check(checks)
  }

  override def perform(b:Worker, w:World, gp:Schema):(Worker, World) = {
    (b.setTask(b.task.none).spendStone, w.placeStair(pt))
  }
}

case object Gather extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    val checks = IList(
      w.hasStone(b.pos) -> "No resource to pick up at %s".format(b.pos),
      b.invFree -> "Worker(#%s) already holding resource".format(b.id)
    )
    Task.check(checks)
  }

  override def perform(b:Worker, w:World, gp:Schema):(Worker, World) = {
    (b.setTask(b.task.none).give(Stone), w.removeResource(b.pos, Stone))
  }
}

case object Drop extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    Task.check(IList(Task.checkStone(b)))
  }

  override def perform(b:Worker, w:World, gp:Schema):(Worker, World) = {
    val newW = b.inv.map{ r => w.addResource(b.pos, r)}.getOrElse(w)
    (b.setTask(b.task.none).spendStone, newW)
  }
}
case class MoveTask(dst:Vox) extends Task {
  override def allowed(b:Worker, p:BuilderProxy, w:World) = {
    val checks = IList(
      Task.checkAdjacent(b.pos, dst),
      w.isPassable(dst) -> "%s is impassable".format(dst)
    )
    Task.rawCheck(checks){
      p.getOccupant(dst) match {
        case Some(blk) if blk.pos != b.pos => TaskBlocked(blk)
        case _ => TaskAvailable
      }
    }
  }

  override def perform(b:Worker, w:World, gp:Schema):(Worker, World) = {
    (b.setTask(b.task.none).move(dst), w)
  }
}

case object NoTask extends Task
