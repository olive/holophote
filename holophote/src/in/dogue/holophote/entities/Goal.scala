package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.FiniteGraph
import in.dogue.antiqua.ai.Dijkstra
import in.dogue.holophote.world.{ResourceManager, World}
import in.dogue.antiqua.Antiqua
import Antiqua._

object Goal {
  var id = 0
}

sealed trait Goal {
  def toOrder(b:Worker, rm:ResourceManager, gr:FiniteGraph[Cell,Cell]):Option[Order]
  def isReserved:Boolean
  def reserve:Goal
  def free:Goal
  def isNone:Boolean = this == NoGoal
  def none:Goal = NoGoal
  def id:Int
  def check(b:Worker, w:World):Boolean
  override def equals(other:Any):Boolean = {
    if (!other.isInstanceOf[Goal]) {
      return false
    }
    val b = other.asInstanceOf[Goal]
    b.id == id
  }
}

case object NoGoal extends Goal {
  override val id = -1
  def toOrder(b:Worker, rm:ResourceManager, gr:FiniteGraph[Cell,Cell]):Option[Order] = None
  def isReserved = false
  def reserve = this
  def free = this
  def check(b:Worker, w:World) = true
}

object Move {
  def create(dst:Cell) = {
    Goal.id += 1
    Move(dst, false, Goal.id)
  }
}

case class Move(dst:Cell, r:Boolean, override val id:Int) extends Goal {
  def reserve = copy(r=true)
  def isReserved = r
  def free = copy(r=false)
  def toOrder(b:Worker, rm:ResourceManager, gr:FiniteGraph[Cell,Cell]) = {
    for {
      path <- Dijkstra.pfind(b.pos, dst, gr)
      if b.pos != dst
    } yield {
      TaskList(List(Path(path)))
    }

  }

  def check(b:Worker, w:World) = {
    b.pos == dst
  }
}

object Build {
  def create(adjPos:Cell, dst:Cell) = {
    Goal.id += 1
    Build(adjPos, dst, false, Goal.id)
  }
}

case class Build private (adjPos:Cell, dst:Cell, r:Boolean, override val id:Int) extends Goal {
  def reserve = copy(r=true)
  def isReserved = r
  def free = copy(r=false)
  def toOrder(b:Worker, rm:ResourceManager, gr:FiniteGraph[Cell,Cell]):Option[Order] = {
    val isHolding = b.hasStone
    val drop = Drop.onlyIfl(isHolding)
    val isBlocked = rm.isOccupied(dst)
    if (isBlocked) {
      return for {
        path <- Dijkstra.pfind(b.pos, dst, gr).map{_.drop(1)}
      } yield {
        TaskList(drop ++ List(Path(path), Gather, MoveTask(adjPos), Place(dst)))
      }
    }
    if (isHolding) {
      //fixme -- hack
      for {
        path <- Dijkstra.pfind(b.pos, adjPos, gr).map{_.drop(1)}.map{_.dropRight(1)}
      } yield {
        TaskList(List(Path(path), MoveTask(adjPos), Place(dst)))
      }

    } else {
      for {
        p <- rm.nearest(dst)
        path1 <- Dijkstra.pfind(b.pos, p, gr).map{_.drop(1)}
        path2 <- Dijkstra.pfind(p, adjPos, gr)
      } yield {
        TaskList(List(Path(path1), Gather, Path(path2), Place(dst)))
      }
    }

  }

  def check(b:Worker, w:World) = {
    b.pos == adjPos && w.isSolid(dst)
  }
}






