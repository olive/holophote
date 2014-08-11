package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.FiniteGraph
import in.dogue.antiqua.ai.Dijkstra
import in.dogue.holophote.world.World
import in.dogue.antiqua.Antiqua
import Antiqua._

object Goal {
  var id = 0
}

sealed trait Goal {
  def toOrder(b:Builder, gr:FiniteGraph[Cell,Cell]):Option[Order]
  def isReserved:Boolean
  def reserve:Goal
  def free:Goal
  def isNone:Boolean = this == NoGoal
  def none:Goal = NoGoal
  def id:Int
  def check(b:Builder, w:World):Boolean
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
  def toOrder(b:Builder, gr:FiniteGraph[Cell,Cell]):Option[Order] = None
  def isReserved = false
  def reserve = this
  def free = this
  def check(b:Builder, w:World) = true
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
  def toOrder(b:Builder, gr:FiniteGraph[Cell,Cell]) = {
    val path = Dijkstra.pfind(b.pos, dst, gr)
    path.map { p =>
      val first = Path(p)
      if (b.pos == dst) {
        None
      } else {
        TaskList(List(first)).some
      }

    }.flatten

  }

  def check(b:Builder, w:World) = {
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
  def toOrder(b:Builder, gr:FiniteGraph[Cell,Cell]) = {
    val path = Dijkstra.pfind(b.pos, adjPos, gr)
    path.map { p =>
      val first = Path(p.drop(1))
      val second = Place(dst)
      if (b.pos == adjPos) {
        TaskList(List(second))
      } else {
        TaskList(List(first, second))
      }

    }

  }

  def check(b:Builder, w:World) = {
    b.pos == adjPos && w.isSolid(dst)
  }
}






