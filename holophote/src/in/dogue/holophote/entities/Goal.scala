package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.FiniteGraph
import in.dogue.antiqua.ai.Dijkstra

sealed trait Goal {
  def toOrder(b:Builder, gr:FiniteGraph[Cell,Cell]):Option[Order]
  def isReserved:Boolean
  def reserve:Goal
  def free:Goal
}

case class Build(dst:Cell, r:Boolean) extends Goal {
  def reserve = copy(r=true)
  def isReserved = r
  def free = copy(r=false)
  def toOrder(b:Builder, gr:FiniteGraph[Cell,Cell]) = {
    val path = Dijkstra.pfind(b.pos, dst, gr)
    path.map { p =>
      val first = Path(p)
      val second = Place(dst)
      TaskList(List(first, second))
    }

  }
}






