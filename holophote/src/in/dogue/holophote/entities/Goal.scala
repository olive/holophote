package in.dogue.holophote.entities

import in.dogue.antiqua.data.{Downward, Graph}
import in.dogue.holophote.world.{ResourceManager, World}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.Holophote

object Goal {
  var id = 0
}

sealed trait Goal {
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):Option[Order]
  def isReserved:Boolean
  def reserve:Goal
  def free:Goal
  def isNone:Boolean = this == NoGoal
  def isPossibleFor(b:Worker, rm:ResourceManager, w:World):Boolean
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
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):Option[Order] = None
  def isReserved = false
  def reserve = this
  def free = this
  def check(b:Worker, w:World) = true
  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = true
}

object Move {
  def create(dst:Vox) = {
    Goal.id += 1
    Move(dst, false, Goal.id)
  }
}

case class Move(dst:Vox, r:Boolean, override val id:Int) extends Goal {
  def reserve = copy(r=true)
  def isReserved = r
  def free = copy(r=false)
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]) = {
    for {
      path <- Holophote.pfind(b.pos, dst, gr).map{_.drop(1)}
      if b.pos != dst
    } yield {
      TaskList(List(Path(path)))
    }

  }

  def check(b:Worker, w:World) = {
    b.pos == dst
  }

  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = {
    true//always try, no better way than to pathfind and return None
  }
}

object Build {
  def create(adjPos:Vox, dst:Vox) = {
    Goal.id += 1
    Build(adjPos, dst, false, Goal.id)
  }
}

case class Build private (adjPos:Vox, dst:Vox, r:Boolean, override val id:Int) extends Goal {
  def reserve = copy(r=true)
  def isReserved = r
  def free = copy(r=false)
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):Option[Order] = {
    val isHolding = b.hasStone
    val drop = Drop.onlyIfl(isHolding)
    val isBlocked = rm.isOccupied(dst)
    if (isBlocked) {
      return for {
        path <- Holophote.pfind(b.pos, dst, gr).map{_.drop(1)}
      } yield {
        TaskList(drop ++ List(Path(path), Gather, MoveTask(adjPos), Place(dst)))
      }
    }
    if (isHolding) {
      for {
        path <- Holophote.pfind(b.pos, adjPos, gr).map{_.drop(1)}
      } yield {
        TaskList(List(Path(path), Place(dst)))
      }

    } else {
      for {
        p <- rm.nearest(dst)
        path1 <- Holophote.pfind(b.pos, p, gr).map{_.drop(1)}
        path2 <- Holophote.pfind(p, adjPos, gr)
      } yield {
        TaskList(List(Path(path1), Gather, Path(path2), Place(dst)))
      }
    }

  }

  def check(b:Worker, w:World) = {
    b.pos == adjPos && w.isSolid(dst)
  }

  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = {
    w.isSolid(adjPos --> Downward) && !w.isSolid(dst)
  }
}

object Stock {
  def create(pt:Vox, from:Vox) = {
    Goal.id += 1
    Stock(from, pt, Goal.id)
  }
}

case class Stock private (from:Vox, pt:Vox, override val id:Int) extends Goal {
  def reserve = this
  def isReserved = false
  def free = this
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):Option[Order] = {
    for {
      p <- rm.nearest(from) if p != pt
      path1 <- Holophote.pfind(b.pos, p, gr).map{_.drop(1)}
      path2 <- Holophote.pfind(p, pt, gr)
    } yield {
      //for simplicity
      val l1 = if (b.hasStone) {
        List(Drop)
      } else {
        List()
      }
      TaskList(l1 ++ List(Path(path1), Gather, Path(path2), Drop))
    }
  }

  def check(b:Worker, w:World) = {
    !w.isSolid(pt)
  }

  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = {
    w.isSolid(pt --> Downward) && !w.isSolid(pt)
  }
}





