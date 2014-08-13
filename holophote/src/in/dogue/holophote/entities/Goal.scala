package in.dogue.holophote.entities

import in.dogue.antiqua.data.{Direction3, Downward, Graph}
import in.dogue.holophote.world.{Stair, ResourceManager, World}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.Holophote
import scalaz._
import scalaz.-\/

object Goal {
  var id = 0
}

sealed trait Goal {
  def parent:PlanId
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):FailureReason \/ Order
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
  private val netherPlan = Plan.create(Map(), Map())
  override val parent = netherPlan.id
  override val id = -1
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):FailureReason \/ Order = {
    -\/(FailureReason.Other("Had no goal in the first place"))
  }
  def isReserved = false
  def reserve = this
  def free = this
  def check(b:Worker, w:World) = true
  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = true
}

object Move {
  def create(dst:Vox)(parent:PlanId) = {
    Goal.id += 1
    Move(dst, false, parent, Goal.id)
  }
}

case class Move(dst:Vox, r:Boolean, override val parent:PlanId, override val id:Int) extends Goal {
  def reserve = copy(r=true)
  def isReserved = r
  def free = copy(r=false)
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):FailureReason \/ Order = {
    if (b.pos == dst) {
      -\/(FailureReason.AlreadyComplete)
    } else {
      for {
        path <- Holophote.pfind(b.pos, dst, gr)
      } yield {
        TaskList(List(Path(path.drop(1))))
      }
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
  def create(adjPos:Vox, dst:Vox)(parent:PlanId):Goal = {
    Goal.id += 1
    Build(adjPos, dst, false, parent, Goal.id)
  }
}

case class Build private (adjPos:Vox, dst:Vox, r:Boolean, override val parent:PlanId, override val id:Int) extends Goal {

  def reserve = copy(r=true)
  def isReserved = r
  def free = copy(r=false)
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):FailureReason \/ Order = {
    val isHolding = b.hasStone
    val drop = Drop.onlyIfl(isHolding)
    val isBlocked = rm.isOccupied(dst)
    if (isBlocked) {
      return for {
        path <- Holophote.pfind(b.pos, dst, gr)
      } yield {
        TaskList(drop ++ List(Path(path.drop(1)), Gather, MoveTask(adjPos), Place(dst)))
      }
    }
    if (isHolding) {
      for {
        path <- Holophote.pfind(b.pos, adjPos, gr)
      } yield {
        TaskList(List(Path(path.drop(1)), Place(dst)))
      }

    } else {
      for {
        p <- rm.nearest(dst)
        path1 <- Holophote.pfind(b.pos, p, gr)
        path2 <- Holophote.pfind(p, adjPos, gr)
      } yield {
        TaskList(List(Path(path1.drop(1)), Gather, Path(path2), Place(dst)))
      }
    }

  }

  def check(b:Worker, w:World) = {
    b.pos == adjPos && w.isSolid(dst)
  }

  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = {
    w.isStandable(adjPos) && !w.isSolid(dst)
  }
}

object Stock {
  def create(tok:Int, to:(Int,Int,Int,Int), from:Vox)(parent:PlanId) = {
    Goal.id += 1
    Stock(from, tok, to, parent, Goal.id)
  }
}

case class Stock private (from:Vox, tok:Int, to:(Int,Int,Int,Int), override val parent:PlanId, override val id:Int) extends Goal {
  def reserve = this
  def isReserved = false
  def free = this
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):FailureReason \/ Order = {
    if (b.hasStone) {
      for {
        p <- rm.sparse(tok, to)
        path <- Holophote.pfind(b.pos, p, gr)
      } yield {
        TaskList(List(Path(path), Drop))
      }
    } else {
      for {
        p <- rm.nearest(from)//fixme// if p != pt
        path <- Holophote.pfind(b.pos, p, gr)
      } yield {
        TaskList(List(Path(path), Gather))
      }
    }

  }

  def check(b:Worker, w:World) = {
    true
  }

  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = {
    w.isStandable(from) //&& to.exists{ case p => w.isStandable(p) }
  }
}

sealed trait DigType
case object StairWall extends DigType
case class StairDown(pt:Vox) extends DigType
case object Tunnel extends DigType

object Dig {
  def create(pt:Vox, dt:DigType)(parent:PlanId) = {
    Goal.id += 1
    Dig(pt, dt, parent, Goal.id)
  }
}

case class Dig private (pt:Vox, dt:DigType, override val parent:PlanId, override val id:Int) extends Goal {
  def reserve = this
  def isReserved = false
  def free = this
  def toOrder(b:Worker, rm:ResourceManager, gr:Graph[Vox,Vox]):FailureReason \/ Order = {
    def digAdjacent(p:Vox, task:Task):FailureReason \/ Order = {
      for (adj <- Direction3.Planar.map { d => p --> d}) {
        for {
          path <- Holophote.pfind(b.pos, adj, gr)
        } yield {
          return \/-(TaskList(List(Path(path), task)))
        }
      }
      -\/(FailureReason.NoPath(b.pos, pt))
    }

    dt match {
      case StairDown(t) =>
        for {
          path <- Holophote.pfind(b.pos, pt, gr)
        } yield {
          TaskList(List(Path(path), DigStair(t)))
        }
      case StairWall =>
        digAdjacent(pt, DigStair(pt))

      case Tunnel =>
        digAdjacent(pt, DigTunnel(pt))
    }

  }

  def check(b:Worker, w:World) = {
    dt match {
      case StairDown(t) => w.isStair(t)
      case StairWall => w.isStair(pt)
      case Tunnel => w.isPassable(pt)
    }
  }

  def isPossibleFor(b:Worker, rm:ResourceManager, w:World) = {
    Direction3.Planar.exists{d =>
      val p = pt --> d
      Holophote.pfind(b.pos, p, w.toGraph(new BuilderProxy(List()))/*hack*/).isRight
    }
  }
}



