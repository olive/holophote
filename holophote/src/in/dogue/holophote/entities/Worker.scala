package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import scala.util.Random
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.resources.{Stone, Resource}
import in.dogue.holophote.world.{ResourceManager, World}
import scalaz.{-\/, \/-}
import in.dogue.holophote.{Schema, Holophote}

object Worker {
  var count = 0
  def create(pos:Vox, job:Job, r:Random) = {
    val code = job match {
      case Builder => CP437.B
      case Supervisor => CP437.S
      case Gatherer => CP437.G
      case Miner => CP437.M
    }
    val tile = code.mkTile(Color.Black, Color.White)
    count += 1
    Worker(pos, tile,  r, 0, NoTask, NoOrder, NoGoal, Stone.some, job, count, None)
  }

  def performTask(builder:Worker, sc:Schema, world:World) = {
    val (b, w) = builder.task.perform(builder, world, sc)
    if (b.task.isNone && !b.goal.isNone && !b.order.isNone) {
      val (no, ntOpt) = b.order.next
      ntOpt match {
        case Some(t) =>
          b.setOrder(t, no) @@ sc @@ w
        case None =>
          if (!b.goal.check(b, w)) {
            throw new RuntimeException("Postcondition was not met\n%s\n%s".format(b.pos, b.goal))
          }
          finishGoal(b, sc) @@ w
      }
    } else {
      b @@ sc @@ w
    }

  }
  def finishGoal(b:Worker, sc:Schema):(Worker, Schema) = {
    b.removeGoal(FailureReason.AlreadyComplete) @@ sc.finish(b.job, b.goal)
  }

}

case class Worker(pos:Vox, tile:Tile, r:Random, t:Int, task:Task, order:Order, goal:Goal, inv:Option[Resource], job:Job, id:Int, lastFailed:Option[FailureReason]) {
  println(this)
  def noOrder = order.isNone
  def noTask = task.isNone
  def noGoal = goal.isNone
  def invFree = inv.isEmpty
  def give(r:Resource) = copy(inv = r.some)
  def hasStone = inv == Some(Stone)
  def spendStone = copy(inv=None)
  private def updateOrder(p:BuilderProxy, w:World, pool:Schema) = {
    if (goal.isNone) {
      this @@ pool
    } else {
      goal.toOrder(this, new ResourceManager(w), w.toGraph(p)) match {
        case \/-(ord) =>
          copy(order = ord) @@ pool
        case -\/(f) =>
          removeGoal(f) @@ pool.surrender(job, goal)
      }
    }
  }

  private def getColor(t:Task) = {
    t match {
      case _:Path => Color.Yellow
      case _:Place | _:BuildStair => Color.Red
      case _:Gather.type => Color.Blue
      case _:Drop.type | _:DigStair => Color.Grey
      case _:MoveTask => Color.Orange
      case tsk if tsk.isNone => Color.White
      case _ => Color.Black
    }
  }

  def update(p:BuilderProxy, w:World, pool:Schema): (Worker, Schema) = {
    if (noOrder) {
      updateOrder(p, w, pool)
    } else {
      this @@ pool
    }
  }

  def removeGoal(reason:FailureReason) = {
    copy(task=task.none, order=order.none, goal=goal.none, lastFailed=reason.some)
  }

  def setOrder(t:Task, o:Order):Worker = {
    copy(task=t, order=o)
  }

  def move(c:Vox) = copy(pos=c)

  def setTask(t:Task):Worker = copy(task=t)

  def giveGoal(g:Goal) = {
    copy(goal=g, lastFailed=None)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (pos.xy, tile.setFg(getColor(task)))
  }

  override def toString:String = {
    val fail = lastFailed.fold(""){l =>  "\n    Failed because:%s".format(l)}
    "%s:#%d@%s\n    Goal:%s\n    Ords:%s\n    Task:%s%s".format(job, id, pos, goal, order, task, fail)
  }
}
