package in.dogue.holophote.mode

import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Rect}
import in.dogue.holophote.input.Controls
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.entities._
import in.dogue.holophote.world.World

object GameMode {
  def create(cols:Int, rows:Int, r:Random) = {
    val r = new Random(0)
    val world = World.create(cols, rows, r)
    val es = (0 until 10).map { i =>
      val x = r.nextInt(cols)
      val y = r.nextInt(rows)
      Builder.create(cols, rows, (x, y), r)
    }.toList
    val gp = GoalPool(List(Build((0,0), false)))
    val em = new EntityManager()
    GameMode(cols, rows, world, es, gp, em)
  }
}

case class GameMode private (cols:Int, rows:Int, world:World, es:List[Builder], gp:GoalPool, em:EntityManager) {
  def update = {
    val (bss, gpp, ww) = es.fold3(gp, world, em.manageTask)
    val (bsss, gppp) = bss.fold2(gpp, em.manageGoal)
    copy(es=bsss.map{_.update(world)}, world=ww, gp=gppp).toMode
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< world.draw <++< es.map { _.draw _ }
  }
  def toMode:Mode = Mode[GameMode](_.update, _.draw, this)

}
