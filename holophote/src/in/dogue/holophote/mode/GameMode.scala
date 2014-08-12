package in.dogue.holophote.mode

import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Rect}
import in.dogue.holophote.input.Controls
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.entities._
import in.dogue.holophote.world.{ResourceManager, WorldViewer, World}
import in.dogue.holophote.blueprints.RectWall

object GameMode {
  def create(cols:Int, rows:Int, r:Random) = {
    var r = new Random(0)
    val world = World.create(cols, rows, 10, r)
    r = new Random(0)
    val jobs = Vector(Builder, Builder, Builder, Gatherer, Supervisor)
    val es = (0 until 5).map { i =>
      val x = r.nextInt(cols)
      val y = r.nextInt(rows)
      Worker.create(cols, rows, (x, y, 4), jobs(i), r)
    }.toList
    val wg = world.toGraph(new BuilderProxy(es))
    val wall = RectWall((10,10,10,10)).generate(4, wg)
    val wall2 = RectWall((11,11,8,8)).generate(5, wg)
    val wall3 = RectWall((12,12,6,6)).generate(6, wg)
    val wall4 = RectWall((13,13,4,4)).generate(7, wg)
    val wall5 = RectWall((14,14,2,2)).generate(8, wg)
    val gp = GoalPool(wall *+* wall2 *+* wall3 *+* wall4 *+* wall5)
    val em = new EntityManager()
    GameMode(cols, rows, world, es, gp, em, WorldViewer.create(cols, rows, world, es), 0)
  }
}

case class GameMode private (cols:Int, rows:Int, world:World, es:List[Worker], gp:GoalPool, em:EntityManager, v:WorldViewer, t:Int) {
  private def updateWorld = {
    if (t % 1 == 0) {
      val (bss, gpp, ww) = em.coordinateTasks(es, gp, world)
      val (bsss, gppp) = bss.fold2(gpp, em.manageGoal(new ResourceManager(ww), ww))
      val (updated, newPool) = bsss.foldLeft((List[Worker](), gppp)) { case ((ls, pool), b) =>
        val (nb, np) = b.update(new BuilderProxy(bsss)/*fixme, use weird fold*/, ww, pool)
        (nb :: ls) @@ np
      }
      copy(es = updated, world = ww, gp = newPool, t=t+1)
    } else {
      copy(t=t+1)
    }
  }

  def update = {
    val up = updateWorld
    up.copy(v=v.update(up.world, up.es)).toMode
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< v.draw
  }
  def toMode:Mode = Mode[GameMode](_.update, _.draw, this)

}
