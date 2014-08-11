package in.dogue.holophote.mode

import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Rect}
import in.dogue.holophote.input.Controls
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.entities._
import in.dogue.holophote.world.{WorldViewer, World}
import in.dogue.holophote.blueprints.RectWall

object GameMode {
  def create(cols:Int, rows:Int, r:Random) = {
    var r = new Random(0)
    val world = World.create(cols, rows, 6, r)
    r = new Random(0)
    val es = (0 until 2).map { i =>
      val x = r.nextInt(cols)
      val y = r.nextInt(rows)
      Worker.create(cols, rows, (x, y, 4), r)
    }.toList
    val wall = RectWall((10,10,50,10))
    val wall2 = RectWall((11,11,48,8))
    val wg = world.toGraph(new BuilderProxy(es))
    val gp = GoalPool(wall.generate(4, wg) ++ wall2.generate(5, wg))
    val em = new EntityManager()
    GameMode(cols, rows, world, es, gp, em, WorldViewer.create(cols, rows, world, es), 0)
  }
}

case class GameMode private (cols:Int, rows:Int, world:World, es:List[Worker], gp:GoalPool, em:EntityManager, v:WorldViewer, t:Int) {
  private def updateWorld = {
    if (t % 1 == 0) {
      val (bss, gpp, ww) = em.coordinateTasks(es, gp, world)
      val (bsss, gppp) = bss.fold2(gpp, em.manageGoal)
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
