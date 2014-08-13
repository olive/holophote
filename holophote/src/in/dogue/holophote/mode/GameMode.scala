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
import in.dogue.holophote.blueprints.{Mine, RectWall}
import in.dogue.antiqua.algebra.Monoid
import in.dogue.holophote.Schema
import scalaz.IList
import com.badlogic.gdx.Gdx

object GameMode {
  def create(cols:Int, rows:Int, worldSize:Vox, r:Random) = {
    val worldCols = worldSize.x
    val worldRows = worldSize.y
    val worldLayers = worldSize.z
    var r = new Random(0)
    val world = World.create(worldCols, worldRows, worldLayers, r)
    r = new Random(0)
    val jobs = Vector(Builder, Gatherer, Miner)
    val es = jobs.map { job =>
      val x = r.nextInt(worldCols)
      val y = r.nextInt(worldRows)
      Worker.create((x, y, 4), job, r)
    }.toList
    val wg = world.toGraph(new BuilderProxy(es))
    val gatherPt = (50,25,4)
    val hole = Mine(3, (10,10,11,11)).generate(wg)
    val wall = RectWall(gatherPt,4,(10,10,11,11)).generate(wg)
    val wall2 = RectWall(gatherPt,5,(11,11,9,9)).generate(wg)
    val wall3 = RectWall(gatherPt,6,(12,12,7,7)).generate(wg)
    val wall4 = RectWall(gatherPt,7,(13,13,5,5)).generate(wg)
    val wall5 = RectWall(gatherPt,8,(14,14,3,3)).generate(wg)
    val wall6 = RectWall(gatherPt,9,(14,14,1,1)).generate(wg)
    import Monoid._
    val (major, minor) = hole <+> wall <+> wall2 <+> wall3 <+> wall4 <+> wall5 <+> wall6
    val gp = Schema.create.insertPlan(major,minor)
    val em = new EntityManager()
    GameMode(cols, rows, world, es, gp, em, WorldViewer.create(cols, rows, world, es), 0)
  }
}

case class GameMode private (cols:Int, rows:Int, world:World, es:List[Worker], sc:Schema, em:EntityManager, v:WorldViewer, t:Int) {
  private def updateWorld = {
    if (t % 1 == 0) {
      val (bss, scc, ww) = em.coordinateTasks(es, sc, world)
      val (bsss, sccc) = bss.fold2(scc, em.manageGoal(new ResourceManager(ww), ww))
      val (updated, newSchema) = bsss.foldLeft((List[Worker](), sccc)) { case ((ls, pool), b) =>
        val (nb, np) = b.update(new BuilderProxy(bsss)/*fixme, use weird fold*/, ww, pool)
        (nb :: ls) @@ np
      }
      copy(es = updated, world = ww, t=t+1, sc=newSchema.update)
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
