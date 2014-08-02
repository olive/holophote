package in.dogue.holophote.world

import in.dogue.holophote.entities.{Player, EntityManager}
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Recti
import in.dogue.holophote.environment.Field
import java.util.Random
import in.dogue.holophote.mode.Mode

object World {
  def create(cols:Int, rows:Int, r:Random) = {
    val buff = 10
    val rect = Recti(-buff, -buff, cols + 2*buff, rows + 2*buff)
    val p = Player.create(cols, rows, (16,24))
    World(Field.create(cols, rows, r), p, EntityManager.create(rect))
  }
}

case class World private (f:Field, p:Player, em:EntityManager) {
  def update = {
    val newEm = em.update
    val (upP, pProj) = p.update
    val newP = newEm.process(upP)
    copy(em=em.update.addEntities(pProj), f=f.update, p = newP).toMode
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< f.draw <+< p.draw <+< em.draw
  }

  def toMode:Mode = Mode[World](_.update, _.draw, this)
}
