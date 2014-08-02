package in.dogue.holophote.world

import in.dogue.holophote.entities.EntityManager
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Recti

object World {
  def create(cols:Int, rows:Int) = {
    val buff = 10
    val rect = Recti(-buff, -buff, cols + 2*buff, rows + 2*buff)
    World(EntityManager.create(rect))
  }
}

case class World private (em:EntityManager) {
  def update = {
    copy(em=em.update)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< em.draw
  }
}
