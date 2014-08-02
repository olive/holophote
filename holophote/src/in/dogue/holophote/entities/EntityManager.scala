package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Recti

object EntityManager {
  def create(rect:Recti) = {
    val pp = PewPew.create((10,10))
    EntityManager(Seq(), rect).addEntity(pp.toEntity)
  }
}

case class EntityManager private (es:Seq[Entity], rect:Recti) {
  def addEntity(e:Entity) = copy(es=es:+e)
  def update = {
    val newEs = es.map{
      _.update
    }.flatten.filter{e =>
      rect.contains(e.pos)
    }
    copy(es = newEs)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< es.map {_.draw _ }
  }
}
