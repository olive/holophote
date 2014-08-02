package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua
import Antiqua._

object EntityManager {
  def create(rect:Recti) = {
    val pp = PewPew.create((16,16))
    EntityManager(Seq(), rect).addEntity(pp.toEntity)
  }
}

case class EntityManager private (es:Seq[Entity], rect:Recti) {
  def addEntity(e:Entity) = copy(es=es:+e)
  def addEntities(ess:Seq[Entity]) = copy(es = es ++ ess)
  def update = {
    val newEs = es.map{
      _.update
    }.flatten.filter{e =>
      rect.contains(e.pos)
    }
    copy(es = newEs)
  }


  def process(p:Player) = {
    val f = if (es.exists{e => e.pos == p.pos}) {
      (p:Player) => p.kill
    } else {
      id[Player] _
    }

    f(p)

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< es.map {_.draw _ }
  }
}
