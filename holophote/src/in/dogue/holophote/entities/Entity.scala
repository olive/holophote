package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.graphics.TileRenderer


object Entity {
  def apply[A](self_ :A,
               getPos_ :A => Cell,
               up_ : A => Seq[Entity],
               dr_ :A => TileRenderer => TileRenderer,
               toEntity_ : A => Entity) = new Entity {
    type T = A
    val self = self_
    val getPos = getPos_
    val up = up_
    val dr = dr_
    val toEntity = toEntity_
  }
}

trait Entity {
  type T
  protected val self:T
  protected val getPos: T => Cell
  protected val up:T => Seq[Entity]
  protected val dr:T => TileRenderer => TileRenderer
  protected val toEntity:T => Entity
  def pos = getPos(self)
  def update:Seq[Entity] = up(self)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)
  }
}
