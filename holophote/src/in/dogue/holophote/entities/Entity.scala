package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.TileRenderer

object Entity {
  def apply[A](up_ : A => A,
               dr_ : A => TileRenderer => TileRenderer,
               self_ : A) = new Entity {
    override type T = A
    override val up = up_
    override val dr = dr_
    override val self = self_
  }
}

trait Entity {
  type T
  val up: T => T
  val dr: T => TileRenderer => TileRenderer
  val self: T



  def update = Entity(up, dr, up(self))
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)
  }
}
