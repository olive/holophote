package in.dogue.holophote.mode

import in.dogue.antiqua.graphics.TileRenderer


object Mode {
  def apply[A](up_ : A => Mode,
               dr_ :A => (TileRenderer) => TileRenderer,
               self_ :A) = new Mode {
    override val self = self_
    override val up = up_
    override val dr = dr_
    override type T = A

  }
}

trait Mode {
  type T
  val up: T => Mode
  val dr:T => (TileRenderer) => TileRenderer
  val self:T
  def update = up(self)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)
  }
}
