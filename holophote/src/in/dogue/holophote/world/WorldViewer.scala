package in.dogue.holophote.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.holophote.input.Controls
import in.dogue.antiqua.Antiqua
import Antiqua._

object WorldViewer {
  def create(w:World) = WorldViewer(w, 0)
}

case class WorldViewer private (w:World, z:Int) {
  def update(w:World) = {
    val newZ = (z + Controls.Zoom.justPressed).clamp(0, w.layers - 1)
    copy(w=w, z=newZ)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< w.tiles.vs(z).flatten.map { case (p, t) => t.draw(p) _ }
  }
}
