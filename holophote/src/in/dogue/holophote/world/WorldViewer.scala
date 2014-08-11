package in.dogue.holophote.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.holophote.input.Controls
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Array2dView

object WorldViewer {
  def create(viewCols:Int, viewRows:Int, w:World) = WorldViewer(viewCols, viewRows, w, 0, (0,0))
}

case class WorldViewer private (cols:Int, rows:Int, w:World, z:Int, off:(Int,Int)) {
  def update(w:World) = {
    val newZ = (z + Controls.Zoom.justPressed).clamp(0, w.layers - 1)
    copy(w=w, z=newZ)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    val layer = Array2dView.cut(w.tiles.getLayer(z), 0, 0, cols, rows)
    layer.foldLeft(tr) { case (r, (p, t)) =>
      r <+< t.draw(p)
    }
  }
}
