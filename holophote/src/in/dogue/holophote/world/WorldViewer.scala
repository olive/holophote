package in.dogue.holophote.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.holophote.input.Controls
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Array2dView
import in.dogue.holophote.entities.Worker

object WorldViewer {
  def create(viewCols:Int, viewRows:Int, w:World, ws:Seq[Worker]) = {
    WorldViewer(viewCols, viewRows, w, ws, 4, (0,0))
  }
}

case class WorldViewer private (cols:Int, rows:Int, w:World, ws:Seq[Worker], z:Int, off:(Int,Int)) {
  def update(w:World, ws:Seq[Worker]) = {
    val newZ = (z + Controls.Zoom.justPressed).clamp(0, w.layers - 1)
    copy(w=w, ws=ws, z=newZ)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    val r = (0,0,cols,rows)
    val layer = Array2dView.cut(w.tiles.getLayer(z), r._1, r._2, r._3, r._4)
    val layerBelow = (z-1 < 0).select(Array2dView.cut(w.tiles.getLayer(z-1), 0, 0, cols, rows).some, None)
    layer.foldLeft(tr) { case (r, (p, t)) =>
      r <+< t.draw(p, layerBelow.map{_.get(p)})
    } <++< ws.filter { wk => wk.pos.xy.inRange(r) && wk.pos.z == z}.map{_.draw _}
  }
}
