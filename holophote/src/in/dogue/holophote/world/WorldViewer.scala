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

case class WorldViewer private (sCols:Int, sRows:Int, w:World, ws:Seq[Worker], z:Int, off:(Int,Int)) {
  def update(w:World, ws:Seq[Worker]) = {
    val newZ = (z + Controls.Zoom.justPressed).clamp(0, w.layers - 1)
    def clamp(p:Cell, min:Int, xMax:Int, yMax:Int) = (p.x.clamp(min, xMax), p.y.clamp(min, yMax))
    val rawOff = off |+| (Controls.AxisX.zip(3,3), Controls.AxisY.zip(3,3))
    val newOff = clamp(rawOff, 0, w.cols - sCols, w.rows-sRows)
    copy(w=w, ws=ws, z=newZ, off=newOff)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    val r = (off.x,off.y,sCols,sRows)
    val layer = Array2dView.cut(w.tiles.getLayer(z), r._1, r._2, r._3, r._4)
    val layerBelow = (z-1 < 0).select(Array2dView.cut(w.tiles.getLayer(z-1), r._1, r._2, r._3, r._4).some, None)
    val tilesDrawn = layer.foldLeft(tr) { case (ren, (p, t)) =>
      ren <+< t.draw(p, layerBelow.map{_.get(p)})
    }
    tilesDrawn.withMove(-off.x, -off.y) { ren =>
      ren <++< ws.filter { wk => wk.pos.xy.inRange(r) && wk.pos.z == z }.map { _.draw _ }
    }
  }
}
