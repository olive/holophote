package in.dogue.holophote.environment

import in.dogue.antiqua.Antiqua.{Cell, TileGroup}
import in.dogue.antiqua.graphics.TileRenderer
import java.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._

object Line {
  def create(cols:Int, r:Random) = {
    def mkTile(r:Random) = {
      val bg = Color.Purple.dim(6 + r.nextDouble)
      val fg = Color.Purple.dim(3 + r.nextDouble)
      val code = Vector(CP437.`'`, CP437.grave, CP437.`,`, CP437.`.`).randomR(r)
      code.mkTile(bg, fg)
    }
    val tg = (0 until cols).map {i => ((i, 0), mkTile(r))}
    Line(tg)
  }
}
case class Line private (tg:TileGroup) {
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++ (tg |++| ij)
  }
}

object Field {
  def create(cols:Int, rows:Int, r:Random) = {
    def mkLine = () => Line.create(cols, r)
    val lines = (0 until rows).map { i => mkLine()}.toVector
    Field(lines)
  }
}

case class Field(lines:Vector[Line]) {
  def update = {
    val last = lines(lines.length - 1)
    val newLines = last +: lines.dropRight(1)
    copy(lines=newLines)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< lines.zipWithIndex.map{ case (l, j) => l.draw((0, j)) _}
  }
}
