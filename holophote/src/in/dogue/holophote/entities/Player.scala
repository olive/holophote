package in.dogue.holophote.entities

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.holophote.input.Controls

object Player {
  def create(cols:Int, rows:Int, ij:Cell) = {
    val tile = CP437.Φ.mkTile(Color.Black, Color.White)
    Player(cols, rows, ij, tile, Alive)
  }
}
sealed trait PlayerState
case object Alive extends PlayerState
case object Dead extends PlayerState
case class Player private (cols:Int, rows:Int, ij:Cell, t:Tile, state:PlayerState) {
  def pos = ij
  def updateAlive = {
    val dx = Controls.AxisX.zip(3, 3)
    val dy = Controls.AxisY.zip(3, 3)
    val p = ij |+| ((dx, dy))
    val ndx = (p.x < 0 || p.x > cols - 1).select(dx, 0)
    val ndy = (p.y < 0 || p.y > rows - 1).select(dy, 0)
    copy(ij= ij |+| ((ndx, ndy)))
  }
  def updateDead = this
  def update = {
    state match {
      case Alive => updateAlive
      case Dead => updateDead
    }
  }

  def kill = copy(state=Dead)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <| (ij, t)
  }
}
