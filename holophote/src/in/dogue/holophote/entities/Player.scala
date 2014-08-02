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
    Player(cols, rows, ij, tile, Alive, 0)
  }
}
sealed trait PlayerState
case object Alive extends PlayerState
case object Dead extends PlayerState
case class Player private (cols:Int, rows:Int, ij:Cell, tile:Tile, state:PlayerState, t:Int) {
  def pos = ij
  private def updateAlive = {
    val dx = Controls.AxisX.zip(3, 3)
    val dy = Controls.AxisY.zip(3, 3)
    val p = ij |+| ((dx, dy))
    val ndx = (p.x < 0 || p.x > cols - 1).select(dx, 0)
    val ndy = (p.y < 0 || p.y > rows - 1).select(dy, 0)
    val proj = getProjectiles
    copy(ij= ij |+| ((ndx, ndy)), t=t+1) @@ proj
  }

  def getProjectiles = {
    val tile = CP437.║.mkTile(Color.Black, Color.Yellow)
    val path = Path.create(15) { t =>
      0
    } { t =>
      -t*3
    }
    def mk(pos:Cell) = Projectile.create(pos, path, tile)
    val ps = if (Controls.Space.isPressed) {
      if (t % 12 == 3) {
        val left = ij |- 1
        val right = ij |+ 1

        Seq(mk(left), mk(right))
      } else if (t % 12 == 9) {
        Seq(mk(ij))
      } else {
        Seq()
      }
    } else {
      Seq()
    }
    ps.map{_.toEntity}
  }

  private def updateDead = this
  def update = {
    state match {
      case Alive => updateAlive
      case Dead => updateDead @@ Seq()
    }
  }

  def kill = copy(state=Dead)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <| (ij, tile)
  }
}
