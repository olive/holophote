package in.dogue.holophote

import in.dogue.antiqua.graphics.{TileRenderer, Tileset, Renderer}
import com.deweyvm.gleany.{AssetLoader, Glean}
import java.util.Random
import in.dogue.holophote.input.Controls
import com.badlogic.gdx.Gdx
import in.dogue.holophote.mode.{GameMode, TitleMode}
import in.dogue.antiqua.geometry.StarPolygon

class Engine {
  val cols = Game.Cols
  val rows = Game.Rows
  val ts = new Tileset(16, 16, Game.TileSize, Game.TileSize, AssetLoader.loadTexture("16x16"))
  val r = new Renderer(cols*Game.TileSize, rows*Game.TileSize, 1, ts)
  val tr = TileRenderer.create(cols, rows)
  val rng = new Random(0)
  var mode = GameMode.create(cols, rows, rng).toMode//TitleMode.create(cols, rows, rng).toMode
  def update() {
    if (Controls.Escape.justPressed) {
      Gdx.app.exit()
    }
    mode = mode.update
  }
  def draw() {
    r.render(tr <+< mode.draw)
  }
}
