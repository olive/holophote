package in.dogue.holophote

import in.dogue.antiqua.graphics.{TileRenderer, Tileset, Renderer}
import com.deweyvm.gleany.{AssetLoader, Glean}
import in.dogue.holophote.world.World
import java.util.Random
import in.dogue.holophote.input.Controls
import com.badlogic.gdx.Gdx

class Engine {
  val cols = 32
  val rows = 32
  val ts = new Tileset(16, 16, 16, 16, AssetLoader.loadTexture("16x16"))
  val r = new Renderer(512, 512, 1, ts)
  val tr = TileRenderer.create(cols, rows)
  val rng = new Random()
  var mode = World.create(cols, rows, rng).toMode
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
