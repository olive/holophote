package in.dogue.holophote

import in.dogue.antiqua.graphics.{TileRenderer, Tileset, Renderer}
import com.deweyvm.gleany.{AssetLoader, Glean}
import in.dogue.holophote.world.World

class Engine {
  val cols = 32
  val rows = 32
  val ts = new Tileset(16, 16, 16, 16, AssetLoader.loadTexture("16x16"))
  val r = new Renderer(512, 512, 1, ts)
  val tr = TileRenderer.create(cols, rows)
  var world = World.create(cols, rows)
  def update() {
    world = world.update
  }
  def draw() {
    r.render(tr <+< world.draw)
  }
}
