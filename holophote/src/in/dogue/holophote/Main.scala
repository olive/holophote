package in.dogue.holophote
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.{GleanyConfig, GleanyInitializer, GleanyGame}
import com.deweyvm.gleany.files.PathResolver
import com.deweyvm.gleany.data.Point2i
import com.deweyvm.gleany.saving.{Settings, SettingDefaults}
import com.deweyvm.gleany.logging.Logger
import in.dogue.holophote.input.HolophoteControls

object Main {
  def main(args: Array[String]) {

    val iconPaths = Seq(
      "sprites/icon16.png",
      "sprites/icon24.png",
      "sprites/icon32.png"
    )
    val settings = new Settings(HolophoteControls, new SettingDefaults() {
      val SfxVolume: Float = 0.2f
      val MusicVolume: Float = 0.2f
      val WindowSize: Point2i = Point2i(Game.Cols*Game.TileSize,Game.Rows*Game.TileSize)
      val DisplayMode: Int = 0
    }, false)
    val config = new GleanyConfig(settings, "Holophote", iconPaths)
    val pathResolver = new PathResolver(
      "fonts",
      "sprites",
      "sfx",
      "music",
      "data",
      "shaders",
      "maps"
    )
    Logger.attachCrasher(".")
    val initializer = new GleanyInitializer(pathResolver, settings)
    GleanyGame.runGame(config, new Game(initializer))

  }
}
