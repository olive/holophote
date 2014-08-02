package in.dogue.holophote

import com.deweyvm.gleany.{Glean, GleanyInitializer, GleanyGame}
import java.util.concurrent.{Callable, Executors, TimeUnit}
import in.dogue.holophote.input.Controls

object Game {
  var t = 0
}

class Game(initializer: GleanyInitializer) extends GleanyGame(initializer) {
  private lazy val engine = new Engine()
  override def update() {
    Controls.update()
    engine.update()
    Game.t += 1
  }

  override def draw() {
    engine.draw()
  }

  override def resize(width: Int, height: Int) {
    Glean.y.settings.setWindowSize(width, height)
  }

  override def dispose() {
    val executor = Executors.newSingleThreadExecutor()
    executor.invokeAll(java.util.Arrays.asList(new Callable[Unit] {
      override def call(): Unit = ()
    }), 2, TimeUnit.SECONDS)
    executor.shutdown()
  }
}