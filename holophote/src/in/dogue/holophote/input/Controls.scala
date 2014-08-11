package in.dogue.holophote.input

import com.deweyvm.gleany.input.triggers.{JoypadTrigger, TriggerAggregate, KeyboardTrigger}
import com.deweyvm.gleany.input.{WheelControl, AxisControl, Control, JoypadButton}
import com.badlogic.gdx.Input
import scala.collection.mutable.ArrayBuffer

object Controls {
  val All = ArrayBuffer[Control[_]]()
  val Left = makePr(Input.Keys.LEFT, JoypadButton("DPadLeft"))
  val Right = makePr(Input.Keys.RIGHT, JoypadButton("DPadRight"))
  val Up = makePr(Input.Keys.UP, JoypadButton("DPadUp"))
  val Down = makePr(Input.Keys.DOWN, JoypadButton("DPadDown"))
  val Space = makePr(Input.Keys.SPACE, JoypadButton("2"))
  val Action = makePr(Input.Keys.C, JoypadButton("3"))
  val Escape = makeKb(Input.Keys.ESCAPE)
  val Zoom = makeZoom()
  val AxisX = new AxisControl(Left, Right)
  val AxisY = new AxisControl(Up, Down)

  def makeKb(key:Int) = {
    val result = new TriggerAggregate(Seq(new KeyboardTrigger(key)))
    All += result
    result
  }

  def makePr(key:Int, ctrl:JoypadButton): TriggerAggregate = {
    val result = new TriggerAggregate(Seq(new KeyboardTrigger(key), new JoypadTrigger(ctrl)))
    All += result
    result
  }

  def makeZoom() = {
    val result = new WheelControl
    All += result
    result
  }


  def update() {
    All foreach (_.update())
  }
}
