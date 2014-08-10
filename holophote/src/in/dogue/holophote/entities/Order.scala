package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua._

sealed trait Order {
  def next:(Order, Option[Task])
  def isNone:Boolean = this == NoOrder
  def none:Order = NoOrder
}

case class TaskList(s:List[Task]) extends Order {
  def next = s match {
    case x::xs =>
      println("next " + x + " " + s)
      TaskList(xs) @@ x.some
    case _ => none @@ None
  }
}

case object NoOrder extends Order {
  def next = NoOrder @@ None
}



