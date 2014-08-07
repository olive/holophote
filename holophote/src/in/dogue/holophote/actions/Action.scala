package in.dogue.holophote.actions

import in.dogue.antiqua.Antiqua.Cell

sealed trait Action
case class Move(c:Cell) extends Action
case class Build(c:Cell) extends Action
case object Exist extends Action
