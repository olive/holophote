package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell

sealed trait Task
case class Move(path:List[Cell]) extends Task
case object Wait extends Task
