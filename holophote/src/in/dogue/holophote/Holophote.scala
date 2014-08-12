package in.dogue.holophote

import in.dogue.antiqua.data.{Graph, Incrementable}
import in.dogue.holophote.entities.{FailureReason, Worker}
import in.dogue.antiqua.Antiqua.Vox
import in.dogue.antiqua.ai.AStar
import scalaz.{\/, -\/, \/-}

object Holophote {

  def pfind(start:Vox, end:Vox, gr:Graph[Vox, Vox]): FailureReason \/ List[Vox] = {
    AStar.pfindVox(start, end, gr) match {
      case Some(p) => \/-(p)
      case None => -\/(FailureReason.NoPath(start, end))
    }
  }

}
