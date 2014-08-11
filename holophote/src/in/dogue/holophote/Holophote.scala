package in.dogue.holophote

import in.dogue.antiqua.data.{Graph, Incrementable}
import in.dogue.holophote.entities.Worker
import in.dogue.antiqua.Antiqua.Vox
import in.dogue.antiqua.ai.AStar

object Holophote {
  implicit def builder2Incrementable(b:Worker) = new Incrementable[Worker] {
    def incr = b.copy(t=b.t+1)
  }

  def pfind(start:Vox, end:Vox, gr:Graph[Vox, Vox]) = {
    AStar.pfindVox(start, end, gr)
  }

}
