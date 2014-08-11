package in.dogue.holophote

import in.dogue.antiqua.data.{Graph, Incrementable}
import in.dogue.holophote.entities.Worker
import in.dogue.antiqua.Antiqua.Vox
import in.dogue.antiqua.ai.AStar

object Holophote {

  def pfind(start:Vox, end:Vox, gr:Graph[Vox, Vox]) = {
    AStar.pfindVox(start, end, gr)
  }

}
