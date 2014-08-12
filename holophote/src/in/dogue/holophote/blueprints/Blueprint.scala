package in.dogue.holophote.blueprints

import in.dogue.antiqua.data.Graph
import in.dogue.antiqua.Antiqua._
import in.dogue.holophote.entities._

object Blueprint {
  def mkMap = Map[Job, List[Goal]]().withDefaultValue(List())
}

trait Blueprint {
  def generate(g:Graph[Vox,Vox]):(Map[Job, List[Goal]],Map[Job, List[Goal]])
}


