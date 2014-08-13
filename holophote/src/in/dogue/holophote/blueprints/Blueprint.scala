package in.dogue.holophote.blueprints

import in.dogue.antiqua.data.Graph
import in.dogue.antiqua.Antiqua._
import in.dogue.holophote.entities._

trait Blueprint {
  def generate(g:Graph[Vox,Vox]):(Seq[(Job, PlanId=>Goal)], Seq[(Job, PlanId=>Goal)])
}


