package in.dogue.holophote.blueprints

import in.dogue.antiqua.data.Graph
import in.dogue.antiqua.Antiqua._
import in.dogue.holophote.entities.Goal

trait Blueprint {
  def generate(g:Graph[Cell,Cell]):Seq[Goal]
}
