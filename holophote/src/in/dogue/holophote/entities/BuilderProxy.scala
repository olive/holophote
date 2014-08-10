package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Cell

class BuilderProxy(s:Seq[Builder]) {
  def isOccupied(c:Cell) = s.exists(b => b.pos == c)
}
