package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua._

class BuilderProxy(s:Seq[Worker]) {
  def isOccupied(c:Vox) = s.exists(b => b.pos == c)
  def getOccupant(c:Vox) = s.find(b => b.pos == c)
}
