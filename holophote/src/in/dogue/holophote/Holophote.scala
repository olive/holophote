package in.dogue.holophote

import in.dogue.antiqua.data.Incrementable
import in.dogue.holophote.entities.Builder

object Holophote {
  implicit def builder2Incrementable(b:Builder) = new Incrementable[Builder] {
    def incr = b.copy(t=b.t+1)
  }


}
