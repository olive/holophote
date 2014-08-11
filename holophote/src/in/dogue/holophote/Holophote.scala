package in.dogue.holophote

import in.dogue.antiqua.data.Incrementable
import in.dogue.holophote.entities.Worker

object Holophote {
  implicit def builder2Incrementable(b:Worker) = new Incrementable[Worker] {
    def incr = b.copy(t=b.t+1)
  }


}
