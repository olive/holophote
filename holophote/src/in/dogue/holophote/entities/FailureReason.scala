package in.dogue.holophote.entities

import in.dogue.antiqua.Antiqua.Vox


object FailureReason {
  case class Jam(w:Worker) extends FailureReason
  case class NoPath(src:Vox, dst:Vox) extends FailureReason
  case class DestinationUnsuitable(pos:Vox) extends FailureReason
  case object AlreadyComplete extends FailureReason
  case object NoResource extends FailureReason
  case class Other(s:String) extends FailureReason


}
sealed trait FailureReason {

}
