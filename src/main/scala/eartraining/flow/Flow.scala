package eartraining.flow

import com.thoughtworks.binding.Binding.Var
import eartraining._

trait FlowStatusSelector
case object InitSelector extends FlowStatusSelector
case object QuerySelector extends FlowStatusSelector
case object TriadCoreGenerating extends FlowStatusSelector

trait FlowStatus

object Flow {

  val status = Var[FlowStatus](Init)

  var audioEngineOption: Option[AudioEngine] = None

  def goToStatus(nextStatus: FlowStatusSelector): Unit = {
    nextStatus match {
      case InitSelector =>
        status := Init
      case QuerySelector =>
        status := Query(audioEngineOption.get)
    }
  }

}
