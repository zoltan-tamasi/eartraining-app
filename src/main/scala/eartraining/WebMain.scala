package eartraining

import com.thoughtworks.binding.dom
import eartraining.flow.{Flow, Query, QuerySelector}
import org.scalajs.dom.document
import org.scalajs.dom.raw.AudioContext

import scala.util.{Failure, Success}

object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

  def main(args: Array[String]): Unit = {
    dom.render(document.body, UI.UI(Flow))
    val context: AudioContext = new AudioContext()
    AudioEngine.create(context)
      .onComplete {
        case Success(_audioEngine) =>
          Flow.audioEngineOption = Some(_audioEngine)
          Flow.goToStatus(QuerySelector)
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
  }
}
