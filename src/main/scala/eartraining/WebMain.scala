package eartraining

import com.thoughtworks.binding.dom
import eartraining.state.{AudioEngineInitialized, Root}
import eartraining.ui.UI
import org.scalajs.dom.document
import org.scalajs.dom.raw.AudioContext

import scala.util.{Failure, Success}

object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val flow = new Root
    dom.render(document.body, UI(flow))
    AudioEngine.createWithAudioContext(new AudioContext())
      .onComplete {
        case Success(audioEngine) =>
          flow.handleAction(AudioEngineInitialized(audioEngine))
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
  }
}
