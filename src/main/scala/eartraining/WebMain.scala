package eartraining

import com.thoughtworks.binding.dom
import eartraining.flow.{Flow}
import eartraining.ui.UI
import org.scalajs.dom.document
import org.scalajs.dom.raw.AudioContext

import scala.util.{Failure, Success}

object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val flow = new Flow
    dom.render(document.body, UI(flow))
    val context: AudioContext = new AudioContext()
    AudioEngine.create(context)
      .onComplete {
        case Success(_audioEngine) =>
          flow.initWithAudioEngine(_audioEngine)
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
  }
}
