package net.zoltantamasi.eartraining

import com.thoughtworks.binding.dom
import net.zoltantamasi.eartraining.state.{AudioEngineInitialized, Root}
import net.zoltantamasi.eartraining.ui._
import org.scalajs.dom.document
import org.scalajs.dom.raw.AudioContext

import scala.util.{Failure, Success}

object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val root = new Root
    dom.render(document.getElementById("main"), UI.toUI(root))
    AudioEngine.createWithAudioContext(new AudioContext())
      .onComplete {
        case Success(audioEngine) =>
          root.handleAction(AudioEngineInitialized(audioEngine))
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
  }
}
