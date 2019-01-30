package net.zoltantamasi.eartraining

import com.thoughtworks.binding.dom
import net.zoltantamasi.eartraining.state._
import net.zoltantamasi.eartraining.ui._
import org.scalajs.dom.{Event, document}
import org.scalajs.dom.raw.AudioContext

import scala.util.{Failure, Success}

object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

  implicit val converter: (AudioEvent) => RootAction = {
    case AudioFinished => RootAudioFinished
  }

  def main(args: Array[String]): Unit = {

    val root = Root.getInitial()

    val audioContext = new AudioContext()

    dom.render(document.getElementById("main"), RootUI.toUI(root, (rootAction: RootAction) => {
      Root.handleAction(root, rootAction) match {
        case NoEffect =>
        case PlayChordEffect(chord) =>
          root.audioEngine match {
            case Some(audioEngine) =>
              audioEngine.playChord(chord)
            case _ =>
          }
      }
    }))

    AudioEngine.createWithAudioContext(audioContext, (audioEvent) => Root.handleAction(root, audioEvent))
      .onComplete {
        case Success(audioEngine) =>
          Root.handleAction(root, AudioEngineInitialized(audioEngine))
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }

    document.getElementById("main").addEventListener("click", (_: Event) => {
      if (audioContext.state == "suspended") {
        audioContext.resume()
      }
    })

  }
}
