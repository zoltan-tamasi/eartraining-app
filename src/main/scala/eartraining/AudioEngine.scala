package eartraining

import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{AudioBuffer, AudioContext}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.typedarray.ArrayBuffer

trait AudioEngine {

  def playChord(chord: Chord): Unit

}

object AudioEngine {

  implicit val executor = scala.concurrent.ExecutionContext.global

  private def playbackRate(note: NoteName): Double = note match {
    case C => 1.0
    case C_# => 1.059463
    case D => 1.122462
    case D_# => 1.189207
    case E => 1.259921
    case F => 1.334840
    case F_# => 1.414214
    case G => 1.498307
    case G_# => 1.587401
    case A => 1.681793
    case A_# => 1.781797
    case B => 1.887749
  }

  private def playSound(note: Note)(implicit context: AudioContext, audioBuffers: List[AudioBuffer]) = {
    val source = context.createBufferSource()
    source.buffer = note.octave match {
      case 2 => audioBuffers(0)
      case 3 => audioBuffers(1)
      case 4 => audioBuffers(2)
      case 5 => audioBuffers(3)
      case 6 => {
        println(s"Not supported octave: 6, playing octave 5")
        audioBuffers(3)
      }
    }
    source.connect(context.destination)
    source.playbackRate.value = playbackRate(note.noteName)
    source.start(0)
  }

  private def loadSound(url: String, context: AudioContext): Future[AudioBuffer] = {
    val p = Promise[AudioBuffer]()
    Ajax
      .get(url, responseType = "arraybuffer")
      .map { r =>
        val arrayBuffer = r.response.asInstanceOf[ArrayBuffer]
        context.decodeAudioData(arrayBuffer, (buffer: AudioBuffer) => {
          p.success(buffer)
        }, () => {
          p.failure(new Throwable(s"Unable to load sound from: ${url}"))
        })
      }
    p.future
  }

  def create(context: AudioContext): Future[AudioEngine] = {

    for {
      c2 <- loadSound("c2mmell.wav", context)
      c3 <- loadSound("c3mmell.wav", context)
      c4 <- loadSound("c4mmell.wav", context)
      c5 <- loadSound("c5mmell.wav", context)
    } yield {

      implicit val audioBuffers = List(c2, c3, c4, c5)
      implicit val audioContext = context

      new AudioEngine {
        override def playChord(chord: Chord): Unit = {
          println(s"Chord played: ${Chord.notesOf(chord)}")
          Chord.notesOf(chord).foreach(note => playSound(note))
        }
      }

    }
  }
}