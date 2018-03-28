package tutorial.webapp

import org.scalajs.dom.Event
import org.scalajs.dom.document
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{AudioBuffer, AudioContext}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Random, Success}

@JSExportTopLevel("WebApp")
object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

  trait NoteName {
    def successor(): NoteName

    def +(toAdd: Int): NoteName = {
      Function.chain(List.fill(toAdd)((noteName: NoteName) => noteName.successor))(this)
    }
  }

  case class Note(noteName: NoteName, octave: Int) {
    def +(toAdd: Int): Note = noteName match {
      case B => Note(noteName + toAdd, octave + 1)
      case _ => Note(noteName + toAdd, octave)
    }
  }

  object Note {

    def playbackRate(note: NoteName): Double = note match {
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

    def playSound(note: Note)(implicit context: AudioContext, audioBuffers: List[AudioBuffer]) = {
      val source = context.createBufferSource()
      source.buffer = note.octave match {
        case 2 => audioBuffers(0)
        case 3 => audioBuffers(1)
        case 4 => audioBuffers(2)
        case 5 => audioBuffers(3)
        case octave => throw new Exception(s"Not supported octave: ${octave}")
      }
      source.connect(context.destination)
      source.playbackRate.value = playbackRate(note.noteName)
      source.start(0)
    }
  }

  case object C extends NoteName {
    override def successor = C_#
  }

  case object C_# extends NoteName {
    override def successor = D
  }

  case object D extends NoteName {
    override def successor = D_#
  }

  case object D_# extends NoteName {
    override def successor = E
  }

  case object E extends NoteName {
    override def successor = F
  }

  case object F extends NoteName {
    override def successor = F_#
  }

  case object F_# extends NoteName {
    override def successor = G
  }

  case object G extends NoteName {
    override def successor = G_#
  }

  case object G_# extends NoteName {
    override def successor = A
  }

  case object A extends NoteName {
    override def successor = A_#
  }

  case object A_# extends NoteName {
    override def successor = B
  }

  case object B extends NoteName {
    override def successor = C
  }

  def loadSound(url: String)(implicit context: AudioContext): Future[AudioBuffer] = {
    val p = Promise[AudioBuffer]()
    Ajax
      .get(url, responseType = "arraybuffer")
      .map { r =>
        println(context)      
        val arrayBuffer = r.response.asInstanceOf[ArrayBuffer]
        context.decodeAudioData(arrayBuffer, (buffer) => {
          p.success(buffer)
        }, () => {
          p.failure(new Throwable(s"Unable to load sound from: ${url}"))
        })
      }
    p.future
  }

  def setup(context: AudioContext): Future[List[AudioBuffer]] = {    
    implicit val audioContext = context  
    for {
      c2 <- loadSound("c2mmell.wav")
      c3 <- loadSound("c3mmell.wav")
      c4 <- loadSound("c4mmell.wav")
      c5 <- loadSound("c5mmell.wav")
    } yield {
      List(c2, c3, c4, c5)
    }
  }

  def constructMinor(): List[Note] = {
    val randomNote1 = Note(pullRandom(List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B)), pullRandom(List(2, 3, 4)))
    val randomNote2 = randomNote1 + 3
    val randomNote3 = randomNote2 + 4
    List(randomNote1, randomNote2, randomNote3)
  }


  def pullRandom[T](list: List[T]): T = {
    list(Random.nextInt(list.length))
  }

  def playChord(chord: List[Note])(implicit context: AudioContext, audioBuffers: List[AudioBuffer]) {
    println(s"Chord played: ${chord}")
    chord.foreach(note => Note.playSound(note))
  }

  @JSExport
  def main(context: AudioContext): Unit = {
    implicit val audioContext: AudioContext = context
    setup(context)
      .onComplete {
        case Success(audioBuffers) =>
          document.getElementById("rand-chord").addEventListener("click", { (event: Event) =>
            implicit val buffers = audioBuffers
            playChord(constructMinor())
          }, false)
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
  }
}
