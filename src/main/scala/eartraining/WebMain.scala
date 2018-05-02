package eartraining

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Constants, Var, Vars}
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{AudioBuffer, AudioContext}
import org.scalajs.dom.{Event, Node, document}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Random, Success}

object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

  sealed trait Rotation

  case object Rotation0 extends Rotation
  case object Rotation1 extends Rotation
  case object Rotation2 extends Rotation

  type OctaveExplode = Boolean

  case class Chord(core: TriadCore, rotation: Rotation, octaveExplode: OctaveExplode, note: Note)

  object Chord {
    def notesOf(chord: Chord): List[Note] = {
      (chord.rotation, chord.octaveExplode) match {
        case (Rotation0, octaveExplode) => {
          val note1 = chord.note
          var note2 = Note.add(note1, chord.core.intervals._1)
          val note3 = Note.add(note2, chord.core.intervals._2)
          if (octaveExplode) {
            note2 = Note.add(note2, 12)
          }
          List(note1, note2, note3)
        }
        case (Rotation1, octaveExplode) => {
          val note1 = chord.note
          var note2 = Note.add(note1, chord.core.intervals._2)
          val note3 = Note.add(note2, chord.core.intervals._3)
          if (octaveExplode) {
            note2 = Note.add(note2, 12)
          }
          List(note1, note2, note3)
        }
        case (Rotation2, octaveExplode) => {
          val note1 = chord.note
          var note2 = Note.add(note1, chord.core.intervals._3)
          val note3 = Note.add(note2, chord.core.intervals._1)
          if (octaveExplode) {
            note2 = Note.add(note2, 12)
          }
          List(note1, note2, note3)
        }
      }
    }
  }

  sealed trait TriadCore {
    val intervals: (Int, Int, Int)
  }

  case object StackedMinor2 extends TriadCore {
    override val intervals = (1, 1, 10)
  }

  case object Minor2PlusMajor2 extends TriadCore {
    override val intervals = (1, 2, 9)
  }

  case object Major2PlusMinor2 extends TriadCore {
    override val intervals = (2, 1, 9)
  }

  case object MinorMajor extends TriadCore {
    override val intervals = (1, 3, 8)
  }

  case object MinorMajorI extends TriadCore {
    override val intervals = (3, 1, 8)
  }

  case object Lyd extends TriadCore {
    override val intervals = (1, 5, 6)
  }

  case object Locr extends TriadCore {
    override val intervals = (1, 6, 5)
  }

  case object Minor7Plus6 extends TriadCore {
    override val intervals = (2, 2, 8)
  }

  case object Minor7With3 extends TriadCore {
    override val intervals = (2, 3, 7)
  }

  case object Minor7With5 extends TriadCore {
    override val intervals = (2, 7, 3)
  }

  case object Minor extends TriadCore {
    override val intervals = (3, 4, 5)
  }

  case object Major extends TriadCore {
    override val intervals: (Int, Int, Int) = (4, 3, 5)
  }

  case object Augmented extends TriadCore {
    override val intervals: (Int, Int, Int) = (4, 4, 4)
  }

  case object Diminished extends TriadCore {
    override val intervals: (Int, Int, Int) = (3, 3, 6)
  }

  case object Major7Without5 extends TriadCore {
    override val intervals: (Int, Int, Int) = (4, 7, 1)
  }

  case object Major7Without3 extends TriadCore {
    override val intervals: (Int, Int, Int) = (7, 4, 1)
  }

  case object Stacked4s extends TriadCore {
    override val intervals: (Int, Int, Int) = (5, 5, 2)
  }

  case object Stacked5s extends TriadCore {
    override val intervals: (Int, Int, Int) = (7, 2, 3)
  }

  def allTriadTypes = List(Minor, Major, Augmented, Diminished, Major7Without5, Major7Without3, Stacked4s, Stacked5s,
    StackedMinor2, Minor2PlusMajor2, Major2PlusMinor2, Minor7Plus6, Minor7With3, Minor7With5, MinorMajor, MinorMajorI, Lyd, Locr)

  case class AudioEngine(context: AudioContext, audioBuffers: List[AudioBuffer])

  def loadSound(url: String, context: AudioContext): Future[AudioBuffer] = {
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

  def createAudioEngine(context: AudioContext): Future[AudioEngine] = {
    for {
      c2 <- loadSound("c2mmell.wav", context)
      c3 <- loadSound("c3mmell.wav", context)
      c4 <- loadSound("c4mmell.wav", context)
      c5 <- loadSound("c5mmell.wav", context)
    } yield {
      AudioEngine(context, List(c2, c3, c4, c5))
    }
  }

  def createRandomChordFromTriadCore(triadCore: TriadCore): Chord = {
    pullRandom(
      for {
        octave <- List(2, 3, 4)
        octaveExplode <- octave match {
          case 2 => List(true)
          case 3 => List(true, false)
          case 4 => List(true, false)
        }
        noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B)
        rotation <- List(Rotation0, Rotation1, Rotation2)
      } yield Chord(triadCore, rotation, octaveExplode, Note(noteName, octave))
    )
  }

  def pullRandom[T](list: List[T]): T = {
    list(Random.nextInt(list.length))
  }

  def playChord(chord: Chord)(implicit audioEngine: AudioEngine) {
    println(s"Chord played: ${Chord.notesOf(chord)}")
    Chord.notesOf(chord).foreach(note => Note.playSound(note))
  }

  def newChord(): Chord  = {
    createRandomChordFromTriadCore(pullRandom(allTriadTypes))
  }

  val status = Var[Status](Init)

  val chord = Var[Chord](newChord())

  val guessed = Var[Boolean](false)

  sealed trait Status

  case object Init extends Status

  case object Query extends Status

  @dom
  def UI(status: Var[Status], guessed: Var[Boolean], chord: Var[Chord])(implicit audioEngine: AudioEngine): Binding[Node] = {
    status.bind match {

      case Init =>

        <div>
          <button onclick={ (_ : Event) => status := Query }>
            Start
          </button>
        </div>

      case Query =>

        <div>
          <button onclick={ (_ : Event) => playChord(chord.get) }>
            Play Chord
          </button>

          {
            Constants(allTriadTypes: _*).map {
              (triadType: TriadCore) =>
                <button onclick = { (_ : Event) =>
                  guessed := triadType == chord.get.core
                }>
                  { triadType.toString }
                </button>
            }
          }

          <button onclick = { (_: Event) => chord := newChord() }>
            Next
          </button>
          <span>
            {
              if (guessed.bind) "OK" else "NO"
            }
          </span>
        </div>
    }
  }

  def main(args: Array[String]): Unit = {
    val context: AudioContext = new AudioContext()
    createAudioEngine(context)
      .onComplete {
        case Success(audioEngine) =>
          implicit val engine = audioEngine
          dom.render(document.body, UI(status, guessed, chord))
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
  }
}
