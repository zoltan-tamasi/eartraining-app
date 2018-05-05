package eartraining

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Constants, Var}
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{AudioBuffer, AudioContext, HTMLInputElement}
import org.scalajs.dom.{Event, Node, document}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Random, Success}

object WebApp {

  implicit val executor = scala.concurrent.ExecutionContext.global

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
        octave <- octaveExtraction.get match {
          case true => List(2, 3, 4)
          case false => List(3, 4)
        }
        octaveExplode <- octaveExtraction.get match {
          case true =>
            octave match {
              case 2 => List(OctaveExploded)
              case 3 => List(OctaveExploded, NotOctaveExploded)
              case 4 => List(OctaveExploded, NotOctaveExploded)
            }
          case false => List(NotOctaveExploded)
        }
        noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B)
        rotation <- rotations.get match {
          case true => List(Rotation0, Rotation1, Rotation2)
          case false => List(Rotation0)
        }
      } yield Chord(triadCore, rotation, octaveExplode, Note(noteName, octave))
    )
  }

  def pullRandom[T](list: List[T]): T = {
    list(Random.nextInt(list.length))
  }

  def playChord(chord: Chord)(implicit audioEngine: Option[AudioEngine]) {
    println(s"Chord played: ${Chord.notesOf(chord)}")
    Chord.notesOf(chord).foreach(note => Note.playSound(note))
  }

  def newChord(): Chord  = {
    createRandomChordFromTriadCore(pullRandom(triadCoreSet.get.toList))
  }

  val status = Var[Status](Init)

  val guessed = Var[GuessStatus](NotGuessed)

  val triadCoreSet = Var[Set[TriadCore]](Set())

  triadCoreSet := triadCoreSet.get ++ (TriadCore.allTriadTypes)

  val rotations = Var[Boolean](true)

  val octaveExtraction = Var[Boolean](true)

  val chord = Var[Chord](newChord())

  implicit var audioEngine : Option[AudioEngine] = None

  sealed trait Status
  case object Init extends Status
  case object Query extends Status

  sealed trait GuessStatus
  case object NotGuessed extends GuessStatus
  case object GuessedWrong extends GuessStatus
  case object GuessedCorrectly extends GuessStatus

  @dom
  def triadCoreSelection(triadCoreSet: Var[Set[TriadCore]], triadCore: TriadCore): Binding[Node] = {
    <td>
      <button onclick = { (_ : Event) =>
                guessed := (if (triadCore == chord.get.core) GuessedCorrectly else GuessedWrong)
              }>
        { triadCore.label }
      </button>
      <input type="checkbox"
             checked={ triadCoreSet.bind.contains(triadCore) }
             onchange={ (event: Event) =>
               if (event.target.asInstanceOf[HTMLInputElement].checked) {
                 triadCoreSet := triadCoreSet.get + triadCore
               } else {
                 triadCoreSet := triadCoreSet.get - triadCore
               }
             }>
      </input>
    </td>
  }

  @dom
  def UI(status: Var[Status], guessed: Var[GuessStatus], chord: Var[Chord], triadCoreSet: Var[Set[TriadCore]], rotations: Var[Boolean], octaveExtraction: Var[Boolean]): Binding[Node] = {
    status.bind match {

      case Init =>

        <div>
            Initializing...
        </div>

      case Query =>

        <div>
          <table>
            <tbody>
              <tr>
                { triadCoreSelection(triadCoreSet, StackedMinor2).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Minor2PlusMajor2).bind }
                { triadCoreSelection(triadCoreSet, Major2PlusMinor2).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, MinorMajor).bind }
                { triadCoreSelection(triadCoreSet, MinorMajorI).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Major7Without3).bind }
                { triadCoreSelection(triadCoreSet, Major7Without5).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Lyd).bind }
                { triadCoreSelection(triadCoreSet, Locr).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Minor7Plus6).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Minor7With3).bind }
                { triadCoreSelection(triadCoreSet, Minor7With5).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, LydSus2).bind }
                { triadCoreSelection(triadCoreSet, AugSus2).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Stacked4s).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Diminished).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Minor).bind }
                { triadCoreSelection(triadCoreSet, Major).bind }
              </tr>
              <tr>
                { triadCoreSelection(triadCoreSet, Augmented).bind }
              </tr>
            </tbody>
          </table>

          <hr />

            Rotations
            <input type="checkbox"
                   checked={ rotations.bind }
                   onchange={ (event: Event) =>
                     if (event.target.asInstanceOf[HTMLInputElement].checked) {
                       rotations := true
                     } else {
                       rotations := false
                     }
                   }>
            </input>

            Octave extraction
            <input type="checkbox"
                   checked={ octaveExtraction.bind }
                   onchange={ (event: Event) =>
                     if (event.target.asInstanceOf[HTMLInputElement].checked) {
                       octaveExtraction := true
                     } else {
                       octaveExtraction := false
                     }
                   }>
            </input>

          <hr />

          <button onclick={ (_ : Event) => playChord(chord.get) }>
            Play Chord
          </button>

          <button onclick = { (_: Event) =>
            chord := newChord()
            guessed := NotGuessed
            playChord(chord.get)
          }>
            Next
          </button>

          <hr />

          {
            guessed.bind match {
              case GuessedCorrectly => <span style="color: green">Correct</span>
              case GuessedWrong => <span style="color: red">Incorrect</span>
              case NotGuessed => <span></span>
            }
          }

        </div>
    }
  }

  def main(args: Array[String]): Unit = {
    dom.render(document.body, UI(status, guessed, chord, triadCoreSet, rotations, octaveExtraction))
    val context: AudioContext = new AudioContext()
    createAudioEngine(context)
      .onComplete {
        case Success(_audioEngine) =>
          audioEngine = Some(_audioEngine)
          status := Query
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
  }
}
