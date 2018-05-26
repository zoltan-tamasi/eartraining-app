package eartraining

import com.thoughtworks.binding.Binding.Var

import scala.util.Random

sealed trait FlowStatus
case object Init extends FlowStatus
case object Query extends FlowStatus

object Flow {

  val status = Var[FlowStatus](Init)

  var audioEngineOption: Option[AudioEngine] = None

  def toQuery(audioEngine: AudioEngine) = {
    this.status := Query
  }

  def createRandomChordFromTriadCore(triadCore: TriadCore): Chord = {
    pullRandom(
      for {
        octave <- if (octaveExtraction.get) List(2, 3, 4) else List(3, 4)

        octaveExplode <- if (octaveExtraction.get) {
          octave match {
            case 2 => List(OctaveExploded)
            case 3 => List(OctaveExploded, NotOctaveExploded)
            case 4 => List(OctaveExploded, NotOctaveExploded)
          }
        } else {
          List(NotOctaveExploded)
        }

        noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B)

        rotation <- if (rotations.get) List(Rotation0, Rotation1, Rotation2) else List(Rotation0)

      } yield Chord(triadCore, rotation, octaveExplode, Note(noteName, octave))
    )
  }

  def pullRandom[T](list: List[T]): T = {
    list(Random.nextInt(list.length))
  }

  def newChord(): Chord = {
    createRandomChordFromTriadCore(pullRandom(triadCoreSet.get.toList))
  }

  def playChord(): Unit = {
    audioEngineOption match {
      case Some(audioEngine) => audioEngine.playChord(chord.get)
      case None => throw new Throwable("Audio engine is not available")
    }
  }

  def next(): Unit = {
    Flow.chord := newChord()
    Flow.guessed := NotGuessed
    audioEngineOption match {
      case Some(audioEngine) => audioEngine.playChord(chord.get)
      case None => throw new Throwable("Audio engine is not available")
    }
  }

  val guessed = Var[GuessStatus](NotGuessed)

  val triadCoreSet = Var[Set[TriadCore]](Set())

  triadCoreSet := triadCoreSet.get ++ (TriadCore.allTriadTypes)

  val rotations = Var[Boolean](true)

  val octaveExtraction = Var[Boolean](true)

  val chord = Var[Chord](newChord())

  implicit var audioEngine : Option[AudioEngine] = None

  sealed trait GuessStatus
  case object NotGuessed extends GuessStatus
  case object GuessedWrong extends GuessStatus
  case object GuessedCorrectly extends GuessStatus

}
