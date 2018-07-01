package eartraining.flow

import com.thoughtworks.binding.Binding.Var
import eartraining._

import scala.util.Random

sealed trait GuessStatus
case object NotGuessed extends GuessStatus
case object GuessedWrong extends GuessStatus
case object GuessedCorrectly extends GuessStatus

case class State(guessed: Var[GuessStatus],
                 rotationsEnabled: Var[Boolean],
                 octaveExplodeEnabled: Var[Boolean],
                 actualChord: Var[Chord],
                 selectedTriadCoreSet: Var[Set[TriadCore]])

trait Query extends FlowStatus {
  var state: State

  def doGuess(triadCore: TriadCore): Unit = {
    state.guessed := (if (triadCore == state.actualChord.get.core) GuessedCorrectly else GuessedWrong)
  }
}

object Query {

  var status = Var[State](initialState())

  def initialState() = State(
    Var(NotGuessed),
    Var(true),
    Var(true),
    Var(createRandomChordFromTriadCore(pullRandom(TriadCore.allTriadTypes), octaveExplodeEnabled = true, rotationsEnabled = true)),
    Var(TriadCore.allTriadTypes.toSet))

  def createRandomChordFromTriadCore(triadCore: TriadCore, octaveExplodeEnabled: Boolean, rotationsEnabled: Boolean): Chord = {
    pullRandom(
      for {
        octave <- if (octaveExplodeEnabled) {
            List(2, 3, 4)
          } else {
            List(3, 4)
          }

        octaveExplode <- if (octaveExplodeEnabled) {
          octave match {
            case 2 => List(OctaveExploded)
            case 3 => List(OctaveExploded, NotOctaveExploded)
            case 4 => List(OctaveExploded, NotOctaveExploded)
          }
        } else {
          List(NotOctaveExploded)
        }

        noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B)

        rotation <- if (rotationsEnabled) List(Rotation0, Rotation1, Rotation2) else List(Rotation0)

      } yield Chord(triadCore, rotation, octaveExplode, Note(noteName, octave))
    )
  }

  def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

  def next(state: State)(implicit audioEngine: AudioEngine): Unit = {
    val newChord = createRandomChordFromTriadCore(pullRandom(state.selectedTriadCoreSet.get.toList), state.octaveExplodeEnabled.get, state.rotationsEnabled.get)
    audioEngine.playChord(newChord)
    state.actualChord := newChord
  }

  def apply(audioEngine: AudioEngine): Query = {
    implicit val audioEngine = audioEngine
    new Query {
      override var state = initialState()
    }
  }
}
