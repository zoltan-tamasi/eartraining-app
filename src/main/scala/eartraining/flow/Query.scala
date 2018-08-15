package eartraining.flow

import com.thoughtworks.binding.Binding.Var
import eartraining._

import scala.util.Random

sealed trait GuessStatus
case object NotGuessed extends GuessStatus
case object GuessedWrong extends GuessStatus
case object GuessedCorrectly extends GuessStatus

sealed trait QueryAction
case object Next extends QueryAction
case class DoGuess(triadCore: TriadCore) extends QueryAction
case class PlayChord(chord: Chord) extends QueryAction

case class QueryState(guessed: Var[GuessStatus],
                      rotationsEnabled: Var[Boolean],
                      octaveExplodeEnabled: Var[Boolean],
                      actualChord: Var[Chord],
                      selectedTriadCoreSet: Var[Set[TriadCore]])

case class Query(val audioEngine: AudioEngine) extends FlowStatus {

  val stateContainer = QueryState(
    guessed = Var(NotGuessed),
    rotationsEnabled = Var(true),
    octaveExplodeEnabled = Var(true),
    actualChord = Var(createRandomChordFromTriadCore(pullRandom(TriadCore.allTriadTypes),
      octaveExplodeEnabled = true,
      rotationsEnabled = true)),
    selectedTriadCoreSet = Var(TriadCore.allTriadTypes.toSet))

  private def createRandomChordFromTriadCore(triadCore: TriadCore,
                                             octaveExplodeEnabled: Boolean,
                                             rotationsEnabled: Boolean): Chord = {
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

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

  def handleAction(action: QueryAction): Query = {
    action match {
      case Next =>
        val newChord = createRandomChordFromTriadCore(
          pullRandom(stateContainer.selectedTriadCoreSet.get.toList),
          stateContainer.octaveExplodeEnabled.get,
          stateContainer.rotationsEnabled.get)
        audioEngine.playChord(newChord)
        stateContainer.actualChord := newChord
        stateContainer.guessed := NotGuessed
        this

      case DoGuess(triadCore: TriadCore) =>
        stateContainer.guessed := (if (triadCore == stateContainer.actualChord.get.core) GuessedCorrectly else GuessedWrong)
        this


      case PlayChord(chord: Chord) =>
        audioEngine.playChord(chord)
        this
    }
  }

}
