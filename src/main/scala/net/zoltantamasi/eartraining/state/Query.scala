package net.zoltantamasi.eartraining.state

import com.thoughtworks.binding.Binding.Var
import net.zoltantamasi.eartraining._

import scala.util.Random

sealed trait GuessStatus
case object NotGuessed extends GuessStatus
case object GuessedWrong extends GuessStatus
case object GuessedCorrectly extends GuessStatus

sealed trait QueryAction extends RootAction
case object Next extends QueryAction
case class DoGuess(triadCore: TriadCore) extends QueryAction
case class ChangeTriadCoreSelection(triadCore: TriadCore, enabled: Boolean) extends QueryAction
case class ChangeRotationSelection(enabled: Boolean) extends QueryAction
case class ChangeOctaveExtractionSelection(enabled: Boolean) extends QueryAction

case class QueryState(guessed: Var[GuessStatus],
                      rotationsEnabled: Var[Boolean],
                      octaveExplodeEnabled: Var[Boolean],
                      actualChord: Var[Chord],
                      selectedTriadCoreSet: Var[Set[TriadCore]])

case class Query(delegator: RootAction => Unit) extends RootOption {

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

  def handleAction(action: RootAction): Unit = {
    action match {
      case Next =>
        val newChord = createRandomChordFromTriadCore(
          pullRandom(stateContainer.selectedTriadCoreSet.get.toList),
          stateContainer.octaveExplodeEnabled.get,
          stateContainer.rotationsEnabled.get)
        delegator(PlayChord(newChord))
        stateContainer.actualChord.value = newChord
        stateContainer.guessed.value = NotGuessed

      case DoGuess(triadCore: TriadCore) =>
        stateContainer.guessed.value = (if (triadCore == stateContainer.actualChord.get.core) GuessedCorrectly else GuessedWrong)

      case ChangeTriadCoreSelection(triadCore: TriadCore, enabled: Boolean) =>
        if (enabled) {
          stateContainer.selectedTriadCoreSet.value = stateContainer.selectedTriadCoreSet.get + triadCore
        } else {
          stateContainer.selectedTriadCoreSet.value = stateContainer.selectedTriadCoreSet.get - triadCore
        }

      case ChangeRotationSelection(enabled: Boolean) =>
        stateContainer.rotationsEnabled.value = enabled

      case ChangeOctaveExtractionSelection(enabled: Boolean) =>
        stateContainer.octaveExplodeEnabled.value = enabled

      case action: RootAction =>
        delegator(action)
    }
  }

}
