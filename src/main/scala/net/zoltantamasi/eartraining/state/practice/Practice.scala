package net.zoltantamasi.eartraining.state.practice

import com.thoughtworks.binding.Binding.Var
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state._

import scala.util.Random

sealed trait GuessStatus
case object NotGuessed extends GuessStatus
case class GuessedWrong(wrongGuess: TriadCore, correct: TriadCore) extends GuessStatus
case class GuessedWrongEither(wrongGuess1: TriadCore, wrongGuess2: TriadCore, correct: TriadCore) extends GuessStatus
case class GuessedCorrectly(triadCore: TriadCore) extends GuessStatus

sealed trait PracticeAction extends RootAction
case object Randomize extends PracticeAction
case object PlayCurrentChord extends PracticeAction
case class ChangeRotation(rotation: Rotation) extends PracticeAction
case class ChangeOctaveExploded(enabled: OctaveExplode) extends PracticeAction
case class ChangeTriadCore(triadCore: TriadCore) extends PracticeAction
case class ChangeBaseNoteName(noteName: NoteName) extends PracticeAction
case class ChangeOctave(octave: Int) extends PracticeAction
case class ChangeBaseNote(note: Note) extends PracticeAction

sealed trait GuessAction extends RootAction
case class UserGuessed(triadCore: TriadCore) extends GuessAction
case class UserGuessedEither(triadCore1: TriadCore, triadCore2: TriadCore) extends GuessAction



case class Guess(delegator: RootAction => Unit) {
  val state = GuessState(
    state = Var(NotGuessed)
  )

  def handleAction(action: RootAction): Unit = {
    action match {
      case action =>
        delegator(action)
    }
  }

}

case class GuessState(state: Var[GuessStatus])

case class PracticeState(rotation: Var[Rotation],
                         octaveExplode: Var[OctaveExplode],
                         triadCore: Var[TriadCore],
                         baseNote: Var[Note],
                         guessStatus: Var[GuessStatus],
                         audioEngineReady: Var[Boolean])

case class Practice(delegator: RootAction => Unit, rootState: RootState) extends RootOption {

  val state = PracticeState(
    rotation = Var(pullRandom(List(Rotation0, Rotation1, Rotation2))),
    octaveExplode = Var(pullRandom(List(OctaveExploded, NotOctaveExploded))),
    triadCore = Var(pullRandom(TriadCore.allTriadTypes)),
    baseNote = Var(pullRandom(for {
      noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#)
      octave <- List(2, 3, 4)
    } yield Note(noteName, octave))),
    guessStatus = Var(NotGuessed),
    audioEngineReady = rootState.audioEngineReady
  )

  def handleAction(action: RootAction): Unit = {
    action match {
      case Randomize =>
        state.rotation.value = pullRandom(List(Rotation0, Rotation1, Rotation2))
        state.octaveExplode.value = pullRandom(List(OctaveExploded, NotOctaveExploded))
        state.triadCore.value = pullRandom(TriadCore.allTriadTypes)
        state.baseNote.value = pullRandom(for {
          noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#)
          octave <- List(2, 3, 4)
        } yield Note(noteName, octave))
        state.guessStatus.value = NotGuessed

        handleAction(PlayCurrentChord)

      case ChangeRotation(rotation: Rotation) =>
        state.rotation.value = rotation

      case ChangeOctaveExploded(enabled: OctaveExplode) =>
        state.octaveExplode.value = enabled

      case ChangeTriadCore(triadCore: TriadCore) =>
        state.triadCore.value = triadCore

      case ChangeBaseNoteName(noteName: NoteName) =>
        state.baseNote.value = Note(noteName, state.baseNote.value.octave)

      case ChangeOctave(octave: Int) =>
        state.baseNote.value = Note(state.baseNote.value.noteName, octave)

      case ChangeBaseNote(noteValue) =>
        state.baseNote.value = noteValue

      case PlayCurrentChord =>
        if (state.audioEngineReady.value) {
          delegator(PlayChord(Chord(state.triadCore.value, state.rotation.value, state.octaveExplode.value, state.baseNote.value)))
          state.audioEngineReady.value = false
        }

      case UserGuessed(triadCore) =>
        if (state.triadCore.value == triadCore) {
          state.guessStatus.value = GuessedCorrectly(triadCore)
        } else {
          state.guessStatus.value = GuessedWrong(triadCore, state.triadCore.value)
        }

      case UserGuessedEither(triadCore1, triadCore2) =>
        if (state.triadCore.value == triadCore1 || state.triadCore.value == triadCore2) {
          state.guessStatus.value = GuessedCorrectly(state.triadCore.value)
        } else {
          state.guessStatus.value = GuessedWrongEither(triadCore1, triadCore2, state.triadCore.value)
        }

      case action: RootAction =>
        delegator(action)
    }
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

}
