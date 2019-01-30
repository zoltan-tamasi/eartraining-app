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

sealed trait GuessAction extends PracticeAction
case class UserGuessed(triadCore: TriadCore) extends GuessAction
case class UserGuessedEither(triadCore1: TriadCore, triadCore2: TriadCore) extends GuessAction


case class PracticeState(rotation: Var[Rotation],
                         octaveExplode: Var[OctaveExplode],
                         triadCore: Var[TriadCore],
                         baseNote: Var[Note],
                         guessStatus: Var[GuessStatus],
                         audioEngineReady: Var[Boolean]) extends RootOption

object Practice extends StateHandler[PracticeState, PracticeAction] {

  def getInitial(audioEngineReady: Var[Boolean] = Var(false)): PracticeState =
    PracticeState(
      rotation = Var(pullRandom(List(Rotation0, Rotation1, Rotation2))),
      octaveExplode = Var(pullRandom(List(OctaveExploded, NotOctaveExploded))),
      triadCore = Var(pullRandom(TriadCore.allTriadTypes)),
      baseNote = Var(pullRandom(for {
        noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#)
        octave <- List(2, 3, 4)
      } yield Note(noteName, octave))),
      guessStatus = Var(NotGuessed),
      audioEngineReady = audioEngineReady
    )

  private def playCurrentChord(state: PracticeState): Effect = {
    println(state.audioEngineReady.value)
    if (state.audioEngineReady.value) {
      state.audioEngineReady.value = false
      PlayChordEffect(Chord(state.triadCore.value, state.rotation.value, state.octaveExplode.value, state.baseNote.value))
    } else {
      NoEffect
    }
  }

  def handleAction(state: PracticeState, action: PracticeAction): Effect = {
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

        playCurrentChord(state)

      case PlayCurrentChord =>
        playCurrentChord(state)

      case ChangeRotation(rotation: Rotation) =>
        state.rotation.value = rotation
        NoEffect

      case ChangeOctaveExploded(enabled: OctaveExplode) =>
        state.octaveExplode.value = enabled
        NoEffect

      case ChangeTriadCore(triadCore: TriadCore) =>
        state.triadCore.value = triadCore
        NoEffect

      case ChangeBaseNoteName(noteName: NoteName) =>
        state.baseNote.value = Note(noteName, state.baseNote.value.octave)
        NoEffect

      case ChangeOctave(octave: Int) =>
        state.baseNote.value = Note(state.baseNote.value.noteName, octave)
        NoEffect

      case ChangeBaseNote(noteValue) =>
        state.baseNote.value = noteValue
        NoEffect

      case UserGuessed(triadCore) =>
        if (state.triadCore.value == triadCore) {
          state.guessStatus.value = GuessedCorrectly(triadCore)
        } else {
          state.guessStatus.value = GuessedWrong(triadCore, state.triadCore.value)
        }
        NoEffect

      case UserGuessedEither(triadCore1, triadCore2) =>
        if (state.triadCore.value == triadCore1 || state.triadCore.value == triadCore2) {
          state.guessStatus.value = GuessedCorrectly(state.triadCore.value)
        } else {
          state.guessStatus.value = GuessedWrongEither(triadCore1, triadCore2, state.triadCore.value)
        }
        NoEffect
    }
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

}
