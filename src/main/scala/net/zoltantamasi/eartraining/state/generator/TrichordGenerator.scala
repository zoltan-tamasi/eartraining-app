package net.zoltantamasi.eartraining.state.generator

import com.thoughtworks.binding.Binding.Var
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state._

import scala.util.Random

sealed trait TrichordGeneratorAction extends RootAction
case object Randomize extends TrichordGeneratorAction
case object PlayCurrentChord extends TrichordGeneratorAction
case class ChangeRotation(rotation: Rotation) extends TrichordGeneratorAction
case class ChangeOctaveExploded(enabled: OctaveExplode) extends TrichordGeneratorAction
case class ChangeTriadCore(triadCore: TriadCore) extends TrichordGeneratorAction
case class ChangeBaseNoteName(noteName: NoteName) extends TrichordGeneratorAction
case class ChangeOctave(octave: Int) extends TrichordGeneratorAction
case class ChangeBaseNote(note: Note) extends TrichordGeneratorAction
case object Invert extends TrichordGeneratorAction

case class TrichordGeneratorState(rotation: Var[Rotation],
                                  octaveExplode: Var[OctaveExplode],
                                  triadCore: Var[TriadCore],
                                  baseNote: Var[Note],
                                  triadCoreLocked: Var[Boolean],
                                  baseNoteLocked: Var[Boolean],
                                  rotationLocked: Var[Boolean],
                                  octaveExplodeLocked: Var[Boolean],
                                  audioEngineReady: Var[Boolean])  extends RootOption

object TrichordGenerator extends StateHandler[TrichordGeneratorState, TrichordGeneratorAction] {

  def getInitial(audioEngineReady: Var[Boolean] = Var(false)) =
    TrichordGeneratorState(
      rotation = Var(Rotation0),
      octaveExplode = Var(NotOctaveExploded),
      triadCore = Var(Major),
      baseNote = Var(Note(C, 3)),
      triadCoreLocked = Var(false),
      baseNoteLocked = Var(false),
      rotationLocked = Var(false),
      octaveExplodeLocked = Var(false),
      audioEngineReady = audioEngineReady
    )

  def handleAction(state: TrichordGeneratorState, action: TrichordGeneratorAction): Effect = {
    action match {
      case Randomize =>
        if (!state.rotationLocked.value) {
          state.rotation.value = pullRandom(List(Rotation0, Rotation1, Rotation2))
        }
        if (!state.octaveExplodeLocked.value) {
          state.octaveExplode.value = pullRandom(List(OctaveExploded, NotOctaveExploded))
        }
        if (!state.triadCoreLocked.value) {
          state.triadCore.value = pullRandom(TriadCore.allTriadTypes)
        }
        if (!state.baseNoteLocked.value) {
          state.baseNote.value = pullRandom(for {
            noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#)
            octave <- List(2, 3, 4)
          } yield Note(noteName, octave))
        }
        NoEffect

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
        state.baseNote.value = Note(noteName, state.baseNote.get.octave)
        NoEffect

      case ChangeOctave(octave: Int) =>
        state.baseNote.value = Note(state.baseNote.get.noteName, octave)
        NoEffect

      case ChangeBaseNote(noteValue) =>
        state.baseNote.value = noteValue
        NoEffect

      case Invert =>
        TriadCore.invert(state.triadCore.value, state.rotation.value) match {
          case (triadCore, rotation) =>
            state.triadCore.value = triadCore
            state.rotation.value = rotation
        }
        NoEffect

      case PlayCurrentChord =>
        if (state.audioEngineReady.value) {
          state.audioEngineReady.value = false
          PlayChordEffect(Chord(state.triadCore.get, state.rotation.get, state.octaveExplode.get, state.baseNote.get))
        } else {
          NoEffect
        }
    }
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

}
