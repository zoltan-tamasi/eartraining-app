package eartraining.state

import com.thoughtworks.binding.Binding.Var
import eartraining._

import scala.util.Random

sealed trait TrichordGeneratorAction extends RootAction
case object Randomize extends TrichordGeneratorAction
case object PlayCurrentChord extends TrichordGeneratorAction
case object BackToMenu extends TrichordGeneratorAction
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
                                  baseNote: Var[Note])

case class TrichordGenerator(delegator: RootAction => Unit) extends RootOption {

  val state = TrichordGeneratorState(
    rotation = Var(Rotation0),
    octaveExplode = Var(NotOctaveExploded),
    triadCore = Var(Major),
    baseNote = Var(Note(C, 3))
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

      case ChangeRotation(rotation: Rotation) =>
        state.rotation.value = rotation

      case ChangeOctaveExploded(enabled: OctaveExplode) =>
        state.octaveExplode.value = enabled

      case ChangeTriadCore(triadCore: TriadCore) =>
        state.triadCore.value = triadCore

      case ChangeBaseNoteName(noteName: NoteName) =>
        state.baseNote.value = Note(noteName, state.baseNote.get.octave)

      case ChangeOctave(octave: Int) =>
        state.baseNote.value = Note(state.baseNote.get.noteName, octave)

      case ChangeBaseNote(noteValue) =>
        state.baseNote.value = noteValue

      case Invert =>
        TriadCore.invert(state.triadCore.value, state.rotation.value) match {
          case (triadCore, rotation) =>
            state.triadCore.value = triadCore
            state.rotation.value = rotation
        }

      case PlayCurrentChord =>
        delegator(PlayChord(Chord(state.triadCore.get, state.rotation.get, state.octaveExplode.get, state.baseNote.get)))

      case action: RootAction =>
        delegator(action)
    }
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

}
