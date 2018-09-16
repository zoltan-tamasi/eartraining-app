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
case class ChangeBaseNote(noteName: NoteName) extends TrichordGeneratorAction
case class ChangeOctave(octave: Int) extends TrichordGeneratorAction

case class TrichordGeneratorState(rotation: Var[Rotation],
                                  octaveExplode: Var[OctaveExplode],
                                  triadCore: Var[TriadCore],
                                  baseNote: Var[Note])

case class TrichordGenerator(delegator: RootAction => Root) extends RootOption {

  val state = TrichordGeneratorState(
    rotation = Var(Rotation0),
    octaveExplode = Var(NotOctaveExploded),
    triadCore = Var(Major),
    baseNote = Var(Note(C, 3))
  )

  def handleAction(action: RootAction): TrichordGenerator = {
    action match {
      case Randomize =>
        state.rotation := pullRandom(List(Rotation0, Rotation1, Rotation2))
        state.octaveExplode := pullRandom(List(OctaveExploded, NotOctaveExploded))
        state.triadCore := pullRandom(TriadCore.allTriadTypes)
        state.baseNote := pullRandom(for {
          noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#)
          octave <- List(2, 3, 4)
        } yield Note(noteName, octave))
        this

      case ChangeRotation(rotation: Rotation) =>
        state.rotation := rotation
        this

      case ChangeOctaveExploded(enabled: OctaveExplode) =>
        state.octaveExplode := enabled
        this

      case ChangeTriadCore(triadCore: TriadCore) =>
        state.triadCore := triadCore
        this

      case ChangeBaseNote(noteName: NoteName) =>
        state.baseNote := Note(noteName, state.baseNote.get.octave)
        this

      case ChangeOctave(octave: Int) =>
        state.baseNote := Note(state.baseNote.get.noteName, octave)
        this

      case PlayCurrentChord =>
        delegator(PlayChord(Chord(state.triadCore.get, state.rotation.get, state.octaveExplode.get, state.baseNote.get)))
        this

      case action: RootAction =>
        delegator(action)
        this
    }
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

}
