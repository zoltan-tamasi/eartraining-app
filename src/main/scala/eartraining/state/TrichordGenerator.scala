package eartraining.state

import com.thoughtworks.binding.Binding.Var
import eartraining._

import scala.util.Random

sealed trait TrichordGeneratorAction extends RootAction
case object Randomize extends TrichordGeneratorAction
case object PlayCurrentChord extends TrichordGeneratorAction
case object BackToMenu extends TrichordGeneratorAction

case class TrichordGenratorState(rotation: Var[Rotation],
                                 octaveExplode: Var[OctaveExplode],
                                 triadCore: Var[TriadCore],
                                 baseNote: Var[Note])

case class TrichordGenerator(delegator: RootAction => Root) extends RootOption {

  val state = TrichordGenratorState(
    rotation = Var(Rotation0),
    octaveExplode = Var(NotOctaveExploded),
    triadCore = Var(Major),
    baseNote = Var(Note(C, 3)))

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
      case PlayCurrentChord =>
        delegator(PlayChord(Chord(state.triadCore.get, state.rotation.get, state.octaveExplode.get, state.baseNote.get)))
        this
      case BackToMenu =>
        delegator(BackToMenuSelected)
        this
    }
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

}
