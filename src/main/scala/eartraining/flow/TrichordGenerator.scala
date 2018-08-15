package eartraining.flow

import com.thoughtworks.binding.Binding.Var
import eartraining._

import scala.util.Random

sealed trait TrichordGeneratorAction
case object Randomize extends TrichordGeneratorAction
case object PlayChord extends TrichordGeneratorAction

case class TrichordGenratorState(rotation: Var[Rotation],
                                 octaveExplode: Var[OctaveExplode],
                                 triadCore: Var[TriadCore],
                                 baseNote: Var[Note])

case class TrichordGenerator(val audioEngine: AudioEngine) extends FlowStatus {

  val state = TrichordGenratorState(
    rotation = Var(Rotation0),
    octaveExplode = Var(NotOctaveExploded),
    triadCore = Var(Major),
    baseNote = Var(Note(C, 3)))

  def handleAction(action: TrichordGeneratorAction): TrichordGenerator = {
    case Randomize =>
      state.rotation := pullRandom(List(Rotation0, Rotation1, Rotation2))
      state.octaveExplode := pullRandom(List(OctaveExploded, NotOctaveExploded))
      state.triadCore := pullRandom(TriadCore.allTriadTypes)
      state.baseNote := pullRandom(for {
        noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#)
        octave <- List(2, 3, 4)
      } yield Note(noteName, octave))
      this
    case PlayChord =>
      audioEngine.playChord(Chord(state.triadCore.get, state.rotation.get, state.octaveExplode.get, state.baseNote.get))
      this
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

}
