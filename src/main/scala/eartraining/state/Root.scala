package eartraining.state

import com.thoughtworks.binding.Binding.Var
import eartraining.{AudioEngine, Chord}

trait RootOption

trait RootAction
case class AudioEngineInitialized(audioEngine: AudioEngine) extends RootAction
case object QueryOptionSelected extends RootAction
case object TrichordGeneratorOptionSelected extends RootAction
case object BackToMenuSelected extends RootAction
case class PlayChord(chord: Chord) extends RootAction

case class RootState(rootState: Var[RootOption], var audioEngine: Option[AudioEngine])

case class Root() {

  val stateContainer: RootState = RootState(Var(Init()), None)

  def handleAction(action: RootAction): Root = {
    (action, this.stateContainer) match {
      case (AudioEngineInitialized(audioEngine), RootState(_, None)) =>
        stateContainer.audioEngine = Some(audioEngine)
        stateContainer.rootState := Menu(handleAction)
        this
      case (QueryOptionSelected, RootState(_, _)) =>
        stateContainer.rootState := Query(handleAction)
        this
      case (TrichordGeneratorOptionSelected, RootState(_, _)) =>
        stateContainer.rootState := TrichordGenerator(handleAction)
        this
      case (BackToMenuSelected, RootState(_, _)) =>
        stateContainer.rootState := Menu(handleAction)
        this
      case (PlayChord(chord), RootState(_, Some(audioEngine))) =>
        audioEngine.playChord(chord)
        this
    }
  }
}
