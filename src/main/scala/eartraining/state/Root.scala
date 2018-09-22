package eartraining.state

import com.thoughtworks.binding.Binding.Var
import eartraining.{AudioEngine, Chord}

trait RootOption

trait RootAction
case class AudioEngineInitialized(audioEngine: AudioEngine) extends RootAction
case object QueryOptionSelected extends RootAction
case object TrichordGeneratorOptionSelected extends RootAction
case class PlayChord(chord: Chord) extends RootAction

case class RootState(rootState: Var[RootOption], var audioEngine: Option[AudioEngine])

case class Root() {

  val stateContainer: RootState = RootState(Var(Init()), None)

  def handleAction(action: RootAction): Unit = {
    (action, stateContainer) match {

      case (AudioEngineInitialized(audioEngine), RootState(_, _)) =>
        stateContainer.audioEngine = Some(audioEngine)
        stateContainer.rootState.value = Menu(handleAction)

      case (QueryOptionSelected, RootState(_, _)) =>
        stateContainer.rootState.value = Query(handleAction)

      case (TrichordGeneratorOptionSelected, RootState(_, _)) =>
        stateContainer.rootState.value = TrichordGenerator(handleAction)

      case (BackToMenu, RootState(_, _)) =>
        stateContainer.rootState.value = Menu(handleAction)

      case (PlayChord(chord), RootState(_, Some(audioEngine))) =>
        audioEngine.playChord(chord)

      case (PlayChord(_), RootState(_, None)) =>
        println("AudioEngine has not been initialized yet")
    }
  }
}
