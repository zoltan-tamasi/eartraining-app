package net.zoltantamasi.eartraining.state

import com.thoughtworks.binding.Binding.Var
import net.zoltantamasi.eartraining.state.generator.{TrichordGenerator}
import net.zoltantamasi.eartraining.state.practice.Practice
import net.zoltantamasi.eartraining.{AudioEngine, Chord}

trait RootOption

trait RootAction
case class AudioEngineInitialized(audioEngine: AudioEngine) extends RootAction
case object QueryOptionSelected extends RootAction
case object TrichordGeneratorOptionSelected extends RootAction
case class PlayChord(chord: Chord) extends RootAction
case object RootAudioFinished extends RootAction
case object BackToMenu extends RootAction

case class RootState(rootState: Var[RootOption], var audioEngine: Option[AudioEngine], audioEngineReady: Var[Boolean])

case class Root() {

  val state: RootState = RootState(
    rootState = Var(Init()),
    audioEngine = None,
    audioEngineReady = Var(false))

  def handleAction(action: RootAction): Unit = {
    action match {

      case AudioEngineInitialized(audioEngine) =>
        state.audioEngine = Some(audioEngine)
        state.rootState.value = Menu(handleAction)
        state.audioEngineReady.value = true

      case QueryOptionSelected =>
        state.rootState.value = Practice(handleAction, state)

      case TrichordGeneratorOptionSelected =>
        state.rootState.value = TrichordGenerator(handleAction, state)

      case BackToMenu =>
        state.rootState.value = Menu(handleAction)

      case PlayChord(chord) =>
        state.audioEngine match {
          case Some(audioEngine) =>
            audioEngine.playChord(chord)
          case None =>
            println("AudioEngine has not been initialized yet")
        }

      case RootAudioFinished =>
        state.audioEngineReady.value = true
    }
  }
}
