package net.zoltantamasi.eartraining.state

import com.thoughtworks.binding.Binding.Var
import net.zoltantamasi.eartraining.state.generator.{TrichordGenerator, TrichordGeneratorAction, TrichordGeneratorState}
import net.zoltantamasi.eartraining.state.practice.{Practice, PracticeAction, PracticeState}
import net.zoltantamasi.eartraining.{AudioEngine, Chord}

trait RootOption

trait RootAction
case class AudioEngineInitialized(audioEngine: AudioEngine) extends RootAction
case object QueryOptionSelected extends RootAction
case object TrichordGeneratorOptionSelected extends RootAction
case object RootAudioFinished extends RootAction
case object BackToMenu extends RootAction

case class RootState(rootState: Var[RootOption], var audioEngine: Option[AudioEngine], audioEngineReady: Var[Boolean])

object Root extends StateHandler[RootState, RootAction] {

  def getInitial(): RootState =
    RootState(
      rootState = Var(Init.getInitial()),
      audioEngine = None,
      audioEngineReady = Var(false)
    )

  def handleAction(state: RootState, action: RootAction): Effect = {
    action match {

      case AudioEngineInitialized(audioEngine) =>
        state.audioEngine = Some(audioEngine)
        state.rootState.value = Menu.getInitial()
        state.audioEngineReady.value = true
        NoEffect

      case QueryOptionSelected =>
        state.rootState.value = Practice.getInitial(state.audioEngineReady)
        NoEffect

      case TrichordGeneratorOptionSelected =>
        state.rootState.value = TrichordGenerator.getInitial(state.audioEngineReady)
        NoEffect

      case BackToMenu =>
        state.rootState.value = Menu.getInitial()
        NoEffect

      case RootAudioFinished =>
        state.audioEngineReady.value = true
        NoEffect

      case practiceAction: PracticeAction =>
        state.rootState.value match {
          case practice: PracticeState =>
            Practice.handleAction(practice, practiceAction)

          case _  =>
            NoEffect
        }

      case trichordGeneratorAction: TrichordGeneratorAction =>
        state.rootState.value match {
          case trichordGeneratorState: TrichordGeneratorState =>
            TrichordGenerator.handleAction(trichordGeneratorState, trichordGeneratorAction)

          case _  =>
            NoEffect
        }
    }
  }
}
