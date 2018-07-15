package eartraining.flow

import com.thoughtworks.binding.Binding.Var
import eartraining._

trait FlowStatus

class Flow {

  val state = Var[FlowStatus](new Init)

  private var audioEngineOption: Option[AudioEngine] = None

  def initWithAudioEngine(audioEngine: AudioEngine): Unit = {
    audioEngineOption = Some(audioEngine)
    state := new Menu(this)
  }

  def toQueryState(): Unit = {
    state := new Query(audioEngineOption.get, this)
  }

  def toTrichordGenerator()= {
    state := new TrichordGenerator(audioEngineOption.get, this)
  }

  def back() = {
    state := new Menu(this)
  }

}
