package eartraining.flow

import eartraining.AudioEngine

sealed trait InitAction
case class Initialized(audioEngine: AudioEngine) extends InitAction

case object Init extends FlowStatus
