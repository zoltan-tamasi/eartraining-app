package eartraining.state

import eartraining.AudioEngine

sealed trait InitAction
case class Initialized(audioEngine: AudioEngine) extends InitAction

case class Init() extends RootOption
