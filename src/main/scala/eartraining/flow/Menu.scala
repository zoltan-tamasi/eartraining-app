package eartraining.flow

import eartraining.{AudioEngine}

sealed trait MenuAction
case object ToQuery extends MenuAction
case object ToTrichordGenerator extends MenuAction

case class Menu(audioEngine: AudioEngine) extends FlowStatus
