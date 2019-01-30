package net.zoltantamasi.eartraining.state

case class InitState() extends RootOption

sealed trait InitAction extends RootAction

object Init extends StateHandler[InitState, InitAction] {

  def getInitial(): InitState = InitState()

  def handleAction(state: InitState, action: InitAction): Effect = NoEffect

}
