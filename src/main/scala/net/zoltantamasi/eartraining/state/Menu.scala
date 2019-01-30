package net.zoltantamasi.eartraining.state

sealed trait MenuAction extends RootAction

case class MenuState() extends RootOption

object Menu extends StateHandler[MenuState, MenuAction] {

  def getInitial(): MenuState = MenuState()

  def handleAction(state: MenuState, action: MenuAction): Effect = NoEffect

}
