package eartraining.state

sealed trait MenuAction extends RootAction
case object ToQuery extends MenuAction
case object ToTrichordGenerator extends MenuAction

case class Menu(delegator: RootAction => Root) extends RootOption {
  def handleAction(action: RootAction): Menu = {
    action match {
      case ToQuery =>
        delegator(QueryOptionSelected)
        this
      case ToTrichordGenerator =>
        delegator(TrichordGeneratorOptionSelected)
        this
      case action:RootAction =>
        delegator(action)
        this
    }
  }
}
