package eartraining.state

sealed trait MenuAction extends RootAction
case object ToQuery extends MenuAction
case object ToTrichordGenerator extends MenuAction

case class Menu(delegator: RootAction => Unit) extends RootOption {

  def handleAction(action: RootAction): Unit = {
    action match {

      case ToQuery =>
        delegator(QueryOptionSelected)

      case ToTrichordGenerator =>
        delegator(TrichordGeneratorOptionSelected)

      case action:RootAction =>
        delegator(action)
    }
  }
}
