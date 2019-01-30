package net.zoltantamasi.eartraining.state

trait StateHandler[STATE, ACTION] {

  def handleAction(state: STATE, ACTION: ACTION): Effect

}
