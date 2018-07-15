package eartraining.flow

class Menu(val parent: Flow) extends FlowStatus {

  def toQueryState(): Unit = {
    parent.toQueryState()
  }

  def toTrichordGenerator() = {
    parent.toTrichordGenerator()
  }

}
