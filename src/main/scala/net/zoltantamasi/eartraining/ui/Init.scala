package net.zoltantamasi.eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining.state.{Init, InitState, RootAction}
import org.scalajs.dom.Node

object InitUI extends StateToUI[InitState] {

  @dom
  def toUI(initState: InitState, actionHandler: RootAction => Unit): Binding[Node] = {
    <div class="mx-auto" style="width: 200px;">
      <img src="img/progress.gif" id="progress"></img>
    </div>
  }

}
