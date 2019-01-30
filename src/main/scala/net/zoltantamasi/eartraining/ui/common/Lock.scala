package net.zoltantamasi.eartraining.ui.common

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining.state.RootAction
import net.zoltantamasi.eartraining.ui.StateToUI
import org.scalajs.dom.{Event, Node}

object Lock extends StateToUI[Var[Boolean]] {

  @dom
  override def toUI(lockState: Var[Boolean], actionHandler: RootAction => Unit): Binding[Node] = {
    <span onclick={(_: Event) => { lockState.value = !lockState.value }}>
      { if (lockState.bind) <img src="img/locked.png"></img> else <img src="img/unlocked.png"></img> }
    </span>
  }


}