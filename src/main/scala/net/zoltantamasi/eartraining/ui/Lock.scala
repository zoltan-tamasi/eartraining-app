package net.zoltantamasi.eartraining.ui

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, Node}

object Lock extends StateToUI[Var[Boolean]] {

  @dom
  override def toUI(lockState: Var[Boolean]): Binding[Node] = {
    <span onclick={(_: Event) => { lockState.value = !lockState.value }}>
      { if (lockState.bind) <img src="/img/locked.png"></img> else <img src="/img/unlocked.png"></img> }
    </span>
  }


}