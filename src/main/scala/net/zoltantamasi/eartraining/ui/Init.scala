package net.zoltantamasi.eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining.state.Init
import org.scalajs.dom.Node

object InitUI extends StateToUI[Init] {

  @dom
  def toUI(init: Init): Binding[Node] = {
    <div class="mx-auto" style="width: 200px;">
      <img src="img/progress.gif" id="progress"></img>
    </div>
  }

}
