package eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import eartraining.state._
import org.scalajs.dom.{Event, Node}

object MenuUI extends StateToUI[Menu] {

  @dom
  def toUI(menu: Menu): Binding[Node] = {
    <div>
      <hr/>
      <button onclick={(_: Event) => menu.handleAction(ToQuery)}>
        Practice hearing trichords
      </button>

      <button onclick={(_: Event) => menu.handleAction(ToTrichordGenerator)}>
        Trichord generator
      </button>
    </div>
  }
}
