package eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import eartraining.flow._
import org.scalajs.dom.{Event, Node}

object MenuUI extends StateToUI[Menu] {

  @dom
  def toUI(menu: Menu): Binding[Node] = {
    <div>
      <hr/>
      <button onclick={(_: Event) => menu.dispatchEvent(ToQuery)}>
        Practice hearing trichords
      </button>

      <button onclick={(_: Event) => menu.dispatchEvent(ToTrichordGenerator)}>
        Trichord generator
      </button>
    </div>
  }
}
