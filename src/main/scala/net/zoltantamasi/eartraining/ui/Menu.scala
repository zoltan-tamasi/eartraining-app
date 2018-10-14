package net.zoltantamasi.eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining.state.{Menu, ToQuery, ToTrichordGenerator}
import org.scalajs.dom.{Event, Node}

object MenuUI extends StateToUI[Menu] {

  @dom
  def toUI(menu: Menu): Binding[Node] = {
    <div class="col-8">
      <div class="row">
        With the generator you can experiment with the triad cores, listen and compare them in different setups and on different base notes.
      </div>
      <div class="row">
        <button type="button" class="btn btn-primary" onclick={(_: Event) => menu.handleAction(ToTrichordGenerator)}>
          Trichord generator
        </button>
      </div>
      <hr/>
      <div class="row">
        In practice mode you can try differentiate between the sonorities of the triad cores by ear. You can change configuration for easier 
        recognition if needed. 
      </div>
      <div class="row">        
        <button type="button" class="btn btn-primary" onclick={(_: Event) => menu.handleAction(ToQuery)}>
          Practice hearing trichords
        </button>
      </div>
      <hr/>
      <div class="row">
        <small style="font-variant: all-petite-caps">
          Created by Zoltan Tamasi, contact: mail@zoltantamasi.net
        </small>
      </div>
    </div>
  }
}
