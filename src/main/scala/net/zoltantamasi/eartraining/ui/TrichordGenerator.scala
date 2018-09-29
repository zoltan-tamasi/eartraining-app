package net.zoltantamasi.eartraining.ui

import com.thoughtworks.binding.Binding.Constants
import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state._
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}
import org.scalajs.dom.{Event, Node, document}

object TrichordGeneratorUI extends StateToUI[TrichordGenerator] {

  def baseNoteSliderHandler(trichordGenerator: TrichordGenerator) = {
    (event: Event) =>
      val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value.toInt
      trichordGenerator.handleAction(ChangeBaseNote(Note.fromInt(selectedValue)))
  }

  def rotationSliderHandler(trichordGenerator: TrichordGenerator) = {
    (event: Event) =>
      val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value.toInt
      trichordGenerator.handleAction(ChangeRotation(Rotation.fromInt(selectedValue)))
  }

  @dom
  def toUI(trichordGenerator: TrichordGenerator): Binding[Node] = {
    <div class="container">
      <form onsubmit={(event:Event) => event.preventDefault()}>
        <div class="row">
          { Lock.toUI(trichordGenerator.state.triadCoreLocked).bind }
          <div class="col-4">
            <div class="form-group form-inline">
              <label for="triadCoreSelect">Triad core</label>
              <select class="form-control" id="triadCoreSelect" onchange={(event: Event) =>
                val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
                trichordGenerator.handleAction(ChangeTriadCore(TriadCore.fromString(selectedValue)))
              }>
                {
                  Constants(TriadCore.allTriadTypes: _*) map { triadCore =>
                    <option selected={trichordGenerator.state.triadCore.bind == triadCore} value={triadCore.toString}>
                      {triadCore.label}
                    </option>
                  }
                }
              </select>
            </div>
          </div>
          { Lock.toUI(trichordGenerator.state.baseNoteLocked).bind }
          <div class="col-4">
            <div class="form-group">
              <label for="baseNoteSlider">Base note: {trichordGenerator.state.baseNote.bind.toString}</label>
              <input type="range" class="form-control-range" id="baseNoteSlider" min="24" max="60"
                     list={document.getElementById("baseNoteMarks").asInstanceOf[HTMLInputElement]}
                     value={Note.toInt(trichordGenerator.state.baseNote.bind).toString}
                     oninput={baseNoteSliderHandler(trichordGenerator)} onchange={baseNoteSliderHandler(trichordGenerator)}/>
              <datalist id="baseNoteMarks">
                <option value="24" label="C 2"/>
                <option value="36" label="C 3"/>
                <option value="48" label="C 4"/>
                <option value="60" label="C 5"/>
              </datalist>
            </div>
          </div>
        </div>
        <div class="row">
          { Lock.toUI(trichordGenerator.state.rotationLocked).bind }
          <div class="col-4">
            <div class="form-group">
              <label for="baseNoteSlider">Rotation: {trichordGenerator.state.rotation.bind match {
                case Rotation0 => "1st position"
                case Rotation1 => "2nd position"
                case Rotation2 => "3rd position"
              }}</label>
              <input type="range" class="form-control-range" id="rotationSlider" min="0" max="2"
                     value={Rotation.toInt(trichordGenerator.state.rotation.bind).toString}
                     oninput={rotationSliderHandler(trichordGenerator)} onchange={rotationSliderHandler(trichordGenerator)}/>
            </div>
          </div>
          { Lock.toUI(trichordGenerator.state.octaveExplodeLocked).bind }
          <div class="col-4">
            <div class="btn-group-toggle">
              {
                val classList = if (trichordGenerator.state.octaveExplode.bind == OctaveExploded) "btn btn-success" else "btn btn-outline-secondary"
                <label class={classList}>
                  <input type="checkbox" checked={trichordGenerator.state.octaveExplode.bind == OctaveExploded}
                         onchange={(event: Event) =>
                           trichordGenerator.handleAction(
                             ChangeOctaveExploded(OctaveExplode.fromBoolean(event.target.asInstanceOf[HTMLInputElement].checked))
                           )}/>
                  Middle note octave up
                </label>
              }
            </div>
          </div>
        </div>
        <div class="row">
          <button class="btn btn-primary" onclick={(_: Event) => trichordGenerator.handleAction(Invert)}>
            Invert
          </button>
          <button class="btn btn-primary" onclick={(_: Event) => trichordGenerator.handleAction(Randomize) }>
            Randomize
          </button>
        </div>

        <hr/>

        { NoteWheel.toUI(trichordGenerator).bind }

        <hr/>

      </form>
    </div>

  }
}