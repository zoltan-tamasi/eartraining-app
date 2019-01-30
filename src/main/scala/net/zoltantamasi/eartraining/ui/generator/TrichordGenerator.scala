package net.zoltantamasi.eartraining.ui.generator

import com.thoughtworks.binding.Binding.Constants
import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state.RootAction
import net.zoltantamasi.eartraining.state.generator._
import net.zoltantamasi.eartraining.ui.common.{Lock, NoteWheel}
import net.zoltantamasi.eartraining.ui.StateToUI
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}
import org.scalajs.dom.{Event, Node, document}

object TrichordGeneratorUI extends StateToUI[TrichordGeneratorState] {

  def baseNoteSliderHandler(actionHandler: RootAction => Unit) = {
    (event: Event) =>
      val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value.toInt
      actionHandler(ChangeBaseNote(Note.fromInt(selectedValue)))
  }

  def rotationSliderHandler(actionHandler: RootAction => Unit) = {
    (event: Event) =>
      val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value.toInt
      actionHandler(ChangeRotation(Rotation.fromInt(selectedValue)))
  }

  @dom
  def toUI(trichordGeneratorState: TrichordGeneratorState, actionHandler: RootAction => Unit): Binding[Node] = {
    <div class="container">
      <form onsubmit={(event:Event) => event.preventDefault()}>
        <div class="row">
          { Lock.toUI(trichordGeneratorState.triadCoreLocked, actionHandler).bind }
          <div class="col-4">
            <div class="form-group form-inline">
              <label for="triadCoreSelect">Triad core</label>
              <select class="form-control" id="triadCoreSelect" onchange={(event: Event) =>
                val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
                actionHandler(ChangeTriadCore(TriadCore.fromString(selectedValue)))
              }>
                {
                  Constants(TriadCore.allTriadTypes: _*) map { triadCore =>
                    <option selected={trichordGeneratorState.triadCore.bind == triadCore} value={triadCore.toString}>
                      {triadCore.label}
                    </option>
                  }
                }
              </select>
            </div>
          </div>
          { Lock.toUI(trichordGeneratorState.baseNoteLocked, actionHandler).bind }
          <div class="col-4">
            <div class="form-group">
              <label for="baseNoteSlider">Base note: {trichordGeneratorState.baseNote.bind.toString}</label>
              <input type="range" class="form-control-range" id="baseNoteSlider" min="24" max="60"
                     list={document.getElementById("baseNoteMarks").asInstanceOf[HTMLInputElement]}
                     value={Note.toInt(trichordGeneratorState.baseNote.bind).toString}
                     oninput={baseNoteSliderHandler(actionHandler)} onchange={baseNoteSliderHandler(actionHandler)}/>
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
          { Lock.toUI(trichordGeneratorState.rotationLocked, actionHandler).bind }
          <div class="col-4">
            <div class="form-group">
              <label for="baseNoteSlider">Rotation: {trichordGeneratorState.rotation.bind match {
                case Rotation0 => "1st position"
                case Rotation1 => "2nd position"
                case Rotation2 => "3rd position"
              }}</label>
              <input type="range" class="form-control-range" id="rotationSlider" min="0" max="2"
                     value={Rotation.toInt(trichordGeneratorState.rotation.bind).toString}
                     oninput={rotationSliderHandler(actionHandler)} onchange={rotationSliderHandler(actionHandler)}/>
            </div>
          </div>
          { Lock.toUI(trichordGeneratorState.octaveExplodeLocked, actionHandler).bind }
          <div class="col-4">
            <div class="btn-group-toggle">
              {
                val classList = if (trichordGeneratorState.octaveExplode.bind == OctaveExploded) "btn btn-success" else "btn btn-outline-secondary"
                <label class={classList}>
                  <input type="checkbox" checked={trichordGeneratorState.octaveExplode.bind == OctaveExploded}
                         onchange={(event: Event) =>
                           actionHandler(
                             ChangeOctaveExploded(OctaveExplode.fromBoolean(event.target.asInstanceOf[HTMLInputElement].checked))
                           )}/>
                  Middle note octave up
                </label>
              }
            </div>
          </div>
        </div>
        <div class="row">
          <button class="btn btn-primary" onclick={(_: Event) => actionHandler(Invert)}>
            Invert
          </button>
          <button class="btn btn-primary" onclick={(_: Event) => actionHandler(Randomize) }>
            Randomize
          </button>
        </div>

        <hr/>

        { NoteWheel.toUI(trichordGeneratorState.triadCore, trichordGeneratorState.rotation, trichordGeneratorState.octaveExplode,
                         trichordGeneratorState.baseNote, trichordGeneratorState.audioEngineReady, actionHandler).bind }

        <hr/>

      </form>
    </div>

  }
}
