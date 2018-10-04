package net.zoltantamasi.eartraining.ui.practice

import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state.practice._
import net.zoltantamasi.eartraining.ui.StateToUI
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}
import org.scalajs.dom.{Event, Node, document}

object PracticeUI extends StateToUI[Practice] {

  def baseNoteSliderHandler(practice: Practice) = {
    (event: Event) =>
      val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value.toInt
      practice.handleAction(ChangeBaseNote(Note.fromInt(selectedValue)))
  }

  def rotationSliderHandler(practice: Practice) = {
    (event: Event) =>
      val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value.toInt
      practice.handleAction(ChangeRotation(Rotation.fromInt(selectedValue)))
  }

  @dom
  def toUI(practice: Practice): Binding[Node] = {
    <div class="container">
      <form onsubmit={(event: Event) => event.preventDefault()}>
        <div class="row">
          <div class="col-4">
            <div class="form-group">
              <label for="baseNoteSlider">Base note:
                {practice.state.baseNote.bind.toString}
              </label>
              <input type="range" class="form-control-range" id="baseNoteSlider" min="24" max="60"
                     list={document.getElementById("baseNoteMarks").asInstanceOf[HTMLInputElement]}
                     value={Note.toInt(practice.state.baseNote.bind).toString}
                     oninput={baseNoteSliderHandler(practice)} onchange={baseNoteSliderHandler(practice)}/>
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
          <div class="col-4">
            <div class="form-group">
              <label for="baseNoteSlider">Rotation:
                {practice.state.rotation.bind match {
                case Rotation0 => "1st position"
                case Rotation1 => "2nd position"
                case Rotation2 => "3rd position"
              }}
              </label>
              <input type="range" class="form-control-range" id="rotationSlider" min="0" max="2"
                     value={Rotation.toInt(practice.state.rotation.bind).toString}
                     oninput={rotationSliderHandler(practice)} onchange={rotationSliderHandler(practice)}/>
            </div>
          </div>
          <div class="col-4">
            <div class="btn-group-toggle">
              {
                val classList = if (practice.state.octaveExplode.bind == OctaveExploded) "btn btn-success" else "btn btn-outline-secondary"
                <label class={classList}>
                  <input type="checkbox" checked={practice.state.octaveExplode.bind == OctaveExploded}
                         onchange={(event: Event) =>
                           practice.handleAction(
                             ChangeOctaveExploded(OctaveExplode.fromBoolean(event.target.asInstanceOf[HTMLInputElement].checked))
                           )}/>
                  Middle note octave up
                </label>
              }
            </div>
          </div>
        </div>

        <hr/>
          <div class="row justify-content-center">
            { GuessUI.toUI(practice.state.guessStatus, practice.handleAction).bind }
            <div class="col-4">

              {
                val disabledClass = if (practice.state.audioEngineReady.bind) "btn-primary" else "btn-secondary disabled"

                <button id="play-chord-button" type="button" class={"btn " + disabledClass} onclick={(_: Event) => practice.handleAction(PlayCurrentChord) }>
                  Play chord
                </button>
              }
              <button class="btn btn-primary" onclick={(_: Event) => practice.handleAction(Randomize)}>
                Get new chord
              </button>

            </div>
          </div>
        <hr/>

      </form>
    </div>
  }
}
