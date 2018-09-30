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
  def buttonForTricore(triadCore: TriadCore): Binding[Node] = {
    <button class="btn btn-outline-secondary">
      {triadCore.label}<img src={triadCore.getImage}></img>
    </button>
  }

  @dom
  def buttonForTricores(triadCore1: TriadCore, triadCore2: TriadCore): Binding[Node] = {
    <button class="btn btn-outline-secondary either">
      Either
    </button>
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
            <div class="col-8" id="guess-chord">
              <div class="row justify-content-center">
                {buttonForTricore(StackedMinor2).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Minor2PlusMajor2).bind}
                {buttonForTricores(Minor2PlusMajor2, Major2PlusMinor2).bind}
                {buttonForTricore(Major2PlusMinor2).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(MinorMajor).bind}
                {buttonForTricores(MinorMajor, MinorMajorI).bind}
                {buttonForTricore(MinorMajorI).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Major7Without3).bind}
                {buttonForTricores(Major7Without3, Major7Without5).bind}
                {buttonForTricore(Major7Without5).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Lyd).bind}
                {buttonForTricores(Lyd, Locr).bind}
                {buttonForTricore(Locr).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Minor7Plus6).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Minor7With3).bind}
                {buttonForTricores(Minor7With3, Minor7With5).bind}
                {buttonForTricore(Minor7With5).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(LydSus2).bind}
                {buttonForTricores(LydSus2, AugSus2).bind}
                {buttonForTricore(AugSus2).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Stacked4s).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Diminished).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Minor).bind}
                {buttonForTricores(Minor, Major).bind}
                {buttonForTricore(Major).bind}
              </div>
              <div class="row justify-content-center">
                {buttonForTricore(Augmented).bind}
              </div>
            </div>
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
