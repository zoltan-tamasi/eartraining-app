package eartraining.ui

import com.thoughtworks.binding.Binding.Constants
import com.thoughtworks.binding.{Binding, dom}
import eartraining._
import eartraining.flow._
import org.scalajs.dom.{Event, Node}
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}

object TrichordGeneratorUI extends StateToUI[TrichordGenerator] {

  @dom
  def toUI(trichordGenerator: TrichordGenerator): Binding[Node] = {
    <div>

      Rotations
      <select onchange={(event: Event) =>
        val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
        trichordGenerator.state.rotation := Rotation.fromString(selectedValue) }>
        <option value={Rotation0.toString} selected={ trichordGenerator.state.rotation.bind == Rotation0 }>1st position</option>
        <option value={Rotation1.toString} selected={ trichordGenerator.state.rotation.bind == Rotation1 }>2nd position</option>
        <option value={Rotation2.toString} selected={ trichordGenerator.state.rotation.bind == Rotation2 }>3rd position</option>
      </select>

      Octave extraction
      <input type="checkbox"
             checked={trichordGenerator.state.octaveExplode.bind == OctaveExploded}
             onchange={(event: Event) =>
               if (event.target.asInstanceOf[HTMLInputElement].checked) {
                 trichordGenerator.state.octaveExplode := OctaveExploded
               } else {
                 trichordGenerator.state.octaveExplode := NotOctaveExploded
               }}>
      </input>

      Triad core
      <select onchange={(event: Event) =>
        val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
        trichordGenerator.state.triadCore := TriadCore.fromString(selectedValue) }>
        {
          Constants(TriadCore.allTriadTypes: _*) map { triadCore =>
            <option selected={ trichordGenerator.state.triadCore.bind == triadCore } value={triadCore.toString}>
              {triadCore.label}
            </option>
          }
        }
      </select>

      Base note
      <select onchange={(event: Event) =>
        val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
        trichordGenerator.state.baseNote := Note(NoteName.fromString(selectedValue), trichordGenerator.state.baseNote.get.octave) }>
        {
          Constants(List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B): _*) map { note =>
            <option selected={trichordGenerator.state.baseNote.bind.noteName == note} value={note.toString}>
              {note.toString}
            </option>
          }
        }
      </select>

      Octave
      <select onchange={(event: Event) =>
        val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
        trichordGenerator.state.baseNote := Note(trichordGenerator.state.baseNote.get.noteName, selectedValue.toInt) }>
        {
          Constants(List(2, 3, 4): _*) map { octave =>
            <option selected={trichordGenerator.state.baseNote.bind.octave == octave} value={octave.toString}>
              {octave.toString}
            </option>
          }
        }
      </select>

      <hr/>

        Chord notes: <br />
        {
          Chord(trichordGenerator.state.triadCore.bind,
                trichordGenerator.state.rotation.bind,
                trichordGenerator.state.octaveExplode.bind,
                Note(trichordGenerator.state.baseNote.bind.noteName, trichordGenerator.state.baseNote.bind.octave)).toString
        }

      <hr/>

      <button onclick={(_: Event) => trichordGenerator.playChord()}>
        Play Chord
      </button>
      <button onclick={(_: Event) => trichordGenerator.randomize()}>
        Randomize
      </button>

      <hr/>

      <button onclick={(_: Event) => trichordGenerator.back()}>
        Back
      </button>


    </div>

  }
}
