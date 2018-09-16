package eartraining.ui

import com.thoughtworks.binding.Binding.Constants
import com.thoughtworks.binding.{Binding, dom}
import eartraining._
import eartraining.state._
import org.scalajs.dom.{Event, Node}
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}

object TrichordGeneratorUI extends StateToUI[TrichordGenerator] {

  private def getImageAndRotation(triadCore: TriadCore, rotation: Rotation): (String, String) = {
    val imageRotation = rotation match {
      case Rotation0 => 0
      case Rotation1 => triadCore.intervals._1 * -30
      case Rotation2 => (triadCore.intervals._1 + triadCore.intervals._2) * -30
    }

    (triadCore.getImage, s"transform: rotate(${imageRotation}deg)")
  }

  private def getNoteWheel(triadCore: TriadCore, rotation: Rotation, octaveExplode: OctaveExplode, baseNote: Note): List[(NoteName, Int, Boolean)] = {

    val chord = Chord(triadCore, rotation, octaveExplode, baseNote)
    val noteList = Function.chain(List.fill(11)((list: List[NoteName]) => list :+ NoteName.successor(list.last)))(List(baseNote.noteName))

    (noteList zip Range(0, 12)).map { case (noteName, position) =>
      if (Chord.notesOf(chord).map(note => note.noteName).contains(noteName)) {
        (noteName, position, true)
      } else {
        (noteName, position, false)
      }
    }
  }

  @dom
  def toUI(trichordGenerator: TrichordGenerator): Binding[Node] = {
    <div>
      <form onsubmit={(event:Event) => event.preventDefault()}>
        <div class="row">
          <div class="col">
            <div class="form-group">
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
          <div class="col">
            <div class="form-group">
              <label for="baseNoteSelect">Base note</label>
              <select class="form-control" id="baseNoteSelect" onchange={(event: Event) =>
                val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
                trichordGenerator.handleAction(ChangeBaseNote(NoteName.fromString(selectedValue)))
              }>
                {
                Constants(List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B): _*) map { note =>
                  <option selected={trichordGenerator.state.baseNote.bind.noteName == note} value={note.toString}>
                    {note.toString}
                  </option>
                }
                }
              </select>
            </div>
          </div>
          <div class="col">
            <div class="form-group">
              <label for="octaveSelect">Octave</label>
              <select class="form-control" id="octaveSelect" onchange={(event: Event) =>
                val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
                trichordGenerator.handleAction(ChangeOctave(selectedValue.toInt))
              }>
                {
                Constants(List(2, 3, 4): _*) map { octave =>
                  <option selected={trichordGenerator.state.baseNote.bind.octave == octave} value={octave.toString}>
                    {octave.toString}
                  </option>
                }
                }
              </select>
            </div>
          </div>
        </div>
        <div class="row">
          <div class="col">
            <div class="form-group">
              <label for="rotationSelect">Rotations</label>
              <select class="form-control" id="rotationSelect" onchange={(event: Event) =>
                val selectedValue = event.target.asInstanceOf[HTMLSelectElement].value
                trichordGenerator.handleAction(ChangeRotation(Rotation.fromString(selectedValue)))
              }>
                <option value={Rotation0.toString} selected={ trichordGenerator.state.rotation.bind == Rotation0 }>1st position</option>
                <option value={Rotation1.toString} selected={ trichordGenerator.state.rotation.bind == Rotation1 }>2nd position</option>
                <option value={Rotation2.toString} selected={ trichordGenerator.state.rotation.bind == Rotation2 }>3rd position</option>
              </select>
            </div>
          </div>
          <div class="col">
            Octave extraction
            <input type="checkbox"
                   checked={trichordGenerator.state.octaveExplode.bind == OctaveExploded}
                   onchange={(event: Event) =>
                     trichordGenerator.handleAction(
                       ChangeOctaveExploded(OctaveExplode.fromBoolean(event.target.asInstanceOf[HTMLInputElement].checked))
                     )
                   }>
            </input>
          </div>
        </div>

        <hr/>

        <div class="row justify-content-center">
          <div id="note-wheel">
            {
              Constants(getNoteWheel(trichordGenerator.state.triadCore.bind,
                trichordGenerator.state.rotation.bind,
                trichordGenerator.state.octaveExplode.bind,
                trichordGenerator.state.baseNote.bind): _*) map { case (noteName, pos, selected) =>
                <span class={ s"note-label pos-${pos} ${if (selected) "selected" else ""}"}> {NoteName.toString(noteName)} </span>
              }
            }
            {
              val (image, rotation) = getImageAndRotation(trichordGenerator.state.triadCore.bind, trichordGenerator.state.rotation.bind)
              <img src={image} style={rotation}></img>
            }
          </div>
          Chord notes: <br />
          {
            Chord(trichordGenerator.state.triadCore.bind,
                  trichordGenerator.state.rotation.bind,
                  trichordGenerator.state.octaveExplode.bind,
                  Note(trichordGenerator.state.baseNote.bind.noteName, trichordGenerator.state.baseNote.bind.octave)).toString
          }
        </div>

        <hr/>

        <div class="row justify-content-center">
          <button onclick={(event: Event) => trichordGenerator.handleAction(PlayCurrentChord)   }>
            Play Chord
          </button>
          <button onclick={(_: Event) => trichordGenerator.handleAction(Randomize) }>
            Randomize
          </button>
        </div>

        <hr/>

        <div class="row justify-content-center">
          <button onclick={(_: Event) => trichordGenerator.handleAction(BackToMenu)}>
            Back
          </button>
        </div>
      </form>
    </div>

  }
}
