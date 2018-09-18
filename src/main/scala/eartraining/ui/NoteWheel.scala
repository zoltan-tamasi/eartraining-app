package eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.Constants
import eartraining.state.TrichordGenerator
import eartraining._
import org.scalajs.dom.Node

object NoteWheel extends StateToUI[TrichordGenerator] {

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
  override def toUI(trichordGenerator: TrichordGenerator): Binding[Node] = {
    <div class="row justify-content-center">
      <div id="note-wheel">
        {
          Constants(getNoteWheel(trichordGenerator.state.triadCore.bind,
            trichordGenerator.state.rotation.bind,
            trichordGenerator.state.octaveExplode.bind,
            trichordGenerator.state.baseNote.bind): _*) map { case (noteName, pos, selected) =>
              <span class={s"note-label pos-${pos} ${if (selected) "selected" else ""}"}>
                {NoteName.toString(noteName)}
              </span>
            }
        }
        {
          val (image, rotation) = getImageAndRotation(trichordGenerator.state.triadCore.bind, trichordGenerator.state.rotation.bind)
          <img src={image} style={rotation}></img>
        }
      </div>
      Chord notes:
      <br/>
      {
        Chord(trichordGenerator.state.triadCore.bind,
          trichordGenerator.state.rotation.bind,
          trichordGenerator.state.octaveExplode.bind,
          Note(trichordGenerator.state.baseNote.bind.noteName, trichordGenerator.state.baseNote.bind.octave)).toString
      }
    </div>
  }
}