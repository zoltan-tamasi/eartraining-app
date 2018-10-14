package net.zoltantamasi.eartraining.ui.common

import com.thoughtworks.binding.Binding.Constants
import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state.RootAction
import net.zoltantamasi.eartraining.state.generator.PlayCurrentChord
import net.zoltantamasi.eartraining.ui.UIHelpers
import org.scalajs.dom.{Event, Node}

object NoteWheel extends UIHelpers {

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
  def toUI(triadCore: Binding[TriadCore], rotationBinding: Binding[Rotation], octaveExplode: Binding[OctaveExplode], baseNote: Binding[Note],
                    audioEngineReady: Binding[Boolean], handleAction: (RootAction) => Unit): Binding[Node] = {
    <div class="row justify-content-center">
      <div id="note-wheel" class="col-lg-4 col-md-8 col-sm-12">
        {
          Constants(getNoteWheel(triadCore.bind,
            rotationBinding.bind,
            octaveExplode.bind,
            baseNote.bind): _*) map { case (noteName, pos, selected) =>
              <span class={s"note-label pos-${pos} ${if (selected) "selected" else ""}"}>
                {noteName.toString}
              </span>
            }
        }
        {
          val (image, rotation) = getImageAndRotation(triadCore.bind, rotationBinding.bind)
          <img src={image} style={rotation}></img>
        }
      </div>
      <div id="chord-notes" class="col-4">
        Semitone-intervals:
        <br/>
        {
          Chord.intervals(Chord(triadCore.bind,
            rotationBinding.bind,
            octaveExplode.bind,
            Note(baseNote.bind.noteName, baseNote.bind.octave))).mkString("(",  ",", ")")
        }
        <br/>
        Chord notes:
        <br/>
        {
          Chord(triadCore.bind,
            rotationBinding.bind,
            octaveExplode.bind,
            Note(baseNote.bind.noteName, baseNote.bind.octave)).toString
        }
        <br/>
        {
          val disabledClass = if (audioEngineReady.bind) "btn-primary" else "btn-secondary disabled"

          <button id="play-chord-button" type="button" class={"btn " + disabledClass} onclick={(_: Event) => handleAction(PlayCurrentChord) }>
            Play chord
          </button>
        }
      </div>
    </div>
  }
}