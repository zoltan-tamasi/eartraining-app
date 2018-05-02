package eartraining

import eartraining.WebApp.AudioEngine

case class Note(noteName: NoteName, octave: Int) {
  override def toString: String = {
    val noteNameString = this.noteName match {
      case C => "C"
      case C_# => "C#"
      case D => "D"
      case D_# => "D#"
      case E => "E"
      case F => "F"
      case F_# => "F#"
      case G => "G"
      case G_# => "G#"
      case A => "A"
      case A_# => "A#"
      case B => "B"
    }
    s"${noteNameString}$octave"
  }
}

object Note {

  def add(note: Note, toAdd: Int): Note = {
    Function.chain(List.fill(toAdd)(noteToAdd => Note.successor(noteToAdd)))(note)
  }

  def successor(note: Note): Note = note.noteName match {
    case C => Note(C_#, note.octave)
    case C_# => Note(D, note.octave)
    case D => Note(D_#, note.octave)
    case D_# => Note(E, note.octave)
    case E => Note(F, note.octave)
    case F => Note(F_#, note.octave)
    case F_# => Note(G, note.octave)
    case G => Note(G_#, note.octave)
    case G_# => Note(A, note.octave)
    case A => Note(A_#, note.octave)
    case A_# => Note(B, note.octave)
    case B => Note(C, note.octave + 1)
  }

  def playbackRate(note: NoteName): Double = note match {
    case C => 1.0
    case C_# => 1.059463
    case D => 1.122462
    case D_# => 1.189207
    case E => 1.259921
    case F => 1.334840
    case F_# => 1.414214
    case G => 1.498307
    case G_# => 1.587401
    case A => 1.681793
    case A_# => 1.781797
    case B => 1.887749
  }

  def playSound(note: Note)(implicit audioEngine: AudioEngine) = {
    val context = audioEngine.context
    val audioBuffers = audioEngine.audioBuffers
    val source = context.createBufferSource()
    source.buffer = note.octave match {
      case 2 => audioBuffers(0)
      case 3 => audioBuffers(1)
      case 4 => audioBuffers(2)
      case 5 => audioBuffers(3)
      case 6 => {
        println(s"Not supported octave: 6, playing octave 5")
        audioBuffers(3)
      }
    }
    source.connect(context.destination)
    source.playbackRate.value = playbackRate(note.noteName)
    source.start(0)
  }

}

sealed trait NoteName

case object C extends NoteName
case object C_# extends NoteName
case object D extends NoteName
case object D_# extends NoteName
case object E extends NoteName
case object F extends NoteName
case object F_# extends NoteName
case object G extends NoteName
case object G_# extends NoteName
case object A extends NoteName
case object A_# extends NoteName
case object B extends NoteName
