package eartraining

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
    Function.chain(List.fill(toAdd)(successor _))(note)
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

sealed trait Rotation

case object Rotation0 extends Rotation
case object Rotation1 extends Rotation
case object Rotation2 extends Rotation

sealed trait OctaveExplode
case object OctaveExploded extends OctaveExplode
case object NotOctaveExploded extends OctaveExplode

case class Chord(core: TriadCore, rotation: Rotation, octaveExplode: OctaveExplode, baseNote: Note)

object Chord {
  def notesOf(chord: Chord): List[Note] = {
    chord.rotation match {
      case Rotation0 => {
        val note1 = chord.baseNote
        var note2 = Note.add(note1, chord.core.intervals._1)
        val note3 = Note.add(note2, chord.core.intervals._2)
        if (chord.octaveExplode == OctaveExploded) {
          note2 = Note.add(note2, 12)
        }
        List(note1, note2, note3)
      }
      case Rotation1 => {
        val note1 = chord.baseNote
        var note2 = Note.add(note1, chord.core.intervals._2)
        val note3 = Note.add(note2, chord.core.intervals._3)
        if (chord.octaveExplode == OctaveExploded) {
          note2 = Note.add(note2, 12)
        }
        List(note1, note2, note3)
      }
      case Rotation2 => {
        val note1 = chord.baseNote
        var note2 = Note.add(note1, chord.core.intervals._3)
        val note3 = Note.add(note2, chord.core.intervals._1)
        if (chord.octaveExplode == OctaveExploded) {
          note2 = Note.add(note2, 12)
        }
        List(note1, note2, note3)
      }
    }
  }
}

sealed class TriadCore(val intervals: (Int, Int, Int)) {
  assert(intervals._1 + intervals._2 + intervals._3 == 12, "TriadCore must be 3 integers summing up to 12")
}

case object StackedMinor2 extends TriadCore(1, 1, 10)
case object Minor2PlusMajor2 extends TriadCore(1, 2, 9)
case object Major2PlusMinor2 extends TriadCore(2, 1, 9)
case object MinorMajor extends TriadCore(1, 3, 8)
case object MinorMajorI extends TriadCore(3, 1, 8)
case object Major7Without5 extends TriadCore(1, 4, 7)
case object Major7Without3 extends TriadCore(1, 7, 4)
case object Lyd extends TriadCore(1, 5, 6)
case object Locr extends TriadCore(1, 6, 5)
case object Minor7Plus6 extends TriadCore(2, 2, 8)
case object Minor7With3 extends TriadCore(2, 3, 7)
case object Minor7With5 extends TriadCore(2, 7, 3)
case object LydSus2 extends TriadCore(2, 4, 6)
case object AugSus2 extends TriadCore(2, 6, 4)
case object Stacked4s extends TriadCore(2, 5, 5)
case object Diminished extends TriadCore(3, 3, 6)
case object Minor extends TriadCore(3, 4, 5)
case object Major extends TriadCore(4, 3, 5)
case object Augmented extends TriadCore(4, 4, 4)

object TriadCore {
  def allTriadTypes = List(Minor, Major, Augmented, Diminished, Major7Without5, Major7Without3, Stacked4s, StackedMinor2,
    Minor2PlusMajor2, Major2PlusMinor2, Minor7Plus6, Minor7With3, Minor7With5, MinorMajor, MinorMajorI, Lyd, Locr, LydSus2, AugSus2)
}
