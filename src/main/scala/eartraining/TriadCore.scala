package eartraining

case class Note(noteName: NoteName, octave: Int) {

  def <= (that: Note) = Note.lessThanOrEqual(this, that)

  def >= (that: Note) = Note.lessThanOrEqual(that, this)

  def ++ () = Note.successor(this)

  def -- () = Note.predecessor(this)

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

  def lessThanOrEqual(note1: Note, note2: Note): Boolean = {
    Note.asInt(note1) <= Note.asInt(note2)
  }

  def asInt(note: Note): Int = {
    val noteValue = note.noteName match {
      case C => 0
      case C_# => 1
      case D => 2
      case D_# => 3
      case E => 4
      case F => 5
      case F_# => 6
      case G => 7
      case G_# => 8
      case A => 9
      case A_# => 10
      case B => 11
    }
    noteValue + (note.octave * 12)
  }

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

  def predecessor(note: Note): Note = note.noteName match {
    case C => Note(B, note.octave - 1)
    case C_# => Note(C, note.octave)
    case D => Note(C_#, note.octave)
    case D_# => Note(D, note.octave)
    case E => Note(D_#, note.octave)
    case F => Note(E, note.octave)
    case F_# => Note(F, note.octave)
    case G => Note(F_#, note.octave)
    case G_# => Note(G, note.octave)
    case A => Note(G_#, note.octave)
    case A_# => Note(A, note.octave)
    case B => Note(A_#, note.octave)
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

object NoteName {
  def fromString(string: String): NoteName =
    List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B).find { _.toString == string }.get
}

sealed trait Rotation

case object Rotation0 extends Rotation
case object Rotation1 extends Rotation
case object Rotation2 extends Rotation

object Rotation {
  def fromString(string: String): Rotation =
    List(Rotation0, Rotation1, Rotation2).find { _.toString == string }.get
}

sealed trait OctaveExplode
case object OctaveExploded extends OctaveExplode
case object NotOctaveExploded extends OctaveExplode

object OctaveExplode {
  def fromBoolean(enabled: Boolean): OctaveExplode = if (enabled) OctaveExploded else NotOctaveExploded
}

case class Chord(core: TriadCore, rotation: Rotation, octaveExplode: OctaveExplode, baseNote: Note) {
  override def toString: String = s"(" + Chord.notesOf(this).mkString(",") + ")"
}

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

sealed abstract class TriadCore(val intervals: (Int, Int, Int)) {
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
  def fromString(string: String): TriadCore = allTriadTypes.find { _.toString == string }.get

  def allTriadTypes: List[TriadCore] = List(Minor, Major, Augmented, Diminished, Major7Without5, Major7Without3,
    Stacked4s, StackedMinor2, Minor2PlusMajor2, Major2PlusMinor2, Minor7Plus6, Minor7With3, Minor7With5, MinorMajor,
    MinorMajorI, Lyd, Locr, LydSus2, AugSus2)
}
