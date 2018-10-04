package net.zoltantamasi.eartraining

case class Note(noteName: NoteName, octave: Int) {

  def <= (that: Note) = Note.lessThanOrEqual(this, that)

  def >= (that: Note) = Note.lessThanOrEqual(that, this)

  def ++ () = Note.successor(this)

  def -- () = Note.predecessor(this)

  override def toString: String = s"${noteName}$octave"

}

object Note {

  def lessThanOrEqual(note1: Note, note2: Note): Boolean =
    Note.toInt(note1) <= Note.toInt(note2)

  def toInt(note: Note): Int =
    NoteName.toInt(note.noteName) + (note.octave * 12)

  def fromInt(noteValue: Int) =
    Note(NoteName.fromInt(noteValue % 12), noteValue / 12)

  def add(note: Note, toAdd: Int): Note =
    Function.chain(List.fill(toAdd)(successor _))(note)

  def successor(note: Note): Note = note match {
    case Note(B, octave) => Note(C, octave + 1)
    case Note(noteName, octave) => Note(NoteName.successor(noteName), octave)
  }

  def predecessor(note: Note): Note = note match {
    case Note(C, octave) => Note(B, octave - 1)
    case Note(noteName, octave) => Note(NoteName.predecessor(noteName), octave)
  }

}

sealed trait NoteName

case object C extends NoteName
case object C_# extends NoteName {
  override val toString = "C#"
}
case object D extends NoteName
case object D_# extends NoteName {
  override val toString = "D#"
}
case object E extends NoteName
case object F extends NoteName
case object F_# extends NoteName {
  override val toString = "F#"
}
case object G extends NoteName
case object G_# extends NoteName {
  override val toString = "G#"
}
case object A extends NoteName
case object A_# extends NoteName {
  override val toString = "A#"
}
case object B extends NoteName

object NoteName {

  def fromString(string: String): NoteName =
    List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#, B).find { _.toString == string }.get

  def toInt(noteName: NoteName): Int = noteName match {
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

  def fromInt(noteValue: Int): NoteName = noteValue match {
    case 0 => C
    case 1 => C_#
    case 2 => D
    case 3 => D_#
    case 4 => E
    case 5 => F
    case 6 => F_#
    case 7 => G
    case 8 => G_#
    case 9 => A
    case 10 => A_#
    case 11 => B
  }

  def add(noteName: NoteName, toAdd: Int): NoteName =
    Function.chain(List.fill(toAdd)(successor _))(noteName)

  def successor(noteName: NoteName): NoteName = noteName match {
    case C => C_#
    case C_# => D
    case D => D_#
    case D_# => E
    case E => F
    case F => F_#
    case F_# => G
    case G => G_#
    case G_# => A
    case A => A_#
    case A_# => B
    case B => C
  }

  def predecessor(noteName: NoteName): NoteName = noteName match {
    case C => B
    case C_# => C
    case D => C_#
    case D_# => D
    case E => D_#
    case F => E
    case F_# => F
    case G => F_#
    case G_# => G
    case A => G_#
    case A_# => A
    case B => A_#
  }
}

sealed trait Rotation

case object Rotation0 extends Rotation
case object Rotation1 extends Rotation
case object Rotation2 extends Rotation

object Rotation {
  def fromString(string: String): Rotation =
    List(Rotation0, Rotation1, Rotation2).find { _.toString == string }.get

  def fromInt(value: Int): Rotation =
    value match {
      case 0 => Rotation0
      case 1 => Rotation1
      case 2 => Rotation2
      case _ => throw new Exception(s"Couldn't convert ${value} to Rotation")
    }

  def toInt(rotation: Rotation): Int = rotation match {
    case Rotation0 => 0
    case Rotation1 => 1
    case Rotation2 => 2
  }
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
case object Major2PlusMinor2 extends TriadCore(1, 9, 2)
case object MinorMajor extends TriadCore(1, 3, 8)
case object MinorMajorI extends TriadCore(1, 8, 3)
case object Major7With5 extends TriadCore(1, 7, 4)
case object Major7With3 extends TriadCore(1, 4, 7)
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
case object Major extends TriadCore(3, 5, 4)
case object Augmented extends TriadCore(4, 4, 4)

object TriadCore {
  def fromString(string: String): TriadCore = allTriadTypes.find { _.toString == string }.get

  def allTriadTypes: List[TriadCore] = List(Minor, Major, Augmented, Diminished, Major7With5, Major7With3,
    Stacked4s, StackedMinor2, Minor2PlusMajor2, Major2PlusMinor2, Minor7Plus6, Minor7With3, Minor7With5, MinorMajor,
    MinorMajorI, Lyd, Locr, LydSus2, AugSus2)

  def invert(triadCore: TriadCore, rotation: Rotation): (TriadCore, Rotation) = {
    def invertRotation(rotation: Rotation): Rotation = rotation match {
      case Rotation0 => Rotation1
      case Rotation1 => Rotation0
      case Rotation2 => Rotation2
    }

    triadCore match {
      case StackedMinor2 => (StackedMinor2, rotation)
      case Minor2PlusMajor2 => (Major2PlusMinor2, invertRotation(rotation))
      case Major2PlusMinor2 => (Minor2PlusMajor2, invertRotation(rotation))
      case MinorMajor => (MinorMajorI, invertRotation(rotation))
      case MinorMajorI => (MinorMajor, invertRotation(rotation))
      case Major7With5 => (Major7With3, invertRotation(rotation))
      case Major7With3 => (Major7With5, invertRotation(rotation))
      case Lyd => (Locr, invertRotation(rotation))
      case Locr => (Lyd, invertRotation(rotation))
      case Minor7Plus6 => (Minor7Plus6, rotation)
      case Minor7With3 => (Minor7With5, invertRotation(rotation))
      case Minor7With5 => (Minor7With3, invertRotation(rotation))
      case LydSus2 => (AugSus2, invertRotation(rotation))
      case AugSus2 => (LydSus2, invertRotation(rotation))
      case Stacked4s => (Stacked4s, rotation)
      case Diminished => (Diminished, rotation)
      case Minor => (Major, invertRotation(rotation))
      case Major => (Minor, invertRotation(rotation))
      case Augmented => (Augmented, rotation)
    }
  }
}
