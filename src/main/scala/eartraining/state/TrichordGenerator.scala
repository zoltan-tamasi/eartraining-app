package eartraining.state

import com.thoughtworks.binding.Binding.Var
import eartraining._
import eartraining.ui._

import scala.util.Random

sealed trait TrichordGeneratorAction extends RootAction
case object Randomize extends TrichordGeneratorAction
case object PlayCurrentChord extends TrichordGeneratorAction
case object BackToMenu extends TrichordGeneratorAction
case class ChangeRotation(rotation: Rotation) extends TrichordGeneratorAction
case class ChangeOctaveExploded(enabled: OctaveExplode) extends TrichordGeneratorAction
case class ChangeTriadCore(triadCore: TriadCore) extends TrichordGeneratorAction
case class ChangeBaseNote(noteName: NoteName) extends TrichordGeneratorAction
case class ChangeOctave(octave: Int) extends TrichordGeneratorAction

case class TrichordGeneratorState(rotation: Var[Rotation],
                                  octaveExplode: Var[OctaveExplode],
                                  triadCore: Var[TriadCore],
                                  baseNote: Var[Note],
                                  imageAndRotation: Var[(String, String)],
                                  noteWheelPositions: Var[List[(NoteName, Int, Boolean)]])

case class TrichordGenerator(delegator: RootAction => Root) extends RootOption {

  val state = TrichordGeneratorState(
    rotation = Var(Rotation0),
    octaveExplode = Var(NotOctaveExploded),
    triadCore = Var(Major),
    baseNote = Var(Note(C, 3)),
    imageAndRotation = Var(("3-5-4", "0")),
    noteWheelPositions = Var(List((C, 0, true), (C_#, 1, false), (D, 2, false), (D_#, 3, false),
      (E, 4, true), (F, 5, false), (F_#, 6, false), (G, 7, true),
      (G_#, 8, false), (A, 9, false), (A_#, 10, false), (B, 11, false)))
  )

  def handleAction(action: RootAction): TrichordGenerator = {
    action match {
      case Randomize =>
        state.rotation := pullRandom(List(Rotation0, Rotation1, Rotation2))
        state.octaveExplode := pullRandom(List(OctaveExploded, NotOctaveExploded))
        state.triadCore := pullRandom(TriadCore.allTriadTypes)
        state.baseNote := pullRandom(for {
          noteName <- List(C, C_#, D, D_#, E, F, F_#, G, G_#, A, A_#)
          octave <- List(2, 3, 4)
        } yield Note(noteName, octave))
        state.noteWheelPositions := getNoteWheel(state)
        this

      case ChangeRotation(rotation: Rotation) =>
        state.rotation := rotation
        state.imageAndRotation := getImageAndRotation(state)
        state.noteWheelPositions := getNoteWheel(state)
        this

      case ChangeOctaveExploded(enabled: OctaveExplode) =>
        state.octaveExplode := enabled
        this

      case ChangeTriadCore(triadCore: TriadCore) =>
        state.triadCore := triadCore
        state.imageAndRotation := getImageAndRotation(state)
        state.noteWheelPositions := getNoteWheel(state)
        this

      case ChangeBaseNote(noteName: NoteName) =>
        state.baseNote := Note(noteName, state.baseNote.get.octave)
        state.noteWheelPositions := getNoteWheel(state)
        this

      case ChangeOctave(octave: Int) =>
        state.baseNote := Note(state.baseNote.get.noteName, octave)
        this

      case PlayCurrentChord =>
        delegator(PlayChord(Chord(state.triadCore.get, state.rotation.get, state.octaveExplode.get, state.baseNote.get)))
        this

      case action: RootAction =>
        delegator(action)
        this
    }
  }

  private def pullRandom[T](list: Seq[T]): T = list(Random.nextInt(list.length))

  private def getImageAndRotation(state: TrichordGeneratorState): (String, String) = {
    val triadCore: TriadCore = state.triadCore.get
    val image = s"${triadCore match {
      case StackedMinor2 => "1-1-10"
      case Minor2PlusMajor2 => "1-2-9"
      case Major2PlusMinor2 => "1-9-2"
      case MinorMajor => "1-3-8"
      case MinorMajorI => "1-8-3"
      case Lyd => "1-5-6"
      case Locr => "1-6-5"
      case Minor7Plus6 => "2-2-8"
      case Minor7With3 => "2-3-7"
      case Minor7With5 => "2-7-3"
      case LydSus2 => "2-4-6"
      case AugSus2 => "2-6-4"
      case Minor => "3-4-5"
      case Major => "3-5-4"
      case Augmented => "4-4-4"
      case Diminished => "3-3-6"
      case Major7Without5 => "1-7-4"
      case Major7Without3 => "1-4-7"
      case Stacked4s => "2-5-5"
    }}.png"
    val rotation = state.rotation.get match {
      case Rotation0 => 0
      case Rotation1 => triadCore.intervals._1 * 30
      case Rotation2 => (triadCore.intervals._1 + triadCore.intervals._2) * 30
    }

    (image, s"transform: rotate(${rotation}deg)")
  }

  private def getNoteWheel(state: TrichordGeneratorState): List[(NoteName, Int, Boolean)] = {

    val chord = Chord(state.triadCore.get, state.rotation.get, state.octaveExplode.get, state.baseNote.get)
    val wheel = Range(0, 12).toList
    val noteName = state.baseNote.get.noteName

    val noteList = Function.chain(List.fill(11)((list: List[NoteName]) => list :+ NoteName.successor(list.last)))(List(noteName))

    (noteList zip wheel).map { case (noteName, position) =>
        if (Chord.notesOf(chord).map(note => note.noteName).contains(noteName)) {
          (noteName, position, true)
        } else {
          (noteName, position, false)
        }
    }
  }

}
