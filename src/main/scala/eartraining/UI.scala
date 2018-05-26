package eartraining

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, Node}
import org.scalajs.dom.raw.HTMLInputElement

trait Labeler[A] {
  def label(in : A): String
}

object UI {

  implicit class LabelerOps[T](data: T) {
    def label(implicit labeler: Labeler[T]) =
      labeler.label(data)
  }

  implicit object TriadCoreLabeler extends Labeler[TriadCore] {
    override def label(in: TriadCore): String = {
      in match {
        case StackedMinor2 => "m2 + m2 (1, 1, 10)"
        case Minor2PlusMajor2 => "m2 + M2 (1, 2, 9)"
        case Major2PlusMinor2 => "M2 + m2 (1, 9, 2)"
        case MinorMajor => "minor/major 1 (1, 3, 8)"
        case MinorMajorI => "minor/major 2 (1, 8, 3)"
        case Lyd => "lydian (1, 5, 6)"
        case Locr => "locrian (1, 6, 5)"
        case Minor7Plus6 => "minor 7 + 6 (2, 2, 8)"
        case Minor7With3 => "minor 7 + 3rd (2, 3, 7)"
        case Minor7With5 => "minor 7 + 5th (2, 7, 3)"
        case LydSus2 => "lydian + 2 (2, 4, 6)"
        case AugSus2 => "augmented + 2 (2, 6, 4)"
        case Minor => "minor (3, 4, 5)"
        case Major => "major (3, 5, 4)"
        case Augmented => "augmented (4, 4, 4)"
        case Diminished => "diminished (3, 3, 6)"
        case Major7Without5 => "major 7 + 3rd (1, 7, 4)"
        case Major7Without3 => "major 7 + 5th (1, 4, 7)"
        case Stacked4s => "stacked 4ths/5ths (2, 5, 5)"
      }
    }
  }

  @dom
  def triadCoreSelection(triadCoreSet: Var[Set[TriadCore]], triadCore: TriadCore): Binding[Node] = {
    <td>
      <button onclick = { (_ : Event) =>
        Flow.guessed := (if (triadCore == Flow.chord.get.core) Flow.GuessedCorrectly else Flow.GuessedWrong)
              }>
        {
          triadCore.label
        }
      </button>
      <input type="checkbox"
             checked={ triadCoreSet.bind.contains(triadCore) }
             onchange={ (event: Event) =>
               if (event.target.asInstanceOf[HTMLInputElement].checked) {
                 triadCoreSet := triadCoreSet.get + triadCore
               } else {
                 triadCoreSet := triadCoreSet.get - triadCore
               }
             }>
      </input>
    </td>
  }

  @dom
  def UI(flow: Flow.type): Binding[Node] = {
    flow.status.bind match {

      case Init =>

        <div>
          Initializing...
        </div>

      case Query =>

        <div>
          <table>
            <tbody>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, StackedMinor2).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Minor2PlusMajor2).bind }
                { triadCoreSelection(flow.triadCoreSet, Major2PlusMinor2).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, MinorMajor).bind }
                { triadCoreSelection(flow.triadCoreSet, MinorMajorI).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Major7Without3).bind }
                { triadCoreSelection(flow.triadCoreSet, Major7Without5).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Lyd).bind }
                { triadCoreSelection(flow.triadCoreSet, Locr).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Minor7Plus6).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Minor7With3).bind }
                { triadCoreSelection(flow.triadCoreSet, Minor7With5).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, LydSus2).bind }
                { triadCoreSelection(flow.triadCoreSet, AugSus2).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Stacked4s).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Diminished).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Minor).bind }
                { triadCoreSelection(flow.triadCoreSet, Major).bind }
              </tr>
              <tr>
                { triadCoreSelection(flow.triadCoreSet, Augmented).bind }
              </tr>
            </tbody>
          </table>

          <hr />

          Rotations
          <input type="checkbox"
                 checked={ flow.rotations.bind }
                 onchange={ (event: Event) =>
                   if (event.target.asInstanceOf[HTMLInputElement].checked) {
                     flow.rotations := true
                   } else {
                     flow.rotations := false
                   }
                 }>
          </input>

          Octave extraction
          <input type="checkbox"
                 checked={ flow.octaveExtraction.bind }
                 onchange={ (event: Event) =>
                   if (event.target.asInstanceOf[HTMLInputElement].checked) {
                     flow.octaveExtraction := true
                   } else {
                     flow.octaveExtraction := false
                   }
                 }>
          </input>

          <hr />

          <button onclick={(_: Event) => Flow.playChord() }>
            Play Chord
          </button>

          <button onclick = { (_: Event) => Flow.next() }>
            Next
          </button>

          <hr />

          {
            Flow.guessed.bind match {
              case Flow.GuessedCorrectly => <span style="color: green">Correct</span>
              case Flow.GuessedWrong => <span style="color: red">Incorrect</span>
              case Flow.NotGuessed => <span></span>
            }
          }

        </div>
    }
  }
}