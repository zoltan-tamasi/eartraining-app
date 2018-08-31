package eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import eartraining._
import eartraining.state._
import org.scalajs.dom.{Event, Node}
import org.scalajs.dom.raw.HTMLInputElement



object QueryUI extends StateToUI[Query] {

  @dom
  def triadCoreSelection(triadCore: TriadCore, query: Query): Binding[Node] = {
    <td>
      <button onclick = { (_ : Event) => query.handleAction(DoGuess(triadCore)) }>
        { triadCore.label }
      </button>
      <input type="checkbox"
             checked={ query.stateContainer.selectedTriadCoreSet.bind.contains(triadCore) }
             onchange={(event: Event) =>
               query.handleAction(ChangeTriadCoreSelection(triadCore, event.target.asInstanceOf[HTMLInputElement].checked))
             }>
      </input>
    </td>
  }

  @dom
  def toUI(query: Query): Binding[Node] = {
    <div>
      <table>
        <tbody>
          <tr>
            {triadCoreSelection(StackedMinor2, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Minor2PlusMajor2, query).bind}{triadCoreSelection(Major2PlusMinor2, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(MinorMajor, query).bind}{triadCoreSelection(MinorMajorI, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Major7Without3, query).bind}{triadCoreSelection(Major7Without5, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Lyd, query).bind}{triadCoreSelection(Locr, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Minor7Plus6, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Minor7With3, query).bind}{triadCoreSelection(Minor7With5, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(LydSus2, query).bind}{triadCoreSelection(AugSus2, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Stacked4s, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Diminished, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Minor, query).bind}{triadCoreSelection(Major, query).bind}
          </tr>
          <tr>
            {triadCoreSelection(Augmented, query).bind}
          </tr>
        </tbody>
      </table>

      <hr/>

      Rotations
      <input type="checkbox"
             checked={query.stateContainer.rotationsEnabled.bind}
             onchange={(event: Event) =>
               query.handleAction(ChangeRotationSelection(event.target.asInstanceOf[HTMLInputElement].checked))
             }>
      </input>

      Octave extraction
      <input type="checkbox"
             checked={query.stateContainer.octaveExplodeEnabled.bind}
             onchange={(event: Event) =>
               query.handleAction(ChangeOctaveExtractionSelection(event.target.asInstanceOf[HTMLInputElement].checked))
             }>
      </input>

      <hr/>

      <button onclick={(_: Event) => query.handleAction(PlayChord(query.stateContainer.actualChord.get)) }>
        Play Chord
      </button>

      <button onclick={(_: Event) => query.handleAction(Next) }>
        Next
      </button>

      <hr/>
      {
        query.stateContainer.guessed.bind match {
          case GuessedCorrectly => <span style="color: green">Correct</span>
          case GuessedWrong => <span style="color: red">Incorrect</span>
          case NotGuessed => <span></span>
        }
      }
      <hr/>

      <button onclick={(_: Event) => query.handleAction(BackToMenu) }>
        Back
      </button>

    </div>

  }
}
