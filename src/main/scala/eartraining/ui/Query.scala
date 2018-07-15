package eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import eartraining._
import eartraining.flow._
import org.scalajs.dom.{Event, Node}
import org.scalajs.dom.raw.HTMLInputElement



object QueryUI extends StateToUI[Query] {

  @dom
  def triadCoreSelection(triadCore: TriadCore, query: Query): Binding[Node] = {
    <td>
      <button onclick = { (_ : Event) => query.doGuess(triadCore) }>
        {
        triadCore.label
        }
      </button>
      <input type="checkbox"
             checked={ query.state.selectedTriadCoreSet.bind.contains(triadCore) }
             onchange={ (event: Event) =>
               if (event.target.asInstanceOf[HTMLInputElement].checked) {
                 query.state.selectedTriadCoreSet := query.state.selectedTriadCoreSet.get + triadCore
               } else {
                 query.state.selectedTriadCoreSet := query.state.selectedTriadCoreSet.get - triadCore
               }
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
             checked={query.state.rotationsEnabled.bind}
             onchange={(event: Event) =>
               if (event.target.asInstanceOf[HTMLInputElement].checked) {
                 query.state.rotationsEnabled := true
               } else {
                 query.state.rotationsEnabled := false
               }}>
      </input>

      Octave extraction
      <input type="checkbox"
             checked={query.state.octaveExplodeEnabled.bind}
             onchange={(event: Event) =>
               if (event.target.asInstanceOf[HTMLInputElement].checked) {
                 query.state.octaveExplodeEnabled := true
               } else {
                 query.state.octaveExplodeEnabled := false
               }}>
      </input>

      <hr/>

      <button onclick={(_: Event) => query.playChord(query.state.actualChord.get)}>
        Play Chord
      </button>

      <button onclick={(_: Event) => query.next()}>
        Next
      </button>

      <hr/>
      {
        query.state.guessed.bind match {
          case GuessedCorrectly => <span style="color: green">Correct</span>
          case GuessedWrong => <span style="color: red">Incorrect</span>
          case NotGuessed => <span></span>
        }
      }
      <hr/>

      <button onclick={(_: Event) => query.back()}>
        Back
      </button>

    </div>

  }
}
