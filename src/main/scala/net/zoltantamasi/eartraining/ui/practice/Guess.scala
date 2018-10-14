package net.zoltantamasi.eartraining.ui.practice

import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state.RootAction
import net.zoltantamasi.eartraining.state.practice._
import net.zoltantamasi.eartraining.ui.UIHelpers
import org.scalajs.dom.Node
import org.scalajs.dom.raw.Event

object GuessUI extends UIHelpers {

  @dom
  def guessButton(triadCore: TriadCore, handleAction: (RootAction) => Unit): Binding[Node] = {
    <button class="btn btn-outline-secondary" onclick={(_:Event) => handleAction(UserGuessed(triadCore))}>
      {triadCore.label}<img src={triadCore.getImage}></img>
    </button>
  }

  @dom
  def eitherButton(triadCore1: TriadCore, triadCore2: TriadCore, handleAction: (RootAction) => Unit): Binding[Node] = {
    <button class="btn btn-outline-secondary either" onclick={(_:Event) => handleAction(UserGuessedEither(triadCore1, triadCore2))}>
      Either
    </button>
  }

  @dom
  def successButton(triadCore: TriadCore): Binding[Node] = {
    <button class="btn btn-success">
      {triadCore.label}<img src={triadCore.getImage}></img>
    </button>
  }

  @dom
  def wrongButton(triadCore: TriadCore): Binding[Node] = {
    <button class="btn btn-danger">
      {triadCore.label}<img src={triadCore.getImage}></img>
    </button>
  }

  @dom
  def toUI(guessStatus: Binding[GuessStatus], handleAction: (RootAction) => Unit): Binding[Node] = {
    guessStatus.bind match {

      case NotGuessed =>
        <div class="col-8" id="guess-chord">
          <div class="row justify-content-center">
            {guessButton(StackedMinor2, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Minor2PlusMajor2, handleAction).bind}
            {eitherButton(Minor2PlusMajor2, Major2PlusMinor2, handleAction).bind}
            {guessButton(Major2PlusMinor2, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(MinorMajor, handleAction).bind}
            {eitherButton(MinorMajor, MinorMajorI, handleAction).bind}
            {guessButton(MinorMajorI, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Major7With3, handleAction).bind}
            {eitherButton(Major7With3, Major7With5, handleAction).bind}
            {guessButton(Major7With5, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Lydian, handleAction).bind}
            {eitherButton(Lydian, Phrygian, handleAction).bind}
            {guessButton(Phrygian, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Major2Plus4, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Minor7With3, handleAction).bind}
            {eitherButton(Minor7With3, Minor7With5, handleAction).bind}
            {guessButton(Minor7With5, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Dominant, handleAction).bind}
            {eitherButton(Dominant, HalfDiminished, handleAction).bind}
            {guessButton(HalfDiminished, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Stacked4s, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Diminished, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Minor, handleAction).bind}
            {eitherButton(Minor, Major, handleAction).bind}
            {guessButton(Major, handleAction).bind}
          </div>
          <div class="row justify-content-center">
            {guessButton(Augmented, handleAction).bind}
          </div>
        </div>

      case GuessedCorrectly(triadCore) =>
        <div class="col-8" id="guess-chord">
          <div class="row justify-content-center">
            <img src="img/correct.png" />{successButton(triadCore).bind}
          </div>
        </div>

      case GuessedWrong(wrongGuess, correct) =>
        <div class="col-8" id="guess-chord">
          <div class="row justify-content-center">
            <img src="img/actual.png" /> {successButton(correct).bind}
          </div>
          <div class="row justify-content-center">
            <img src="img/wrong.png" /> {wrongButton(wrongGuess).bind}
          </div>
        </div>

      case GuessedWrongEither(wrongGuess1, wrongGuess2, correct) =>
        <div class="col-8" id="guess-chord">
          <div class="row justify-content-center">
            <img src="img/actual.png" /> {successButton(correct).bind}
          </div>
          <div class="row justify-content-center">
            <img src="img/wrong.png" />
            {wrongButton(wrongGuess1).bind}
            {wrongButton(wrongGuess2).bind}
          </div>
        </div>
    }
  }

}