package eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import eartraining._
import eartraining.flow._
import org.scalajs.dom.Node

trait Labeler[A] {
  def label(in : A): String
}

trait StateToUI[A] {

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

  implicit class LabelerOps[T](data: T) {
    def label(implicit labeler: Labeler[T]) =
      labeler.label(data)
  }

  def toUI(in : A): Binding[Node]

}

object UI {
  @dom
  def apply(flow: Flow): Binding[Node] = {
    flow.state.bind match {
      case status: Init => { InitUI.toUI(status).bind }
      case status: Query => { QueryUI.toUI(status).bind }
      case status: Menu => { MenuUI.toUI(status).bind }
      case status: TrichordGenerator => { TrichordGeneratorUI.toUI(status).bind }
    }
  }
}