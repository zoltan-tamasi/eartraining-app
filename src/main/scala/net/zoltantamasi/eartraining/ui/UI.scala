package net.zoltantamasi.eartraining.ui

import com.thoughtworks.binding.{Binding, dom}
import net.zoltantamasi.eartraining._
import net.zoltantamasi.eartraining.state._
import org.scalajs.dom.{Event, Node}

trait Labeler[A] {
  def label(in: A): String
}

trait GetImage[A] {
  def getImage(in : A): String
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

  implicit object TriadCoreImage extends GetImage[TriadCore] {
    override def getImage(in: TriadCore): String = {
      in match {
        case StackedMinor2 => "/img/1-1-10.png"
        case Minor2PlusMajor2 => "/img/1-2-9.png"
        case Major2PlusMinor2 => "/img/1-9-2.png"
        case MinorMajor => "/img/1-3-8.png"
        case MinorMajorI => "/img/1-8-3.png"
        case Lyd => "/img/1-5-6.png"
        case Locr => "/img/1-6-5.png"
        case Minor7Plus6 => "/img/2-2-8.png"
        case Minor7With3 => "/img/2-3-7.png"
        case Minor7With5 => "/img/2-7-3.png"
        case LydSus2 => "/img/2-4-6.png"
        case AugSus2 => "/img/2-6-4.png"
        case Minor => "/img/3-4-5.png"
        case Major => "/img/3-5-4.png"
        case Augmented => "/img/4-4-4.png"
        case Diminished => "/img/3-3-6.png"
        case Major7Without5 => "/img/1-7-4.png"
        case Major7Without3 => "/img/1-4-7.png"
        case Stacked4s => "/img/2-5-5.png"
      }
    }
  }

  implicit class LabelerOps[T](data: T) {
    def label(implicit labeler: Labeler[T]) =
      labeler.label(data)

    def getImage(implicit imageGetter: GetImage[T]) =
      imageGetter.getImage(data)
  }

  def toUI(in : A): Binding[Node]

}

object UI extends StateToUI[Root] {
  @dom
  def toUI(root: Root): Binding[Node] = {
    {
      root.state.rootState.bind match {
        case status: Init => {
          InitUI.toUI(status).bind
        }

        case status => {
          <div>
            <nav class="navbar navbar-expand-lg navbar-light bg-light">
              <a class="navbar-brand" href="#" onclick={(e: Event) => root.handleAction(BackToMenu)}>
                Triad cores
              </a>
              <button class="navbar-toggler" type="button">
                <span class="navbar-toggler-icon"></span>
              </button>

              <div class="collapse navbar-collapse" id="navbarSupportedContent">
                <ul class="navbar-nav mr-auto">
                  <li class={"nav-item " + (status match { case _: Query => "active" case _ => "" })}>
                    <a class="nav-link" href="#" onclick={(e: Event) => root.handleAction(QueryOptionSelected)}>
                      Practice hearing
                    </a>
                  </li>
                  <li class={"nav-item " + (status match { case _: TrichordGenerator => "active" case _ => "" })}>
                    <a class="nav-link" href="#" onclick={(e: Event) => root.handleAction(TrichordGeneratorOptionSelected)}>
                      Trichord generator
                    </a>
                  </li>
                </ul>
              </div>
            </nav>
            {
              status match {
                case status: Query => {
                  QueryUI.toUI(status).bind
                }
                case status: Menu => {
                  MenuUI.toUI(status).bind
                }
                case status: TrichordGenerator => {
                  TrichordGeneratorUI.toUI(status).bind
                }
              }
            }
          </div>
        }
      }
    }
  }
}
