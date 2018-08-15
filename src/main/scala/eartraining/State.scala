package eartraining

import eartraining.flow.{ToQuery, _}

class StateMapper {

  def mapState(state: FlowStatus, action: Any): FlowStatus = {

    (state, action) match {
      case (Init, Initialized(audioEngine)) => Menu(audioEngine)

      case (Menu(audioEngine), ToQuery) => Query(audioEngine)
      case (Menu(audioEngine), ToTrichordGenerator) => TrichordGenerator(audioEngine)

      case (query:Query, action:QueryAction) => query.handleAction(action)
      case (trichordGenerator: TrichordGenerator, action: TrichordGeneratorAction) => trichordGenerator.handleAction(action)

    }

  }

}

