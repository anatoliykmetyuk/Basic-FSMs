package nlp.fsm.generic

/** A simple recursive state space search implementation. */
trait RecursiveFSM extends FSM {

  def apply(input: String): Boolean = {
    val tokens = tokenize(input)

    def loop(state: State, buffer: List[Token]): Boolean = buffer match {
      case Nil if isTerminal(state) => true   // No input in terminal state
      case Nil                      => false  // Didn't make it to the terminal state
      case x :: xs                  => nextStates(state, x) match {
        case Nil    => false
        case states => states.find(loop(_, xs)).isDefined
      }
    }

    loop(initial, tokens)
  }

}