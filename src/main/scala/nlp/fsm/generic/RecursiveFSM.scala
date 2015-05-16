package nlp.fsm.generic

/** A simple recursive state space search implementation. */
trait RecursiveFSM extends FSM {

  def apply(input: String): Boolean = {
    val tokens = tokenize(input)

    def loop(state: State, buffer: Seq[Token]): Boolean = buffer.toList match {
      case Nil if isTerminal(state) => true   // No input in terminal state
      case Nil                      => false  // Didn't make it to the terminal state
      case x :: xs                  => nextState(state, x) match {
        case Some(s) => loop(s, xs)
        case None    => false
      }
    }

    loop(initial, tokens)
  }

}