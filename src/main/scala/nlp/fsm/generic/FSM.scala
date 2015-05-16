package nlp.fsm.generic

/** Matches a string of input. */
trait FSM {

  /** A state of this FSM. Is it reasonable for it to carry some payload? */
  type State

  /**
   * The input string will be tokenized. This is the type of the token.
   * Normally, either Char or String.
   */
  type Token

  /** Initial state of this FSM. */
  def initial: State

  /** Extracts tokens to match on from the given string. */
  def tokenize(input: String): Seq[Token]

  /**
   * Generates next state given current state and
   * the input token the machine is about to consume in this state.
   */
  def nextState(current: State, token: Token): Option[State]

  /**
   * Whether this is a terminal state.
   * If the FSM has no more input in the terminal state, it succeeds.
   */
  def isTerminal(state: State): Boolean

  /** Attempts to match the given input. */
  def apply(input: String): Boolean

}