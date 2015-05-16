package nlp.fsm.concrete

import nlp.fsm.generic._

/** Matches "sheeptalk" strings: /ba+!/ */
trait Sheeptalk extends FSM {

  type State = Int
  type Token = Char

  // Possible states
  val INITIAL  =  0
  val FINAL    =  1
  val MISMATCH = -1

  val B        =  2
  val A        =  3

  def initial: State = INITIAL

  def tokenize(input: String): List[Token] = input.toList

  def nextStates(current: State, token: Token): Seq[State] = {
    val next = current match {
      case INITIAL if token == 'b' => B
      case B       if token == 'a' => A
      case A       if token == 'a' => A
      case A       if token == '!' => FINAL
      case _                       => MISMATCH
    }

    if (next != MISMATCH) Seq(next) else Nil
  }

  def isTerminal(state: State): Boolean = state == FINAL

}

class RecursiveSheeptalk extends Sheeptalk with RecursiveFSM