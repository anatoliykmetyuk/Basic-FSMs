package nlp.fsm.concrete

import scala.language.implicitConversions

import nlp.fsm.generic._

/**
 * Matches "sheeptalk" strings: /ba+!/
 * FSM: b-a-!
 */
trait Sheeptalk extends FSM with FSMHelpers {
  import Implicits._

  type State = Int
  type Token = Char

  // Possible states
  val INITIAL  =  0
  val FINAL    =  1

  val B        =  2
  val A        =  3

  // A map that defines what transitions are possible
  // given the current state and an input to the machine in this state.
  val stateMap: Map[(State, Token), Seq[State]] = Map(
    INITIAL -> 'b' -> B
  , B       -> 'a' -> A
  , A       -> 'a' -> A
  , A       -> '!' -> FINAL
  )

  def initial: State = INITIAL

  def tokenize(input: String): List[Token] = input.toList

  def nextStates(current: State, token: Token): Seq[State] =
    stateMap.get(current -> token).getOrElse(Nil)

  def isTerminal(state: State): Boolean = state == FINAL

}

/**
 * This sheeptalk is non-deterministic: b-a-a-! with
 * loop over first 'a'.
 */
trait WeirdSheeptalk extends Sheeptalk {
  import Implicits._

  // We start numbering of the new states from 10 to avoid
  // collisions with the previous state. If this is not enough,
  // we can use 100 etc.
  val A1 = 10
  val A2 = 11

  override val stateMap: Map[(State, Token), Seq[State]] = Map(
    INITIAL -> 'b' -> B
  , B       -> 'a' -> A1

  , A1      -> 'a' -> Seq(A2, A1)
  , A1      -> '!' -> FINAL

  , A2      -> '!' -> FINAL
  )
}

class RecursiveSheeptalk      extends Sheeptalk      with RecursiveFSM
class WeirdRecursiveSheeptalk extends WeirdSheeptalk with RecursiveFSM

class DFLoopSheeptalk      extends Sheeptalk with DepthFirstLoopFSM
class WeirdDFLoopSheeptalk extends Sheeptalk with DepthFirstLoopFSM

class BFLoopSheeptalk      extends Sheeptalk with BreadthFirstLoopFSM
class WeirdBFLoopSheeptalk extends Sheeptalk with BreadthFirstLoopFSM