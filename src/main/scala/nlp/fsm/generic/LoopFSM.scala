package nlp.fsm.generic

import scala.annotation.tailrec

/**
 * A loop-based FSM implementation. You can tweak
 * the state space search strategy via tweaking the
 * agenda-related methods.
 */
trait LoopFSM extends FSM {
  
  /**
   * A search state. Captures the state of the SEARCH over the space of all possible states
   * given the inputs. Contains the state of an FSM and an index of the token
   * that will be fed to the machine in this state as an input.
   */
  type SearchState = (State, Int)

  /**
   * Push a state and an input it will receive to the
   * state space search queue.
   */
  def enqueue(state: State, tokenIndex: Int): Unit

  /**
   * Retrieve the next state and its input to analyze.
   */
  def dequeue(): Option[SearchState]

  def apply(input: String): Boolean = {
    val tokens = tokenize(input)

    // Push the an i
    enqueue(initial, 0)

    @tailrec def loop(): Boolean = dequeue() match {

      // Terminal state reached, and no more input left => match
      case Some((state, idx)) if isTerminal(state) && idx >= tokens.size => true
      
      // No input left, but terminal state not reached => fail
      case Some((state, idx)) if                      idx >= tokens.size => false

      // Nothing left to explore => fail
      case None                                                          => false

      // Input is still left, and there are states to explore. Generate next states
      // and push them to the agenda, then reiterate the loop.
      case Some((state, idx)) =>
        for (s <- nextStates(state, tokens(idx))) enqueue(s, idx + 1)
        loop()
    }

    loop()
  }

}

/**
 * Backed by a LIFO stack, this FSM performs depth-first search over the state space.
 */
trait DepthFirstLoopFSM extends LoopFSM {

  val stack = collection.mutable.Stack[SearchState]()

  /**
   * Push a state and an input it will receive to the
   * state space search queue.
   */
  def enqueue(state: State, tokenIndex: Int): Unit = stack push state -> tokenIndex

  /**
   * Retrieve the next state and its input to analyze.
   */
  def dequeue(): Option[SearchState] = if (stack.isEmpty) None else Some(stack.pop())

}

/**
 * Backed by a FIFO queue, this FSM performs breadth-first search over the state space.
 */
trait BreadthFirstLoopFSM extends LoopFSM {

  val queue = collection.mutable.Queue[SearchState]()

  /**
   * Push a state and an input it will receive to the
   * state space search queue.
   */
  def enqueue(state: State, tokenIndex: Int): Unit = queue enqueue state -> tokenIndex

  /**
   * Retrieve the next state and its input to analyze.
   */
  def dequeue(): Option[SearchState] = if (queue.isEmpty) None else Some(queue.dequeue())

}