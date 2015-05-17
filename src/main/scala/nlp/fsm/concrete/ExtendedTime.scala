package nlp.fsm.concrete

import scala.language.implicitConversions

import nlp.fsm.generic._

trait ExtendedTime extends Time with ExtendedTimeTokens {

  override def stateMap: Seq[((State, Predicate), Seq[State])] = super.stateMap ++ Seq[((State, Predicate), Seq[State])](
    1 -> Today -> 100
  )

  override def terminals = super.terminals ++ Set(100)

}

trait ExtendedTimeTokens {this: ExtendedTime =>

  val Today: Predicate = "today"

}

class DFLExtendedTime extends ExtendedTime with DepthFirstLoopFSM