package nlp.fsm.concrete

import scala.language.implicitConversions

import nlp.fsm.generic._

trait ExtendedTime extends Time with ExtendedTimeTokens {

  override def stateMap: Seq[((State, Predicate), Seq[State])] = super.stateMap ++ Seq[((State, Predicate), Seq[State])](
    1 -> SimplePointer -> 100
  , 1 -> DayOfWeek     -> 100
  , 1 -> Signum        -> 104
  , 1 -> Number        -> 106
  , 1 -> Article       -> 110

  , 104 -> (DayOfWeek || Span) -> 100
  , 106 -> (Span       + "s" ) -> 108
  , 108 -> Direction           -> 1

  , 110 -> Span -> 108
  )

  override def terminals = super.terminals ++ Set(100)

}

trait ExtendedTimeTokens {this: ExtendedTime =>

  val SimplePointer: Predicate = "today tomorrow now yesterday"
  val DayOfWeek    : Predicate = "sunday monday tuseday wednesday thursday friday saturday"
  val Span         : Predicate = "day week month year century millenium"

  val Signum   : Predicate = "this previous next"
  val Article  : Predicate = "the a"
  val Direction: Predicate = "before from after"

  val Number: Predicate = x => x.forall {c => ('0' to '9') contains c}

}

class DFLExtendedTime extends ExtendedTime with DepthFirstLoopFSM