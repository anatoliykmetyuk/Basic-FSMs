package nlp.fsm.concrete

import scala.language.implicitConversions

import nlp.fsm.generic._

trait Time extends FSM
              with FSMMultiStateImplicits
              with FSMIntState
              with FSMStringToken
              with FSMPredicateGeneration
              with FSMStringPredicate
              with TimeTokens {

  // A map that defines what transitions are possible
  // given the current state and an input to the machine in this state.
  def stateMap: Seq[((State, Predicate), Seq[State])] = Seq(
    1 -> Month    -> 2
  , 2 -> DigitAny -> 3

  , 1 -> DigitSufixed             -> 5
  , 1 -> {x: Token => x == "the"} -> 6
  , 6 -> DigitSufixed             -> 5

  , 5 -> {x: Token => x == "of"}  -> 8
  
  , 8 -> Month          -> 9
  , 8 -> MonthWithComma -> 10

  , 10 -> Anything -> 11
  , 11 -> Anything -> 11
  )

  val initial   = 1
  def terminals = Set(3, 9, 11)

}

trait TimeTokens {this: Time with FSMStringPredicate =>

  val Month: Predicate =
    "January February March April May June July August September October November December"
  
  val MonthWithComma: Predicate = Month + ","

  val Digit1: Predicate = "1 21 31"
  val Digit2: Predicate = "2 22"
  val Digit3: Predicate = "3 23"
  val Digit4: Predicate = (1 to 31).map(_.toString).filter {!(Digit1 || Digit2 || Digit3)}
  
  val DigitSuf1: Predicate = Digit1 + "st"
  val DigitSuf2: Predicate = Digit2 + "nd"
  val DigitSuf3: Predicate = Digit3 + "rd"
  val DigitSuf4: Predicate = Digit4 + "th"

  val DigitRaw     : Predicate = Digit1    || Digit2    || Digit3    || Digit4
  val DigitSufixed : Predicate = DigitSuf1 || DigitSuf2 || DigitSuf3 || DigitSuf4
  val DigitAny     : Predicate = DigitRaw  || DigitSufixed

  val Anything: Predicate = _ => true

}

class DFLTime extends Time with DepthFirstLoopFSM