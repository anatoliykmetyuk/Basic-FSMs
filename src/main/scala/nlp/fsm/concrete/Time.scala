package nlp.fsm.concrete

import scala.language.implicitConversions

import nlp.fsm.generic._

/**
 * Matches "sheeptalk" strings: /ba+!/
 * FSM: b-a-!
 */
trait Time extends FSM
              with FSMMultiStateImplicits
              with FSMIntState
              with FSMStringToken
              with TimeTokens {

  // A map that defines what transitions are possible
  // given the current state and an input to the machine in this state.
  val stateMap: Map[(State, Token), Seq[State]] = Map[(State, Seq[Token]), Seq[State]](
    1 -> Month    -> 2
  , 2 -> DigitAny -> 3

  , 1 -> DigitSufixed -> 5
  , 1 -> Seq("the")   -> 6
  , 6 -> DigitSufixed -> 5

  , 5 -> Seq("of")    -> 8
  
  , 8 -> Month          -> 9
  , 8 -> MonthWithComma -> 10

  , 10 -> Anything -> 11
  , 11 -> Anything -> 11
  ).flatMap {case ((s, tokens), sx) => tokens.map {t => s -> t -> sx}}

  val initial   = 1
  def terminals = Set(3, 9, 11)

}

trait TimeTokens {this: Time =>

  val Month: Seq[Token] =
    "January February March April May June July August September October November December".toLowerCase.split(" ")
  
  val MonthWithComma: Seq[Token] = Month.map(_ + ",")

  val Digit1: Seq[Token] = "1 21 31".split(" ")
  val Digit2: Seq[Token] = "2 22"   .split(" ")
  val Digit3: Seq[Token] = "3 23"   .split(" ")
  val Digit4: Seq[Token] = (1 to 31).filter {d => !(Digit1 ++ Digit2 ++ Digit3).map(_.toInt).contains(d)}.map(_.toString)
  
  val DigitSuf1: Seq[Token] = Digit1.map(_ + "st")
  val DigitSuf2: Seq[Token] = Digit2.map(_ + "nd")
  val DigitSuf3: Seq[Token] = Digit3.map(_ + "rd")
  val DigitSuf4: Seq[Token] = Digit4.map(_ + "th")

  val DigitRaw     : Seq[Token] = Digit1    ++ Digit2    ++ Digit3    ++ Digit4
  val DigitSufixed : Seq[Token] = DigitSuf1 ++ DigitSuf2 ++ DigitSuf3 ++ DigitSuf4
  val DigitAny     : Seq[Token] = DigitRaw  ++ DigitSufixed

  val Anything = Seq("omg")

}

class DFLTime extends Time with DepthFirstLoopFSM