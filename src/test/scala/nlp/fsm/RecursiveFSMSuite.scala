package nlp.fsm

import org.scalatest._

import nlp.fsm.generic._
import nlp.fsm.concrete._

class RecursiveFSMSuite extends FlatSpec with Matchers with SheeptalkBehaviours {

  "Recursive sheeptalk"                   should behave like sheeptalkMatcher(new RecursiveSheeptalk      )
  "Non-deterministic recursive sheeptalk" should behave like sheeptalkMatcher(new WeirdRecursiveSheeptalk )
  "Loop sheeptalk"                        should behave like sheeptalkMatcher(new LoopSheeptalk           )
  "Non-deterministic loop sheeptalk"      should behave like sheeptalkMatcher(new WeirdLoopSheeptalk      )

}

trait SheeptalkBehaviours {this: RecursiveFSMSuite =>

  def sheeptalkMatcher(fsm: => FSM) {
    def fMatch(in: String, matches: Boolean = true) = fsm(in) shouldBe matches

    it should "match ba!"      in fMatch("ba!"     )
    it should "match baa!"     in fMatch("baa!"    )
    it should "match baaaaaa!" in fMatch("baaaaaa!")

    it should "not match aba!" in fMatch("aba!", false)
    it should "not match baaa" in fMatch("baaa", false)
    it should "not match b!"   in fMatch("b!"  , false)
  }

}