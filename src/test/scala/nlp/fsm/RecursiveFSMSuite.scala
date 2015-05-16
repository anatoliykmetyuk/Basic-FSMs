package nlp.fsm

import org.scalatest._

import nlp.fsm.generic._
import nlp.fsm.concrete._

class RecursiveFSMSuite extends FlatSpec with Matchers with SheeptalkBehaviours {

  "Recursive sheeptalk"                            should behave like sheeptalkMatcher(new RecursiveSheeptalk      )
  "Non-deterministic recursive sheeptalk"          should behave like sheeptalkMatcher(new WeirdRecursiveSheeptalk )
  
  "Depth first Loop sheeptalk"                     should behave like sheeptalkMatcher(new DFLoopSheeptalk           )
  "Non-deterministic depth-first loop sheeptalk"   should behave like sheeptalkMatcher(new WeirdDFLoopSheeptalk      )

  "Breadth first Loop sheeptalk"                   should behave like sheeptalkMatcher(new BFLoopSheeptalk           )
  "Non-deterministic breadth-first loop sheeptalk" should behave like sheeptalkMatcher(new WeirdBFLoopSheeptalk      )

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