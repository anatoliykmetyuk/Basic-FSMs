package nlp.fsm

import org.scalatest._

import nlp.fsm.generic._
import nlp.fsm.concrete._

class FSMSuite extends FlatSpec with Matchers with SampleFSMsBehavioursComponent {

  SheeptalkBehaviour.checkAll (
    "Recursive sheeptalk"                            -> { () => new RecursiveSheeptalk      }     
  , "Non-deterministic recursive sheeptalk"          -> { () => new WeirdRecursiveSheeptalk }
    
  , "Depth first Loop sheeptalk"                     -> { () => new DFLoopSheeptalk         }        
  , "Non-deterministic depth-first loop sheeptalk"   -> { () => new WeirdDFLoopSheeptalk    }   

  , "Breadth first Loop sheeptalk"                   -> { () => new BFLoopSheeptalk         }        
  , "Non-deterministic breadth-first loop sheeptalk" -> { () => new WeirdBFLoopSheeptalk    }   
  )

}

trait FSMBehavioursComponent {this: FlatSpec with Matchers =>

  trait FSMBehaviours {
    val matches   : Seq[String]
    val notMatches: Seq[String]


    def behaviour(fsm: => FSM) {
      def fMatch(in: String, matches: Boolean) = fsm(in) shouldBe matches

      for (str <- matches   ) it should s"match $str"     in fMatch(str, true )
      for (str <- notMatches) it should s"not match $str" in fMatch(str, false)
    }

    def check(name: String, constructor: => FSM) {
      name should behave like behaviour(constructor)
    }

    def checkAll(nameConstructor: (String, () => FSM)*) =
      for ((name, constructor) <- nameConstructor) check(name, constructor())
  }

}

trait SampleFSMsBehavioursComponent extends FSMBehavioursComponent {this: FlatSpec with Matchers =>

  object SheeptalkBehaviour extends FSMBehaviours {
    val matches = Seq(
      "ba!"
    , "baa!"
    , "baaaaaa!"
    )

    val notMatches = Seq(
      "aba!"
    , "baaa"
    , "b!"
    )
  }

}