package nlp.fsm.generic

trait FSMCharToken extends FSM {

  type Token = Char

  def tokenize(input: String): List[Token] = input.toList  

}

trait FSMStringToken extends FSM {
  
  type Token = String

  def tokenize(input: String): List[Token] = input.split(" ").map(_.toLowerCase).toList

}

trait FSMIntState extends FSM {
  type State = Int
}

trait FSMMultiStateImplicits {this: FSM =>
  /** So that we don't need to wrap states in the map into a Seq(). */
  implicit def state2seq(state: State): Seq[State] = Seq(state)
}

trait FSMMapGeneration {this: FSM =>

  def stateMap: Map[(State, Token), Seq[State]]

  def nextStates(current: State, token: Token): Seq[State] =
    stateMap.get(current -> token).getOrElse(Nil)

}

trait FSMPredicateGeneration {this: FSM =>

  type Predicate = Token => Boolean

  // Yes, it is NOT a map...
  def stateMap: Seq[((State, Predicate), Seq[State])]

  def nextStates(current: State, token: Token): Seq[State] = stateMap
    .filter  {case ((state, p), _) => state == current && p(token)}
    .flatMap {case ((_, _), next)  => next}

}