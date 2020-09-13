package bartosutton.exercise.four

import com.robertjneal.rl.types._

import com.robertjneal.rl.types.goal._

def iterativePolicyEvaluation(π: Map[State, List[(Action, Probability)]], stateTransitions: Map[(State, Action), State], stateRewards: Map[State, Reward], γ: Double = 0.9, θ: Double = 0.001): Map[State, Reward] = {
  val initialPolicyValues: Map[State, Reward] = π.mapValues(r => Reward(0)).toMap

  def nextState(policyValues: Map[State, Reward], currentState: State): State = {
    if (policyValues.keys.lastOption.filter(_ == currentState).isDefined) policyValues.keys.head
    else {
      policyValues.keys.dropWhile(_ != currentState).tail.head
    }
  }

  def expectedUpdate(currentPolicyValues: Map[State, Reward], state: State, currentΔs: Map[State, Double], step: Int = 0): Map[State, Reward] = {
    val updatedPolicyValue = Reward(π(state).map( (action, probability) => {
      val sPrime = stateTransitions(state, action)
      probability.toDouble * (stateRewards(sPrime).toDouble + γ * currentPolicyValues(sPrime).toDouble)
    }).sum)
    val updatedΔs = currentΔs.updated(state, Math.min(currentΔs(state), Math.abs((currentPolicyValues(state) - updatedPolicyValue).toDouble)))
    val updatedPolicyValues = currentPolicyValues.updated(state, updatedPolicyValue)
    val stopEvaluating = updatedΔs.values.max < θ

    val k = step / 15
    if (stopEvaluating | ((step % 15 == 0) && List(0, 1, 2, 3, 10).contains(k))) {
      println(s"==== k: $k =====")
      currentPolicyValues.foreach { (s, r) =>
        println(s"State: ${s}, Value: ${r}")
      }
    }

    if (stopEvaluating) updatedPolicyValues
    else expectedUpdate(updatedPolicyValues, nextState(currentPolicyValues, state), updatedΔs, step + 1)
  }

  def evaluate(): Map[State, Reward] = {
    val initialΔs: Map[State, Double] = π.mapValues(r => Double.PositiveInfinity).toMap

    expectedUpdate(initialPolicyValues, initialPolicyValues.keys.head, initialΔs)
  }

  evaluate()
}

def policyImprovement(π: Map[State, List[(Action, Probability)]], stateTransitions: Map[(State, Action), State], policyValues: Map[State, Reward], θ: Double = 0.001): (Boolean, Map[State, List[Action]]) = {
  val maxStateActions: Map[State, List[Action]] = π.map { case (state, actionProbabilities) => {
    val oldActionProbabilities: List[(Action, Probability)] = actionProbabilities.filter { (_, probability) => probability > Probability.Never }

    val maxActions: Map[Action, State] = oldActionProbabilities.foldLeft(Map.empty[Action, State])((max, elem) => {
      val (currentAction, currentProbability) = elem
      if (max.size == 0) {
        Map(currentAction -> stateTransitions(state, currentAction))
      } else {
        val (_, maxState) = max.head
        if ((policyValues(stateTransitions(state, currentAction)) - policyValues(maxState)).toDouble > θ) Map(currentAction -> stateTransitions(state, currentAction))
        else if (Math.abs((policyValues(stateTransitions(state, currentAction)) - policyValues(maxState)).toDouble) < θ) max.updated(currentAction, stateTransitions(state, currentAction))
        else max
      }
    })

    (state, maxActions.keys.toList)
  }}

  ((maxStateActions == π.mapValues{ _.map { (action, _) => action } }.toMap), maxStateActions)
}

def policyIteration(π: Map[State, List[(Action, Probability)]], stateTransitions: Map[(State, Action), State], stateRewards: Map[State, Reward], γ: Double = 0.9, θ: Double = 0.001) = {
  def iterate(πPrime: Map[State, List[(Action, Probability)]]): Map[State, List[(Action, Probability)]] = {
    val policyValues = iterativePolicyEvaluation(πPrime, stateTransitions, stateRewards, γ, θ)
    val (stable, maxActions) = policyImprovement(πPrime, stateTransitions, policyValues)
    val newPolicy = maxActions.mapValues(actions => actions.zip(Probability.evenProbabilities(actions.length))).toMap
    if (stable) newPolicy
    else iterate(newPolicy)
  }

  iterate(π)
}

val example1GridTransitions: Map[(State, Action), State] = Map(
    (State("1"), Action("up")) -> State("1"),
    (State("1"), Action("down")) -> State("5"),
    (State("1"), Action("left")) -> State("T"),
    (State("1"), Action("right")) -> State("2"),
    
    (State("2"), Action("up")) -> State("2"),
    (State("2"), Action("down")) -> State("6"),
    (State("2"), Action("left")) -> State("1"),
    (State("2"), Action("right")) -> State("3"),
    
    (State("3"), Action("up")) -> State("2"),
    (State("3"), Action("down")) -> State("7"),
    (State("3"), Action("left")) -> State("2"),
    (State("3"), Action("right")) -> State("3"),
    
    (State("4"), Action("up")) -> State("T"),
    (State("4"), Action("down")) -> State("8"),
    (State("4"), Action("left")) -> State("4"),
    (State("4"), Action("right")) -> State("5"),
    
    (State("5"), Action("up")) -> State("1"),
    (State("5"), Action("down")) -> State("9"),
    (State("5"), Action("left")) -> State("4"),
    (State("5"), Action("right")) -> State("6"),
    
    (State("6"), Action("up")) -> State("2"),
    (State("6"), Action("down")) -> State("10"),
    (State("6"), Action("left")) -> State("5"),
    (State("6"), Action("right")) -> State("7"),
    
    (State("7"), Action("up")) -> State("3"),
    (State("7"), Action("down")) -> State("11"),
    (State("7"), Action("left")) -> State("6"),
    (State("7"), Action("right")) -> State("7"),
    
    (State("8"), Action("up")) -> State("4"),
    (State("8"), Action("down")) -> State("12"),
    (State("8"), Action("left")) -> State("8"),
    (State("8"), Action("right")) -> State("9"),
    
    (State("9"), Action("up")) -> State("5"),
    (State("9"), Action("down")) -> State("13"),
    (State("9"), Action("left")) -> State("8"),
    (State("9"), Action("right")) -> State("10"),
    
    (State("10"), Action("up")) -> State("6"),
    (State("10"), Action("down")) -> State("14"),
    (State("10"), Action("left")) -> State("9"),
    (State("10"), Action("right")) -> State("11"),
    
    (State("11"), Action("up")) -> State("7"),
    (State("11"), Action("down")) -> State("T"),
    (State("11"), Action("left")) -> State("10"),
    (State("11"), Action("right")) -> State("11"),
    
    (State("12"), Action("up")) -> State("8"),
    (State("12"), Action("down")) -> State("12"),
    (State("12"), Action("left")) -> State("12"),
    (State("12"), Action("right")) -> State("13"),
    
    (State("13"), Action("up")) -> State("9"),
    (State("13"), Action("down")) -> State("13"),
    (State("13"), Action("left")) -> State("12"),
    (State("13"), Action("right")) -> State("14"),
    
    (State("14"), Action("up")) -> State("10"),
    (State("14"), Action("down")) -> State("14"),
    (State("14"), Action("left")) -> State("13"),
    (State("14"), Action("right")) -> State("T"),

    (State("T"), Action("up")) -> State("T"),
    (State("T"), Action("down")) -> State("T"),
    (State("T"), Action("left")) -> State("T"),
    (State("T"), Action("right")) -> State("T"),
  )


def figure4dot1column1() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[(Action, Probability)] = possibleActions.map { _ -> Probability.unsafe(0.25) }
  val randomPolicy: Map[State, List[(Action, Probability)]] = (1 to 14).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Map[State, Reward] = (1 to 14).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  iterativePolicyEvaluation(randomPolicy, example1GridTransitions, gridRewards, γ = 1.0)
}

def exercise2b() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[(Action, Probability)] = possibleActions.map { _ -> Probability.unsafe(0.25) }
  val randomPolicy: Map[State, List[(Action, Probability)]] = (1 to 15).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Map[State, Reward] = (1 to 15).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  val gridTransitions = example1GridTransitions ++ //.updated((State("13"), Action("down")), State("15")) ++ 
    Map(
      (State("15"), Action("up")) -> State("13"),
      (State("15"), Action("down")) -> State("15"),
      (State("15"), Action("left")) -> State("12"),
      (State("15"), Action("right")) -> State("14")
    )


  iterativePolicyEvaluation(randomPolicy, gridTransitions, gridRewards, γ = 1.0)
}

def figure4dot1column2() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[(Action, Probability)] = possibleActions.map { _ -> Probability.unsafe(0.25) }
  val randomPolicy: Map[State, List[(Action, Probability)]] = (1 to 14).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Map[State, Reward] = (1 to 14).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  val optimalPolicy = policyIteration(randomPolicy, example1GridTransitions, gridRewards, γ = 1.0)

  optimalPolicy.foreach(println)
}