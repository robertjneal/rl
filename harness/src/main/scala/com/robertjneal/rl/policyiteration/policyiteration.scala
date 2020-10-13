package com.robertjneal.rl.policyiteration

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

private[policyiteration] enum IterationType {
  case Policy, Value
}

private[policyiteration] def expectedUpdate(
  stateActionProbabilities: Map[State, List[ActionProbability]], 
  stateTransitions: Map[StateAction, List[ProbabilityState]], 
  stateRewards: Action => State => Reward, 
  γ: Double, 
  θ: Double, 
  currentStateValues: Map[State, Reward], 
  state: State, 
  currentΔs: Map[State, Double], 
  iterationType: IterationType,
  step: Int = 0, 
  logFrequency: Int = 0
): Map[State, Reward] = {
  import Ordering.Double.TotalOrdering
  if (stateActionProbabilities.isEmpty) Map.empty[State, Reward]
  else {
    def nextState(stateValues: Map[State, Reward], currentState: State): State = {
      if (stateValues.keys.lastOption.filter(_ == currentState).isDefined) stateValues.keys.head
      else {
        stateValues.keys.dropWhile(_ != currentState).tail.head
      }
    }

    val updatedStateValue: Reward = Reward({
      val values: Seq[Double] = stateActionProbabilities(state).map{ case ActionProbability(action, probability) => {
        val sPrimes = stateTransitions(StateAction(state, action))
        probability * 
          sPrimes.foldLeft(0D)((acc, probabilityState) => 
            acc + 
            (probabilityState.probability * 
              (stateRewards(action)(probabilityState.state).toDouble + 
              γ * 
              currentStateValues(probabilityState.state).toDouble)
            )
          )
      }}
      if (iterationType == IterationType.Policy) values.sum
      else values.max
    })
    val updatedΔs = currentΔs.updated(state, Math.min(currentΔs(state), Math.abs((currentStateValues(state) - updatedStateValue).toDouble)))
    val updatedPolicyValues = currentStateValues.updated(state, updatedStateValue)
    import Ordering.Double.TotalOrdering
    val stopEvaluating = updatedΔs.values.max < θ

    if (logFrequency > 0 && (step % logFrequency == 0 || stopEvaluating)) { 
      println(s"==== step: $step =====")
      currentStateValues.toList.sortBy(_._1.toString).foreach { (s, r) =>
        println(s"State: ${s}, Value: ${r}")
      }
    }

    if (stopEvaluating) updatedPolicyValues
    else expectedUpdate(stateActionProbabilities, stateTransitions, stateRewards, γ, θ, updatedPolicyValues, nextState(currentStateValues, state), updatedΔs, iterationType, step + 1)
  }
}


def iterativePolicyEvaluation(
  π: Map[State, List[ActionProbability]], 
  stateTransitions: Map[StateAction, List[ProbabilityState]], 
  stateRewards: Action => State => Reward, 
  γ: Double = 0.9, 
  θ: Double = 0.001, 
  logFrequency: Int = 0
): Map[State, Reward] = {
  if (π.isEmpty) Map.empty[State, Reward]
  else {
    val initialPolicyValues: Map[State, Reward] = π.view.mapValues(r => Reward(0)).toMap

    def evaluate(): Map[State, Reward] = {
      val initialΔs: Map[State, Double] = π.view.mapValues(r => Double.PositiveInfinity).toMap

      expectedUpdate(π, stateTransitions, stateRewards, γ, θ, initialPolicyValues, initialPolicyValues.keys.head, initialΔs, IterationType.Policy)
    }

    evaluate()
  }
}

def policyImprovement(π: Map[State, List[ActionProbability]], stateTransitions: Map[StateAction, List[ProbabilityState]], policyValues: Map[State, Reward], θ: Double = 0.001): (Boolean, Map[State, List[Action]]) = {
  val maxStateActions: Map[State, List[Action]] = π.map { 
    case (state, actionProbabilities) => {
      //val oldActionProbabilities: List[ActionProbability] = actionProbabilities.filter { (_, probability) => probability > Probability.Never }

      val maxActions: Map[Action, Reward] = actionProbabilities.foldLeft(Map.empty[Action, Reward])((maxima, elem) => {
        val ActionProbability(currentAction, currentProbability) = elem
        val reward = Reward(
          stateTransitions(StateAction(state, currentAction)).foldLeft(0D)((r, probabilityState) => {
            val ProbabilityState(prob: Probability, sPrime: State) = probabilityState
            r + (prob * policyValues(sPrime).toDouble)
          })
        )

        if (maxima.size == 0) {
          Map(currentAction -> reward)
        } else {
          val (_, maxReward) = maxima.head
          if ((reward - maxReward).toDouble > θ) Map(currentAction -> reward)
          else if (Math.abs((reward - maxReward).toDouble) < θ) maxima.updated(currentAction, reward)
          else maxima
        }
      })

      (state, maxActions.keys.toList)
    }
  }

  val policyActions = π.view.mapValues{ _.map { (action, _) => action } }.toMap
  val isStable = (maxStateActions == policyActions)
  (isStable, maxStateActions)
}

def policyIteration(π: Map[State, List[ActionProbability]], stateTransitions: Map[StateAction, List[ProbabilityState]], stateRewards: Action => State => Reward, γ: Double = 0.9, θ: Double = 0.001, logFrequency: Int = 0): Map[State, List[ActionProbability]] = {
  def iterate(πPrime: Map[State, List[ActionProbability]]): Map[State, List[ActionProbability]] = {
    val policyValues = iterativePolicyEvaluation(πPrime, stateTransitions, stateRewards, γ, θ, logFrequency)
    val (stable, maxActions) = policyImprovement(πPrime, stateTransitions, policyValues)
    val newPolicy = maxActions.view.mapValues(actions => actions.map(ActionProbability(_, Probability.evenProbability(actions.length)))).toMap
    if (stable) newPolicy
    else iterate(newPolicy)
  }

  iterate(π)
}

def valueIteration(stateActionProbabilities: Map[State, List[ActionProbability]], stateTransitions: Map[StateAction, List[ProbabilityState]], stateRewards: Action => State => Reward, γ: Double = 0.9, θ: Double = 0.001): Map[State, List[(Action, Probability)]] = {
  import Ordering.Double.TotalOrdering

  val initialStateValues: Map[State, Reward] = stateActionProbabilities.view.mapValues(r => Reward(0)).toMap

  val bestRewards = expectedUpdate(
    stateActionProbabilities,
    stateTransitions,
    stateRewards,
    γ,
    θ,
    initialStateValues,
    stateActionProbabilities.keys.head,
    stateActionProbabilities.view.mapValues(_ => Double.PositiveInfinity).toMap,
    IterationType.Value
  )

  stateActionProbabilities.map{ case (state, actionProbabilities) => {
    state -> {
      val actionRewards: Map[Action, Reward] = actionProbabilities.map{ (action, _) => action -> Reward({
        stateTransitions(StateAction(state, action)).foldLeft(0D)((acc, probabilityState) => {
          val reward = probabilityState.probability.toDouble * bestRewards(probabilityState.state).toDouble
          reward + acc
        })
      })}.toMap
      val maxValue: Reward = Reward(actionProbabilities.map((action, _) => actionRewards(action).toDouble).max)
      val bestActions = actionProbabilities.filter{ case ActionProbability(action, _) => actionRewards(action) == maxValue }
      val bestActionProbabilities = bestActions.map { case ActionProbability(action, _) => (action, Probability.unsafe(1/bestActions.length.toDouble)) }
      bestActionProbabilities
    }
  }}
}
