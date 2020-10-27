package com.robertjneal.rl.policyiteration

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

private[policyiteration] enum IterationType {
  case Policy, Value
}

case class PolicyHistory(policy: Map[State, List[ActionProbability]], history: List[Map[State, Reward]])
case class ExpectationHistory(expectation: Map[State, Reward], history: List[Map[State, Reward]])

private[policyiteration] def expectedUpdate(
    stateActionProbabilities: Map[State, List[ActionProbability]],
    stateTransitions: (StateAction) => List[ProbabilityState],
    stateRewards: Action => State => Reward,
    γ: Double,
    θ: Double,
    state: State,
    iterationType: IterationType,
    stateValues: Option[Map[State, Reward]] = None,
    Δs: Option[Map[State, Double]] = None,
    step: Int = 0,
    logFrequency: Int = 0,
    logMaxSteps: Int = 0,
    recordHistory: Boolean = false,
    history: List[Map[State, Reward]] = List.empty
): ExpectationHistory = {
  import Ordering.Double.TotalOrdering
  if (stateActionProbabilities.isEmpty) ExpectationHistory(Map.empty[State, Reward], history)
  else {
    val currentStateValues = stateValues.getOrElse(
      stateActionProbabilities.view.mapValues(r => Reward(0)).toMap
    )
    val currentΔs = Δs.getOrElse(
      stateActionProbabilities.view
        .mapValues(r => Double.PositiveInfinity)
        .toMap
    )
    def nextState(
        stateValues: Map[State, Reward],
        currentState: State
    ): State = {
      if (stateValues.keys.lastOption.filter(_ == currentState).isDefined)
        stateValues.keys.head
      else {
        stateValues.keys.dropWhile(_ != currentState).tail.head
      }
    }

    val updatedStateValue: Reward = Reward({
      val values: Seq[Double] = stateActionProbabilities(state).map {
        case ActionProbability(action, probability) => {
          val sPrimes = stateTransitions(StateAction(state, action))
          probability *
            sPrimes.foldLeft(0d)((acc, probabilityState) =>
              acc +
                (probabilityState.probability *
                  (stateRewards(action)(probabilityState.state).toDouble +
                    γ *
                    currentStateValues(probabilityState.state).toDouble))
            )
        }
      }
      if (iterationType == IterationType.Policy) values.sum
      else {
        if (values.isEmpty) 0
        else values.max
      }
    })
    val updatedΔs = currentΔs.updated(
      state,
      Math.min(
        currentΔs(state),
        Math.abs((currentStateValues(state) - updatedStateValue).toDouble)
      )
    )
    val updatedPolicyValues =
      currentStateValues.updated(state, updatedStateValue)
    val stopEvaluating = updatedΔs.values.max < θ

    if (
      logFrequency > 0 && ((step < logMaxSteps && step % logFrequency == 0) || stopEvaluating)
    ) {
      println(s"==== step: $step =====")
      currentStateValues.toList.sortBy(_._1.toString).foreach { (s, r) =>
        println(s"State: ${s}, Value: ${r}")
      }
    }

    val updatedHistory = if (recordHistory) { // && step % stateActionProbabilities.size == 0) {
        history :+ updatedPolicyValues
    } else history

    if (stopEvaluating) ExpectationHistory(updatedPolicyValues, updatedHistory)
    else
      expectedUpdate(
        stateActionProbabilities,
        stateTransitions,
        stateRewards,
        γ,
        θ,
        nextState(currentStateValues, state),
        iterationType,
        Some(updatedPolicyValues),
        Some(updatedΔs),
        step + 1,
        logFrequency,
        logMaxSteps,
        recordHistory,
        updatedHistory
      )
  }
}

def iterativePolicyEvaluation(
    π: Map[State, List[ActionProbability]],
    stateTransitions: Map[StateAction, List[ProbabilityState]],
    stateRewards: Action => State => Reward,
    γ: Double = 0.9,
    θ: Double = 0.001,
    logFrequency: Int = 0,
    logMaxSteps: Int = 0
): Map[State, Reward] = {
  if (π.isEmpty) Map.empty[State, Reward]
  else {
    expectedUpdate(
      π,
      stateTransitions,
      stateRewards,
      γ,
      θ,
      π.keys.head,
      IterationType.Policy,
      logFrequency = logFrequency,
      logMaxSteps
    ).expectation
  }
}

def policyImprovement(
    π: Map[State, List[ActionProbability]],
    stateTransitions: (StateAction) => List[ProbabilityState],
    policyValues: Map[State, Reward],
    θ: Double = 0.001
): (Boolean, Map[State, List[Action]]) = {
  val maxStateActions: Map[State, List[Action]] = π.map {
    case (state, actionProbabilities) => {
      val maxActions: Map[Action, Reward] = actionProbabilities.foldLeft(
        Map.empty[Action, Reward]
      )((maxima, elem) => {
        val ActionProbability(currentAction, currentProbability) = elem
        val reward = Reward(
          stateTransitions(StateAction(state, currentAction)).foldLeft(0d)(
            (r, probabilityState) => {
              val ProbabilityState(prob: Probability, sPrime: State) =
                probabilityState
              r + (prob * policyValues(sPrime).toDouble)
            }
          )
        )

        if (maxima.size == 0) {
          Map(currentAction -> reward)
        } else {
          val (_, maxReward) = maxima.head
          if ((reward - maxReward).toDouble > θ) Map(currentAction -> reward)
          else if (Math.abs((reward - maxReward).toDouble) < θ)
            maxima.updated(currentAction, reward)
          else maxima
        }
      })

      (state, maxActions.keys.toList)
    }
  }

  val policyActions: Map[State, Set[Action]] = π.view.mapValues {
    _.map { (action, _) => action }.toSet
  }.toMap
  val isStable =
    (maxStateActions.view.mapValues(_.toSet).toMap == policyActions)
  (isStable, maxStateActions)
}

def policyIteration(
    π: Map[State, List[ActionProbability]],
    stateTransitions: Map[StateAction, List[ProbabilityState]],
    stateRewards: Action => State => Reward,
    γ: Double = 0.9,
    θ: Double = 0.001,
    logFrequency: Int = 0
): Map[State, List[ActionProbability]] = {
  def iterate(
      πPrime: Map[State, List[ActionProbability]]
  ): Map[State, List[ActionProbability]] = {
    val policyValues = iterativePolicyEvaluation(
      πPrime,
      stateTransitions,
      stateRewards,
      γ,
      θ,
      logFrequency
    )
    val (stable, maxActions) =
      policyImprovement(πPrime, stateTransitions, policyValues)
    val newPolicy = maxActions.view
      .mapValues(actions =>
        actions.map(
          ActionProbability(_, Probability.evenProbability(actions.length))
        )
      )
      .toMap
    if (stable) newPolicy
    else iterate(newPolicy)
  }

  iterate(π)
}

def valueIteration(
    stateActionProbabilities: Map[State, List[ActionProbability]],
    stateTransitions: (StateAction) => List[ProbabilityState],
    stateRewards: Action => State => Reward,
    γ: Double = 0.9,
    θ: Double = 0.001,
    deterministic: Boolean = true,
    logFrequency: Int = 0,
    logMaxSteps: Int = 0,
    recordHistory: Boolean = false
): PolicyHistory = {
  if (stateActionProbabilities.isEmpty) PolicyHistory(Map.empty, List.empty)
  else {
      import Ordering.Double.TotalOrdering

      val bestRewards: ExpectationHistory = expectedUpdate(
        stateActionProbabilities,
        stateTransitions,
        stateRewards,
        γ,
        θ,
        stateActionProbabilities.keys.head,
        IterationType.Value,
        logFrequency = logFrequency,
        logMaxSteps = logMaxSteps,
        recordHistory = recordHistory
      )

      val policy: Map[State, List[ActionProbability]] = stateActionProbabilities.map {
        case (state, actionProbabilities) => {
          state -> {
            val actionRewards
                : Map[Action, Reward] = actionProbabilities.map { (action, _) =>
              action -> Reward({
                stateTransitions(StateAction(state, action)).foldLeft(0d)(
                  (acc, probabilityState) => {
                    val reward =
                      probabilityState.probability.toDouble * bestRewards.expectation(
                        probabilityState.state
                      ).toDouble
                    reward + acc
                  }
                )
              })
            }.toMap
            val maxValue: Reward = Reward({
              val rewards = actionProbabilities.map((action, _) =>
                actionRewards(action).toDouble
              )
              if (rewards.isEmpty) 0
              else rewards.max
            })
            val bestActions = actionProbabilities.filter {
              case ActionProbability(action, _) =>
                actionRewards(action) == maxValue
            }
            val bestActionProbabilities = bestActions.map {
              case ActionProbability(action, _) =>
                ActionProbability(
                  action,
                  Probability.unsafe(1 / bestActions.length.toDouble)
                )
            }
            if (deterministic) bestActionProbabilities.headOption.map(List(_)).getOrElse(List.empty)
            else bestActionProbabilities
          }
        }
      }
      
      PolicyHistory(policy, bestRewards.history)
  }
}
