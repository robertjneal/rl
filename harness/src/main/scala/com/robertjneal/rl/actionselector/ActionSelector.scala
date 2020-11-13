package com.robertjneal.rl.actionselector

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import scala.util.Random

sealed trait ActionSelector[A]

trait RewardSelector extends ActionSelector[Reward] {
  def apply(
    step: Step,
    stateActionSteps: Map[State, Map[Action, Step]]
  )(actionRewards: Map[Action, Reward]): (Action, IsExploratory)
}

trait PreferenceSelector extends ActionSelector[Preference] {
  def apply(
    actionPreferences: Map[Action, Preference]
  ): (Action, IsExploratory)
}

def softMaxProbabilities(
    actionPreferences: Map[Action, Preference]
): Map[Action, Probability] = {
  val eulersActionPreferences: Map[Action, Double] = actionPreferences.map {
    ap =>
      val (action, preference) = ap
      (action, Math.exp(preference.toDouble))
  }
  val eulersTotalPreference: Double = eulersActionPreferences.values.sum
  val actionProbabilities = eulersActionPreferences.map { ap =>
    val (action, preference) = ap
    val probability =
      Probability.unsafe(preference.toDouble / eulersTotalPreference.toDouble)
    action -> probability
  }
  actionProbabilities
}

def softMax: PreferenceSelector = (actionPreferences: Map[Action, Preference]) => {
  import Ordering.Double.TotalOrdering
  val actionProbabilities = softMaxProbabilities(actionPreferences).toList.sortBy { (_, probability) => -probability.toDouble}
  val action = Probability.pickWithProbabilty[Action](Probability.random, actionProbabilities)
  (action, actionProbabilities.headOption.map(_ == action).getOrElse(false))
}

def upperConfidenceBound(c: Double, state: State): RewardSelector = {
  new RewardSelector { 
    def apply(
    step: Step,
    actionSteps: Map[State, Map[Action, Step]]
    )(actionRewards: Map[Action, Reward]): (Action, IsExploratory) = {
      def valueTransformer(action: Action, reward: Reward): Double = {
        val actionStep = actionSteps
          .getOrElse(state, Map(action -> Step(1)))
          .getOrElse(action, Step(1))
        reward.toDouble + c * Math.sqrt(
          Math.log(step.toDouble) / actionStep.toDouble
        )
      }

      val maxima = collectMaxima(valueTransformer, actionRewards)
      val (action, _): (Action, Reward) = maxima(Random.nextInt(maxima.length))
      (action, false)
    }
  }
}

def εGreedy(
    ε: Probability
): RewardSelector = {
  def valueTransformer(action: Action, reward: Reward): Double =
    reward.toDouble

  new RewardSelector {
    def apply(
      step: Step,
      stateActionSteps: Map[State, Map[Action, Step]]
    )(actionRewards: Map[Action, Reward]): (Action, IsExploratory) = {
      val shouldExplore = ε > Probability.Never && ε.wonLottery() 
      val actionIsExploratory: (Action, IsExploratory) = {
        if (shouldExplore) {
          val array = actionRewards.toArray
          val (action, _): (Action, Reward) = array(Random.nextInt(array.size))
          (action, true)
        } else {
          val maxima = collectMaxima(valueTransformer, actionRewards)
          val (action, _): (Action, Reward) = maxima(Random.nextInt(maxima.length))
          (action, false)
        }
      }

      actionIsExploratory
    }
  }
}

private def collectMaxima(
    valueTransformer: (Action, Reward) => Double,
    actionRewards: Map[Action, Reward]
): Vector[(Action, Reward)] = {
  actionRewards.foldLeft(Vector.empty[(Action, Reward)])(
    (maxima, current) => {
      val (currentAction, currentReward) = current
      maxima.headOption match {
        case Some((maxAction, maxReward)) =>
          if (
            valueTransformer(maxAction, maxReward) > valueTransformer(
              currentAction,
              currentReward
            )
          ) maxima
          else if (
            valueTransformer(maxAction, maxReward) == valueTransformer(
              currentAction,
              currentReward
            )
          ) maxima :+ current
          else Vector(current)
        case None => Vector(current)
      }
    }
  )
}
