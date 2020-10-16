package com.robertjneal.rl.actionselector

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import scala.util.Random

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

def softMax(actionPreferences: Map[Action, Preference]): Action = {
  val actionProbabilities = softMaxProbabilities(actionPreferences).toList

  def pickWithProbabilty[T](
      p: Probability,
      map: List[(T, Probability)],
      cumulativeProbabilities: Double = 0
  ): T = {
    val hd :: tl = map
    val acc = cumulativeProbabilities.toDouble + hd._2.toDouble
    if (acc > p.toDouble) hd._1
    else pickWithProbabilty(p, tl, acc)
  }

  pickWithProbabilty[Action](Probability.random, actionProbabilities)
}

def upperConfidenceBound(c: Double, state: State)(
    step: Step,
    actionSteps: Map[State, Map[Action, Step]]
)(actionRewards: Map[Action, Reward]): Action = {
  def valueTransformer(action: Action, reward: Reward): Double = {
    val actionStep = actionSteps
      .getOrElse(state, Map(action -> Step(1)))
      .getOrElse(action, Step(1))
    reward.toDouble + c * Math.sqrt(
      Math.log(step.toInt) / actionStep.toInt.toDouble
    )
  }

  val maxima = collectMaxima(valueTransformer, actionRewards)
  val (action, _): (Action, Reward) = maxima(Random.nextInt(maxima.length))
  action
}

def εGreedy(
    ε: Probability
)(step: Step, actionSteps: Map[State, Map[Action, Step]])(
    actionRewards: Map[Action, Reward]
): Action = {
  def valueTransformer(action: Action, reward: Reward): Double =
    reward.toDouble

  if (ε > Probability.Never && ε.wonLottery()) {
    val array = actionRewards.toArray
    val (action, _): (Action, Reward) = array(Random.nextInt(array.size))
    action
  } else {
    val maxima = collectMaxima(valueTransformer, actionRewards)
    val (action, _): (Action, Reward) = maxima(Random.nextInt(maxima.length))
    action
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
