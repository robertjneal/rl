package com.robertjneal.rl.actionselector

import com.robertjneal.rl.types._
import scala.util.Random

def upperConfidenceBound(c: Int, state: State)(step: Step, actionSteps: Map[State, Map[Action, Step]])(actionRewards: Map[Action, Reward]): Action = {
  def valueTransformer(action: Action, reward: Reward): Double = {
    val actionStep = actionSteps.getOrElse(state, Map(action -> Step(1))).getOrElse(action, Step(1))
    reward.toDouble + c * Math.sqrt(Math.log(step.toInt)/actionStep.toInt.toDouble)
  }

  val maxima = collectMaxima(valueTransformer, actionRewards)
  val (action, _): (Action, Reward) = maxima(Random.nextInt(maxima.length))
  action
}

def εGreedy(ε: Probability)(step: Step, actionSteps: Map[State, Map[Action, Step]])(actionRewards: Map[Action, Reward]): Action = {
  def valueTransformer(action: Action, reward: Reward): Double = reward.toDouble

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

private def collectMaxima(valueTransformer: (Action, Reward) => Double, actionRewards: Map[Action, Reward]): Vector[(Action, Reward)] = {
  actionRewards.foldLeft(Vector.empty[(Action, Reward)])((maxima, current) => {
    val (currentAction, currentReward) = current
    maxima.headOption match {
      case Some((maxAction, maxReward)) =>
        if (valueTransformer(maxAction, maxReward) > valueTransformer(currentAction, currentReward)) maxima
        else if (valueTransformer(maxAction, maxReward) == valueTransformer(currentAction, currentReward)) maxima :+ current
        else Vector(current) 
      case None => Vector(current)
    }
  })
}

// When recencyWeight is 1/n, this is equivalent to:
// Qn + 1/n(Rn - Qn)
// and in all cases it's:
// Qn + α(Rn - Qn)
private def updateAverage(Q: Double, n: Step, R: Double, averageMethod: (Step) => Double): Double = {
  val recencyWeight = averageMethod(n)
  val error = R - Q
  Q + (recencyWeight * error)
}

def sampleAverage(step: Step): Double = 1D / step.toInt
def exponentialRecencyWeightedAverage(weight: Double)(step: Step): Double = weight

def average(averageMethod: (Step) => Double)(actionRewards: Map[Action, Reward], currentAction: Action, currentReward: Reward, currentStep: Step): Map[Action, Reward] = {
  actionRewards.updated(currentAction, Reward(
    updateAverage(
      actionRewards(currentAction).toDouble,
      currentStep,
      currentReward.toDouble,
      averageMethod
    )
  ))
}
