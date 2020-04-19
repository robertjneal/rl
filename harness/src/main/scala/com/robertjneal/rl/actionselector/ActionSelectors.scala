package com.robertjneal.rl.actionselector

import com.robertjneal.rl.types._
import scala.util.Random

def εGreedy(ε: Probability)(actionRewards: Map[Action, Reward]): Action = {
  if (ε > Probability.Never && ε.wonLottery()) {
    val array = actionRewards.toArray
    val (action, _): (Action, Reward) = array(Random.nextInt(array.size))
    action
  } else {
    val max = actionRewards.foldLeft(Vector.empty[(Action, Reward)])((maxima, current) => {
      val (_, currentReward) = current
      maxima.headOption match {
        case Some((_, maxReward)) =>
          if (maxReward > currentReward) maxima
          else if (maxReward == currentReward) maxima :+ current
          else Vector(current) 
        case None => Vector(current)
      }
    })
    val (action, _): (Action, Reward) = max(Random.nextInt(max.length))
    action
  }
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
