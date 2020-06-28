package com.robertjneal.rl.updater

import com.robertjneal.rl.actionselector.softMaxProbabilities
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

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

def average(averageMethod: (Step) => Double)(actionRewards: Map[Action, Reward], currentAction: Action, currentReward: Reward, currentStep: Step, averageReward: Option[Reward] = None): Map[Action, Reward] = {
  actionRewards.updated(currentAction, Reward(
    updateAverage(
      actionRewards(currentAction).toDouble,
      currentStep,
      currentReward.toDouble,
      averageMethod
    )
  ))
}

def stochasticGradientAscent(α: Double, constantBaseline: Option[Double] = None)(actionPreferences: Map[Action, Preference], currentAction: Action, currentReward: Reward, currentStep: Step, averageReward: Option[Reward] = None): Map[Action, Preference] = {
  val actionProbabilities = softMaxProbabilities(actionPreferences)

  actionPreferences.map { case (action, preference) => {
    action -> {
      if (action == currentAction) {
        Preference(preference.toDouble + α * (currentReward.toDouble - constantBaseline.getOrElse(averageReward.getOrElse(Reward(1)).toDouble)) * (1 - actionProbabilities(action).toDouble))
      } else {
        Preference(preference.toDouble - α * (currentReward.toDouble - constantBaseline.getOrElse(averageReward.getOrElse(Reward(1)).toDouble)) * actionProbabilities(action).toDouble)
      }
    }
  }}
}