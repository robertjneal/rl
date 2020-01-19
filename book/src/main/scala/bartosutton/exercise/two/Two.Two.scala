package bartosutton.exercise.two

import com.robertjneal.rl.Types._
import scala.collection.mutable
import scala.util.Random

def εGreedy(ε: Probability = Probability.unsafe(0))(actionRewards: mutable.Map[Action, Reward]): Action = {
  if (ε > Probability.Never && ε.sample()) {
    val array = actionRewards.toArray
    val (action, _): (Action, Reward) = array(Random.nextInt(actionRewards.size))
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

def sampleAverage(actionRewards: mutable.Map[Action, Reward], currentAction: Action, currentReward: Reward, currentStep: Step): Unit = {
  actionRewards(currentAction) = Reward(
    actionRewards(currentAction).toDouble + ((currentReward - actionRewards(currentAction)).toDouble / currentStep.toInt.toDouble)
  )
}