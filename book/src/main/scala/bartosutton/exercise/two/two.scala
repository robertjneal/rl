package bartosutton.exercise.two

import breeze.linalg.DenseVector
import com.robertjneal.rl.TabularAgent
import com.robertjneal.rl._
import com.robertjneal.rl.testbed
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

// When recencyWeight is 1/n, this is equivalent to formula 2.3
// and in all cases it's formula 2.5
private def updateAverage(Q: Double, n: Int, R: Double, recencyWeight: Double): Double = {
  val error = R - Q
  Q + (recencyWeight * error / n.toDouble)
}

def sampleAverage(recencyWeight: Option[Double])(actionRewards: mutable.Map[Action, Reward], currentAction: Action, currentReward: Reward, currentStep: Step): Unit = {
  actionRewards(currentAction) = Reward(
    updateAverage(
      actionRewards(currentAction).toDouble,
      currentStep.toInt,
      currentReward.toDouble,
      recencyWeight.getOrElse(1 / currentStep.toInt.toDouble)
    )
  )
}

def figure2dot2(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εs = Vector(
    Probability.unsafe(0.1),
    Probability.unsafe(0.01),
    Probability.unsafe(0.0)
  )
  val environment = testbed.tenArmEnvironment
  
  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = εs.map(ε => { 
    val agent = TabularAgent(
      environment,
      OneState,
      εGreedy(ε),
      sampleAverage(None),
      true
    )
    val result = testbed.run(
      agent,
      runs = 2000,
      steps = 1000
    )
    ((ε.toString, result.meanRewards), (ε.toString, result.optimalActs))
  })

  if (debug)
    for (i <- 0 until 90)
      println(indexedResults.head._1._2(i))

  if (generatePlots) {
    val (meanRewards, optimalActs) = indexedResults.unzip
    testbed.generatePlot(meanRewards.toMap, s"2.2 ${meanRewards.head._1}", "mean reward")
    testbed.generatePlot(optimalActs.toMap, s"2.2 ${optimalActs.head._1}", "% optimal acts")
  }

}