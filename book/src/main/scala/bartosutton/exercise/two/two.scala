package bartosutton.exercise.two

import breeze.linalg.DenseVector
import com.robertjneal.rl.TabularAgent
import com.robertjneal.rl._
import com.robertjneal.rl.testbed
import com.robertjneal.rl.types._
import org.apache.commons.math3.distribution._
import scala.collection.mutable
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

// When recencyWeight is 1/n, this is equivalent to formula 2.3
// and in all cases it's formula 2.5
private def updateAverage(Q: Double, n: Step, R: Double, averageMethod: (Step) => Double): Double = {
  val recencyWeight = averageMethod(n)
  val error = R - Q
  Q + (recencyWeight * error)
}

def sampleAverage(step: Step) = 1D / step.toInt
def exponentialRecencyWeightedAverage(weight: Double)(step: Step) = weight

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

def figure2dot2(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εs = Vector(
    Probability.unsafe(0.1),
    Probability.unsafe(0.01),
    Probability.Never
  )
  val environment = testbed.tenArmEnvironment
  
  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = εs.map(ε => { 
    val agent = TabularAgent.blankSlate(
      environment,
      εGreedy(ε),
      average(sampleAverage),
      true
    )
    val result = testbed.run(
      agent,
      runs = 2000,
      steps = 1000
    )
    ((ε.toString, result.meanRewards), (ε.toString, result.optimalActs))
  })

  if (debug) {
    for (i <- 0 until 100) {
      println(indexedResults.head._1._2(i))
      println(indexedResults.head._2._2(i))
      println("-")
    }
  }

  if (generatePlots) {
    val (meanRewards, optimalActs) = indexedResults.unzip
    testbed.generatePlot(meanRewards.toMap, s"2.2 ${meanRewards.head._1}", "mean reward")
    testbed.generatePlot(optimalActs.toMap, s"2.2 ${optimalActs.head._1}", "% optimal acts")
  }
}

def exercise2dot5(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val ε = Probability.unsafe(0.1)

  val averageMethods: Vector[(String, (Step) => Double)] = Vector(
    ("Sample Average", sampleAverage),
    ("Exponential Recency", exponentialRecencyWeightedAverage(0.1))
  )

  val actions: Vector[Action] = Range(0, 9).map(n => Action(n.toString)).toVector
  val actionValues: Map[Action, RandomReward] = actions.map(a =>
    (a, (NonstationaryReward.randomStart))
  ).toMap

  val environment = testbed.tenArmEnvironment.copy(actionRewards = actionValues)
  
  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = 
    averageMethods.map((name, am) => { 
      val agent = TabularAgent.blankSlate(
        environment,
        εGreedy(ε),
        average(am),
        true
      )
      val result = testbed.run(
        agent,
        runs = 2000,
        steps = 10000
      )
      ((name, result.meanRewards), (name, result.optimalActs))
    }
  )

  if (debug)
    for (i <- 0 until 90)
      println(indexedResults.head._1._2(i))

  if (generatePlots) {
    val (meanRewards, optimalActs) = indexedResults.unzip
    testbed.generatePlot(meanRewards.toMap, s"2.5", "mean reward")
    testbed.generatePlot(optimalActs.toMap, s"2.5", "% optimal acts")
  }
}
