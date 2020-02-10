package bartosutton.exercise.two

import breeze.linalg.DenseVector
import com.robertjneal.rl.TabularAgent
import com.robertjneal.rl._
import com.robertjneal.rl.testbed
import com.robertjneal.rl.types._
import org.apache.commons.math3.distribution._
import scala.collection.mutable
import scala.util.Random

def εGreedy(ε: Probability)(actionRewards: mutable.Map[Action, Reward]): Action = {
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
private def updateAverage(Q: Double, n: Int, R: Double, recencyWeight: Double): Double = {
  val error = R - Q
  Q + (recencyWeight * error)
}

def sampleAverage(recencyWeight: Option[Double])(actionRewards: mutable.Map[Action, Reward], currentAction: Action, currentReward: Reward, currentStep: Step): Unit = {
  actionRewards(currentAction) = Reward(
    updateAverage(
      actionRewards(currentAction).toDouble,
      currentStep.toInt,
      currentReward.toDouble,
      recencyWeight.getOrElse(1D / currentStep.toInt)
    )
  )
}

def figure2dot2(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εs = Vector(
    Probability.unsafe(0.1),
    Probability.unsafe(0.01),
    Probability.Never
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

  if (debug) {
    println(environment.maxReward)
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
  val εs = Vector(
    Probability.unsafe(0.1),
    Probability.unsafe(0.01),
    Probability.unsafe(0.0)
  )

  class NonstationaryReward extends RandomReward {
    val random = new NormalDistribution(0, 1)
    var nonstationaryTrueReward: Reward = Reward(random.sample)

    private def updateTrueReward = {
      nonstationaryTrueReward = Reward(new NormalDistribution(0D, 0.01).sample)
    }

    override def sample: Reward = {
      val sampler = new NormalDistribution(nonstationaryTrueReward.toDouble, 1)
      Reward(sampler.sample)
    }

    override def trueReward: Reward = nonstationaryTrueReward
  }

  val actions: Vector[Action] = Range(0, 9).map(n => Action(n.toString)).toVector
  val actionValues: Action => RandomReward = actions.map(a =>
    (a, (NonstationaryReward()))
  ).toMap

  val environment = testbed.tenArmEnvironment.copy(actionRewards = actionValues)
  
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
    testbed.generatePlot(meanRewards.toMap, s"2.5 ${meanRewards.head._1}", "mean reward")
    testbed.generatePlot(optimalActs.toMap, s"2.5 ${optimalActs.head._1}", "% optimal acts")
  }
}
