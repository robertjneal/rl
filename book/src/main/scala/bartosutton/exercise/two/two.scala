package bartosutton.exercise.two

import breeze.linalg.DenseVector
import com.robertjneal.rl.actionselector._
import com.robertjneal.rl.TabularAgent
import com.robertjneal.rl._
import com.robertjneal.rl.testbed
import com.robertjneal.rl.types._
import org.apache.commons.math3.distribution._
import scala.collection.mutable

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

def figure2dot3(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εAndBiases = Vector(
    (Probability.unsafe(0.1), 0),
    (Probability.unsafe(0.1), 5)
  )
  val environment = testbed.tenArmEnvironment

  val initialActionSteps = environment.possibleStateActions.map { 
    case (s, as) => s -> Map(as.map(_ -> Step(0)): _*) 
  }

  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = εAndBiases.map((ε, bias) => { 
    val initialTable = environment.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Reward(bias)): _*) 
    } 
    val agent = TabularAgent(
      environment,
      εGreedy(ε),
      average(sampleAverage),
      Step(0),
      initialActionSteps,
      initialTable,
      true
    )
    val result = testbed.run(
      agent,
      runs = 2000,
      steps = 1000
    )
    ((s"ε=$ε, Q1=$bias" , result.meanRewards), (s"ε=$ε, Q1=$bias", result.optimalActs))
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
