package bartosutton.exercise.two

import breeze.linalg.DenseVector
import com.robertjneal.rl.actionselector._
import com.robertjneal.rl.agent._
import com.robertjneal.rl._
import com.robertjneal.rl.testbed
import com.robertjneal.rl.types._
import com.robertjneal.rl.updater._
import org.apache.commons.math3.distribution._
import scala.collection.mutable

def debugger(indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))]): Unit = {
  for (i <- 0 until 100) {
    println(indexedResults.head._1._2(i))
    println(indexedResults.head._2._2(i))
    println("-")
  }
}

def plotGenerator(prefix: String, indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))], plotMeanRewards: Boolean = true): Unit = {
  val (meanRewards, optimalActs) = indexedResults.unzip
  if (plotMeanRewards) testbed.generatePlot(meanRewards.toMap, s"$prefix ${meanRewards.head._1}", "mean reward")
  testbed.generatePlot(optimalActs.toMap, s"$prefix ${optimalActs.head._1}", "% optimal acts")
}

def figure2dot2(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εs = Vector(
    Probability.unsafe(0.1),
    Probability.unsafe(0.01),
    Probability.Never
  )
  val environment = testbed.tenArmEnvironment()
  
  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = εs.map(ε => { 
    val agent = TabularRewardAgent.blankSlate(
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
    ((s"ε=$ε", result.meanRewards), (s"ε=$ε", result.optimalActs))
  })

  if (debug) debugger(indexedResults)
  if (generatePlots) plotGenerator("2.2", indexedResults)
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

  val environment = testbed.tenArmEnvironment().copy(actionRewards = actionValues)
  
  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = 
    averageMethods.map((name, am) => { 
      val agent = TabularRewardAgent.blankSlate(
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
    })

  if (debug) debugger(indexedResults)

  if (generatePlots) plotGenerator("2.5", indexedResults)
}

def figure2dot3(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εAndBiases = Vector(
    (Probability.unsafe(0.1), 0),
    (Probability.unsafe(0.1), 5)
  )
  val environment = testbed.tenArmEnvironment()

  val initialActionSteps = environment.possibleStateActions.map { 
    case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
  }

  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = εAndBiases.map((ε, bias) => { 
    val initialTable = environment.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Reward(bias)): _*) 
    } 
    val agent = TabularRewardAgent(
      environment,
      εGreedy(ε),
      average(sampleAverage),
      Step(1),
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

  if (debug) debugger(indexedResults)
  if (generatePlots) plotGenerator("2.3", indexedResults)
}

def figure2dot4(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val fs = Vector(
    (εGreedy(Probability.unsafe(0.1)), "ε=0.1"),
    (upperConfidenceBound(2, OneState), "UCB, c=2")
  )
  val environment = testbed.tenArmEnvironment()

  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = fs.map((f, name) => { 
    val agent = TabularRewardAgent.blankSlate(
      environment,
      f,
      average(sampleAverage),
      true
    )
    val result = testbed.run(
      agent,
      runs = 2000,
      steps = 1000
    )
    ((name, result.meanRewards), (name, result.optimalActs))
  })

  if (debug) debugger(indexedResults)
  if (generatePlots) plotGenerator("2.4", indexedResults)
}

def figure2dot5(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val fs = Vector(
    (0.1, Some(0D), "α=0.1, no baseline"),
    (0.1, None, "α=0.1, baseline"),
    (0.4, Some(0D), "α=0.4, no baseline"),
    (0.4, None, "α=0.4, baseline")
  )

  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = fs.map((stepSize, baseline, name) => { 
    val environment = testbed.tenArmEnvironment(4D)

    val initialActionSteps = environment.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
    }
  
    val initialTable = environment.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> (Reward(0), Preference(0))): _*) 
    } 
    val agent = TabularPreferenceAgent(
      environment,
      softMax,
      stochasticGradientAscent(stepSize, baseline),
      Step(1),
      initialActionSteps,
      initialTable,
      true
    )
    val result = testbed.run(
      agent,
      runs = 2000,
      steps = 1000
    )
    ((name, result.meanRewards), (name, result.optimalActs))
  })

  if (debug) debugger(indexedResults)
  if (generatePlots) plotGenerator("2.5", indexedResults, false)
}
