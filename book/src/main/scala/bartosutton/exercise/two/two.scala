package bartosutton.exercise.two

import breeze.linalg.DenseVector
import breeze.linalg.sum
import com.robertjneal.rl.actionselector._
import com.robertjneal.rl.agent._
import com.robertjneal.rl._
import com.robertjneal.rl.testbed
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
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
  val path = "src/main/scala/bartosutton/exercise/two/"
  if (plotMeanRewards) testbed.generatePlot(meanRewards.toMap, path, s"$prefix-rewards", "mean reward")
  testbed.generatePlot(optimalActs.toMap, path, s"$prefix-optimal-acts", "% optimal acts", percentage=true)
}

def figure2dot2(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εs = Vector(
    Probability.unsafe(0.1),
    Probability.unsafe(0.01),
    Probability.Never
  )
  val environment = testbed.tenArmEnvironment()
  
  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = εs.map(ε => { 
    val agent = TabularAgent.rewardBlankSlate(
      environment,
      εGreedy(ε),
      average(sampleAverage),
      true
    )
    val result = testbed.run[Reward](
      agent,
      runs = 2000,
      steps = 1000
    )
    ((s"ε=$ε", result.meanRewards), (s"ε=$ε", result.optimalActs))
  })

  if (debug) debugger(indexedResults)
  if (generatePlots) plotGenerator("figure2.2", indexedResults)
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
      val agent = TabularAgent.rewardBlankSlate(
        environment,
        εGreedy(ε),
        average(am),
        true
      )
      val result = testbed.run[Reward](
        agent,
        runs = 2000,
        steps = 10000
      )
      ((name, result.meanRewards), (name, result.optimalActs))
    })

  if (debug) debugger(indexedResults)

  if (generatePlots) plotGenerator("exercise2.5", indexedResults)
}

def figure2dot3(generatePlots: Boolean = false, seed: Integer = 1, debug: Boolean = false) = {
  val εAndBiases = Vector(
    (Probability.unsafe(0.1), 0D),
    (Probability.unsafe(0.1), 5D)
  )
  val environment = testbed.tenArmEnvironment()

  val initialActionSteps = environment.possibleStateActions.map { 
    case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
  }

  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = εAndBiases.map((ε, bias) => { 
    val initialTable = environment.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Reward(bias)): _*) 
    } 
    val agent = TabularAgent[Reward](
      environment,
      RewardSelector(εGreedy(ε)),
      average(sampleAverage),
      Step(1),
      initialActionSteps,
      initialTable,
      Reward(0),
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
    val agent = TabularAgent.rewardBlankSlate(
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

  val environment = testbed.tenArmEnvironment(4D)

  val indexedResults: Seq[((String, DenseVector[Double]), (String, DenseVector[Double]))] = fs.map((stepSize, baseline, name) => { 
    val initialActionSteps = environment.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
    }
  
    val initialTable = environment.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Preference(0)): _*) 
    } 
    val agent = TabularAgent[Preference](
      environment,
      PreferenceSelector(softMax),
      stochasticGradientAscent(stepSize, baseline),
      Step(1),
      initialActionSteps,
      initialTable,
      Reward(0),
      true
    )
    val result = testbed.run[Preference](
      agent,
      runs = 2000,
      steps = 1000
    )
    ((name, result.meanRewards), (name, result.optimalActs))
  })

  if (debug) debugger(indexedResults)
  if (generatePlots) plotGenerator("2.5", indexedResults, false)
}

def exercise2dot9(debug: Boolean = false) = {
  def logisiticProbabilities(actionPreferences: Map[Action, Preference]): Map[Action, Probability] = {
    if (actionPreferences.size != 2) actionPreferences.view.mapValues{_ => Probability.Never}.toMap
    else {
      val action1 = actionPreferences.keys.head
      val action2 = actionPreferences.keys.last
      val beta = -(actionPreferences(action1).toDouble - actionPreferences(action2).toDouble)
      val action1Probability = Probability.unsafe(Math.exp(-beta) / (1 + Math.exp(-beta)))
      val action2Probability = Probability.unsafe(1 / (1 + Math.exp(-beta)))
      actionPreferences.map { case (action, _) =>
        if (action == action1) action -> action1Probability
        else action -> action2Probability
      }
    }
  }

  val actionPreferences = Map(
    Action("A") -> Preference(1.1),
    Action("B") -> Preference(.99)
  )

  val softMaxResult = softMaxProbabilities(actionPreferences)
  val logisticResult = logisiticProbabilities(actionPreferences)

  println("Comparison")
  actionPreferences.foreach { (action, _) =>
      println(s"action: $action, softMax: ${softMaxResult(action)}, logisticResult: ${logisticResult(action)}")
  }
}

def figure2dot6(debug: Boolean = false) = {
  val runs = 2000
  val steps = 1000
  val εs = List(1/128D, 1/64D, 1/32D, 1/16D, 1/8D, 1/4D).map(Probability.unsafe)

  val environment = testbed.tenArmEnvironment()

  val A: (String, Seq[(Double, Double)]) = (
    "ε-greedy", 
    εs.map{ ε => {
      val agent = TabularAgent.rewardBlankSlate(
        environment,
        εGreedy(ε),
        average(sampleAverage),
        true
      )
      val result = testbed.run(
        agent,
        runs,
        steps
      )

      (ε.toDouble, sum(result.meanRewards) / steps.toDouble)
    }}
  )

  val αs: List[Double] = List(1/32D, 1/16D, 1/8D, 1/4D, 1/2D, 1, 2, 3)

  val B: (String, Seq[(Double, Double)]) = (
    "stochastic gradient ascent", 
    αs.map{ α => {
      val agent = TabularAgent.preferenceBlankSlate(
        environment,
        softMax,
        stochasticGradientAscent(α, None),
        true
      )
      val result = testbed.run(
        agent,
        runs,
        steps
      )

      if (debug) {
        println("stochastic gradient")
        println(s"α: $α")
        println(s"mean reward: ${sum(result.meanRewards) / steps.toDouble}")
        (0 to 20).foreach { i =>
          println(result.meanRewards(i))
        }
      }

      (α, sum(result.meanRewards) / steps.toDouble)
    }}
  )

  val cs: List[Double] = List(1/16D, 1/8D, 1/4D, 1/2D, 1, 2, 3, 4)

  val C: (String, Seq[(Double, Double)]) = (
    "UCB", 
    cs.map{ c => {
      val agent = TabularAgent.rewardBlankSlate(
        environment,
        upperConfidenceBound(c, OneState),
        average(sampleAverage),
        true
      )
      val result = testbed.run(
        agent,
        runs,
        steps
      )

      (c, sum(result.meanRewards) / steps.toDouble)
    }}
  )

  val is: List[Double] = List(1/4D, 1/2D, 1, 2, 3, 4)

  val D: (String, Seq[(Double, Double)]) = (
    "ε-greedy(0.1)", 
    is.map{ i => {
      val initialActionSteps = environment.possibleStateActions.map { 
        case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
      }
  
      val initialTable = environment.possibleStateActions.map { 
        case (s, as) => s -> Map(as.map(_ -> Reward(i)): _*) 
      } 
      val agent = TabularAgent[Reward](
        environment,
        RewardSelector(εGreedy(Probability.unsafe(0.1))),
        average(sampleAverage),
        Step(1),
        initialActionSteps,
        initialTable,
        Reward(0),
        true
      )

      val result = testbed.run(
        agent,
        runs,
        steps
      )

      (i, sum(result.meanRewards) / steps.toDouble)
    }}
  )

  import breeze.plot._

  val f = breeze.plot.Figure()
  val p = f.subplot(0)
  p.legend = true
  p.xlabel = "Avg reward over first 1000 steps"
  p.ylabel = "parameter (ε, α, c, Q0)"
  //p.ylim(Math.min(0D, dvs.values.minBy(_.min).min), Math.max(0D, dvs.values.maxBy(_.max).max))
  
  List(A, B, C, D).foreach { case (name, xsys) =>
    p += plot(
      x = xsys.map((x, y) => x), 
      y = xsys.map((x, y) => y), 
      name = name
    )
  }
  
  p.logScaleX = true
  f.saveas(s"figure 2.6.png")
}

def exercise2dot11(runs: Integer = 2000, steps: Integer = 200000, includeεGreedy: Boolean = false, includeStochasticGradientAscent: Boolean = false, includeUpperConfidenceBound: Boolean = false, includeBiasedεGreedy: Boolean = false, debug: Boolean = false) = {

  val actions: Vector[Action] = Range(0, 9).map(n => Action(n.toString)).toVector
  val actionValues: Map[Action, RandomReward] = actions.map(a =>
    (a, (NonstationaryReward.randomStart))
  ).toMap

  val environment = testbed.tenArmEnvironment().copy(actionRewards = actionValues)

  val εs = List(1/128D,  1/32D, 0.1, 1/4D).map(Probability.unsafe)
  def A: (String, Seq[(Double, Double)]) = (
    "ε-greedy", 
    εs.map{ ε => {
      val agent = TabularAgent.rewardBlankSlate(
        environment,
        εGreedy(ε),
        average(exponentialRecencyWeightedAverage(0.1)),
        true
      )
      val result = testbed.run(
        agent,
        runs,
        steps
      )

      (ε.toDouble, sum(result.meanRewards.slice(steps / 2, steps.toInt, 1)) / (steps / 2).toDouble)
    }}
  )

  val αs: List[Double] = List(1/32D, 1/16D, 1/8D, 1/4D, 1/2D, 1, 2, 3)

  def B: (String, Seq[(Double, Double)]) = (
    "stochastic gradient ascent", 
    αs.map{ α => {
      val agent = TabularAgent.preferenceBlankSlate(
        environment,
        softMax,
        stochasticGradientAscent(α, None),
        true
      )
      val result = testbed.run(
        agent,
        runs,
        steps
      )

      if (debug) {
        println("stochastic gradient")
        println(s"α: $α")
        println(s"mean reward: ${sum(result.meanRewards) / steps.toDouble}")
        (0 to 20).foreach { i =>
          println(result.meanRewards(i))
        }
      }

      (α, sum(result.meanRewards.slice(steps / 2, steps.toInt, 1)) / (steps / 2).toDouble)
    }}
  )

  val cs: List[Double] = List(1/16D, 1/8D, 1/4D, 1/2D, 1, 2, 3, 4)

  def C: (String, Seq[(Double, Double)]) = (
    "UCB", 
    cs.map{ c => {
      val agent = TabularAgent.rewardBlankSlate(
        environment,
        upperConfidenceBound(c, OneState),
        average(exponentialRecencyWeightedAverage(0.1)),
        true
      )
      val result = testbed.run(
        agent,
        runs,
        steps
      )

      (c, sum(result.meanRewards.slice(steps / 2, steps.toInt, 1)) / (steps / 2).toDouble)
    }}
  )

  val is: List[Double] = List(1/4D, 1/2D, 1, 2, 3, 4)

  def D: (String, Seq[(Double, Double)]) = (
    "ε-greedy(0.1)", 
    is.map{ i => {
      val initialActionSteps = environment.possibleStateActions.map { 
        case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
      }
  
      val initialTable = environment.possibleStateActions.map { 
        case (s, as) => s -> Map(as.map(_ -> Reward(i)): _*) 
      } 
      val agent = TabularAgent[Reward](
        environment,
        RewardSelector(εGreedy(Probability.unsafe(0.1))),
        average(exponentialRecencyWeightedAverage(0.1)),
        Step(1),
        initialActionSteps,
        initialTable,
        Reward(0),
        true
      )

      val result = testbed.run[Reward](
        agent,
        runs,
        steps
      )

      (i, sum(result.meanRewards.slice(steps / 2, steps.toInt, 1)) / (steps / 2).toDouble)
    }}
  )

  val list = 
    (if (includeεGreedy) List(A) else List.empty) ++
    (if (includeStochasticGradientAscent) List(B) else List.empty) ++
    (if (includeUpperConfidenceBound) List(C) else List.empty) ++
    (if (includeBiasedεGreedy) List(D) else List.empty)
  
  import breeze.plot._

  val f = breeze.plot.Figure()
  val p = f.subplot(0)
  p.legend = true
  p.ylabel = "Avg reward over last 100,000 steps"
  p.xlabel = "parameter (ε, α, c, Q0)"
  //p.ylim(Math.min(0D, dvs.values.minBy(_.min).min), Math.max(0D, dvs.values.maxBy(_.max).max))

  list.foreach { case (name, xsys) =>
    p += plot(
      x = xsys.map((x, y) => x), 
      y = xsys.map((x, y) => y), 
      name = name
    )
  }
  
  p.logScaleX = true
  f.saveas(s"exercise2.11.png")
}
