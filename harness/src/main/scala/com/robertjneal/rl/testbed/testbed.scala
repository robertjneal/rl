package com.robertjneal.rl.testbed

import breeze.linalg.{Vector => BreezeVector, _}
import com.robertjneal.rl._
import com.robertjneal.rl.types._
import java.util.concurrent.ThreadLocalRandom
import org.apache.commons.math3.distribution._
import org.apache.commons.math3.random.RandomGenerator

case class MeanOptimal(meanRewards: DenseVector[Double], optimalActs: DenseVector[Double])

class ThreadLocalRandomGenerator extends RandomGenerator {
  private val rng = ThreadLocalRandom.current
  
  def nextBoolean(): Boolean = rng.nextBoolean
  def	nextBytes(bytes: Array[Byte]): Unit = rng.nextBytes(bytes)
  def nextDouble(): Double = rng.nextDouble
  def	nextFloat(): Float = rng.nextFloat
  def nextGaussian(): Double = rng.nextGaussian
  def	nextInt(): Int = rng.nextInt
  def	nextInt(n: Int): Int = rng.nextInt(n)
  def nextLong(): Long = rng.nextLong
  def setSeed(seed: Int): Unit = rng.setSeed(seed.toLong)
  def setSeed(seed: Array[Int]): Unit = {
    // the following number is the largest prime that fits in 32 bits (it is 2^32 - 5)
    val prime: Long = 4294967291L

    val combined = seed.foldLeft(0L)((acc, elem) => 
      {
        acc * prime + elem
      }
    )

    setSeed(combined)
  }
  def setSeed(seed: Long): Unit = rng.setSeed(seed)
}

lazy val tenArmEnvironment: BanditEnvironment = {
  val random = new NormalDistribution(0, 1)
  val actions: Vector[Action] = Range(0, 9).map(n => Action(n.toString)).toVector
  val actionValues: Map[Action, RandomReward] = actions.map(a =>
    (a, StationaryDistribution(new NormalDistribution(ThreadLocalRandomGenerator(), random.sample, 1)))
  ).toMap
  val environment = BanditEnvironment(
    actions,
    actionValues
  )
  environment
}

def run(agent: TabularAgent, runs: Int, steps: Int): MeanOptimal = {
  import scala.collection.parallel.ParSeq
  import scala.collection.parallel.CollectionConverters._
  
  val meansOptimalsList: ParSeq[MeanOptimal] = (1 to runs).par.map { elem =>
    val a = agent.copy()

    def continueActing(actable: TabularAgent, counter: Int): TabularAgent = {
      if (counter <= 0) actable
      else continueActing(actable.act, counter - 1)
    }

    val agentAtFinalState = continueActing(a, steps)

    MeanOptimal(
      DenseVector(agentAtFinalState.history.map((_, reward) => reward.toDouble).toArray),
      DenseVector(agentAtFinalState.history.map((isOptimal, _) => isOptimal.toDouble).toArray)
    )
  }

  val meansOptimals: MeanOptimal = meansOptimalsList.reduce((mo1: MeanOptimal, mo2: MeanOptimal) => {
    MeanOptimal(mo1.meanRewards + mo2.meanRewards, mo1.optimalActs + mo2.optimalActs)
  })
    
  MeanOptimal(
    meansOptimals.meanRewards / runs.toDouble,
    meansOptimals.optimalActs / runs.toDouble
  )  
}  

def generatePlot(dvs: Map[String, DenseVector[Double]], fileName: String, ylabel: String, xlabel: String = "steps"): Unit = {
  import breeze.plot._

  val f = breeze.plot.Figure()
  val p = f.subplot(0)
  p.legend = true
  p.xlabel = xlabel
  p.ylabel = ylabel
  p.ylim(Math.min(0D, dvs.values.minBy(_.min).min), Math.max(0D, dvs.values.maxBy(_.max).max))
  dvs.foreach { (name, dv) =>
    p += plot(
      x = (1 to dv.length).map(_.toDouble).toSeq, 
      y = dv.toVector, 
      name = name)
  }
  f.saveas(s"$fileName$ylabel.png")

}