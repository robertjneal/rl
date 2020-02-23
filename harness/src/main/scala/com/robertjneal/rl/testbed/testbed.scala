package com.robertjneal.rl.testbed

import breeze.linalg.{Vector => BreezeVector, _}
import com.robertjneal.rl._
import com.robertjneal.rl.types._
import org.apache.commons.math3.distribution._

case class MeanOptimal(meanRewards: DenseVector[Double], optimalActs: DenseVector[Double])

lazy val tenArmEnvironment: BanditEnvironment = {
  val random = new NormalDistribution(0, 1)
  val actions: Vector[Action] = Range(0, 9).map(n => Action(n.toString)).toVector
  val actionValues: Action => RandomReward = actions.map(a =>
    (a, StationaryDistribution(new NormalDistribution(random.sample, 1)))
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

    (1 to steps).foreach(_ => a.act)

    MeanOptimal(
      DenseVector(a.history.map((_, reward) => reward.toDouble).toArray),
      DenseVector(a.history.map((isOptimal, _) => isOptimal.toDouble).toArray)
    )
  }

  for (i <- 0 to 10) println("first ten: " + meansOptimalsList.head.meanRewards(i))

  val meansOptimals: MeanOptimal = meansOptimalsList.reduce((mo1: MeanOptimal, mo2: MeanOptimal) => {
    val mo = MeanOptimal(mo1.meanRewards + mo2.meanRewards, mo1.optimalActs + mo2.optimalActs)
    var i = 0
    mo.meanRewards.foreach { x =>
      if (x.isNaN) println ("NaN: " + mo1.meanRewards(i) + ", " + mo2.meanRewards(i))
      i = i + 1
    }
    mo
  })
    
  for (i <- 0 to 10) println("combined: " + meansOptimals.meanRewards(i))

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
  dvs.foreach { (name, dv) =>
    p += plot(
      x = (1 to dv.length).map(_.toDouble).toSeq, 
      y = dv.toVector, 
      name = name)
    p.xlabel = xlabel
    p.ylabel = ylabel
    p.ylim(0D, Math.max(1D, dv.max))
  }
  f.saveas(s"$fileName$ylabel.png")

}