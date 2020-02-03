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
  val meansOptimals: MeanOptimal = (1 to runs)
    .foldLeft(MeanOptimal(DenseVector.zeros[Double](steps), DenseVector.zeros[Double](steps)))((acc: MeanOptimal, elem: Int) => {
        val a = agent.copy()

        (1 to steps).foreach(_ => a.act)

        MeanOptimal(
          acc.meanRewards + DenseVector(a.history.map((_, reward) => reward.toDouble).toArray),
          acc.optimalActs + DenseVector(a.history.map((isOptimal, _) => isOptimal.toDouble).toArray)
        )
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
  dvs.foreach { (name, dv) =>
    p += plot(
      x = (1 to dv.length).map(_.toDouble).toSeq, 
      y = dv.toVector, 
      name = name)
    p.xlabel = xlabel
    p.ylabel = ylabel
    p.ylim(0D, 1D)
  }
  f.saveas(s"$fileName$ylabel.png")

}