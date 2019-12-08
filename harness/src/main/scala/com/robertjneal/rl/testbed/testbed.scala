package com.robertjneal.rl.testbed

import breeze.linalg._
import com.robertjneal.rl._
import com.robertjneal.rl.Types._

case class MeanOptimal(meanRewards: DenseVector[Double], optimalActs: DenseVector[Double])

def test(agent: TabularAgent, runs: Int, steps: Int): MeanOptimal = {
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