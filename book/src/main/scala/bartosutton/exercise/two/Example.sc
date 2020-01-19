import com.robertjneal.rl._
import com.robertjneal.rl.Types._
import org.apache.commons.math3.distribution._
import scala.collection.mutable

val actionRewards: Map[Action, RandomReward] = Map(
  (Action.fromString("A"), RandomReward(new NormalDistribution(1, .2))),
  Action.fromString("B") -> RandomReward(new NormalDistribution(12, .2)),
  Action.fromString("C") -> RandomReward(new NormalDistribution(9, .2))
)

def reward(a: Action): RandomReward = {
  actionRewards(a)
}

val e = BanditEnvironment(
  actionRewards.keys.toVector,
  reward
)

val agent = TabularAgent(
  e,
  OneState,
  bartosutton.exercise.two.εGreedy(Probability.unsafe(0.1)),
  bartosutton.exercise.two.sampleAverage,
  true
)

import breeze.linalg._

val runs = 500
val steps = 500
val meanReward: DenseVector[Double] = (1 to runs)
  .foldLeft(DenseVector.zeros[Double](runs))((acc, elem) => {
    val ta = TabularAgent(
      e,
      OneState,
      bartosutton.exercise.two.εGreedy(Probability.unsafe(0.1)),
      bartosutton.exercise.two.sampleAverage,
      true
    )

    (1 to steps).foreach(_ => ta.act)
    acc + DenseVector(ta.history.map((_, reward) => reward.toDouble).toArray) 
  }).values.map(_ / runs)

  meanReward.foreach(println)

import breeze.plot._

val f = breeze.plot.Figure()
val p = f.subplot(0)
p.legend = true
p += plot(
  x = (1 to steps).map(_.toDouble).toSeq, 
  y = meanReward.toVector, 
  name = "run 1")
f.saveas("reward.png")
