package com.robertjneal.rl.testbed

import com.robertjneal.rl._
import com.robertjneal.rl.Types._
import org.apache.commons.math3.distribution._
import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable

class testbedTest {
  @Test def bestActionIsOptimal(): Unit = {
    val threeActions = Array(
      (Action("A"), RandomReward(new NormalDistribution(5, 0.1))),
      (Action("B"), RandomReward(new NormalDistribution(0, 0.1))),
      (Action("C"), RandomReward(new NormalDistribution(-5, 0.1)))
    )
    val environment = BanditEnvironment(
      threeActions.map(_._1).toVector,
      threeActions.toMap
    )
    def pickBest(map: mutable.Map[Action, Reward]): Action = {
      map.maxBy(_._2.toDouble)._1
    }
    def sampleAverage(actionRewards: mutable.Map[Action, Reward], currentAction: Action, currentReward: Reward, currentStep: Step): Unit = {
      val error = currentReward - actionRewards(currentAction)
      actionRewards(currentAction) = Reward(
        actionRewards(currentAction).toDouble + ( error.toDouble / currentStep.toInt.toDouble)
      )
    }
    val agent = TabularAgent(environment, OneState, pickBest, sampleAverage, true)

    val result: MeanOptimal = run(agent, 2000, 1000)

    // reward is the true reward of the best action, mas o menos
    assertEquals(result.meanRewards.sum / result.meanRewards.length, threeActions.head._2.trueReward.toDouble, 0.01)
    // optimal action selected 100% of the time
    assertEquals(result.optimalActs.sum / result.optimalActs.length, 1.0, 0.0)
  }
}