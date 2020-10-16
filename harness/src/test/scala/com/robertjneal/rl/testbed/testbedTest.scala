package com.robertjneal.rl.testbed

import com.robertjneal.rl._
import com.robertjneal.rl.agent._
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import org.apache.commons.math3.distribution._
import org.junit.Test
import org.junit.Assert._

class testbedTest {
  @Test def bestActionIsOptimal(): Unit = {
    val threeActions = Array(
      (
        Action("A"),
        StationaryDistribution(
          new NormalDistribution(ThreadLocalRandomGenerator(), 5, 0.1)
        )
      ),
      (
        Action("B"),
        StationaryDistribution(
          new NormalDistribution(ThreadLocalRandomGenerator(), 0, 0.1)
        )
      ),
      (
        Action("C"),
        StationaryDistribution(
          new NormalDistribution(ThreadLocalRandomGenerator(), -5, 0.1)
        )
      )
    )
    val environment = BanditEnvironment(
      threeActions.map(_._1).toVector,
      threeActions.toMap
    )
    def pickBest(step: Step, actionsSteps: Map[State, Map[Action, Step]])(
        map: Map[Action, Reward]
    ): Action = {
      map.maxBy(_._2.toDouble)._1
    }
    def sampleAverage(
        actionRewards: Map[Action, Reward],
        currentAction: Action,
        currentReward: Reward,
        currentStep: Step,
        averageReward: Option[Reward] = None
    ): Map[Action, Reward] = {
      val error = currentReward - actionRewards(currentAction)
      actionRewards.updated(
        currentAction,
        Reward(
          actionRewards(
            currentAction
          ).toDouble + (error.toDouble / currentStep.toInt.toDouble)
        )
      )
    }
    val agent =
      TabularAgent.rewardBlankSlate(environment, pickBest, sampleAverage, true)

    val result: MeanOptimal = run(agent, 2000, 1000)

    // reward is the true reward of the best action, mas o menos
    assertEquals(
      threeActions.head._2.trueReward.toDouble,
      result.meanRewards.sum / result.meanRewards.length,
      0.01
    )
    // optimal action selected 100% of the time
    assertEquals(1.0, result.optimalActs.sum / result.optimalActs.length, 0.0)
  }
}
