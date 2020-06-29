package com.robertjneal.rl

import com.robertjneal.rl._
import com.robertjneal.rl.actionselector._
import com.robertjneal.rl.agent._
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import com.robertjneal.rl.updater._
import org.apache.commons.math3.distribution._
import org.junit.Test
import org.junit.Assert._

class AgentTest {
  @Test
  def act() = {
    val bestRewardValue = 1.9
    val bestAction = Action(s"A$bestRewardValue")
    val best = (bestAction, StationaryDistribution(new NormalDistribution(bestRewardValue, 1)))
    val actionRewards: Map[Action, RandomReward] = Map(
        (Action("A=1.4"), StationaryDistribution(new NormalDistribution(1.4, 1))),
        best,
        (Action("C=1.1"), StationaryDistribution(new NormalDistribution(1.1, 1))),
    )

    val possibleActions = actionRewards.map { case (a, r) => a }.toVector
    val e = BanditEnvironment(possibleActions, actionRewards)

    val agent = TabularAgent.rewardBlankSlate(
      e,
      εGreedy(Probability.unsafe(0.1)),
      average(sampleAverage)
    )

    val result1: TabularAgent[Reward] = agent.act

    assertEquals(agent.e.possibleStateActions, result1.e.possibleStateActions)
    assertEquals(1, result1.actionSteps(OneState).values.map(_.toInt).sum - actionRewards.size)
    assertEquals(result1.step, result1.actionSteps(OneState).values.map(_.toInt).sum - actionRewards.size)

    val result2: TabularAgent[Reward] = result1.act

    assertEquals(result1.e.possibleStateActions, result2.e.possibleStateActions)
    assertEquals(2, result2.actionSteps(OneState).values.map(_.toInt).sum - actionRewards.size)
    assertEquals(result2.step, result2.actionSteps(OneState).values.map(_.toInt).sum - actionRewards.size)
  }
}