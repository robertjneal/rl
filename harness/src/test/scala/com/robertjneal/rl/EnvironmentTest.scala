package com.robertjneal.rl

import com.robertjneal.rl._
import com.robertjneal.rl.types._
import org.apache.commons.math3.distribution._
import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable

class EnvironmentTest {
  @Test
  def isOptimal1() = {
    val bestRewardValue = 1.9
    val bestAction = Action(s"A$bestRewardValue")
    val best = (bestAction, StationaryDistribution(new NormalDistribution(bestRewardValue, 1)))
    val actionRewards: mutable.Map[Action, RandomReward] = mutable.Map(
        (Action("A=1.4"), StationaryDistribution(new NormalDistribution(1.4, 1))),
        best,
        (Action("C=1.1"), StationaryDistribution(new NormalDistribution(1.1, 1))),
    )

    val possibleActions = actionRewards.map { case (a, r) => a }.toVector
    val e = BanditEnvironment(possibleActions, actionRewards)

    for (a <- possibleActions) {
      assertEquals(a == bestAction, e.isOptimal(OneState, a))
    }
  }
}