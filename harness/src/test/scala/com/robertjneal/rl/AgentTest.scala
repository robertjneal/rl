package com.robertjneal.rl

import com.robertjneal.rl._
import com.robertjneal.rl.environment._
import com.robertjneal.rl.actionselector._
import com.robertjneal.rl.agent._
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import com.robertjneal.rl.updater._
import org.apache.commons.math3.distribution._
import org.junit.Test
import org.junit.Assert._
import scala.util.Random

class AgentTest {
  @Test
  def act() = {
    given Random = Random(171406)

    val bestRewardValue = 1.9
    val bestAction = Action(s"A$bestRewardValue")
    val best = (
      bestAction,
      StationaryDistribution(new NormalDistribution(bestRewardValue, 1))
    )
    val actionRewards: Map[Action, RandomReward] = Map(
      (Action("A=1.4"), StationaryDistribution(new NormalDistribution(1.4, 1))),
      best,
      (Action("C=1.1"), StationaryDistribution(new NormalDistribution(1.1, 1)))
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
    assertEquals(
      1L,
      result1
        .actionSteps(OneState)
        .values
        .map(_.toInt)
        .sum - actionRewards.size.toLong
    )
    assertEquals(
      result1.step.toInt.toLong,
      result1
        .actionSteps(OneState)
        .values
        .map(_.toInt)
        .sum - actionRewards.size.toLong
    )

    val result2: TabularAgent[Reward] = result1.act

    assertEquals(result1.e.possibleStateActions, result2.e.possibleStateActions)
    assertEquals(
      2L,
      result2
        .actionSteps(OneState)
        .values
        .map(_.toInt)
        .sum - actionRewards.size.toLong
    )
    assertEquals(
      result2.step.toInt.toLong,
      result2
        .actionSteps(OneState)
        .values
        .map(_.toInt)
        .sum - actionRewards.size.toLong
    )
  }

  @Test
  def actTemporalDifference() = {
    given Random = Random(171406)
    case class TestEnvironment(stateActions: Map[State, Vector[Action]], override val state: State) 
    extends Environment(stateActions, state) {
      def act(a: Action): (Reward, Environment, EndOfEpisode) = {
        val reward = Reward(
          a match {
            case Action("A") => 1
            case Action("B") => 2
            case Action("C") => 3
          }
        )
        val newState = 
          a match {
            case Action("A") => State("2")
            case Action("B") => State("3")
            case Action("C") => State("3")
            case Action("D") => State("3")
          }
        (reward, this.copy(state = newState), true)
      }
      def isOptimal(a: Action): OptimalAct = OptimalAct(true)
    }

    val stateActions: Map[State, Vector[Action]] = Map(
      State("1") -> Vector(Action("A"), Action("B")),
      State("2") -> Vector(Action("C")),
      State("3") -> Vector(Action("D"))
    )
    val e = TestEnvironment(stateActions, State("1")) 

    val agent = TabularAgent(
      e,
      εGreedy(Probability.Never),
      average(sampleAverage),
      Step(1),
      Reward(0),
      Map(
        State("1") -> Map(
          Action("A") -> Step(2),
          Action("B") -> Step(2)
        ),
        State("2") -> Map(
          Action("C") -> Step(2)
        ),
        State("3") -> Map(
          Action("D") -> Step(2)
        )
      ),
      Map(
        State("1") -> Map(
          Action("A") -> Reward(0.5),
          Action("B") -> Reward(0.25)
        ),
        State("2") -> Map(
          Action("C") -> Reward(1)
        ),
        State("3") -> Map(
          Action("D") -> Reward(1)
        )
      )
    )
  
    // Values are initialized properly
    assertEquals(Reward(0.5), agent.table(State("1"))(Action("A")))
    val agent2 = agent.act
    println(agent2.e)
    val agent3 = agent2.act
    // Value is updated properly
    assertEquals(Reward(0.75), agent3.table(State("1"))(Action("A")))
    val agent4 = agent3.copy(e = TestEnvironment(stateActions, State("1")))
    val agent5 = agent4.act
    // End state is not updated if we switch states
    assertEquals(Reward(1), agent5.table(State("3"))(Action("D")))

  }
}
