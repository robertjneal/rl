package com.robertjneal.rl.actionselector

import com.robertjneal.rl.OneState
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import com.robertjneal.rl.updater._
import org.junit.Assert._
import org.junit.Test

class ActionSelectorsTest {
  import scala.language.implicitConversions

  private val acceptableMargin = 0.0123
  private val bestRewardValue = 1.9
  private val bestAction = Action(s"A$bestRewardValue")
  private val best = (bestAction, Reward(bestRewardValue))
  private val actionRewards: Map[Action, Reward] = Map(
    (Action("A=1.4"), Reward(1.4)),
    best,
    (Action("C=1.1"), Reward(1.1))
  )

  /*
    When always greedy, the best action should always be taken.
   */
  @Test
  def εGreedyTest() = {
    for (i <- 0 until 1000) {
      val result = εGreedy(Probability.unsafe(0))(
        Step(0),
        Map.empty[State, Map[Action, Step]]
      )(actionRewards)
      assertEquals((bestAction, false), result)
    }
  }

  /*
    When greedy 90% of the time with three possible actions, the best action should be taken 93.3% of the time.
   */
  @Test
  def εGreedy10Test() = {
    val iterations = 10000
    val actionsSelected =
      for (i <- 0 until iterations)
        yield εGreedy(Probability.unsafe(0.1))(
          Step(0),
          Map.empty[State, Map[Action, Step]]
        )(actionRewards)

    val bestChosen =
      actionsSelected.count{(action, _) => action == bestAction} / iterations.toDouble
    val expected = 0.9 + (.1 / 3.0)
    assertEquals(expected, bestChosen, acceptableMargin)
  }

  /*
    When greedy with three actions tied for best, each best action should be taken 33.3% of the time.
   */
  @Test
  def εGreedyTieTest() = {
    val bestAction2 = Action(s"A2$bestRewardValue")
    val bestAction3 = Action(s"A3$bestRewardValue")
    val threeGreedyActions = actionRewards + (bestAction2 -> Reward(
      bestRewardValue
    )) + (bestAction3 -> Reward(bestRewardValue))

    val iterations = 10000
    val actionsSelected =
      for (i <- 0 until iterations)
        yield εGreedy(Probability.unsafe(0))(
          Step(0),
          Map.empty[State, Map[Action, Step]]
        )(threeGreedyActions)

    val bestChosen =
      actionsSelected.count{(action, _) => action == bestAction} / iterations.toDouble
    val best2Chosen =
      actionsSelected.count{(action, _) => action == bestAction2} / iterations.toDouble
    val best3Chosen =
      actionsSelected.count{(action, _) => action == bestAction3} / iterations.toDouble

    val expected = 1d / 3d
    assertEquals(expected, bestChosen, acceptableMargin)
    assertEquals(expected, best2Chosen, acceptableMargin)
    assertEquals(expected, best3Chosen, acceptableMargin)
    assert(actionsSelected.forall{(_, isExploratory) => !isExploratory})
  }

  /*
    When c is high enough, each of n arms should be tried once within the first n times
   */
  @Test
  def upperConfidenceBoundHighCTest() = {
    val c = 4

    def agent(
        n: Int,
        actionSteps: Map[State, Map[Action, Step]],
        i: Int = 0,
        actionsSelected: Seq[Action] = Seq.empty
    ): Seq[Action] = {
      if (n == i) actionsSelected
      else {
        val action =
          upperConfidenceBound(c, OneState)(Step(i + 1), actionSteps)(
            actionRewards
          )
        agent(
          n,
          actionSteps.updated(
            OneState,
            actionSteps(OneState)
              .updated(action, actionSteps(OneState)(action).increment)
          ),
          i + 1,
          action +: actionsSelected
        )
      }
    }

    val iterations = actionRewards.size
    val actionsSelected = agent(
      iterations,
      Map(OneState -> actionRewards.map(ar => (ar._1, Step(1))))
    )

    actionRewards.keys.foreach(a =>
      assertEquals(1, actionsSelected.count(selected => selected == a))
    )
  }

  /*
    When c is 0, the best arm should be selected every time
   */
  @Test
  def upperConfidenceBoundZeroCTest() = {
    val c = 0

    def agent(
        n: Int,
        actionSteps: Map[State, Map[Action, Step]],
        i: Int = 0,
        actionsSelected: Seq[Action] = Seq.empty
    ): Seq[Action] = {
      if (n == i) actionsSelected
      else {
        val action =
          upperConfidenceBound(c, OneState)(Step(i + 1), actionSteps)(
            actionRewards
          )
        agent(
          n,
          actionSteps.updated(
            OneState,
            actionSteps(OneState)
              .updated(action, actionSteps(OneState)(action).increment)
          ),
          i + 1,
          action +: actionsSelected
        )
      }
    }

    val iterations = 100
    val actionsSelected = agent(
      iterations,
      Map(OneState -> actionRewards.map(ar => (ar._1, Step(1))))
    )

    actionRewards.keys.foreach(a =>
      assert(
        (actionsSelected.contains(a) && a == bestAction) || actionsSelected
          .count(selected => selected == a) == 0
      )
    )
  }

  @Test
  def softMaxProbabilities1Test() = {
    val action1 = Action("1")
    val action2 = Action("2")
    val action3 = Action("3")
    val inputExpectedOutput1 =
      Map(
        action1 -> (Preference(100.1), Probability.unsafe(0.45732888)),
        action2 -> (Preference(99.1), Probability.unsafe(0.16824189)),
        action3 -> (Preference(99.9), Probability.unsafe(0.37442922))
      )

    val output1 = softMaxProbabilities(inputExpectedOutput1.view.mapValues {
      _._1
    }.toMap)

    inputExpectedOutput1.foreach { (action, preferenceProbability) =>
      val (preference, expectedProbability) = preferenceProbability
      assertEquals(
        expectedProbability.toDouble,
        output1(action).toDouble,
        acceptableMargin
      )
    }
  }

  /*
    Most, by far, preferred action should be 100%
   */
  @Test
  def softMaxProbabilities2Test() = {
    val action1 = Action("1")
    val action2 = Action("2")
    val action3 = Action("3")
    val inputExpectedOutput2 =
      Map(
        action1 -> (Preference(10), Probability.unsafe(0.0)),
        action2 -> (Preference(1), Probability.unsafe(0.0)),
        action3 -> (Preference(100), Probability.unsafe(1.0))
      )

    val output1 = softMaxProbabilities(inputExpectedOutput2.view.mapValues {
      (pref, prob) => pref
    }.toMap)

    inputExpectedOutput2.foreach { (action, preferenceProbability) =>
      val (preference, expectedProbability) = preferenceProbability
      assertEquals(
        expectedProbability.toDouble,
        output1(action).toDouble,
        acceptableMargin
      )
    }
  }

  /*
    Mix of negative and positive inputs should result in probability [0, 1]
   */
  @Test
  def softMaxProbabilities3Test() = {
    val action1 = Action("1")
    val action2 = Action("2")
    val action3 = Action("3")
    val input =
      Map(
        action1 -> Preference(-100),
        action2 -> Preference(1),
        action3 -> Preference(100)
      )

    val output = softMaxProbabilities(input)

    output.foreach { (action, preferenceProbability) =>
      assertTrue(
        preferenceProbability.toDouble >= 0 && preferenceProbability.toDouble <= 1
      )
    }
  }

  /*
    Action should be selected proportional to their softmax probabilities
   */
  @Test
  def softMax1Test() = {
    val runs = 10000
    val action1 = Action("1")
    val action2 = Action("2")
    val action3 = Action("3")
    val input1 =
      Map(
        action1 -> Preference(100.1),
        action2 -> Preference(99.1),
        action3 -> Preference(99.9)
      )

    val expectedProbabilities = Map(
      action1 -> Probability.unsafe(0.45732888),
      action2 -> Probability.unsafe(0.16824189),
      action3 -> Probability.unsafe(0.37442922)
    )

    val initialCounts: Map[Action, Int] = input1.view.mapValues { x => 0 }.toMap

    val counts = (0 to runs).foldLeft(initialCounts) { (acc, x) =>
      val (chosenAction, _) = softMax(input1)
      acc.updated(chosenAction, acc(chosenAction) + 1)
    }

    expectedProbabilities.foreach { (action, probability) =>
      assertEquals(
        probability.toDouble,
        counts(action).toDouble / runs,
        acceptableMargin
      )
    }
  }

  /*
    Most, by far, preferred action should always be selected
   */
  @Test
  def softMax2Test() = {
    val action1 = Action("1")
    val action2 = Action("2")
    val action3 = Action("3")
    val input2 =
      Map(
        action1 -> Preference(10),
        action2 -> Preference(1),
        action3 -> Preference(100)
      )

    (0 to 1000).foreach { x =>
      assertEquals((action3, false), softMax(input2))
    }
  }
}
