package com.robertjneal.rl.actionselector

import com.robertjneal.rl.OneState
import com.robertjneal.rl.types._
import org.junit.Assert._
import org.junit.Test
import scala.annotation.tailrec

class TwoTest {
    import scala.language.implicitConversions

    private val acceptableMargin = 0.01
    private val bestRewardValue = 1.9
    private val bestAction = Action(s"A$bestRewardValue")
    private val best = (bestAction, Reward(bestRewardValue))
    private val actionRewards: Map[Action, Reward] = Map(
        (Action("A=1.4"), Reward(1.4)),
        best,
        (Action("C=1.1"), Reward(1.1)),
    )

    @tailrec
    private def updatedActionRewards(averageMethod: (Step) => Double, actionRewardsToUpdate: Map[Action, Reward], numbers: List[Int], action: Action, originalSize: Int): Map[Action, Reward] = {
      if (numbers.length == 0) actionRewardsToUpdate
      else {
        val n = originalSize - numbers.length
        val hd::tl = numbers
        updatedActionRewards(
          averageMethod,
          average(averageMethod)(actionRewardsToUpdate, action, Reward(hd.toDouble), Step(n + 1)),
          tl,
          action,
          originalSize
        )
      }
    }

    /*
    When always greedy, the best action should always be taken.
    */
    @Test
    def εGreedyTest() = {
        for (i <- 0 until 1000) {
            val result = εGreedy(Probability.unsafe(0))(Step(0), Map.empty[State, Map[Action, Step]])(actionRewards)
            assertEquals(bestAction, result)
        }
    }

    /*
    When greedy 90% of the time with three possible actions, the best action should be taken 93.3% of the time.
    */
    @Test
    def εGreedy10Test() = {
        val iterations = 10000
        val actionsSelected = for (i <- 0 until iterations) 
            yield εGreedy(Probability.unsafe(0.1))(Step(0), Map.empty[State, Map[Action, Step]])(actionRewards)

        val bestChosen = actionsSelected.count(_ == bestAction) / iterations.toDouble
        val expected = 0.9 + (.1/3.0)
        assertEquals(expected, bestChosen, acceptableMargin)
    }

    /*
    When greedy with three actions tied for best, each best action should be taken 33.3% of the time.
    */
    @Test
    def εGreedyTieTest() = {
        val bestAction2 = Action(s"A2$bestRewardValue")
        val bestAction3 = Action(s"A3$bestRewardValue")
        val threeGreedyActions = actionRewards + (bestAction2 -> Reward(bestRewardValue)) + (bestAction3 -> Reward(bestRewardValue))
        
        val iterations = 10000
        val actionsSelected = for (i <- 0 until iterations) 
            yield εGreedy(Probability.unsafe(0))(Step(0), Map.empty[State, Map[Action, Step]])(threeGreedyActions)
            
        val bestChosen = actionsSelected.count(_ == bestAction) / iterations.toDouble
        val best2Chosen = actionsSelected.count(_ == bestAction2) / iterations.toDouble
        val best3Chosen = actionsSelected.count(_ == bestAction3) / iterations.toDouble

        val expected = 1D/3D
        assertEquals(expected, bestChosen, acceptableMargin)
        assertEquals(expected, best2Chosen, acceptableMargin)
        assertEquals(expected, best3Chosen, acceptableMargin)
    }

    /*
    Sample average should be the mean
    */
    @Test
    def unweightedSampleAverageTest() = {
        val size = 101
        val action = Action("A")
        val numbersToAverage = List.fill(size)(scala.util.Random.nextInt(1000))
        val actionRewards = Map((action, Reward(0)))
        val expected = numbersToAverage.sum / size.toDouble

        val finalActionRewards = updatedActionRewards(sampleAverage, actionRewards, numbersToAverage, action, numbersToAverage.length)

        assertEquals(expected, finalActionRewards(action).toDouble, acceptableMargin)
    }

    /*
    Exponential recency weighted average should equal the last number
    "(If 1 − α = 0, then all the weight goes on the very last reward, Rn, because of the convention that 0^0 = 1.)"
    */
    @Test
    def exponentialRecencyWeightedAverageTest() = {
        val size = 101
        val action = Action("A")
        val orderedRewards = List.fill(size)(scala.util.Random.nextInt(1000))
        val actionRewards = Map((action, Reward(0)))
        val expected = orderedRewards.last.toDouble

        val finalActionRewards = updatedActionRewards(exponentialRecencyWeightedAverage(1), actionRewards, orderedRewards, action, orderedRewards.length)

        assertEquals(expected, finalActionRewards(action).toDouble, acceptableMargin)
    }

    /*
    Recency weight average should equal (1-α)^n*Q1+Σ[i=1->n]α(1-α)^(n-i)*Ri
    */
    @Test
    def recencyWeightedAverageTest() = {
        val α = 0.7
        val size = 101
        val action = Action("A")
        val orderedRewards = List.fill(size)(scala.util.Random.nextInt(1000))
        val actionRewards = Map((action, Reward(0)))
        val expected = Math.pow(1-α, size) * 0 +
            (for (i <- 1 to size) yield {
                α * Math.pow((1 - α), size - i) * orderedRewards(i - 1)
            }).sum

        val finalActionRewards = updatedActionRewards(exponentialRecencyWeightedAverage(α), actionRewards, orderedRewards, action, orderedRewards.length)

        assertEquals(expected, finalActionRewards(action).toDouble, acceptableMargin)
    }

    /*
    When c is high enough, each of n arms should be tried once within the first n times
    */
    @Test
    def upperConfidenceBoundTest() = {
        val c = 4

        def agent(n: Int, actionSteps: Map[State, Map[Action, Step]], i: Int = 0, actionsSelected: Seq[Action] = Seq.empty): Seq[Action] = {
            if (n == i) actionsSelected
            else {
                val action = upperConfidenceBound(c, OneState)(Step(i), actionSteps)(actionRewards)
                agent(n,
                    actionSteps.updated(OneState, 
                        actionSteps(OneState).updated(action,
                            actionSteps(OneState)(action).increment
                        )
                    ),
                    i + 1, 
                    action +: actionsSelected
                )
            }
        }

        val iterations = actionRewards.size
        val actionsSelected = agent(iterations, Map(OneState -> actionRewards.map(ar => (ar._1, Step(1)))))

        actionRewards.keys.foreach(a =>    
            assertEquals(1, actionsSelected.count(selected => selected == a))
        )
    }
}