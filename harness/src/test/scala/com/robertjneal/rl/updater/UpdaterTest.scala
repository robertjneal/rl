package com.robertjneal.rl.updater

import com.robertjneal.rl.types._
import org.junit.Assert._
import org.junit.Test
import scala.annotation.tailrec

class UpdaterTest {
  private val acceptableMargin = 0.0111
  
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
    If the reward is higher than its previous average, its preference should go up
    */
    @Test
    def stochasticGradientAscentHigherTest() = {
      val α = 0.1
      val actionRewardPreference1 = (Action("A") -> (Reward(1), Preference(100)))
      val actionRewardPreference2 = (Action("B") -> (Reward(2), Preference(120)))
      val actionRewardPreference3 = (Action("C") -> (Reward(3), Preference(150)))
      val actionRewardPreferences = Map(
        actionRewardPreference1,
        actionRewardPreference2,
        actionRewardPreference3
      )

      val updated = stochasticGradientAscent(α)(actionRewardPreferences, Action("A"), Reward(5), Step(100))
      updated.foreach {
        (action, rewardPreference) =>
          val (_, preference) = rewardPreference
          val (_, initialPreference) = actionRewardPreferences(action)
          if (action == Action("A")) assertTrue(preference > initialPreference)
          else assertTrue(preference <= initialPreference)
      }
    }

    /*
    If the reward is lower than its previous average, its preference should go down
    */
    @Test
    def stochasticGradientAscentLowerTest() = {
      val α = 0.1
      val actionRewardPreference1 = (Action("A") -> (Reward(1), Preference(100)))
      val actionRewardPreference2 = (Action("B") -> (Reward(2), Preference(120)))
      val actionRewardPreference3 = (Action("C") -> (Reward(3), Preference(150)))
      val actionRewardPreferences = Map(
        actionRewardPreference1,
        actionRewardPreference2,
        actionRewardPreference3
      )

      val updated = stochasticGradientAscent(α)(actionRewardPreferences, Action("B"), Reward(1), Step(10))
      updated.foreach {
        (action, rewardPreference) =>
          val (r, preference) = rewardPreference
          val (ir, initialPreference) = actionRewardPreferences(action)
          println(s"action: $action")
          println(s"ir: $ir, cr: $r")
          println(s"ip: $initialPreference, cp: $preference")
          if (action == Action("B")) assertTrue(preference < initialPreference)
          else assertTrue(preference >= initialPreference)
      }
    }

    /*
    If the reward is equalt to its previous average, the preferences should remain unchanged
    */
    @Test
    def stochasticGradientAscentEqualTest() = {
      val α = 0.1
      val actionRewardPreference1 = (Action("A") -> (Reward(1), Preference(100)))
      val actionRewardPreference2 = (Action("B") -> (Reward(2), Preference(120)))
      val actionRewardPreference3 = (Action("C") -> (Reward(3), Preference(150)))
      val actionRewardPreferences = Map(
        actionRewardPreference1,
        actionRewardPreference2,
        actionRewardPreference3
      )

      val updated = stochasticGradientAscent(α)(actionRewardPreferences, Action("B"), Reward(1), Step(10))
      updated.foreach {
        (action, rewardPreference) =>
          val (r, preference) = rewardPreference
          val (ir, initialPreference) = actionRewardPreferences(action)
          println(s"action: $action")
          println(s"ir: $ir, cr: $r")
          println(s"ip: $initialPreference, cp: $preference")
          if (action == Action("B")) assertTrue(preference < initialPreference)
          else assertTrue(preference >= initialPreference)
      }
    }
}