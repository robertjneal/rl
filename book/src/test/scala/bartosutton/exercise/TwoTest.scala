package bartosutton.exercise.two

import com.robertjneal.rl.types._
import org.junit.Assert._
import org.junit.Test
import scala.collection.mutable

class TwoTest {
    import scala.language.implicitConversions

    val acceptableMargin = 0.01
    val bestRewardValue = 1.9
    val bestAction = Action(s"A$bestRewardValue")
    val best = (bestAction, Reward(bestRewardValue))
    val actionRewards: mutable.Map[Action, Reward] = mutable.Map(
        (Action("A=1.4"), Reward(1.4)),
        best,
        (Action("C=1.1"), Reward(1.1)),
    )

    /*
    When always greedy, the best action should always be taken.
    */
    @Test
    def εGreedyTest() = {
        for (i <- 0 until 1000) {
            val result = εGreedy(Probability.unsafe(0))(actionRewards)
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
            yield εGreedy(Probability.unsafe(0.1))(actionRewards)

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
        val threeGreedyActions = actionRewards.addOne((bestAction2 -> Reward(bestRewardValue))).addOne((bestAction3 -> Reward(bestRewardValue)))
        
        val iterations = 10000
        val actionsSelected = for (i <- 0 until iterations) 
            yield εGreedy(Probability.unsafe(0))(threeGreedyActions)
            
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
        val numbersToAverage = Vector.fill(size)(scala.util.Random.nextInt(1000))
        val actionRewards = mutable.Map((action, Reward(0)))
        val expected = numbersToAverage.sum / size.toDouble

        Range(0, size).foreach { n =>
            average(sampleAverage)(actionRewards, action, Reward(numbersToAverage(n).toDouble), Step(n + 1))
        }

        assertEquals(expected, actionRewards(action).toDouble, acceptableMargin)
    }

    /*
    Exponential recency weighted average should equal the last number
    "(If 1 − α = 0, then all the weight goes on the very last reward, Rn, because of the convention that 0^0 = 1.)"
    */
    @Test
    def exponentialRecencyWeightedAverageTest() = {
        val size = 101
        val action = Action("A")
        val orderedRewards = Vector.fill(size)(scala.util.Random.nextInt(1000))
        val actionRewards = mutable.Map((action, Reward(0)))
        val expected = orderedRewards.last.toDouble

        Range(0, size).foreach { n =>
            average(exponentialRecencyWeightedAverage(1))(actionRewards, action, Reward(orderedRewards(n).toDouble), Step(n + 1))
        }

        assertEquals(expected, actionRewards(action).toDouble, acceptableMargin)
    }

    /*
    Recency weight average should equal (1-α)^n*Q1+Σ[i=1->n]α(1-α)^(n-i)*Ri
    */
    @Test
    def recencyWeightedAverageTest() = {
        val α = 0.7
        val size = 101
        val action = Action("A")
        val orderedRewards = Vector.fill(size)(scala.util.Random.nextInt(1000))
        val actionRewards = mutable.Map((action, Reward(0)))
        val expected = Math.pow(1-α, size) * 0 +
            (for (i <- 1 to size) yield {
                α * Math.pow((1 - α), size - i) * orderedRewards(i - 1)
            }).sum

        Range(0, size).foreach { n =>
            average(exponentialRecencyWeightedAverage(α))(actionRewards, action, Reward(orderedRewards(n).toDouble), Step(n + 1))
        }

        assertEquals(expected, actionRewards(action).toDouble, acceptableMargin)
    }
}