package bartosutton.exercise.two

import com.robertjneal.rl.types._
import org.junit.Assert._
import org.junit.Test
import scala.collection.mutable

class TwoTest {
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
        assertEquals(expected, bestChosen, 0.01)
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
        assertEquals(expected, bestChosen, 0.01)
        assertEquals(expected, best2Chosen, 0.01)
        assertEquals(expected, best3Chosen, 0.01)
    }

}