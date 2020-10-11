package com.robertjneal.rl.policyiteration

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import org.junit.Test
import org.junit.Assert._

class policyIterationTest {
    @Test
    def expectedUpdateTest() = {
        val stateActionProbabilities = Map(
            State("S1") -> List(
                ActionProbability(Action("A1"), Probability.CoinToss),
                ActionProbability(Action("A2"), Probability.CoinToss)
            ),
            State("S2") -> List(
                ActionProbability(Action("A1"), Probability.unsafe(0.33)),
                ActionProbability(Action("A2"), Probability.unsafe(0.33)),
                ActionProbability(Action("A3"), Probability.unsafe(0.34))
            ),
            State("S3") -> List(
                ActionProbability(Action("A1"), Probability.unsafe(0.1)),
                ActionProbability(Action("A2"), Probability.unsafe(0.9))
            )
        )
        val stateTransitions = Map(
            StateAction(State("S1"), Action("A1")) -> List(
                ProbabilityState(Probability.CoinToss, State("S2")),
                ProbabilityState(Probability.CoinToss, State("S3"))
            ),
            StateAction(State("S1"), Action("A2")) -> List(
                ProbabilityState(Probability.CoinToss, State("S1")),
                ProbabilityState(Probability.CoinToss, State("S3"))
            ),
            StateAction(State("S2"), Action("A1")) -> List(
                ProbabilityState(Probability.Certain, State("S1")),
                ProbabilityState(Probability.Never, State("S3"))
            ),
            StateAction(State("S2"), Action("A2")) -> List(
                ProbabilityState(Probability.unsafe(0.1), State("S1")),
                ProbabilityState(Probability.unsafe(0.9), State("S3"))
            ),
            StateAction(State("S2"), Action("A3")) -> List(
                ProbabilityState(Probability.unsafe(0.33), State("S1")),
                ProbabilityState(Probability.unsafe(0.33), State("S2")),
                ProbabilityState(Probability.unsafe(0.34), State("S3"))
            ),
            StateAction(State("S3"), Action("A1")) -> List(
                ProbabilityState(Probability.CoinToss, State("S1")),
                ProbabilityState(Probability.CoinToss, State("S2"))
            ),
            StateAction(State("S3"), Action("A2")) -> List(
                ProbabilityState(Probability.unsafe(0.1), State("S1")),
                ProbabilityState(Probability.unsafe(0.8), State("S2")),
                ProbabilityState(Probability.unsafe(0.1), State("S3"))
            )
        )
        def reward(action: Action)(state: State): Reward = {
            (action, state) match {
                case (Action("A1"), State("S1")) => Reward(1)
                case (Action("A1"), State("S2")) => Reward(2)
                case (Action("A1"), State("S3")) => Reward(3)
                case (Action("A2"), State("S1")) => Reward(2)
                case (Action("A2"), State("S2")) => Reward(4)
                case (Action("A2"), State("S3")) => Reward(5)
                case (Action("A3"), State("S1")) => Reward(0.5)
                case (Action("A3"), State("S2")) => Reward(1)
                case (Action("A3"), State("S3")) => Reward(1.5)
                case (_, _) => Reward(0)
            }
        }

        val ev = expectedUpdate(
            stateActionProbabilities,
            stateTransitions,
            reward,
            0,
            0.1,
            stateActionProbabilities.view.mapValues(_ => Reward(0)).toMap,
            State("S1"),
            stateActionProbabilities.view.mapValues(_ => Double.PositiveInfinity).toMap,
            IterationType.Policy
        )

        /*
        S1 = 3
          A1 - 0.5 * ((0.5 * S2[2]) + (0.5 * S3[3])) = 1.25
          A2 - 0.5 * ((0.5 * S1[2]) + (0.5 * S3[5])) = 1.75
        S2 = 2.2227
          A1 - 1/3 * S1[1] = 0.33
          A2 - 1/3 * ((0.1 * S1[2]) + (0.9 * S3[5])) = 1.551
          A3 - 1/3 * ((1/3 * S1[0.5]) + (1/3 * S2[1]) + (1/3 * S3[1.5])) = 0.3417 
        S3 = 3.66
          A1 - 0.1 * ((0.5 * S1[1]) + (0.5 * S2[2])) = 0.15
          A2 - 0.9 * ((0.1 * S1[2]) + (0.8 * S2[4]) + (0.1 * S3[5])) = 3.51
        */

        assert(ev.values.toList == List(Reward(3), Reward(2.2227), Reward(3.66)))
    }
}