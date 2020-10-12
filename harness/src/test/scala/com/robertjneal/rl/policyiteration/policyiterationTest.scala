package com.robertjneal.rl.policyiteration

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import org.junit.Test
import org.junit.Assert._

class policyIterationTest {
    @Test
    def expectedUpdateNoDiscountTest() = {
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

    @Test
    def expectedUpdateWithDiscountTest() = {
        val stateActionProbabilities = Map(
            State("S1") -> List(
                ActionProbability(Action("A1"), Probability.CoinToss),
                ActionProbability(Action("A2"), Probability.CoinToss)
            ),
            State("S2") -> List(
                ActionProbability(Action("A3"), Probability.CoinToss),
                ActionProbability(Action("A4"), Probability.CoinToss)
            ),
            State("S3") -> List(
                ActionProbability(Action("A5"), Probability.CoinToss),
                ActionProbability(Action("A6"), Probability.CoinToss)
            )
        )
        val stateTransitions = Map(
            StateAction(State("S1"), Action("A1")) -> List(
                ProbabilityState(Probability.Certain, State("S2"))
            ),
            StateAction(State("S1"), Action("A2")) -> List(
                ProbabilityState(Probability.Certain, State("S3"))
            ),
            StateAction(State("S2"), Action("A3")) -> List(
                ProbabilityState(Probability.Certain, State("S1"))
            ),
            StateAction(State("S2"), Action("A4")) -> List(
                ProbabilityState(Probability.Certain, State("S3"))
            ),
            StateAction(State("S3"), Action("A5")) -> List(
                ProbabilityState(Probability.Certain, State("S1"))
            ),
            StateAction(State("S3"), Action("A6")) -> List(
                ProbabilityState(Probability.Certain, State("S2"))
            )
        )
        def reward(action: Action)(state: State): Reward = {
            (action, state) match {
                case (Action("A1"), State("S2")) => Reward(1)
                case (Action("A2"), State("S3")) => Reward(3)
                case (Action("A3"), State("S1")) => Reward(2)
                case (Action("A4"), State("S3")) => Reward(4)
                case (Action("A5"), State("S1")) => Reward(3)
                case (Action("A6"), State("S2")) => Reward(1)
                case (_, _) => Reward(0)
            }
        }

        val ev = expectedUpdate(
            stateActionProbabilities,
            stateTransitions,
            reward,
            γ = 0.1,
            θ = 0.01,
            stateActionProbabilities.view.mapValues(_ => Reward(0)).toMap,
            State("S1"),
            stateActionProbabilities.view.mapValues(_ => Double.PositiveInfinity).toMap,
            IterationType.Policy
        )

        /*
        1st iteration
        S1 = 2
          A1 - 0.5 * 1 = 0.5
          A2 - 0.5 * 3 = 1.5
        S2 = 3
          A3 - 0.5 * 2 = 1
          A4 - 0.5 * 4 = 2
        S3 = 2
          A5 - 0.5 * 3 = 1.5
          A6 - 0.5 * 1 = 0.5
        2nd iteration
        S1 = 2.25
          A1 - 0.5 * (1 + 0.1(3)) = 0.65
          A2 - 0.5 * (3 + 0.1(2)) = 1.6
        S2 = 3.2
          A3 - 0.5 * (2 + 0.1(2)) = 1.1
          A4 - 0.5 * (4 + 0.1(2)) = 2.1
        S3 = 2.25
          A5 - 0.5 * (3 + 0.1(3)) = 1.65
          A6 - 0.5 * (1 + 0.1(2)) = 0.6
        3rd iteration
        S1 = 2.2725
          A1 - 0.5 * (1 + 0.1(3.2)) = 0.66
          A2 - 0.5 * (3 + 0.1(2.25)) = 1.6125
        S2 = 3.225
          A3 - 0.5 * (2 + 0.1(2.25)) = 1.1125
          A4 - 0.5 * (4 + 0.1(2.25)) = 2.1125
        S3 = 2.2725
          A5 - 0.5 * (3 + 0.1(2.25)) = 1.6125
          A6 - 0.5 * (1 + 0.1(3.2)) = 0.66
        */

        assertEquals(2.2725, ev(State("S1")).toDouble, 0.01)
        assertEquals(3.225, ev(State("S2")).toDouble, 0.01)
        assertEquals(2.2725, ev(State("S3")).toDouble, 0.01)
    }
}