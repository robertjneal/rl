package com.robertjneal.rl.policyiteration

import com.robertjneal.rl.environment.Environment
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import org.junit.Test
import org.junit.Assert._
import scala.util.Random

class policyIterationTest {
  @Test
  def expectedUpdateNoDiscountTest() = {
    val stateActionProbabilities: Map[State, List[ActionProbability]] = Map(
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
        case (Action("A1"), StateString("S1")) => Reward(1)
        case (Action("A1"), StateString("S2")) => Reward(2)
        case (Action("A1"), StateString("S3")) => Reward(3)
        case (Action("A2"), StateString("S1")) => Reward(2)
        case (Action("A2"), StateString("S2")) => Reward(4)
        case (Action("A2"), StateString("S3")) => Reward(5)
        case (Action("A3"), StateString("S1")) => Reward(0.5)
        case (Action("A3"), StateString("S2")) => Reward(1)
        case (Action("A3"), StateString("S3")) => Reward(1.5)
        case (_, _)                      => Reward(0)
      }
    }

    val ev = expectedUpdate(
      stateActionProbabilities,
      stateTransitions,
      reward,
      0,
      0.1,
      State("S1"),
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

    assert(
      ev.expectation.values.toList == List(
        Reward(3),
        Reward(2.2227),
        Reward(3.66)
      )
    )
  }

  @Test
  def expectedUpdateWithDiscountTest() = {
    val stateActionProbabilities: Map[State, List[ActionProbability]] = Map(
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
        case (Action("A1"), StateString("S2")) => Reward(1)
        case (Action("A2"), StateString("S3")) => Reward(3)
        case (Action("A3"), StateString("S1")) => Reward(2)
        case (Action("A4"), StateString("S3")) => Reward(4)
        case (Action("A5"), StateString("S1")) => Reward(3)
        case (Action("A6"), StateString("S2")) => Reward(1)
        case (_, _)                      => Reward(0)
      }
    }

    val ev = expectedUpdate(
      stateActionProbabilities,
      stateTransitions,
      reward,
      γ = 0.1,
      θ = 0.01,
      State("S1"),
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

    assertEquals(2.2725, ev.expectation(State("S1")).toDouble, 0.01)
    assertEquals(3.225, ev.expectation(State("S2")).toDouble, 0.01)
    assertEquals(2.2725, ev.expectation(State("S3")).toDouble, 0.01)
  }

  @Test
  def expectedUpdateEmptyMapTest() = {
    val ev = expectedUpdate(
      Map.empty[State, List[ActionProbability]],
      Map.empty[StateAction, List[ProbabilityState]],
      (action: Action) => (state: State) => Reward(0),
      0.0,
      0.0,
      State("S1"),
      IterationType.Policy
    )

    assertEquals(ExpectationHistory(Map.empty[State, Reward], List.empty[Map[State, Reward]]), ev)
  }

  @Test
  def iterativePolicyEvaluationEmptyMapTest() = {
    val policyValue = iterativePolicyEvaluation(
      Map.empty[State, List[ActionProbability]],
      Map.empty[StateAction, List[ProbabilityState]],
      (action: Action) => (state: State) => Reward(0)
    )

    assertEquals(Map.empty[State, Reward], policyValue)
  }

  @Test
  def policyImprovementEmptyMapTest() = {
    val (stable, policy) = policyImprovement(
      Map.empty[State, List[ActionProbability]],
      Map.empty[StateAction, List[ProbabilityState]],
      Map.empty[State, Reward]
    )

    assert(policy.isEmpty)
  }

  @Test
  def policyImprovementTest() = {
    // It should select the actions that have the highest immediate reward
    val policy: Map[State, List[ActionProbability]] = Map(
      State("A") -> List(
        ActionProbability(Action("A"), Probability.CoinToss),
        ActionProbability(Action("B"), Probability.CoinToss)
      ),
      State("B") -> List(
        ActionProbability(Action("A"), Probability.unsafe(0.1)),
        ActionProbability(Action("B"), Probability.unsafe(0.8)),
        ActionProbability(Action("C"), Probability.unsafe(0.1))
      ),
      State("C") -> List.empty,
      State("D") -> List.empty
    )
    val stateTransitions = Map(
      StateAction(State("A"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("B"))
      ),
      StateAction(State("A"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      ),
      StateAction(State("B"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("D"))
      ),
      StateAction(State("B"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      ),
      StateAction(State("B"), Action("C")) -> List(
        ProbabilityState(Probability.Certain, State("A"))
      )
    )
    val rewards: Map[State, Reward] = Map(
      State("A") -> Reward(1),
      State("B") -> Reward(2),
      State("C") -> Reward(3),
      State("D") -> Reward(3)
    )

    val optimalActions = Map(
      State("A") -> List(Action("B")),
      State("B") -> List(Action("A"), Action("B")),
      State("C") -> List.empty,
      State("D") -> List.empty
    )

    val (stable, computedOptimalActions) = policyImprovement(
      policy,
      stateTransitions,
      rewards
    )

    assertEquals(optimalActions, computedOptimalActions)
  }

  @Test
  def policyIterationEmptyMapTest() = {
    val policy = policyIteration(
      Map.empty[State, List[ActionProbability]],
      Map.empty[StateAction, List[ProbabilityState]],
      (a: Action) => Map.empty[State, Reward]
    )

    assert(policy.isEmpty)
  }

  @Test
  def policyIterationTest() = {
    // It should iteratre to the policy that lets us visit more states before the terminal C or D
    val policy: Map[State, List[ActionProbability]] = Map(
      State("A") -> List(
        ActionProbability(Action("A"), Probability.CoinToss),
        ActionProbability(Action("B"), Probability.CoinToss)
      ),
      State("E") -> List(
        ActionProbability(Action("A"), Probability.CoinToss),
        ActionProbability(Action("B"), Probability.CoinToss)
      ),
      State("B") -> List(
        ActionProbability(Action("A"), Probability.unsafe(0.1)),
        ActionProbability(Action("B"), Probability.unsafe(0.6)),
        ActionProbability(Action("C"), Probability.unsafe(0.1)),
        ActionProbability(Action("D"), Probability.unsafe(0.2))
      ),
      State("C") -> List.empty,
      State("D") -> List.empty
    )
    val stateTransitions = Map(
      StateAction(State("A"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("B"))
      ),
      StateAction(State("A"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      ),
      StateAction(State("B"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("D"))
      ),
      StateAction(State("B"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      ),
      StateAction(State("B"), Action("C")) -> List(
        ProbabilityState(Probability.Certain, State("A"))
      ),
      StateAction(State("B"), Action("D")) -> List(
        ProbabilityState(Probability.Certain, State("E"))
      ),
      StateAction(State("E"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("B"))
      ),
      StateAction(State("E"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      )
    )
    val rewards: Map[State, Reward] = Map(
      State("A") -> Reward(1),
      State("B") -> Reward(2),
      State("C") -> Reward(3),
      State("D") -> Reward(3),
      State("E") -> Reward(1)
    )

    val optimalPolicy: Map[State, List[ActionProbability]] = Map(
      State("A") -> List(ActionProbability(Action("A"), Probability.Certain)),
      State("B") -> List(
        ActionProbability(Action("C"), Probability.CoinToss),
        ActionProbability(Action("D"), Probability.CoinToss)
      ),
      State("C") -> List.empty,
      State("D") -> List.empty,
      State("E") -> List(ActionProbability(Action("A"), Probability.Certain))
    )

    val computedOptimal = policyIteration(
      policy,
      stateTransitions,
      (a: Action) => rewards
    )

    assertEquals(optimalPolicy, computedOptimal)
  }

  @Test
  def valueIterationEmptyMapTest() = {
    val policy = valueIteration(
      Map.empty[State, List[ActionProbability]],
      Map.empty[StateAction, List[ProbabilityState]],
      (a: Action) => Map.empty[State, Reward]
    )

    assert(policy.policy.isEmpty)
  }

  @Test
  def valueIterationTest() = {
    // It should iteratre to the policy that lets us visit more states before the terminal C or D
    val policy: Map[State, List[ActionProbability]] = Map(
      State("A") -> List(
        ActionProbability(Action("A"), Probability.CoinToss),
        ActionProbability(Action("B"), Probability.CoinToss)
      ),
      State("E") -> List(
        ActionProbability(Action("A"), Probability.CoinToss),
        ActionProbability(Action("B"), Probability.CoinToss)
      ),
      State("B") -> List(
        ActionProbability(Action("A"), Probability.unsafe(0.1)),
        ActionProbability(Action("B"), Probability.unsafe(0.6)),
        ActionProbability(Action("C"), Probability.unsafe(0.1)),
        ActionProbability(Action("D"), Probability.unsafe(0.2))
      ),
      State("C") -> List.empty,
      State("D") -> List.empty
    )
    val stateTransitions = Map(
      StateAction(State("A"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("B"))
      ),
      StateAction(State("A"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      ),
      StateAction(State("B"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("D"))
      ),
      StateAction(State("B"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      ),
      StateAction(State("B"), Action("C")) -> List(
        ProbabilityState(Probability.Certain, State("A"))
      ),
      StateAction(State("B"), Action("D")) -> List(
        ProbabilityState(Probability.Certain, State("E"))
      ),
      StateAction(State("E"), Action("A")) -> List(
        ProbabilityState(Probability.Certain, State("B"))
      ),
      StateAction(State("E"), Action("B")) -> List(
        ProbabilityState(Probability.Certain, State("C"))
      )
    )
    val rewards: Map[State, Reward] = Map(
      State("A") -> Reward(1),
      State("B") -> Reward(2),
      State("C") -> Reward(3),
      State("D") -> Reward(3),
      State("E") -> Reward(1)
    )

    val optimalPolicy: Map[State, List[ActionProbability]] = Map(
      State("A") -> List(ActionProbability(Action("A"), Probability.Certain)),
      State("B") -> List(
        ActionProbability(Action("C"), Probability.CoinToss),
        ActionProbability(Action("D"), Probability.CoinToss)
      ),
      State("C") -> List.empty,
      State("D") -> List.empty,
      State("E") -> List(ActionProbability(Action("A"), Probability.Certain))
    )

    val computedOptimal = valueIteration(
      policy,
      stateTransitions,
      (a: Action) => rewards,
      deterministic = false
    )

    assertEquals(optimalPolicy, computedOptimal.policy)
  }

  private val testEnvironmentAndPolicy = {
    case class TestEnvironment(stateActions: Map[State, Vector[Action]], override val state: State) 
    extends Environment(stateActions, state) {
      def act(a: Action): (Reward, Environment, EndOfEpisode) = {
        val reward = Reward(
          a match {
            case Action("A") => 1
            case Action("B") => 2
            case Action("C") => 3
            case Action("D") => 4
            case Action("E") => 5
            case Action("F") => 6
          }
        )
        val newState: State = 
          a match {
            case Action("A") => State(2)
            case Action("B") => State(3)
            case Action("C") => State(1)
            case Action("D") => State(2)
            case Action("E") => State(3)
            case Action("F") => State("T")
          }
        val isEndOfEpisode = newState == State("T")
        (reward, this.copy(state = newState), isEndOfEpisode)
      }
      def isOptimal(a: Action): OptimalAct = OptimalAct(true)
    }

    val environment = new TestEnvironment(Map.empty, State(1))
    val π: Map[State, List[ActionProbability]] = Map(
      State(1) -> List(
        ActionProbability(Action("A"), Probability.CoinToss),
        ActionProbability(Action("B"), Probability.CoinToss)
      ),
      State(2) -> List(
        ActionProbability(Action("C"), Probability.evenProbability(3)),
        ActionProbability(Action("D"), Probability.evenProbability(3)),
        ActionProbability(Action("E"), Probability.evenProbability(3))
      ),
      State(3) -> List(
        ActionProbability(Action("F"), Probability.Certain)
      ),
      State("T") -> List.empty
    )

    (environment, π)
  }

  @Test
  def generateEpisodeTest() = {
    given Random = Random(4110)

    val (environment, π) = testEnvironmentAndPolicy
    val result: Map[State, Reward] = generateEpisode(environment, π, 0.9).view.mapValues {
      case (reward, _) => reward
    }.toMap

    /*
    State 1 has even probability for A or B with values 1 and 2
    State 2 has even probability for C or D or E with values 3, 4, and 5
    State 3 will always choose F for value of 6

    With seed 4110 we expect the following episode:
    (3,F,6.0), (1,B,2.0), (2,C,3.0), (2,D,4.0), (2,D,4.0), (1,A,1.0)
    G <- 0
    G <- γ * G + Rt+1
    Iteration 1:
    G <- 0.9 * 0 + 6
    State(3) is not in remaining, so State(3) <- 6
    Iteration 2:
    G <- 0.9 * 6 + 2
    State(1) is in remaining
    Iteration 3:
    G <- 0.9 * 7.4 + 3
    State(2) is in remaining
    Iteration 4:
    G <- 0.9 * 9.66 + 4
    State(2) is in remaining
    Iteration 5:
    G <- 0.9 * 12.694 + 4
    State(2) is not in remaining, so State(2) <- 15.4246
    Iteration 6:
    G <- 0.9 * 15.4246 + 1
    State(1) is not in remaining, so State(1) <- 14.88214
    */

    val expected = Map(
      State(1) -> Reward(14.88214),
      State(2) -> Reward(15.4246),
      State(3) -> Reward(6)
    )
    
    expected.foreach { (state, reward) =>
      assertEquals(reward.toDouble, result(state).toDouble, 0.00001)
    }
  }

  @Test
  def monteCarloPolicyEvaluationTest() = {
    given Random = Random(4110)

    val (environment, π) = testEnvironmentAndPolicy
    val simulations = 1
    val result: Map[State, Reward] = monteCarloPolicyEvaluation(environment, π, simulations)

    /*
    State 1 has even probability for A or B with values 1 and 2
    State 2 has even probability for C or D or E with values 3, 4, and 5
    State 3 will always choose F for value of 6

    With seed 4110 we expect the following episode:
    (3,F,6.0), (1,B,2.0), (2,C,3.0), (2,D,4.0), (2,D,4.0), (1,A,1.0)
    G <- 0
    G <- γ * G + Rt+1
    Iteration 1:
    G <- 0.9 * 0 + 6
    State(3) is not in remaining, so State(3) <- 6
    Iteration 2:
    G <- 0.9 * 6 + 2
    State(1) is in remaining
    Iteration 3:
    G <- 0.9 * 7.4 + 3
    State(2) is in remaining
    Iteration 4:
    G <- 0.9 * 9.66 + 4
    State(2) is in remaining
    Iteration 5:
    G <- 0.9 * 12.694 + 4
    State(2) is not in remaining, so State(2) <- 15.4246
    Iteration 6:
    G <- 0.9 * 15.4246 + 1
    State(1) is not in remaining, so State(1) <- 14.88214
    */

    val expected = Map(
      State(1) -> Reward(14.88214),
      State(2) -> Reward(15.4246),
      State(3) -> Reward(6)
    )
    
    expected.foreach { (state, reward) =>
      assertEquals(reward.toDouble, result(state).toDouble, 0.00001)
    }
  }

}
