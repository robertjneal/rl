package bartosutton.exercise.three

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

type TransitionProbabilities = Map[StateAction, Map[State, Probability]]

val transitionProbabilities: TransitionProbabilities = Map(
  StateAction(State("A"), Action("0")) -> Map(State("A") -> Probability.unsafe(1), State("B") -> Probability.unsafe(0.0), State("C") -> Probability.unsafe(0.0)),
  StateAction(State("A"), Action("1")) -> Map(State("A") -> Probability.unsafe(0.2), State("B") -> Probability.unsafe(0.7), State("C") -> Probability.unsafe(0.1)),
  StateAction(State("B"), Action("2")) -> Map(State("A")-> Probability.unsafe(0.0), State("B") -> Probability.unsafe(0.4), State("C") -> Probability.unsafe(0.6)),
  StateAction(State("B"), Action("3")) -> Map(State("A") -> Probability.unsafe(0.2), State("B") -> Probability.unsafe(0.6), State("C") -> Probability.unsafe(0.2)),
  StateAction(State("C"), Action("4")) -> Map(State("A") -> Probability.unsafe(0.7), State("B") -> Probability.unsafe(0.1), State("C") -> Probability.unsafe(0.2)),
  StateAction(State("C"), Action("5")) -> Map(State("A") -> Probability.unsafe(0.3), State("B") -> Probability.unsafe(0.6), State("C") -> Probability.unsafe(0.1))
)

val stateRewards: Map[State, Reward] = Map(
  State("A") -> Reward(1),
  State("B") -> Reward(-3),
  State("C") -> Reward(2)
)

val possibleRewards: Set[Reward] = stateRewards.values.toSet

def equation3dot2(sPrime: State, reward: Reward, currentState: State, action: Action): Probability = {
  if (stateRewards(sPrime) == reward) {
    val stateProbabilities = transitionProbabilities(StateAction(currentState, action))
    stateProbabilities(sPrime)
  } 
  else Probability.Never
}

def equation3dot4(sPrime: State, currentState: State, action: Action): Probability = {
  possibleRewards.foldLeft(Probability.Never)( (p, r) => equation3dot2(sPrime, r, currentState, action) + p )
}

def equation3dot5(state: State, action: Action): Reward = {
  possibleRewards.fold(Reward(0)){ (expectedReward, r) =>
    expectedReward +
    transitionProbabilities(StateAction(state, action)).keys.foldLeft(Reward(0)) { (reward, sPrime) => Reward(equation3dot2(sPrime, r, state, action).toDouble * r.toDouble) + reward }
  }
}

def equation3dot6(sPrime: State, currentState: State, action: Action): Reward = {
  possibleRewards.fold(Reward(0)){ (expectedReward, r) =>
    expectedReward +
    Reward(r.toDouble * (equation3dot2(sPrime, r, currentState, action) / equation3dot4(sPrime, currentState, action)).toDouble)
  }
}

def equation3dot7(rewards: List[Reward]): Reward = 
  rewards.reduce(_ + _)

def equation3dot8(rewards: List[Reward], γ: Double): Reward = {
  val (g, _) = rewards.foldLeft((Reward(0), 0)){ (running, r)  =>
    val (runningSum, i) = running
    (runningSum + Reward(r.toDouble * Math.pow(γ, i)), i + 1)
  }
  g
}

def equation3dot9(rewards: List[Reward], γ: Double): Reward = {
  rewards.headOption match
    case None => Reward(0)
    case Some(d) => d + Reward(γ * equation3dot9(rewards.tail, γ).toDouble)
}

def equivalenceTest() = {
  def doubleIsDifferent(d1: Double, d2: Double, delta: Double): Boolean = {
    if (d1 == d2) {
      false
    }
    else if ((Math.abs(d1 - d2) <= delta)) {
      false
    }
    else true
  }

  val rewards = List(
    Reward(1),
    Reward(5.5),
    Reward(0),
    Reward(-13),
    Reward(7),
    Reward(98),
  )
  val γ = 0.89 
  val eight = equation3dot8(rewards, γ)
  val nine = equation3dot9(rewards, γ)
  println(eight)
  println(nine)
  println(
    !doubleIsDifferent(eight.toDouble, nine.toDouble, 0.0001)
  )
}

def equation3dot11(γ: Double = 0.9, constant: Double = 0, t: Step = Step(0)): Double = {
  val sequence = List(
    Reward(1), Reward(2), Reward(-1), Reward(5), Reward(7), Reward(0), Reward(3)
  ).map(_ + Reward(constant))

  (for (i <- t.toInt until sequence.length) yield {
    val k = i + 1
    Math.pow(γ, k - (t.toDouble) - 1) * sequence(i).toDouble
  }).sum
}

def exercise3dot15(γ: Double = 0.9, constant: Double = 1): Double = {
    if (γ > 0 && γ < 1) {
      constant / (1 - γ)
    } else Double.PositiveInfinity
}

def exercise3dot16(γ: Double = 0.9, constant: Double = 0, step: Step = Step(0)): Double = {
    equation3dot11(γ, constant, step)
}