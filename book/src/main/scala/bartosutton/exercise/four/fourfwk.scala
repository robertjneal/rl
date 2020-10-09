package bartosutton.exercise.four

import com.robertjneal.rl._
import com.robertjneal.rl.agent._
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

def returnCars(state: State): State = {
  val random1 = Probability.unsafe(scala.util.Random.nextDouble)
  val random2 = Probability.unsafe(scala.util.Random.nextDouble)
  val carsReturned1Probabilities: Seq[(Int, Probability)] = (0 to 10).map(n => (n, poisson(2)(n)))
  val carsReturned2Probabilities: Seq[(Int, Probability)] = (0 to 10).map(n => (n, poisson(3)(n)))

  val carsReturned1 = carsReturned1Probabilities.foldLeft((Probability.Never, -1))((probAndWinner, elem) => {
    val (probabilityThusFar, winningNumber) = probAndWinner
    val (n, probabilityOfN) = elem
    val currentProbability = probabilityThusFar + probabilityOfN
    if (winningNumber < 0 && currentProbability > random1) (currentProbability, n)
    else (currentProbability, winningNumber)
  })._2

  val carsReturned2 = carsReturned1Probabilities.foldLeft((Probability.Never, -1))((probAndWinner, elem) => {
    val (probabilityThusFar, winningNumber) = probAndWinner
    val (n, probabilityOfN) = elem
    val currentProbability = probabilityThusFar + probabilityOfN
    if (winningNumber < 0 && currentProbability > random2) (currentProbability, n)
    else (currentProbability, winningNumber)
  })._2

  val (stateLocation1: Int, stateLocation2: Int) = state.toString.split(",") match { case Array(oneOne: String, oneTwo: String) => (oneOne.toInt, oneTwo.toInt) }
  val stateLocation1Updated = Math.max(0, Math.min(20, stateLocation1 + carsReturned1))
  val stateLocation2Updated = Math.max(0, Math.min(20, stateLocation2 + carsReturned2))
  State(s"$stateLocation1Updated,$stateLocation2Updated")
}

def returnCarsTest() = {
  for {
    i <- 0 to 10
    j <- 0 to 10
  } yield s"($i,$j) => ${returnCars(State(s"$i,$j"))}"
}

object JacksRentalCarsFramework {

  def iterativePolicyEvaluation(π: Map[State, List[(Action, Probability)]], stateTransitions: Map[StateAction, List[ProbabilityState]], stateRewards: Action => State => Reward, γ: Double = 0.9, θ: Double = 0.001): Map[State, Reward] = {
    val initialPolicyValues: Map[State, Reward] = π.mapValues(r => Reward(0)).toMap

    def nextState(policyValues: Map[State, Reward], currentState: State): State = {
      if (policyValues.keys.lastOption.filter(_ == currentState).isDefined) policyValues.keys.head
      else {
        policyValues.keys.dropWhile(_ != currentState).tail.head
      }
    }

    def expectedUpdate(currentPolicyValues: Map[State, Reward], state: State, currentΔs: Map[State, Double], step: Int = 0): Map[State, Reward] = {
      val updatedPolicyValue: Reward = Reward(π(state).map( (action, probability) => {
          val sPrimes = stateTransitions(StateAction(state, action))
          probability.toDouble * sPrimes.foldLeft(0D)((acc, probabilityState) => acc + (probabilityState.probability.toDouble * (stateRewards(action)(probabilityState.state).toDouble + γ * currentPolicyValues(probabilityState.state).toDouble)))
      }).sum)
      val updatedΔs = currentΔs.updated(state, Math.min(currentΔs(state), Math.abs((currentPolicyValues(state) - updatedPolicyValue).toDouble)))
      val updatedPolicyValues = currentPolicyValues.updated(state, updatedPolicyValue)
      val stopEvaluating = updatedΔs.values.max < θ

      val k = step / 15
      if (step %  1500 == 0 ) { //step == 200 || step == 1000 || step == 5000 || stopEvaluating) { // (stopEvaluating || ((step % 150 == 0))) { // && List(0, 1, 2, 3, 10).contains(k))) {
        println(s"==== step: $step =====")
        currentPolicyValues.toList.sortBy(_._1.toString).foreach { (s, r) =>
          println(s"State: ${s}, Value: ${r}")
        }
      }

      if (stopEvaluating) updatedPolicyValues
      else expectedUpdate(updatedPolicyValues, nextState(currentPolicyValues, state), updatedΔs, step + 1)
    }

    def evaluate(): Map[State, Reward] = {
      val initialΔs: Map[State, Double] = π.mapValues(r => Double.PositiveInfinity).toMap

      expectedUpdate(initialPolicyValues, initialPolicyValues.keys.head, initialΔs)
    }

    evaluate()
  }

  def jacksRentalCars() = {
    val location1States = (0 to 20).map(i => State(s"$i")) 
    val location2States = (0 to 20).map(i => State(s"$i"))
    val carsToMove = (-5 to 5)

    val stateActionProbabilities: Map[State, List[(Action, Probability)]] = { for {
      s1 <- location1States
      s2 <- location2States
    } yield
        State(s"${s1.toString},${s2.toString}") ->
          carsToMove.map { carsMoved =>
            (Action(s"$carsMoved"), Probability.unsafe(1/carsToMove.length.toDouble))
          }.toList
    }.toMap

    val stateActions: Map[State, Vector[Action]] = { for {
      s1 <- location1States
      s2 <- location2States
    } yield
        State(s"${s1.toString},${s2.toString}") ->
          carsToMove.map { carsMoved =>
            Action(s"$carsMoved")
          }.toVector
    }.toMap

    //val environment = Environment(stateActions, stateActions.map{ case (state, _) => state }.head)
    /*val agent = TabularAgent[Reward](
      environment,
      PreferenceSelector((m: Map[Action, Preference]) => Action("None")), // Not needed for evaluation and iteration
      replace,
      Step(0),
      stateActions.map{ case (_, actions) => actions.map(a => (a, Step(1))).toMap},
      stateActions.map{ case (_, actions) => actions.map(a => (a, Reward(0))).toMap},
      recordHistory = true
    )*/
  }
}
