package bartosutton.exercise.four

import com.robertjneal.rl._
import com.robertjneal.rl.agent._
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

case class StateAction(state: State, action: Action)
case class ProbabilityState(probability: Probability, state: State)

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

def policyImprovement(π: Map[State, List[(Action, Probability)]], stateTransitions: Map[StateAction, List[ProbabilityState]], policyValues: Map[State, Reward], θ: Double = 0.001): (Boolean, Map[State, List[Action]]) = {
  val maxStateActions: Map[State, List[Action]] = π.map { case (state, actionProbabilities) => {
    val oldActionProbabilities: List[(Action, Probability)] = actionProbabilities.filter { (_, probability) => probability > Probability.Never }

    val maxActions: Map[Action, Reward] = actionProbabilities.foldLeft(Map.empty[Action, Reward])((maxima, elem) => {
      val (currentAction, currentProbability) = elem
      val reward = Reward(stateTransitions(StateAction(state, currentAction)).foldLeft(0D)((r, probabilityState) => {
        val ProbabilityState(prob: Probability, sPrime: State) = probabilityState
        r + (prob.toDouble * policyValues(sPrime).toDouble)
      }))
      if (maxima.size == 0) {
        Map(currentAction -> reward)
      } else {
        val (_, maxReward) = maxima.head
        if ((reward - maxReward).toDouble > θ) Map(currentAction -> reward)
        else if (Math.abs((reward - maxReward).toDouble) < θ) maxima.updated(currentAction, reward)
        else maxima
      }
    })

    (state, maxActions.keys.toList)
  }}

  val isStable = (maxStateActions == π.mapValues{ _.map { (action, _) => action } }.toMap)
  (isStable, maxStateActions)
}

def policyIteration(π: Map[State, List[(Action, Probability)]], stateTransitions: Map[StateAction, List[ProbabilityState]], stateRewards: Action => State => Reward, γ: Double = 0.9, θ: Double = 0.001): Map[State, List[(Action, Probability)]] = {
  def iterate(πPrime: Map[State, List[(Action, Probability)]]): Map[State, List[(Action, Probability)]] = {
    val policyValues = iterativePolicyEvaluation(πPrime, stateTransitions, stateRewards, γ, θ)
    val (stable, maxActions) = policyImprovement(πPrime, stateTransitions, policyValues)
    val newPolicy = maxActions.mapValues(actions => actions.zip(Probability.evenProbabilities(actions.length))).toMap
    if (stable) newPolicy
    else iterate(newPolicy)
  }

  iterate(π)
}

def valueIteration(stateActionProbabilities: Map[State, List[(Action, Probability)]], stateTransitions: Map[StateAction, List[ProbabilityState]], stateRewards: Action => State => Reward, γ: Double = 0.9, θ: Double = 0.001): Map[State, List[(Action, Probability)]] = {

  def nextState(stateValues: Map[State, Reward], currentState: State): State = {
    if (stateValues.keys.lastOption.filter(_ == currentState).isDefined) stateValues.keys.head
    else {
      stateValues.keys.dropWhile(_ != currentState).tail.head
    }
  }

  def expectedUpdate(currentStateValues: Map[State, Reward], state: State, currentΔs: Map[State, Double], step: Int = 0): Map[State, Reward] = {
    val updatedStateValue: Reward = Reward(stateActionProbabilities(state).map{ case (action, probability) => {
        val sPrimes = stateTransitions(StateAction(state, action))
        probability.toDouble * sPrimes.foldLeft(0D)((acc, probabilityState) => acc + (probabilityState.probability.toDouble * (stateRewards(action)(probabilityState.state).toDouble + γ * currentStateValues(probabilityState.state).toDouble)))
    }}.max)
    val updatedΔs = currentΔs.updated(state, Math.min(currentΔs(state), Math.abs((currentStateValues(state) - updatedStateValue).toDouble)))
    val updatedStateValues = currentStateValues.updated(state, updatedStateValue)
    val stopEvaluating = updatedΔs.values.max < θ

    if (stopEvaluating) updatedStateValues
    else expectedUpdate(updatedStateValues, nextState(currentStateValues, state), updatedΔs, step + 1)
  }

  val bestRewards = expectedUpdate(
    stateActionProbabilities.mapValues(_ => Reward(0)).toMap,
    stateActionProbabilities.keys.head,
    stateActionProbabilities.mapValues(_ => Double.PositiveInfinity).toMap
  )

  println(bestRewards)

  stateActionProbabilities.map{ case (state, actionProbabilities) => {
    state -> {
      val actionRewards: Map[Action, Reward] = actionProbabilities.map{ (action, _) => action -> Reward({
        stateTransitions(StateAction(state, action)).foldLeft(0D)((acc, probabilityState) => {
          val reward = probabilityState.probability.toDouble * bestRewards(probabilityState.state).toDouble
          reward + acc
        })
      })}.toMap
      val maxValue: Reward = Reward(actionProbabilities.map((action, _) => actionRewards(action).toDouble).max)
      val bestActions = actionProbabilities.filter{ (action, _) => actionRewards(action) == maxValue }
      val bestActionProbabilities = bestActions.map { case (action, _) => (action, Probability.unsafe(1/bestActions.length.toDouble)) }
      bestActionProbabilities
    }
  }}
}

val example1GridTransitions: Map[StateAction, List[ProbabilityState]] = Map(
    StateAction(State("1"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("1"))),
    StateAction(State("1"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("5"))),
    StateAction(State("1"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("T"))),
    StateAction(State("1"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("2"))),
    
    StateAction(State("2"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("2"))),
    StateAction(State("2"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("6"))),
    StateAction(State("2"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("1"))),
    StateAction(State("2"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("3"))),
    
    StateAction(State("3"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("2"))),
    StateAction(State("3"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("7"))),
    StateAction(State("3"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("2"))),
    StateAction(State("3"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("3"))),
    
    StateAction(State("4"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("T"))),
    StateAction(State("4"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("8"))),
    StateAction(State("4"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("4"))),
    StateAction(State("4"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("5"))),
    
    StateAction(State("5"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("1"))),
    StateAction(State("5"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("9"))),
    StateAction(State("5"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("4"))),
    StateAction(State("5"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("6"))),
    
    StateAction(State("6"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("2"))),
    StateAction(State("6"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("10"))),
    StateAction(State("6"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("5"))),
    StateAction(State("6"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("7"))),
    
    StateAction(State("7"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("3"))),
    StateAction(State("7"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("11"))),
    StateAction(State("7"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("6"))),
    StateAction(State("7"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("7"))),
    
    StateAction(State("8"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("4"))),
    StateAction(State("8"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("12"))),
    StateAction(State("8"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("8"))),
    StateAction(State("8"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("9"))),
    
    StateAction(State("9"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("5"))),
    StateAction(State("9"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("13"))),
    StateAction(State("9"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("8"))),
    StateAction(State("9"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("10"))),
    
    StateAction(State("10"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("6"))),
    StateAction(State("10"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("14"))),
    StateAction(State("10"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("9"))),
    StateAction(State("10"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("11"))),
    
    StateAction(State("11"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("7"))),
    StateAction(State("11"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("T"))),
    StateAction(State("11"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("10"))),
    StateAction(State("11"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("11"))),
    
    StateAction(State("12"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("8"))),
    StateAction(State("12"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("12"))),
    StateAction(State("12"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("12"))),
    StateAction(State("12"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("13"))),
    
    StateAction(State("13"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("9"))),
    StateAction(State("13"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("13"))),
    StateAction(State("13"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("12"))),
    StateAction(State("13"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("14"))),
    
    StateAction(State("14"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("10"))),
    StateAction(State("14"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("14"))),
    StateAction(State("14"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("13"))),
    StateAction(State("14"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("T"))),

    StateAction(State("T"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("T"))),
    StateAction(State("T"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("T"))),
    StateAction(State("T"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("T"))),
    StateAction(State("T"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("T")))
  )


def figure4dot1column1() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[(Action, Probability)] = possibleActions.map { _ -> Probability.unsafe(0.25) }
  val randomPolicy: Map[State, List[(Action, Probability)]] = (1 to 14).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Action => Map[State, Reward] = (a: Action) => (1 to 14).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  iterativePolicyEvaluation(randomPolicy, example1GridTransitions, gridRewards, γ = 1.0)
}

def exercise2b() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[(Action, Probability)] = possibleActions.map { _ -> Probability.unsafe(0.25) }
  val randomPolicy: Map[State, List[(Action, Probability)]] = (1 to 15).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Action => Map[State, Reward] = (a: Action) => (1 to 15).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  val gridTransitions = example1GridTransitions ++ //.updated((State("13"), Action("down")), State("15")) ++ 
    Map(
      StateAction(State("15"), Action("up")) -> List(ProbabilityState(Probability.Certain, State("13"))),
      StateAction(State("15"), Action("down")) -> List(ProbabilityState(Probability.Certain, State("15"))),
      StateAction(State("15"), Action("left")) -> List(ProbabilityState(Probability.Certain, State("12"))),
      StateAction(State("15"), Action("right")) -> List(ProbabilityState(Probability.Certain, State("14")))
    )


  iterativePolicyEvaluation(randomPolicy, gridTransitions, gridRewards, γ = 1.0)
}

def figure4dot1column2() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[(Action, Probability)] = possibleActions.map { _ -> Probability.unsafe(0.25) }
  val randomPolicy: Map[State, List[(Action, Probability)]] = (1 to 14).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Action => Map[State, Reward] = (a: Action) => (1 to 14).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  val optimalPolicy = policyIteration(randomPolicy, example1GridTransitions, gridRewards, γ = 1.0)

  optimalPolicy.foreach(println)
}

def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
def poisson(λ: Int)(n: Int): Probability = Probability.unsafe((Math.pow(λ, n) / factorial(n).toDouble) * Math.exp(-λ))

def jacksRentalCars(value: Boolean = false, freeCar: Boolean = false, rentParking: Boolean = false) = {
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

  val realisticReturns = (0 to 10)

  val transitions: Map[StateAction, List[ProbabilityState]] = { for {
    (s, aps) <- stateActionProbabilities
    (a, p) <- aps
  } yield {
    StateAction(s, a) -> {
  { for {
    returnedTo1 <- realisticReturns
    returnedTo2 <- realisticReturns
  } yield
      {
        val (location1Begin: Int, location2Begin: Int) = stringToTuple2Int(s.toString)
        val toMove = a.toString.toInt match {
          case i if i < 0 => Math.max(i, -(location2Begin + returnedTo2))
          case i if i > 0 => Math.min(i, location1Begin + returnedTo1)
          case i => i
        }
        val location1Cars = Math.max(0, Math.min(20, (location1Begin - toMove + returnedTo1)))
        val location2Cars = Math.max(0, Math.min(20, (location2Begin + toMove + returnedTo2)))
        val newLocationCars = s"${location1Cars.toString},${location2Cars.toString}"
        val probabilityOfReturns = poisson(2)(returnedTo1) & poisson(3)(returnedTo2)
        ProbabilityState(probabilityOfReturns, State(newLocationCars))
      }}.toList
  }}}.toMap

  val optimalPolicy: Map[State, List[(Action, Probability)]] = if (value) valueIteration(stateActionProbabilities, transitions, reward(freeCar, rentParking), θ=0.1D) else policyIteration(stateActionProbabilities, transitions, reward(freeCar, rentParking), θ=0.1D)
  println(optimalPolicy)
  val optimalActions: List[(State, Action)] = optimalPolicy.mapValues(_.sortBy(_._1.toString.toInt).headOption.getOrElse((Action("0"), Probability.Never))._1).toList
  val sortedByState: List[(State, Action)] = optimalActions.sortBy { case (s, a) => (s.toString.split(",")(0).toInt, s.toString.split(",")(1).toInt) }
  val probabilities: List[(Int, List[(State, Action)])] = sortedByState.groupBy(_._1.toString.split(",")(0)).map{ case (k, v) => k.toInt -> v }.toList.sortBy(_._1)
  val rows: List[List[Double]] = probabilities.map(_._2.map(_._2.toString.toDouble + 5))

  import breeze.linalg._
  import breeze.plot._
  val matrix = DenseMatrix(rows:_*)
    
  val f2 = Figure()
  f2.subplot(0) += image(matrix)
  f2.subplot(0).legend = true
  f2.saveas("image.png")

  println(transitions.take(10))
  probabilities.reverse.foreach { println }
}

def stringToTuple2Int(string: String): Tuple2[Int, Int] = {
  string.split(",") match { case Array(first: String, second: String) => (first.toInt, second.toInt) }
}

def reward(freeCar: Boolean = false, rentParking: Boolean = false)(action: Action)(state: State): Reward = {
  val (location1: Int, location2: Int) = stringToTuple2Int(state.toString)
  val moved: Int = Math.abs(action.toString.toInt)
  val likelyCarsRequested = (0 to 10)

  val random1 = Probability.unsafe(scala.util.Random.nextDouble)
  val carsRequestedLocation1: Int = likelyCarsRequested.map(n => (n, poisson(3)(n))).foldLeft((Probability.Never, -1))((probAndWinner, nAndProbabiltyOfN) => {
    val (probabilityThusFar, winningNumber) = probAndWinner
    val (n, probabilityOfN) = nAndProbabiltyOfN
    val currentProbability = probabilityThusFar + probabilityOfN
    if (winningNumber < 0 && currentProbability > random1) (currentProbability, n)
    else (currentProbability, winningNumber)
  })._2

  val random2 = Probability.unsafe(scala.util.Random.nextDouble)
  val carsRequestedLocation2: Int = likelyCarsRequested.map(n => (n, poisson(4)(n))).foldLeft((Probability.Never, -1))((probAndWinner, nAndProbabiltyOfN) => {
    val (probabilityThusFar, winningNumber) = probAndWinner
    val (n, probabilityOfN) = nAndProbabiltyOfN
    val currentProbability = probabilityThusFar + probabilityOfN
    if (winningNumber < 0 && currentProbability > random2) (currentProbability, n)
    else (currentProbability, winningNumber)
  })._2

  val carsRented1 = Math.min(location1, carsRequestedLocation1)
  val carsRented2 = Math.min(location2, carsRequestedLocation2)

  val adjustment: Int = if (freeCar && moved > 0) 2 else 0

  val carRentalIncome = 10 * (carsRented1 + carsRented2)
  val costToMove = 2 * moved + adjustment
  val parkingRentCost = if (rentParking) 4 * Math.min(1, location1 / 10) + 4 * Math.min(1, location2 / 10) else 0
  val r = Reward(carRentalIncome - costToMove - parkingRentCost)
  r
}

def exercise4dot7() = {
  jacksRentalCars(false, true, true)
}
