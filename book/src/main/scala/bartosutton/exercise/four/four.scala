package bartosutton.exercise.four

import breeze.linalg.DenseVector
import com.robertjneal.rl._
import com.robertjneal.rl.agent._
import com.robertjneal.rl.policyiteration._
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

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
  val actionProbabilities: List[ActionProbability] = possibleActions.map { ActionProbability(_, Probability.unsafe(0.25)) }
  val randomPolicy: Map[State, List[ActionProbability]] = (1 to 14).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Action => Map[State, Reward] = (a: Action) => (1 to 14).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  iterativePolicyEvaluation(randomPolicy, example1GridTransitions, gridRewards, γ = 1.0, logFrequency=10, logMaxSteps = 100)
}

def exercise4dot2a() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[ActionProbability] = possibleActions.map { ActionProbability(_, Probability.unsafe(0.25)) }
  val randomPolicy: Map[State, List[ActionProbability]] = (1 to 15).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
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

def exercise4dot2b() = {
  val possibleActions: List[Action] = List("up", "down", "left", "right").map(Action(_))
  val actionProbabilities: List[ActionProbability] = possibleActions.map { ActionProbability(_, Probability.unsafe(0.25)) }
  val randomPolicy: Map[State, List[ActionProbability]] = (1 to 15).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Action => Map[State, Reward] = (a: Action) => (1 to 15).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  val gridTransitions = example1GridTransitions.updated(StateAction(State("13"), Action("down")), List(ProbabilityState(Probability.Certain, State("15")))) ++ 
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
  val actionProbabilities: List[ActionProbability] = possibleActions.map { ActionProbability(_, Probability.unsafe(0.25)) }
  val randomPolicy: Map[State, List[ActionProbability]] = (1 to 14).map(state => (State(state.toString), actionProbabilities)).toMap.updated(State("T"), actionProbabilities)
  val gridRewards: Action => Map[State, Reward] = (a: Action) => (1 to 14).map(state => (State(state.toString), Reward(-1))).toMap.updated(State("T"), Reward(0))

  val optimalPolicy = policyIteration(randomPolicy, example1GridTransitions, gridRewards, γ = 1.0, logFrequency = 15)

  optimalPolicy.foreach(println)
}

def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
def poisson(λ: Int)(n: Int): Probability = Probability.unsafe((Math.pow(λ.toDouble, n.toDouble) / factorial(n).toDouble) * Math.exp(-λ.toDouble))

def jacksRentalCars(freeCar: Boolean = false, rentParking: Boolean = false) = {
  val location1States = (0 to 20).map(i => State(s"$i")) 
  val location2States = (0 to 20).map(i => State(s"$i"))
  val carsToMove = (-5 to 5)

  val stateActionProbabilities: Map[State, List[ActionProbability]] = { for {
    s1 <- location1States
    s2 <- location2States
  } yield
      State(s"${s1.toString},${s2.toString}") ->
        carsToMove.map { carsMoved =>
          ActionProbability(Action(s"$carsMoved"), Probability.unsafe(1/carsToMove.length.toDouble))
        }.toList
  }.toMap

  val realisticReturns = (0 to 10)

  val transitions: Map[StateAction, List[ProbabilityState]] = { for {
    (s, aps) <- stateActionProbabilities
    ActionProbability(a, p) <- aps
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

  val optimalPolicy: Map[State, List[ActionProbability]] = policyIteration(stateActionProbabilities, transitions, reward(freeCar, rentParking), θ=0.1D)
  val optimalActions: List[(State, Action)] = optimalPolicy.view.mapValues(_.sortBy(_._1.toString.toInt).head._1).toList
  val sortedByState: List[(State, Action)] = optimalActions.sortBy { case (s, a) => (s.toString.split(",")(0).toInt, s.toString.split(",")(1).toInt) }
  val probabilities: List[(Int, List[(State, Action)])] = sortedByState.groupBy(_._1.toString.split(",")(0)).map{ case (k, v) => k.toInt -> v }.toList.sortBy(_._1)
  val rows: List[List[Double]] = probabilities.map(_._2.map(_._2.toString.toDouble + 5))

  import breeze.linalg._
  import breeze.plot._
  val matrix = DenseMatrix(rows:_*)
    
  val f2 = Figure()
  f2.subplot(0) += image(matrix)
  f2.subplot(0).legend = true
  f2.saveas("src/main/scala/bartosutton/exercise/four/jacksRental4.7.png")

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
  jacksRentalCars(true, true)
}

def gamblersProblem(numberOfStates: Int, probabilityOfWinning: Probability, θ: Double = 0.01) = {
  val states = (0 until numberOfStates).toList
  val stateActionProbabilities: Map[State, List[ActionProbability]] = states.map {s =>
    val numberOfActions = Math.min(s, numberOfStates - s)
    State(s.toString) ->
    (0 to numberOfActions).map(a => ActionProbability(Action(a.toString), Probability.evenProbability(numberOfActions))).toList
  }.toMap.updated(State(numberOfStates.toString), List.empty)

  def stateTransitions(stateAction: StateAction): List[ProbabilityState] = {
    val StateAction(state, action) = stateAction
    val stake = state.toString.toInt
    val bet = action.toString.toInt
    List(
      ProbabilityState(Probability.Certain - probabilityOfWinning, State((stake - bet).toString)),
      ProbabilityState(probabilityOfWinning, State((stake + bet).toString))
    )
  }

  def stateRewards(action: Action)(state: State): Reward = {
    if (state.toString == numberOfStates.toString) Reward(1)
    else Reward(0)
  }

  val optimalPolicy: PolicyHistory = valueIteration(
    stateActionProbabilities,
    stateTransitions,
    stateRewards,
    γ = 1,
    θ = 0.01,
    logFrequency = 0,
    logMaxSteps = 100,
    recordHistory = true
  )

  val bets: Array[Double] = optimalPolicy.policy.toList.sortBy(_._1.toString.toInt).map(_._2.headOption.getOrElse(ActionProbability(Action("0"), Probability.Never)).action.toString.toDouble).toArray

  val denseVector = breeze.linalg.DenseVector(
    bets
  )

  val tenthOfHistory: List[Map[State, Reward]] = optimalPolicy.history.grouped(10).map(_.head).toList

  val historyAsArray: Array[(Array[Double], Int)] = tenthOfHistory.map(stateRewards =>
    stateRewards.toList.sortBy(_._1.toString.toInt).map(_._2.toDouble).toArray
  ).zipWithIndex.toArray

  val history: Map[String, DenseVector[Double]] = historyAsArray.map((rewards,index) => index.toString -> DenseVector(rewards)).toMap

  testbed.generatePlot(
    history,
    "src/main/scala/bartosutton/exercise/four/",
    s"gamblersProblem${probabilityOfWinning.toString}-${θ}a",
    "Value Estimate",
    "Bankroll"
  )

  testbed.generatePlot(
    Map("Bets" -> denseVector),
    "src/main/scala/bartosutton/exercise/four/",
    s"gamblersProblem${probabilityOfWinning.toString}-${θ}b",
    "Bet",
    "Bankroll"
  )
}

def exercise4dot8() = {
  val numberOfStates = 100
  val probabilityOfWinning = Probability.unsafe(0.4)
  gamblersProblem(numberOfStates, probabilityOfWinning)
}

def exercise4dot9(θ: Double = 0.01) = {
  val numberOfStates = 100
  val probabilityOfWinning1 = Probability.unsafe(0.25)
  gamblersProblem(numberOfStates, probabilityOfWinning1, θ)

  val probabilityOfWinning2 = Probability.unsafe(0.55)
  gamblersProblem(numberOfStates, probabilityOfWinning2, θ)
}
