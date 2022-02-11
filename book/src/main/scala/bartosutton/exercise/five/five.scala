package bartosutton.exercise.five

import bartosutton.exercise.three.TransitionProbabilities
import com.robertjneal.rl.agent._
import com.robertjneal.rl.environment._
import com.robertjneal.rl.actionselector.RewardSelector
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import com.robertjneal.rl.updater._
import scala.util.Random
import io.circe.Decoder.state

object BlackJack {
  val cards = Seq("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  val playerHit = Action("playerHit")
  val dealerHit = Action("dealerHit")
  val stay = Action("stay")

  def extractPlayerHand(state: State): String = {
    if state.toString.length == 0 then ""
    else state.toString.split(',')(0)
  }
  def extractDealerHand(state: State): String = {
    val splitState = state.toString.split(',')
    if splitState.length < 2 then ""
    else splitState(1)
  }

  def cardsToValue(cardsToSum: Array[String]): Int = {
    val sum: Int = cardsToSum.foldLeft(0)((acc, c) => {
      val v = c match
        case "" => 0
        case "A" => 1
        case "K" => 10
        case "Q" => 10
        case "J" => 10
        case s => s.toInt
      acc + v
    })

    if ((sum >= 7 && sum <= 11) && cardsToSum.contains("A")) {
      sum + 10
    } else sum
  }

  def cardsToValueTests() = {
    println(cardsToValue(Array("1", "5", "Q")) == 16)
    println(cardsToValue(Array("A", "1", "5", "Q")) == 17)
    println(cardsToValue(Array("2", "3", "4")) == 9)
    println(cardsToValue(Array("6", "7", "K")) == 23)
    println(cardsToValue(Array("J", "K")) == 20)
    println(cardsToValue(Array("A", "A", "A", "4")) == 17)
    println(cardsToValue(Array("4", "3", "A", "A")) == 19)
    println(cardsToValue(Array("2", "6", "K")) == 18)
    println(cardsToValue(Array("Q", "A")) == 21)
  }

  def playerReward(state: State): Int = {
    val playerHandValue = cardsToValue(extractPlayerHand(state).split('|'))
    if playerHandValue > 21 then -1
    else {
      val dealerHand = extractDealerHand(state)
      //println("dh: " + dealerHand)
      val dealerHandValue = {
        cardsToValue(dealerHand.split('|'))
      }
      if playerHandValue == dealerHandValue then 0
      else if dealerHandValue <= 21 && playerHandValue < dealerHandValue then -1
      else 1
    }
  }

  def playerRewardTests(): Unit = {
    println(playerReward(State("5|7|8,10|Q")) == 0)
    println(playerReward(State("A|Q,10|Q")) == 1)
    println(playerReward(State("5|7|3|10,5|2")) == -1)
    println(playerReward(State("3|2|5|6|2|3,5|5|2")) == 1)
    println(playerReward(State("9|9,2|10|J")) == 1)
    println(playerReward(State("1|2|3|4|5|6,A|Q")) == 0)
    println(playerReward(State("7|1,3|2")) == 1)
    println(playerReward(State("7|1,8|7|6|5")) == 1)
    println(playerReward(State("8|7|6|5,7|1")) == -1)
    println(playerReward(State("13|8,K|K")) == 1)
  }

  def stateActions(state: State): Vector[Action] = {
    val playerHand = extractPlayerHand(state)
    val dealerHand = extractDealerHand(state)
    val playerHandValue = cardsToValue(playerHand.split('|'))
    val dealerShowingValue = cardsToValue(dealerHand.split('|').take(1))
    val dealerHandValue = cardsToValue(dealerHand.split('|'))

    Vector(
      if playerHandValue >= 20 && dealerHandValue >= 17 then {
        stay
      } else if playerHandValue <= 11 then {
        playerHit
      } else if playerHandValue < 20 then { 
        // taking this conjunct out because the book said so, but just wild play...wild: 
        // && (dealerShowingValue == 1 || dealerShowingValue >= 7) then {
        playerHit
      } else if dealerHandValue >= 17 then {
        stay
      } else {
        dealerHit
      }
    )
  }

  def draw(): String = {
    cards(Random.nextInt(13))
  }
  case class BlackJackEnvironment(override val possibleStateActions: (State) => Vector[Action], override val state: State) 
    extends Environment(possibleStateActions, state) {
    def act(a: Action): (Reward, Environment, EndOfEpisode) = {
      if a == stay then { (Reward(playerReward(state)), this, true) }
      else if a == playerHit then {
        val newCard = draw()
        val updatedState = State(
          if (state.toString.length == 0) {
            newCard + '|' + draw() + ',' + draw() + '|' + draw()
          } else {
            val splitState = state.toString.split(',')
            splitState(0) + '|' + newCard + ',' +
            { 
              if splitState.length == 1 then ""
              else splitState(1)
            }
          }
        )
        (
          Reward(0),
          BlackJackEnvironment(stateActions, updatedState),
          false
        )
      }
      else {
        val newCard = draw()
        val updatedState = State(
          if (state.toString.last == ',') {
            state.toString + newCard
          } else {
            state.toString + '|' + newCard
          }
        )
        (
          Reward(0),
          BlackJackEnvironment(stateActions, updatedState),
          false
        )
      }
    }

    def isOptimal(a: Action): OptimalAct = {
      OptimalAct(true)
    }
  }

  val blackJackEnvironment: Environment = {
    def stateActions(state: State): Vector[Action] = {
      val splitState = state.toString.split(',')
      val playerHand = if splitState.length > 0 then splitState(0) else ""
      val dealerHand = if splitState.length > 1 then splitState(1) else ""
      val playerHandValue = cardsToValue(playerHand.split('|'))
      val dealerHandValue = cardsToValue(dealerHand.split('|'))

      if playerHandValue > 21 || dealerHandValue > 21 then {
        Vector(stay)
      } else if playerHandValue < 20 then {
        Vector(playerHit)
      } else if dealerHandValue < 17 then {
        Vector(dealerHit)
      } else {
        Vector(stay)
      }
    }

    BlackJackEnvironment(
      stateActions,
      State("")
    )
  }

  val actionSelector: RewardSelector = {
    new RewardSelector { 
        def apply(
        step: Step,
        actionSteps: Map[State, Map[Action, Step]]
        )(actionRewards: Map[Action, Reward]): (Action, IsExploratory) = {
          (actionRewards.head._1, false)
        }
      }
  }
  
  val blackJackAgent: TabularAgent[Reward] = TabularAgent.rewardBlankSlate(
    blackJackEnvironment,
    actionSelector,
    average(sampleAverage)
  )
}

def simulateBJ(): TabularAgent[Reward] = {
  var a = BlackJack.blackJackAgent
  var i = 0

  while {
    i += 1
    a = a.act
    !a.table.values.exists(_.keys.toList.contains(BlackJack.stay)) && i < 20
  } do () 

  a
}

def sampleBJ(samples: Int): Map[(Int, String), Double] = {
  import BlackJack._
  // Map[(Player Starting Hand, Dealer Showing) -> Reward]
  val result: Map[(Int, String), Double] = {
    (for (i <- 0 until samples) yield {
      val agentResult = simulateBJ()
      val finalHand: State = 
        (for {
          (k, v) <- agentResult.table
          if (v.keys.toList.contains(BlackJack.stay))
        } yield k).head

      if i % (samples / 10) == 0 then println(finalHand)

      val reward = agentResult.table(finalHand).toList.head._2
      val playerHand = BlackJack.extractPlayerHand(finalHand)
      val dealerShowing = BlackJack.extractDealerHand(finalHand).split('|')(0)
      val playerHandValue = playerHand.split('|').foldLeft(0)((acc, elem) => {
        if (acc >= 12) acc
        else {
          if (acc <= 10 && elem == "A") {
            acc + 11
          } else {
            acc + BlackJack.cardsToValue(Array(elem))
          }
        }
      })
      (playerHandValue, dealerShowing) -> reward.toDouble
    }).groupBy(_._1).mapValues(v => (v.map(_._2).sum / v.length.toDouble)).toMap
  }
  result
}


def scaleToProbability(dbl: Double): Probability = {
  Probability.unsafe(Math.min((dbl + 1.001) / 2, 1D))
}

def scaleToProbabilityTests() = {
  println(scaleToProbability(-1))
  println(scaleToProbability(-1) == Probability(0.0005))
  println(scaleToProbability(-0.5))
  println(scaleToProbability(-0.5) == Probability(0.2505))
  println(scaleToProbability(0))
  println(scaleToProbability(0) == Probability(0.5005))
  println(scaleToProbability(0.5))
  println(scaleToProbability(0.5) == Probability(0.7505))
  println(scaleToProbability(1))
  println(scaleToProbability(1) == Probability(1))
  println(scaleToProbability(0.99))
  println(scaleToProbability(0.99) == Probability(0.9955))
}

def transformToMass(map: Map[(Int, String), Double]): Seq[(Int, Int)] = {
  val seq = map.map {(k, v) => 
    val (p, d) = k
    val dealerValue = BlackJack.cardsToValue(Array(d))
    val dv = if dealerValue == 1 then 11 else dealerValue
    val num = (scaleToProbability(v).toDouble * 1000).toInt
    val l = List.fill(num)((dv, p))
    k -> l
  }.values.flatten.toSeq

  seq
}

def plotBJSimulation(samples: Int) = {

  val simulatedResults: Map[(Int, String), Double] = sampleBJ(samples)
  val dealerRewards: Seq[(Int, Double)] = simulatedResults.map((k, v) => {
    val dealerValue = BlackJack.cardsToValue(Array(k._2))
    val dv = if dealerValue == 1 then 11 else dealerValue
    dv -> v
  }).toSeq
  val playerRewards: Seq[(Int, Double)] = simulatedResults.map((k, v) => k._1 -> v).toSeq
  val points: Seq[(Int, Int)] = transformToMass(simulatedResults)

  contourPlot(points, playerRewards, dealerRewards)
}

def contourPlot(points: Seq[(Int, Int)], playerRewards: Seq[(Int, Double)], dealerRewards: Seq[(Int, Double)]) = {
  import com.cibo.evilplot._
  import com.cibo.evilplot.geometry._
  import com.cibo.evilplot.plot._
  import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  import com.cibo.evilplot.numeric._
  import com.cibo.evilplot.colors._
  import com.cibo.evilplot.plot.renderers._
  import com.cibo.evilplot.plot.aesthetics._
  
  implicit val extent: Extent = Extent(400, 400)

    displayPlot(
      ContourPlot(points.map { p =>
        Point(p._1.toDouble, p._2.toDouble)
      }, surfaceRenderer = Some(SurfaceRenderer.contours()))
      .xAxis()
      .yAxis()
      .frame()
      .xLabel("Dealer")
      .yLabel("Player")
      .topPlot(Histogram(points.map(_._1.toDouble)))
      .rightPlot(Histogram(points.map(_._2.toDouble)))
      .render()
    )

}