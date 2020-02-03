package com.robertjneal.rl

import org.apache.commons.math3.distribution._

object Types {
  opaque type State = String
  object State {
    def apply(s: String): State = s
  }

  opaque type Action = String
  object Action {
    def apply(s: String): Action = s
    def fromString(s: String): Action = Action(s)
  }

  opaque type OptimalAct = Boolean
  object OptimalAct {
    def apply(b: Boolean): OptimalAct = b

    implicit class OptimalActOps(val self: OptimalAct) extends AnyVal {
      def toInt: Int = if (self) 1 else 0
      def toDouble: Double = self.toInt.toDouble
    }
  }
  //opaque type ActionValue = (State, Action) => Reward
  //opaque type StateValue = State => Reward

  opaque type Reward = Double
  object Reward {
    def apply(d: Double): Reward = d
    
    implicit class RewardOps(val self: Reward) extends AnyVal {
      def +(r2: Reward): Reward = self + r2
      def -(r2: Reward): Reward = self - r2
      def >(r2: Reward): Boolean = self > r2
      def <(r2: Reward): Boolean = self < r2

      def toDouble: Double = self
    }
  }

  opaque type RandomReward = NormalDistribution
  object RandomReward {
    def apply(d: NormalDistribution): RandomReward = d

    implicit class RandomRewardOps(val self: RandomReward) extends AnyVal {
      def sample: Reward = Reward(self.sample)
      def trueReward: Reward = Reward(self.getMean)
    }
  }

  opaque type Step = Int
  object Step { 
    def apply(i: Int): Step = i 

    implicit class StepOps(val self: Step) extends AnyVal {
      def toInt: Int = self
      def increment: Step = self + 1
    }
  }

  // Probability type from https://docs.scala-lang.org/sips/opaque-types.html
  opaque type Probability = Double
  object Probability {
    def apply(n: Double): Option[Probability] =
      if (0.0 <= n && n <= 1.0) Some(n) else None

    def unsafe(p: Double): Probability = {
      require(0.0 <= p && p <= 1.0, s"probabilities lie in [0, 1] (got $p)")
      p
    }

    def asDouble(p: Probability): Double = p

    val Never: Probability = 0.0
    val CoinToss: Probability = 0.5
    val Certain: Probability = 1.0

    implicit val ordering: Ordering[Probability] =
      implicitly[Ordering[Double]]

    implicit class ProbabilityOps(p1: Probability) extends AnyVal {
      def unary_~ : Probability = Certain - p1
      def &(p2: Probability): Probability = p1 * p2
      def |(p2: Probability): Probability = p1 + p2 - (p1 * p2)
      def >(p2: Probability): Boolean = p1 > p2
      def <(p2: Probability): Boolean = p1 < p2

      def isImpossible: Boolean = p1 == Never
      def isCertain: Boolean = p1 == Certain

      import scala.util.Random

      def sample(r: Random = Random): Boolean = r.nextDouble <= p1
      def toDouble: Double = p1
      override def toString: String = p1.toString
    }
  }
}
