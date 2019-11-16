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
  }
  //opaque type ActionValue = (State, Action) => Reward
  //opaque type StateValue = State => Reward

  opaque type Reward = Double
  object Reward {
    def apply(d: Double): Reward = d
    
    implicit class RewardOps(val self: Reward) extends AnyVal {
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
}
