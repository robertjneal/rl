package com.robertjneal.rl.types

import com.robertjneal.rl.types.goal.Reward
import org.apache.commons.math3.distribution._

class StationaryDistribution(distribution: AbstractRealDistribution) extends RandomReward {
  override def sample: (Reward, RandomReward) = {
    val sample = distribution.sample
    if (sample.isNaN) {
      throw Exception("You're probably using an RNG that isn't thread safe.")
    }
    (Reward(sample), this)
  }
  override def trueReward: Reward = Reward(distribution.getNumericalMean)
}
