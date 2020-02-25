package com.robertjneal.rl.types

import org.apache.commons.math3.distribution._

class StationaryDistribution(distribution: AbstractRealDistribution) extends RandomReward {
  override def sample: Reward = {
    val sample = distribution.sample
    if (sample.isNaN) {
      throw Exception("You're probably using an RNG that isn't thread safe.")
    }
    Reward(sample)
  }
  override def trueReward: Reward = Reward(distribution.getNumericalMean)
}
