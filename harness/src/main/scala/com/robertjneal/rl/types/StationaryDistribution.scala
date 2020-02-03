package com.robertjneal.rl.types

import org.apache.commons.math3.distribution._

class StationaryDistribution(distribution: AbstractRealDistribution) extends RandomReward {
  override def sample: Reward = Reward(distribution.sample)
  override def trueReward: Reward = Reward(distribution.getNumericalMean)
}
