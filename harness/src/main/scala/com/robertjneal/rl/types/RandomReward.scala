package com.robertjneal.rl.types

import com.robertjneal.rl.types.goal._

trait RandomReward {
  // returns an updated RandomReward to support non-stationary rewards
  def sample: (Reward, RandomReward)
  def trueReward: Reward
}
