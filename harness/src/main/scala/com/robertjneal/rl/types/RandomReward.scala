package com.robertjneal.rl.types

trait RandomReward {
  // returns an updated RandomReward to support non-stationary rewards
  def sample: (Reward, RandomReward)
  def trueReward: Reward
}
