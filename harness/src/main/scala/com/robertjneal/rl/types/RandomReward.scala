package com.robertjneal.rl.types

trait RandomReward {
  def sample: Reward
  def trueReward: Reward
}
