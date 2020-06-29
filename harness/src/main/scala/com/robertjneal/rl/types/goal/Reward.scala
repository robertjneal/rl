package com.robertjneal.rl.types.goal

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