package com.robertjneal.rl.types.goal

opaque type Reward = Double
object Reward {
  def apply(d: Double): Reward = d
  def apply(i: Int): Reward = i.toDouble
  
  implicit class RewardOps(val self: Reward) extends AnyVal {
    def +(r2: Reward): Reward = self + r2
    def -(r2: Reward): Reward = self - r2
    def >(r2: Reward): Boolean = self > r2
    def <(r2: Reward): Boolean = self < r2

    def toDouble: Double = self
  }

  implicit val rewardOrdering: Ordering[Reward] = new Ordering[Reward] {
    override def compare(r1: Reward, r2:Reward): Int = {
        r1.toDouble.compareTo(r2.toDouble)
    }
}
}