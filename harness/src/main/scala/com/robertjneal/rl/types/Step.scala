package com.robertjneal.rl.types

opaque type Step = Int
object Step {
  def apply(i: Int): Step = i

  implicit class StepOps(val self: Step) extends AnyVal {
    def +(s2: Step): Step = self + s2
    def toInt: Int = self
    def toDouble: Double = self.toDouble
    def increment: Step = self + 1
    def decrement: Step = self - 1
  }
}
