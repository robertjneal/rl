package com.robertjneal.rl.types

opaque type Step = Int
object Step { 
  def apply(i: Int): Step = i 

  implicit class StepOps(val self: Step) extends AnyVal {
    def toInt: Int = self
    def increment: Step = self + 1
  }
}