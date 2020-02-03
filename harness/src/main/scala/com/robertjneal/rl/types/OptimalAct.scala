package com.robertjneal.rl.types

opaque type OptimalAct = Boolean
object OptimalAct {
  def apply(b: Boolean): OptimalAct = b

  implicit class OptimalActOps(val self: OptimalAct) extends AnyVal {
    def toInt: Int = if (self) 1 else 0
    def toDouble: Double = self.toInt.toDouble
  }
}