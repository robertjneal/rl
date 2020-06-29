package com.robertjneal.rl.types.goal

opaque type Preference = Double
object Preference {
  def apply(n: Double): Preference = n

  implicit class PreferenceOps(self: Preference) extends AnyVal {
    def +(p2: Preference): Preference = self + p2
    def >(p2: Preference): Boolean = self > p2
    def >=(p2: Preference): Boolean = self >= p2
    def <(p2: Preference): Boolean = self < p2
    def <=(p2: Preference): Boolean = self <= p2

    def toDouble: Double = self
  }
}