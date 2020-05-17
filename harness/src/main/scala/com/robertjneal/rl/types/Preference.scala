package com.robertjneal.rl.types

opaque type Preference = Double
object Preference {
  def apply(d: Double): Preference = d

  implicit class PreferenceOps(self: Preference) extends AnyVal {
    def +(p2: Preference): Preference = self + p2

    def toDouble: Double = self
  }
}