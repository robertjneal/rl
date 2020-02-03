package com.robertjneal.rl.types

opaque type State = String
object State {
  def apply(s: String): State = s
}