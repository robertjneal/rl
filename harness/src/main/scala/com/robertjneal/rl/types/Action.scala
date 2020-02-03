package com.robertjneal.rl.types

opaque type Action = String
object Action {
  def apply(s: String): Action = s
  def fromString(s: String): Action = Action(s)
}