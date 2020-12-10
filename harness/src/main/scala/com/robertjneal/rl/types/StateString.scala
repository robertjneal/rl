package com.robertjneal.rl.types

opaque type StateString = String
object StateString {
  def apply(s: String): StateString = s
  def unapply(s: String): Option[String] = Some(s)
}