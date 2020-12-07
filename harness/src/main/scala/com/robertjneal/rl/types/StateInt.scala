package com.robertjneal.rl.types

opaque type StateInt = Int

object StateInt {
  def apply(i: Int): StateInt = i
  def unapply(i: Int): Option[Int] = Some(i)
}