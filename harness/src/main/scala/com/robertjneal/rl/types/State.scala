package com.robertjneal.rl.types

type State = StateString | StateInt

object State {
  def apply(i: Int): StateInt = StateInt(i)
  def apply(s: String): StateString = StateString(s)

  object Ordering extends Ordering[State] {
    def compare(s1: State, s2: State) = (s1, s2) match {
      case (string1: String, string2: String) => string1.compare(string2)
      case (int1: Int, int2: Int) => int1.compare(int2)
      case (_: Int, _: String) => -1
      case (_: String, _: Int) => 1
    }
  }
}