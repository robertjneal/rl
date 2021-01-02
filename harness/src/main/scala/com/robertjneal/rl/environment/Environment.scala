package com.robertjneal.rl.environment

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

trait Environment(val possibleStateActions: Map[State, Vector[Action]], val state: State) {
  type EndOfEpisode = Boolean
  def act(a: Action): (Reward, Environment, EndOfEpisode)
  def isOptimal(a: Action): OptimalAct
}
