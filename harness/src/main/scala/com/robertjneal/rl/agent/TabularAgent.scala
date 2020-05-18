package com.robertjneal.rl.agent

import com.robertjneal.rl.Environment
import com.robertjneal.rl.types._

trait TabularAgent{
  val history: Array[(OptimalAct, Reward)]
  def act: TabularAgent
}