package com.robertjneal.rl.agent

import com.robertjneal.rl.Environment
import com.robertjneal.rl.types._
import scala.util.Try

trait TabularAgent{
  val history: Array[(OptimalAct, Reward)]
  def act: TabularAgent
}