package com.robertjneal.rl

import com.robertjneal.rl.types._
import scala.collection.mutable

case class TabularAgent(
  e: Environment, 
  var s: State, 
  actionSelector: mutable.Map[Action, Reward] => Action, 
  updater: (mutable.Map[Action, Reward], Action, Reward, Step) => Unit,
  recordHistory: Boolean = false
  ) {  
  private var mutableHistory: Array[(OptimalAct, Reward)] = Array.empty
  private val table: Map[State, mutable.Map[Action, Reward]] = e.possibleStateActions.map { 
    case (s, as) => s -> mutable.Map(as.map(a => a -> Reward(0)): _*) 
  }

  private var step: Step = Step(0)

  def act: Unit = {
    step = step.increment

    val act = actionSelector(table(s))
    val (reward, _) = e.act(s, act)
    updater(table(s), act, reward, step)

    if (recordHistory) {
      val appendage = (e.isOptimal(s, act), reward)
      mutableHistory = mutableHistory :+ appendage
    }
  }

  def history: Array[(OptimalAct, Reward)] = mutableHistory
}