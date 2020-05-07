package com.robertjneal.rl

import com.robertjneal.rl.types._

case class TabularAgent(
  e: Environment, 
  actionSelector: (Step, Map[State, Map[Action, Step]]) => Map[Action, Reward] => Action, 
  updater: (Map[Action, Reward], Action, Reward, Step) => Map[Action, Reward],
  step: Step,
  actionSteps: Map[State, Map[Action, Step]],
  table: Map[State, Map[Action, Reward]],
  recordHistory: Boolean = false,
  history: Array[(OptimalAct, Reward)] = Array.empty
  ) {  
  private val mutableHistory: Array[(OptimalAct, Reward)] = Array.empty

  def act: TabularAgent = {
    val action = actionSelector(step, actionSteps)(table(e.state))

    val (reward, updatedEnvironment) = e.act(action)
    val updatedTable: Map[State, Map[Action, Reward]] = Map(e.state -> updater(table(e.state), action, reward, actionSteps(e.state)(action)))

    val updatedHistory = if (recordHistory) {
      val appendage = (e.isOptimal(action), reward)
      history :+ appendage
    } else history

    val updatedActionSteps: Map[State, Map[Action, Step]] = actionSteps.updated(e.state,
      actionSteps(e.state).updated(
        action, 
        actionSteps(e.state)(action).increment
      )
    )

    TabularAgent(
      updatedEnvironment,
      actionSelector,
      updater,
      step.increment,
      updatedActionSteps,
      updatedTable,
      recordHistory,
      updatedHistory
    )
  }
}

object TabularAgent {
  def blankSlate(e: Environment, 
  actionSelector: (Step, Map[State, Map[Action, Step]]) => Map[Action, Reward] => Action,
  updater: (Map[Action, Reward], Action, Reward, Step) => Map[Action, Reward],
recordHistory: Boolean = false): TabularAgent = {
    val initialActionSteps = e.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
    }
    val initialTable = e.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Reward(0)): _*) 
    }

    TabularAgent(
      e,
      actionSelector,
      updater,
      Step(0),
      initialActionSteps,
      initialTable,
      recordHistory
    )
  }
}