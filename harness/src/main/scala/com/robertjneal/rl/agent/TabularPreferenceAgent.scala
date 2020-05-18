package com.robertjneal.rl.agent

import com.robertjneal.rl.Environment
import com.robertjneal.rl.OneState
import com.robertjneal.rl.types._

case class TabularPreferenceAgent(
  e: Environment, 
  actionSelector: Map[Action, Preference] => Action, 
  updater: (Map[Action, (Reward, Preference)], Action, Reward, Step) => Map[Action, (Reward, Preference)],
  step: Step,
  actionSteps: Map[State, Map[Action, Step]],
  table: Map[State, Map[Action, (Reward, Preference)]],
  recordHistory: Boolean = false,
  history: Array[(OptimalAct, Reward)] = Array.empty
  ) extends TabularAgent {  

  def act: TabularPreferenceAgent = {
    val action = actionSelector(table(e.state).map { actionRewardPreference => actionRewardPreference._1 -> actionRewardPreference._2._2 })

    val (reward, updatedEnvironment) = e.act(action)
    val updatedTable: Map[State, Map[Action, (Reward, Preference)]] = Map(e.state -> updater(table(e.state), action, reward, actionSteps(e.state)(action)))

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

    TabularPreferenceAgent(
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

object TabularPreferenceAgent {
  def blankSlate(e: Environment, 
  actionSelector: Map[Action, Preference] => Action, 
  updater: (Map[Action, (Reward, Preference)], Action, Reward, Step) => Map[Action, (Reward, Preference)],
  recordHistory: Boolean = false): TabularPreferenceAgent = {
    val initialActionSteps = e.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> Step(1)): _*) 
    }
    val initialTable = e.possibleStateActions.map { 
      case (s, as) => s -> Map(as.map(_ -> (Reward(0), Preference(0))): _*) 
    }

    TabularPreferenceAgent(
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