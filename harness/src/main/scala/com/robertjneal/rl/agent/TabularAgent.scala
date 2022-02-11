package com.robertjneal.rl.agent

import com.robertjneal.rl.environment.Environment
import com.robertjneal.rl.actionselector._
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import com.robertjneal.rl.updater._
import scala.util.Try

case class TabularAgent[A <: Goal](
    e: Environment,
    actionSelector: ActionSelector[A],
    updater: Updater[A],
    step: Step,
    initialGoalValue: A,
    actionSteps: Map[State, Map[Action, Step]] = Map.empty,
    table: Map[State, Map[Action, A]] = Map.empty,
    totalReward: Reward = Reward(0),
    recordHistory: Boolean = false,
    history: Array[(OptimalAct, Reward)] = Array.empty,
    temporalDifference: Option[TemporalDifference] = None
) {
  val isTemporalDifference = temporalDifference.isDefined
  
  def act: TabularAgent[A] = {
    val stateTable = table.getOrElse(
      e.state,
      e.possibleStateActions(e.state).map(_ -> initialGoalValue).toMap
    )

    val (action, isExploratory): (Action, Boolean) = actionSelector match {
      case rewardSelector: RewardSelector => 
        rewardSelector(step, actionSteps)(
          stateTable.asInstanceOf[Map[Action, Reward]]
        )
      case preferenceSelector: PreferenceSelector => 
        preferenceSelector(
          table.asInstanceOf[Map[State, Map[Action, Preference]]](e.state)
        )
    }

    val stateActionSteps: Map[Action, Step] = 
      actionSteps.getOrElse(e.state, e.possibleStateActions(e.state).map(_ -> Step(1))).toMap

    val averageReward = Reward(
      totalReward.toDouble / stateActionSteps.values.reduce(_ + _).toInt
    )
    val (environmentReward, updatedEnvironment, _) = e.act(action)
    val reward = temporalDifference match
      case Some(td) => 
        Reward(td.stepSize * table(e.state)(action).asInstanceOf[Reward].toDouble)
      case None => environmentReward

    val shouldUpdateValues = !(isTemporalDifference && isExploratory)

    val updates: Map[Action, A] = if !shouldUpdateValues then table(e.state)
      else updater(
        stateTable,
        action,
        reward,
        stateActionSteps(action),
        Some(averageReward)
      )
    val updatedTable: Map[State, Map[Action, A]] = table.updated(e.state, updates)

    val updatedHistory = if (recordHistory) {
      val appendage = (e.isOptimal(action), reward)
      history :+ appendage
    } else history

    val updatedActionSteps: Map[State, Map[Action, Step]] =
      actionSteps.updated(
        e.state,
        stateActionSteps.updated(
          action,
          stateActionSteps(action).increment
        )
      )

    TabularAgent[A](
      updatedEnvironment,
      actionSelector,
      updater,
      step.increment,
      initialGoalValue,
      updatedActionSteps,
      updatedTable,
      totalReward + reward,
      recordHistory,
      updatedHistory,
      temporalDifference
    )
  }
}

object TabularAgent {
  def blankSlate[A <: Goal](
      e: Environment,
      actionSelector: ActionSelector[A],
      updater: Updater[A],
      initialGoalValue: A,
      recordHistory: Boolean = false
  ): TabularAgent[A] = {

    TabularAgent[A](
      e,
      actionSelector,
      updater,
      Step(0),
      initialGoalValue,
      recordHistory = recordHistory
    )
  }

  def rewardBlankSlate(
      e: Environment,
      actionSelector: RewardSelector,
      updater: Updater[Reward],
      recordHistory: Boolean = false
  ): TabularAgent[Reward] = {
    blankSlate[Reward](
      e,
      actionSelector,
      updater,
      Reward(0),
      recordHistory
    )
  }

  def preferenceBlankSlate(
      e: Environment,
      actionSelector: PreferenceSelector,
      updater: Updater[Preference],
      recordHistory: Boolean = false
  ): TabularAgent[Preference] = {
    blankSlate[Preference](
      e,
      actionSelector,
      updater,
      Preference(0),
      recordHistory
    )
  }
}
