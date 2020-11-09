package com.robertjneal.rl.agent

import com.robertjneal.rl.Environment
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import com.robertjneal.rl.updater._
import scala.util.Try

case class TabularAgent[A <: Goal](
    e: Environment,
    actionSelector: Selector[A],
    updater: Updater[A],
    step: Step,
    actionSteps: Map[State, Map[Action, Step]],
    table: Map[State, Map[Action, A]],
    totalReward: Reward = Reward(0),
    recordHistory: Boolean = false,
    history: Array[(OptimalAct, Reward)] = Array.empty,
    temporalDifference: Option[TemporalDifference] = None
) {
  val isTemporalDifference = temporalDifference.isDefined
  
  def act: TabularAgent[A] = {
    val (action, isExploratory): (Action, Boolean) = actionSelector match {
      case rewardSelector: RewardSelector[_] => 
        rewardSelector.actionSelector(step, actionSteps)(
          table.asInstanceOf[Map[State, Map[Action, Reward]]](e.state)
        )
      case preferenceSelector: PreferenceSelector[_] => 
        preferenceSelector.actionSelector(
          table.asInstanceOf[Map[State, Map[Action, Preference]]](e.state)
        )
    }

    val averageReward = Reward(
      totalReward.toDouble / actionSteps(e.state).values.reduce(_ + _).toInt
    )
    val (environmentReward, updatedEnvironment, endOfEpisode) = e.act(action)
    val reward = temporalDifference.map{ td => Reward(td.stepSize * table(e.state)(action).asInstanceOf[Reward].toDouble)}.getOrElse(environmentReward)
    val updates: Map[Action, A] = if isTemporalDifference && isExploratory then table(e.state)
      else updater(
        table(e.state),
        action,
        reward,
        actionSteps(e.state)(action),
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
        actionSteps(e.state).updated(
          action,
          actionSteps(e.state)(action).increment
        )
      )

    TabularAgent[A](
      updatedEnvironment,
      actionSelector,
      updater,
      step.increment,
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
      actionSelector: Selector[A],
      updater: Updater[A],
      initialGoalValue: A,
      recordHistory: Boolean = false
  ): TabularAgent[A] = {
    val initialActionSteps = e.possibleStateActions.map { (s, as) =>
      s -> Map(as.map(_ -> Step(1)): _*)
    }
    val initialTable = e.possibleStateActions.map { (s, as) =>
      s -> Map(as.map(_ -> initialGoalValue): _*)
    }

    TabularAgent[A](
      e,
      actionSelector,
      updater,
      Step(0),
      initialActionSteps,
      initialTable,
      recordHistory = recordHistory
    )
  }

  def rewardBlankSlate(
      e: Environment,
      actionSelector: (
          Step,
          Map[State, Map[Action, Step]]
      ) => Map[Action, Reward] => (Action, Boolean),
      updater: Updater[Reward],
      recordHistory: Boolean = false
  ): TabularAgent[Reward] = {
    blankSlate[Reward](
      e,
      RewardSelector(actionSelector),
      updater,
      Reward(0),
      recordHistory
    )
  }

  def preferenceBlankSlate(
      e: Environment,
      actionSelector: Map[Action, Preference] => (Action, Boolean),
      updater: Updater[Preference],
      recordHistory: Boolean = false
  ): TabularAgent[Preference] = {
    blankSlate[Preference](
      e,
      PreferenceSelector(actionSelector),
      updater,
      Preference(0),
      recordHistory
    )
  }
}
