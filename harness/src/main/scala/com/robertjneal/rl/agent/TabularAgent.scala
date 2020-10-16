package com.robertjneal.rl.agent

import com.robertjneal.rl.Environment
import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import scala.util.Try

case class TabularAgent[A](
    e: Environment,
    actionSelector: Selector,
    updater: (
        Map[Action, A],
        Action,
        Reward,
        Step,
        Option[Reward]
    ) => Map[Action, A],
    step: Step,
    actionSteps: Map[State, Map[Action, Step]],
    table: Map[State, Map[Action, A]],
    totalReward: Reward = Reward(0),
    recordHistory: Boolean = false,
    history: Array[(OptimalAct, Reward)] = Array.empty
) {
  def act: TabularAgent[A] = {
    val actionOption: Option[Action] = (actionSelector, table) match {
      case (
            rewardSelector: RewardSelector,
            rewardTable: Map[State, Map[Action, Reward]]
          ) => {
        Some(
          rewardSelector.actionSelector(step, actionSteps)(rewardTable(e.state))
        )
      }
      case (
            preferenceSelector: PreferenceSelector,
            preferenceTable: Map[State, Map[Action, Preference]]
          ) => {
        Some(preferenceSelector.actionSelector(preferenceTable(e.state)))
      }
      case _ => None
    }

    actionOption match {
      case None => this
      case Some(action) => {
        val averageReward = Reward(
          totalReward.toDouble / actionSteps(e.state).values.reduce(_ + _).toInt
        )
        val (reward, updatedEnvironment) = e.act(action)
        val updates: Map[Action, A] = updater(
          table(e.state),
          action,
          reward,
          actionSteps(e.state)(action),
          Some(averageReward)
        )
        val updatedTable: Map[State, Map[Action, A]] = Map(e.state -> updates)

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
          updatedHistory
        )
      }
    }
  }
}

object TabularAgent {
  def blankSlate[A](
      e: Environment,
      actionSelector: Selector,
      updater: (
          Map[Action, A],
          Action,
          Reward,
          Step,
          Option[Reward]
      ) => Map[Action, A],
      initialGoalValue: A,
      recordHistory: Boolean = false
  ): TabularAgent[A] = {
    val initialActionSteps = e.possibleStateActions.map { case (s, as) =>
      s -> Map(as.map(_ -> Step(1)): _*)
    }
    val initialTable = e.possibleStateActions.map { case (s, as) =>
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
      ) => Map[Action, Reward] => Action,
      updater: (
          Map[Action, Reward],
          Action,
          Reward,
          Step,
          Option[Reward]
      ) => Map[Action, Reward],
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
      actionSelector: Map[Action, Preference] => Action,
      updater: (
          Map[Action, Preference],
          Action,
          Reward,
          Step,
          Option[Reward]
      ) => Map[Action, Preference],
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
