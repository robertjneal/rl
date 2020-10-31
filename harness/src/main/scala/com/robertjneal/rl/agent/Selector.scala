package com.robertjneal.rl.agent

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

sealed trait Selector[A]

case class RewardSelector[A <: Reward](
    actionSelector: (
        Step,
        Map[State, Map[Action, Step]]
    ) => Map[Action, Reward] => Action
) extends Selector[A]

case class PreferenceSelector[A <: Preference](
    actionSelector: Map[Action, Preference] => Action
) extends Selector[A]
