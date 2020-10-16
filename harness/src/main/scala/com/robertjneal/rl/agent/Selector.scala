package com.robertjneal.rl.agent

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

sealed trait Selector

case class RewardSelector(
    actionSelector: (
        Step,
        Map[State, Map[Action, Step]]
    ) => Map[Action, Reward] => Action
) extends Selector

case class PreferenceSelector(
    actionSelector: Map[Action, Preference] => Action
) extends Selector
