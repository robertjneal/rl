package com.robertjneal.rl

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._

trait Environment(val possibleStateActions: Map[State, Vector[Action]], val state: State) {
  def act(a: Action): (Reward, Environment, Boolean)
  def isOptimal(a: Action): OptimalAct
}

final val OneState = State("OneState")

/*
 * A bandit only has a single state, so we ignore state most of the time.
 */
case class BanditEnvironment(
  possibleActions: Vector[Action],
  actionRewards: Map[Action, RandomReward]
  ) extends Environment(Map(OneState -> possibleActions), OneState) {
  import Ordering.Double.TotalOrdering
  require(possibleStateActions.size == 1)

  def act(a: Action): (Reward, Environment, Boolean) = {
    val (reward, updatedRewardFunction): (Reward, RandomReward) = actionRewards(a).sample
    val updatedActionRewards = actionRewards.updated(a, updatedRewardFunction)
    (reward, this.copy(actionRewards = updatedActionRewards), true)
  }

  // TODO: memoize when stationary?
  def isOptimal(a: Action): OptimalAct = {
    OptimalAct(optimalActs.contains(a))
  }

  private val actionTrueRewards: Vector[(Reward, Action)] = possibleActions.map(a => (actionRewards(a).trueReward, a))

  // TODO: memoize when stationary?
  private def maxReward: Reward = {
    val (reward, _) = actionTrueRewards.maxBy{ (r, _) => r }
    reward
  }

  // TODO: memoize when stationary?
  private def optimalActs: Vector[Action] = 
    actionTrueRewards
      .filter { (r, _) => r == maxReward }
      .map { (_, a) => a }
}