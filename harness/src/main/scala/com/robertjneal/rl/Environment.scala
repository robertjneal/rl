package com.robertjneal.rl

import com.robertjneal.rl.types._

trait Environment(val possibleStateActions: Map[State, Vector[Action]], val state: State) {
  def act(a: Action): (Reward, Environment)
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
  require(possibleStateActions.size == 1)

  def act(a: Action): (Reward, Environment) = {
    val (reward, updatedRewardFunction): (Reward, RandomReward) = actionRewards(a).sample
    val updatedActionRewards = actionRewards.updated(a, updatedRewardFunction)
    (reward, this.copy(actionRewards = updatedActionRewards))
  }

  // TODO: memoize when stationary?
  def isOptimal(a: Action): OptimalAct = {
    OptimalAct(optimalActs.contains(a))
  }

  private val actionTrueRewards: Vector[(Reward, Action)] = possibleActions.map(a => (actionRewards(a).trueReward, a))

  // TODO: memoize when stationary?
  private def maxReward: Reward = {
    Reward(actionTrueRewards.map {
      case (r, _) => r.toDouble
    }.max)
  }

  // TODO: memoize when stationary?
  private def optimalActs: Vector[Action] = {
    actionTrueRewards
      .filter { case (r, _) => r == maxReward }
      .map { case (_, a) => a }
  }
}