package com.robertjneal.rl

import com.robertjneal.rl.types._

trait Environment(val possibleStateActions: Map[State, Vector[Action]]) {
  def act(s: State, a: Action): (Reward, State)
  def isOptimal(s: State, a: Action): OptimalAct
}

final val OneState = State("OneState")

/*
 * A bandit only has a single state, so we ignore state most of the time.
 */
case class BanditEnvironment(
  possibleActions: Vector[Action],
  actionRewards: Action => RandomReward
  ) extends Environment(Map(OneState -> possibleActions)) {
  require(possibleStateActions.size == 1)

  def act(s: State, a: Action): (Reward, State) = {
    (actionRewards(a).sample, s)
  }

  // TODO: memoize when stationary?
  def isOptimal(s: State, a: Action): OptimalAct = {
    OptimalAct(optimalActs.contains(a))
  }

  private val actionTrueRewards: Vector[(Reward, Action)] = possibleActions.map(a => (actionRewards(a).trueReward, a))

  private lazy val maxReward: Reward = {
    Reward(actionTrueRewards.map {
      case (r, _) => r.toDouble
    }.max)
  }

  private lazy val optimalActs: Vector[Action] = {
    actionTrueRewards
      .filter { case (r, _) => r == maxReward }
      .map { case (_, a) => a }
  }
}