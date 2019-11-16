package com.robertjneal.rl

import com.robertjneal.rl.Types._

trait Environment(val possibleStateActions: Map[State, Vector[Action]]) {
  def act(s: State, a: Action): (Reward, State)
  def isOptimal(s: State, a: Action): OptimalAct
}

final val OneState = State("OneState")

/*
 * A bandit only has a single state, so we ignore state most of the time.
 */
case class BanditEnvironment(
  val possibleActions: Vector[Action],
  actionRewards: Action => RandomReward
  ) extends Environment(Map(OneState -> possibleActions)) {
  require(possibleStateActions.size == 1)

  def act(s: State, a: Action): (Reward, State) = {
    (actionRewards(a).sample, s)
  }

  // TODO: memoize
  def isOptimal(s: State, a: Action): OptimalAct = {
    OptimalAct(optimalActs.contains(a))
  }

  private def optimalActs: Vector[Action] = {
    val actionTrueRewards: Vector[(Reward, Action)] = possibleActions.map(a => (actionRewards(a).trueReward, a))
    val maxReward: Reward = Reward(actionTrueRewards.map(_._1.toDouble).max)
    actionTrueRewards.filter(_._1 == maxReward).map(_._2)
  }
}