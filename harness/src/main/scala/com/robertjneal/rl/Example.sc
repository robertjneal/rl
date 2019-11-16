import com.robertjneal.rl._
import com.robertjneal.rl.Types._
import org.apache.commons.math3.distribution._
import scala.collection.mutable

val actionRewards: Map[Action, RandomReward] = Map(
  (Action.fromString("A"), RandomReward(new NormalDistribution(1, .2))),
  Action.fromString("B") -> RandomReward(new NormalDistribution(1.2, .2)),
  Action.fromString("C") -> RandomReward(new NormalDistribution(.9, .2))
)

def reward(a: Action): RandomReward = {
  actionRewards(a)
}

val e = BanditEnvironment(
  actionRewards.keys.toVector,
  reward
)

val agent = TabularAgent(
  e,
  OneState,
  (m: mutable.Map[Action, Reward]) => m.keys.toList(scala.util.Random.nextInt(m.size)),
  (m: mutable.Map[Action, Reward], a: Action, r: Reward, s:Step) => println(s"$a, $r, $s"),
  true
)

Range(0, 1000).foreach(_ => agent.act)

import breeze.plot._

val f = breeze.plot.Figure()
val p = f.subplot(0)
p.legend = true
p += plot(
  x = Range(0, agent.history.size).map(_.toDouble).toSeq, 
  y = agent.history.map((_, reward) => reward.toDouble).toSeq, 
  name = "run 1")
f.saveas("reward.png")
