
package bartosutton.exercise.two

import com.robertjneal.rl.types._
import com.robertjneal.rl.types.goal._
import org.apache.commons.math3.distribution._

class NonstationaryReward(val trueReward: Reward) extends RandomReward {
  val random = new NormalDistribution(0, 1)
  val randomStepper = new NormalDistribution(0D, 0.01)

  private def updatedTrueReward: Reward = {
    trueReward + Reward(randomStepper.sample)
  }

  override def sample: (Reward, RandomReward) = {
    val sampler = new NormalDistribution(updatedTrueReward.toDouble, 1)
    (Reward(sampler.sample), NonstationaryReward(updatedTrueReward))
  }
}

object NonstationaryReward {
  def randomStart = {
    NonstationaryReward(Reward((new NormalDistribution(0, 1)).sample))
  }
}