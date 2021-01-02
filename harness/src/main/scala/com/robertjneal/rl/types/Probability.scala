package com.robertjneal.rl.types

// Probability type inspired by https://docs.scala-lang.org/sips/opaque-types.html
opaque type Probability = Double
object Probability {
  import scala.util.Random

  def apply(n: Double): Option[Probability] =
    if (0.0 <= n && n <= 1.0) Some(n) else None

  def unsafe(p: Double): Probability = {
    require(0.0 <= p && p <= 1.0, s"probabilities lie in [0, 1] (got $p)")
    p
  }

  def evenProbability(n: Int): Probability = {
    if (n < 1) Probability.Never
    else if (n == 1) Probability.Certain
    else {
      Probability.unsafe(1 / n.toDouble)
    }
  }

  def asDouble(p: Probability): Double = p

  val Never: Probability = 0.0
  val CoinToss: Probability = 0.5
  val Certain: Probability = 1.0

  def random(using random: Random): Probability = random.nextDouble

  implicit val ordering: Ordering[Probability] =
    implicitly[Ordering[Double]]

  implicit class ProbabilityOps(p1: Probability) extends AnyVal {
    def unary_~ : Probability = Certain - p1
    def &(p2: Probability): Probability = p1 * p2
    def |(p2: Probability): Probability = p1 + p2 - (p1 * p2)
    def >(p2: Probability): Boolean = p1 > p2
    def <(p2: Probability): Boolean = p1 < p2
    def +(p2: Probability): Probability = p1 + p2
    def /(p2: Probability): Probability = p1 / p2
    def *(double: Double): Double = p1 * double
    def -(p2: Probability): Probability = p1 - p2

    def isImpossible: Boolean = p1 == Never
    def isCertain: Boolean = p1 == Certain

    def wonLottery(r: Random = Random): Boolean = r.nextDouble <= p1
    def toDouble: Double = p1
    def inverse: Probability = 1 - p1
    override def toString: String = p1.toString
  }

  def pickWithProbabilty[T](
      p: Probability,
      items: List[(T, Probability)],
      cumulativeProbabilities: Probability = Probability.Never
  ): T = {
    val hd :: tl = items
    val (item, prob) = hd
    val acc = cumulativeProbabilities + prob
    if (acc > p) item
    else pickWithProbabilty(p, tl, acc)
  }

}
