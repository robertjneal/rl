package com.robertjneal.rl.types

// Probability type from https://docs.scala-lang.org/sips/opaque-types.html
opaque type Probability = Double
object Probability {
  import scala.util.Random

  def apply(n: Double): Option[Probability] =
    if (0.0 <= n && n <= 1.0) Some(n) else None

  def unsafe(p: Double): Probability = {
    require(0.0 <= p && p <= 1.0, s"probabilities lie in [0, 1] (got $p)")
    p
  }

  def evenProbabilities(n: Int): List[Probability] = {
    if (n < 1) List.empty
    else if (n == 1) List(Probability.Certain)
    else {
      val each = 1 / n.toDouble
      val nMinusOne = List.fill(n - 1)(each)
      (1 - nMinusOne.sum) +: nMinusOne
    }
  }

  def asDouble(p: Probability): Double = p

  val Never: Probability = 0.0
  val CoinToss: Probability = 0.5
  val Certain: Probability = 1.0

  def random: Probability = Random.nextDouble

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

    def isImpossible: Boolean = p1 == Never
    def isCertain: Boolean = p1 == Certain

    def wonLottery(r: Random = Random): Boolean = r.nextDouble <= p1
    def toDouble: Double = p1
    def inverse: Probability = 1 - p1
    override def toString: String = p1.toString
  }
}