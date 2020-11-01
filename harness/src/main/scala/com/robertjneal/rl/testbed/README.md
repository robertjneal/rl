## How to use the testbed 

In 2.3 of the Barto, Sutton book they introduce a 10-armed testbed. This is 
used to compare various n-armed bandit algorithms. That is, you have the 
same agent and environment with some number of arms having different rewards 
and you can use it to compare various algorithms on this same problem. 
The testbed can also be changed to see how various algorithms perform in 
variations of the testbed.

This package includes what's needed to perform the tests in Chapter 2 as 
well as others. First, it includes a function `tenArmEnvironment` that 
generates a 10-armed environment with arms that have rewards per the 
specifications of the book, viz. each arm has a reward sampled from N(0, 1) 
and the reward is generated from N(r, 1) where r is set by the sample from 
N(0, 1).

The `tenArmEnvironment` then can be passed to the constructor of an `Agent`. 
So, suppose you wanted to compare two algorithms, you could generate the 
environment as decribed and then instantiate one agent with this environment 
and algorithm A, then instantiate another agent with this environment and 
algorithm B. It might look something like this:

``` scala
import breeze.linalg.DenseVector
import com.robertjneal.rl._
import com.robertjneal.rl.actionselector._
import com.robertjneal.rl.agent._
import com.robertjneal.rl.testbed._
import com.robertjneal.rl.types._
import com.robertjneal.rl.updater._

// Set up the environment
val e = tenArmEnvironment()

// Create the first agent
val εGreedyAgent = TabularAgent.rewardBlankSlate(
    e, 
    εGreedy(Probability.unsafe(0.1)), 
    average(sampleAverage), 
    recordHistory=true
)

// Create the second agent
val ucbAgent = TabularAgent.rewardBlankSlate(
    e, 
    upperConfidenceBound(2, OneState), 
    average(sampleAverage), 
    recordHistory=true
)

// Run the first agent 2000 times with 1000 steps for each run
val εGreedyMeansOptimals = testbed.run(
    εGreedyAgent,
    runs = 2000,
    steps = 1000
)

// Run the second agent 2000 times with 1000 steps for each run
val ucbMeansOptimals = testbed.run(
    ucbAgent,
    runs = 2000,
    steps = 1000
)

// Collect the results for rewards
val meanRewards: Seq[(String, DenseVector[Double])] = 
    Vector(
        ("εGreedy", εGreedyMeansOptimals.meanRewards),
        ("UCB", ucbMeansOptimals.meanRewards)
    )

// Generate the graph for mean rewards
testbed.generatePlot(meanRewards.toMap, "", s"mean-rewards", "mean reward")

// Collect the results for acts
val optimalActs: Seq[(String, DenseVector[Double])] = 
    Vector(
        ("εGreedy", εGreedyMeansOptimals.optimalActs),
        ("UCB", ucbMeansOptimals.optimalActs)
    )

// Generate the graph for optimal acts
testbed.generatePlot(optimalActs.toMap, "", s"optimal-acts", "optimal acts", percentage=true)
```

This generates two plots: one comparing the average means for the two algorithms, 
and the other comparing the percentage of optimal actions for the two algorithms.
You can easily run the code above by pasting it into the `sbt console` prompt.