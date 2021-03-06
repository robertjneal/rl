# rl
Scala reinforcement learning (RL) framework

This is code I'm using to play around with different reinforcement learning and n-armed bandit algorithms.
I'm also using it to mess around with Dotty/Scala 3. Someone else might find it useful. I plan to work through 
Barto and Sutton's book using this library and repository.

For now I have the Breeze jars committed to the repo because I had to compile them with my version of the JVM 
to get them to work. I'll try to fix that at some point. Feel free to add issues or open PRs. Though I don't 
have a formal process in place right now.

[How Scala 3 Features Are Used Herein](https://github.com/robertjneal/rl/blob/master/scala3/README.md)

### The Model

Part of the goal of this package is to have a reasonable model of the RL problem that can work for a variety of RL implementations. The model is currently implemented as per this diagram:

![RL Model](https://github.com/robertjneal/rl/blob/master/model.png?raw=true)

You can find the model in the [harness project](https://github.com/robertjneal/rl/tree/master/harness).

### How to Use the Code

You need Dotty. So [follow this process](https://dotty.epfl.ch) to install Dotty. Then you can use this like 
any other Scala app that uses sbt. For example, you can run `sbt console` from the `/book/` directory. In 
console, you can try running something like `bartosutton.exercise.two.figure2dot2(generatePlots = true)`.

In the future I'll include some examples on how to use the framework and various algorithms. Though for now, a 
good example can be found in the function `bartosutton.exercise.two.figure2dot2`. As I add more algorithms, 
I'll move them around to be consumed more like a library.

#### How to use various pieces of the framework:
* [The 10-armed testbed](https://github.com/robertjneal/rl/blob/master/harness/src/main/scala/com/robertjneal/rl/testbed/README.md)

### The Book

I've started putting the output of the code for the book's figures and exercises in the repo. As I add them I'll update the list.

* [Chapter 2](https://github.com/robertjneal/rl/blob/master/book/src/main/scala/bartosutton/exercise/two/two.md)
* [Chapter 4](https://github.com/robertjneal/rl/blob/master/book/src/main/scala/bartosutton/exercise/four/four.md)

### Some Features of Dotty Used in this Library

#### Opaque Types

This library leverages [Opaque Types](https://dotty.epfl.ch/docs/reference/other-new-features/opaques.html) pretty heavily. Opaque Types have a lot of nice features. See for example, [harness/src/main/scala/com/robertjneal/rl/types/Reward.scala](https://github.com/robertjneal/rl/blob/master/harness/src/main/scala/com/robertjneal/rl/types/Reward.scala). In addition to being able to provide type safety at compile time without overhead costs of boxing, it allows for easily adding operations. It would be nice if the Opaque Type could "inherit" the operations of the type it is aliasing, but still a wonderful feature nonetheless, especially in libraries where there are a lot of numeric operations, like a reinforcement library. Without Opaque Types we would need to either have classes for each type (or Value Classes in Scala 2) or use the primitive types. The former has a lot of overhead, and the latter is confusing.

#### Union Types

Because Opaque Types cannot inherit from other types, union types come in handy when you want to restrict a class with a type
parameter to some number of Opaque Types. You can see an example in the [Updaters class](https://github.com/robertjneal/rl/blob/master/harness/src/main/scala/com/robertjneal/rl/updater/Updaters.scala#L7) which uses the Union Type [Goal](https://github.com/robertjneal/rl/blob/master/harness/src/main/scala/com/robertjneal/rl/types/goal/Goal.scala).

#### Trait Parameters

I also take advantage of [trait parameters](https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html). For example, see [harness/src/main/scala/com/robertjneal/rl/Environment.scala](https://github.com/robertjneal/rl/blob/master/harness/src/main/scala/com/robertjneal/rl/Environment.scala#L5). 

#### Parameter Untupling

You will also find [parameter untupling](https://dotty.epfl.ch/docs/reference/other-new-features/parameter-untupling.html) preferred as in [harness/src/main/scala/com/robertjneal/rl/Environment.scala](https://github.com/robertjneal/rl/blob/master/harness/src/main/scala/com/robertjneal/rl/Environment.scala#L37).
